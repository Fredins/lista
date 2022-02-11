package main

import (
	"bytes"
	"database/sql"
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"os"

	_ "github.com/go-sql-driver/mysql"
	"github.com/google/uuid"
)

func jsonPrint(uglyJson []byte) {
	var prettyJson bytes.Buffer
	json.Indent(&prettyJson, uglyJson, "", "   ")
	prettyJson.WriteTo(os.Stdout)
}
func structPrint(v interface{}) {
	prettyJson, err := json.MarshalIndent(v, "", "  ")
	if err != nil {
		fmt.Println(err)
	}
	fmt.Println(string(prettyJson))
}

func database() *sql.DB {
	db, err := sql.Open("mysql", os.Getenv("DSN_LISTA"))
	if err != nil {
		log.Fatal(err)
	}
	err = db.Ping()
	if err != nil {
		log.Fatal(err)
	} else {
		fmt.Println("connected to db!")
	}
	return db
}

type Item struct {
	Description string    `json:"description"`
	Uuid        uuid.UUID `json:"uuid"`
	Completed   bool      `json:"completed"`
}

var db *sql.DB

func handleFetch(w http.ResponseWriter, r *http.Request) {
	fmt.Println("get")
	w.Header().Add("Access-Control-Allow-Origin", "*")
	rows, err := db.Query("SELECT * FROM items")
	if err != nil {
		log.Fatal(err)
	}
	defer rows.Close()

	var ii []Item

	for rows.Next() {
		var i Item
		if err := rows.Scan(&i.Description, &i.Uuid, &i.Completed); err != nil {
			log.Fatal(err)
		}
		ii = append(ii, i)
	}

	j, err := json.Marshal(ii)
	if err != nil {
		log.Fatal(err)
	}

	structPrint(ii)

	fmt.Fprint(w, string(j))
}

func clear() {
	db.Exec("DELETE FROM items")
}

func insert(ii []Item) {
	for _, i := range ii {
		uuid, err := i.Uuid.MarshalBinary()
		if err != nil {
			log.Fatal(err)
		}
		db.Exec("INSERT INTO items (description, uuid, completed) VALUES (?, ?, ?)",
			i.Description, uuid, i.Completed)
	}
}

func handlePost(w http.ResponseWriter, r *http.Request) {
	fmt.Println("get")
	w.Header().Add("Access-Control-Allow-Origin", "*")
	w.Header().Add("Access-Control-Request-Method", "POST, OPTIONS")
	w.Header().Set("Access-Control-Allow-Headers", "Content-Type, Content-Length")

	if r.Method == "OPTIONS" {
		return
	}

	var ii []Item
	decoder := json.NewDecoder(r.Body)
	decoder.Decode(&ii)
	clear()
	insert(ii)
}

func main() {
	db = database()

	http.HandleFunc("/test", func(w http.ResponseWriter, r *http.Request) {
		w.Header().Add("Access-Control-Allow-Origin", "*")

		fmt.Fprint(w, "hello world")
	})

	http.HandleFunc("/post", handlePost)
	http.HandleFunc("/fetch", handleFetch)
	http.ListenAndServe(":4000", nil)

}
