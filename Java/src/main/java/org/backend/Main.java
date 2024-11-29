package org.backend;


import org.springframework.boot.autoconfigure.SpringBootApplication;

@SpringBootApplication
public class Main {
    public static void main(String[] args) {
        try {
            ErlangController erlangController = new ErlangController();
            erlangController.createConnection();
            erlangController.stopErlangMaster();
        } catch (Exception e) {
            System.err.println("Error: " + e.getMessage());
        }
    }
}

