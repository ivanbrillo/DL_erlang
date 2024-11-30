package org.backend;


import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

@SpringBootApplication
public class Main {

    private static Thread erlangControllerThread;   // TODO check static if make sense

    public static void main(String[] args) {

        SpringApplication.run(Main.class, args);   // TODO check if right call

        try {
            ErlangController erlangController = new ErlangController();
            erlangControllerThread = new Thread(erlangController);
            erlangControllerThread.start();
        } catch (Exception e) {
            System.err.println("Error: " + e.getMessage());
            e.printStackTrace();
        }
    }
}

