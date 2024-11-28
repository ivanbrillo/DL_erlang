package org.backend;


public class Main {
    public static void main(String[] args) {
        try {

            ErlangController erlangController = new ErlangController();
            erlangController.startErlangMaster();
            erlangController.createConnection();
            erlangController.setupErlangMaster();
            erlangController.stopErlangMaster();

        } catch (Exception e) {
            System.err.println("Error: " + e.getMessage());
        }
    }

}

