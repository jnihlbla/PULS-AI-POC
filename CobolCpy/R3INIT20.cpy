000010*** EDIT ALLOWED                                                          
000100*                     FILE TO SAP R/3 (FIRST RECORD)                      
000200*                                                                         
000300*                     ENGLISH EXPLANATION ACCORDING TO R/3                
000400*                                                                         
000500 01  INIT-R3.                                                             
000600     03  INIT-COMPANY-CODE    PIC X(4).                                   
000700*                          COMPANY (SEXX)                                 
000800     03  INIT-FEEDER-SYSTEM   PIC X(4).                                   
000900*                          FEEDER SYSTEM                                  
001000     03  INIT-FILE-ID         PIC X(4).                                   
001100*                          FEEDER SYSTEM, FILE IDENTIFIER                 
001200     03  INIT-USER            PIC X(12).                                  
001300*                          USER NAME                                      
001400     03  INIT-PASSWORD        PIC X(8).                                   
001500*                          PASSWORD                                       
001600     03  INIT-TIME-STAMP-DATE PIC 9(8).                                   
001700*                          TIME STAMP, DATE - YYYYMMDD                    
001800     03  INIT-TIME-STAMP-TIME PIC 9(6).                                   
001900*                          TIME STAMP, TIME - HHMMSS                      
002000     03  INIT-FILLER          PIC X(54).                                  
002100*                                                                         
002200*** END OF VILMAII-COPY LENGTH= 100 OLD LENGTH= 100                       
