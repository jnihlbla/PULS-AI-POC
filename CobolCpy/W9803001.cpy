000100*** EDIT ALLOWED                                                          
010000 01  W98030.                                                              
020000*                                POST FRÅN DATA MANAGER                   
030000     03  STYR-KOL-1          PIC X(1).                                    
040000*                                STYRTECKEN I POSITION 1                  
050000     03  TEST-RAD.                                                        
060000         05  TKN-KOL-2-3         PIC X(2).                                
070000*                        VÄRDET I POSITION 2 OCH 3                        
080002         05  FILLER              PIC X(249).                              
090000     03  PROCESS-RAD REDEFINES TEST-RAD.                                  
100000         05  PROCESS-NAMN        PIC X(32).                               
110000         05  FILLER              PIC X(219).                              
120000     03  PROCESS-TYP-RAD REDEFINES TEST-RAD.                              
130000         05  FILLER              PIC X(2).                                
140000         05  PROCESS-TYP.                                                 
150000             07  TYP-POS-1-3     PIC X(3).                                
160000             07  FILLER          PIC X(29).                               
170000         05  FILLER              PIC X(37).                               
180000         05  REFERS-TO           PIC X(9).                                
190000         05  FILLER              PIC X(171).                              
200000     03  INNEHALL-RAD    REDEFINES TEST-RAD.                              
210000         05  FILLER              PIC X(23).                               
220000         05  SUB-PROCESS-TYP.                                             
230000             07  SUB-TYP-POS-1-3 PIC X(3).                                
240000             07  FILLER          PIC X(29).                               
250000         05  FILLER              PIC X.                                   
260000         05  SUB-PROCESS-NAMN    PIC X(32).                               
270000         05  FILLER              PIC X(163).                              
280000     03  KATALOG-ORD-RUB-RAD REDEFINES TEST-RAD.                          
290000         05  FILLER              PIC X(2).                                
300000         05  CAT-AS              PIC X(13).                               
310000         05  FILLER              PIC X(236).                              
320000     03  KATALOG-ORD-RAD REDEFINES TEST-RAD.                              
330000         05  FILLER              PIC X(4).                                
340000         05  KATALOG-ORD.                                                 
350000             07  KAT-ORD-POS-1-3 PIC X(3).                                
360000             07  FILLER          PIC X(67).                               
370000         05  FILLER              PIC X(177).                              
380000     03  KOMMENTAR-RUB-RAD REDEFINES TEST-RAD.                            
390000         05  FILLER              PIC X(12).                               
400000         05  COMMENT             PIC X(7).                                
410000         05  FILLER              PIC X(232).                              
420000     03  KOMMENTAR-RAD REDEFINES TEST-RAD.                                
430000         05  FILLER              PIC X(45).                               
440000         05  KOMMENTAR           PIC X(206).                              
450000     03  RESURS-ORD-RUB-RAD REDEFINES TEST-RAD.                           
460000         05  FILLER              PIC X(12).                               
470000         05  SEE                 PIC X(3).                                
480000         05  FILLER              PIC X(236).                              
490000     03  RESURS-ORD-RAD REDEFINES TEST-RAD.                               
500000         05  FILLER              PIC X(45).                               
510000         05  RESURS              PIC X(206).                              
510010                                                                          
510100     03  FILLER                  PIC X(4).                                
520003*** END COPY W9803001C0  LENGTH=256   OLD LENGTH=                         
