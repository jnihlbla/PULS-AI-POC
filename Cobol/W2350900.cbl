000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2350900.                                                
000400*AUTHOR.         JOHAN NIHLBLAD.                                          
000500*DATE-WRITTEN.   JAN 2005.                                                
000600                                                                          
000700*    REMARKS                                                              
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        SKAPAR LISTA MED AVROP SOM SELEKTERATS PÅ BILD 2323.             
001400*                                                                         
001600*                                                                         
001700*    ABENDKODER:                                                          
001800*        U0016 -  . . . .                                                 
001900*        U1000 -  . . . .                                                 
002000*                                                                         
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800     SKIP2                                                                
002900*          ---                                                            
003000     SELECT W23509                     ASSIGN TO W23509D1.                
003001*          ---                                                            
003010     SELECT W23510                     ASSIGN TO W23509D2.                
003020*          --- UT-FIL                                                     
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP3                                                                
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
004010 FD  W23509                                                               
004020     LABEL RECORD STANDARD                                                
004030     RECORDING V                                                          
004040     BLOCK CONTAINS 0.                                                    
004050                                                                          
004051*01  POST -COPY W23509  -L.                                               
004061                                                                          
004070 FD  W23510                                                               
004080     LABEL RECORD STANDARD                                                
004090     RECORDING V                                                          
004091     BLOCK CONTAINS 0.                                                    
004092                                                                          
004093 01  UT-POST                 PIC X(120).                                  
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300     SKIP2                                                                
004301                                                                          
004310*    -- CHECKED BY WY2000                                                 
004400 77  IDPGM                       PIC X(8)    VALUE 'W2350900'.            
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004620 77  WS-IDARTNR                  PIC S9(9)   VALUE ZERO COMP-3.           
004710 77  ANTAL-IX                    PIC S9(5)   VALUE +0.                    
004711     SKIP2                                                                
004720                                                                          
004802                                                                          
004826                                                                          
004830 01  FELTEXT.                                                             
004900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005100                                                                          
005200 77  W23509-EOF-SW               PIC X       VALUE 'N'.                   
005300     88  END-OF-W23509                       VALUE 'J'.                   
005301                                                                          
005500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005600 01  FILLER REDEFINES DAGENS-DATUM.                                       
005700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006000     EJECT                                                                
006100 01  DYNAMISKA-SUBPROGRAM.                                                
006200*                                                                         
006300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006600     EJECT                                                                
006700*    --- PARAMETRAR TILL POSTSUM                                          
006800*                                                                         
006900*01  -COPY W0005   -PRE  POSTSUM-                                         
007000     EJECT                                                                
007100 01  IN-AREA-START               PIC X(24)   VALUE                        
007200                                             'IN-AREA-START'.             
007300     SKIP2                                                                
007400*01  AREA -COPY W23509     -PRE IN-                                       
007600*                                                                         
007760                                                                          
007770 01  UT-AREA-1.                                                           
007780     03  FILLER               PIC X(4)    VALUE 'PROC'.                   
007790     03  FILLER               PIC X       VALUE X'05'.                    
007791     03  FILLER               PIC X(5)    VALUE 'SUPPL'.                  
007792     03  FILLER               PIC X       VALUE X'05'.                    
007793     03  FILLER               PIC X(13)   VALUE 'SUPPL PART NO'.          
007794     03  FILLER               PIC X       VALUE X'05'.                    
007796     03  FILLER               PIC X(7)    VALUE 'PART NO'.                
007797     03  FILLER               PIC X       VALUE X'05'.                    
007798     03  FILLER               PIC X(8)    VALUE 'CALL OFF'.               
007799     03  FILLER               PIC X       VALUE X'05'.                    
007800     03  FILLER               PIC X(8)    VALUE 'QUANTITY'.               
007801     03  FILLER               PIC X       VALUE X'05'.                    
007802     03  FILLER               PIC X(8)    VALUE 'SUP CODE'.               
007803     03  FILLER               PIC X       VALUE X'05'.                    
007805     03  FILLER               PIC X(9)    VALUE 'AGREEMENT'.              
007806     03  FILLER               PIC X       VALUE X'05'.                    
007822     EJECT                                                                
007823 01  UT-AREA-2.                                                           
007824     03  UT-IDANSK            PIC Z(2)9   VALUE ZERO.                     
007825     03  FILLER               PIC X       VALUE X'05'.                    
007826     03  UT-IDLEVNR           PIC X(5)    VALUE SPACE.                    
007827     03  FILLER               PIC X       VALUE X'05'.                    
007828     03  UT-BELEV             PIC X(35)   VALUE SPACE.                    
007829     03  FILLER               PIC X       VALUE X'05'.                    
007830     03  UT-IDARTNR           PIC Z(8)9   VALUE ZERO.                     
007831     03  FILLER               PIC X       VALUE X'05'.                    
007832     03  UT-AVROPS-DAT        PIC Z(6)9   VALUE ZERO.                     
007833     03  FILLER               PIC X       VALUE X'05'.                    
007834     03  UT-KVAVROP           PIC Z(6)9   VALUE ZERO.                     
007835     03  FILLER               PIC X       VALUE X'05'.                    
007836     03  UT-KDERS             PIC Z(2)9   VALUE ZERO.                     
007837     03  FILLER               PIC X       VALUE X'05'.                    
007838     03  UT-KDAVT             PIC 9(1)    VALUE ZERO.                     
007839     03  FILLER               PIC X       VALUE X'05'.                    
007842     EJECT                                                                
007843 01  UT-AREA-3.                                                           
007844     03  FILLER                  PIC X(33)                                
007845                       VALUE '25000 AVTAL VISAS MER AVTAL FINNS'.         
007846     EJECT                                                                
007850 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007900     SKIP3                                                                
012200 PROCEDURE DIVISION.                                                      
012400                                                                          
012600     PERFORM A-INIT                                                       
012700     PERFORM S01-LAES-W23509                                              
012710     MOVE +1 TO ANTAL-IX                                                  
012800     PERFORM UNTIL END-OF-W23509 OR ANTAL-IX > 25000                      
012820                                                                          
012900       PERFORM B-BEHANDLA-POSTER                                          
012910       ADD +1 TO ANTAL-IX                                                 
013000       PERFORM S01-LAES-W23509                                            
013060                                                                          
013100     END-PERFORM                                                          
013110     IF ANTAL-IX >= 25000                                                 
013120       WRITE UT-POST FROM UT-AREA-3                                       
013130     END-IF                                                               
013200     PERFORM Z-FINIT                                                      
013300                                                                          
013400     MOVE ZERO TO RETURN-CODE                                             
013500     GOBACK                                                               
013600     .                                                                    
013700     EJECT                                                                
013800 A-INIT SECTION.                                                          
013900     SKIP2                                                                
014000                                                                          
014001     ACCEPT DAGENS-DATUM FROM DATE                                        
014002                                                                          
014100     OPEN INPUT  W23509                                                   
014110     OPEN OUTPUT W23510                                                   
014120     WRITE UT-POST FROM UT-AREA-1                                         
014200                                                                          
014300                                                                          
014400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014500     .                                                                    
014600     EJECT                                                                
014700 B-BEHANDLA-POSTER SECTION.                                               
014800                                                                          
014900     MOVE IN-IDARTNR         TO UT-IDARTNR                                
015010     MOVE IN-BELEV           TO UT-BELEV                                  
015020     MOVE IN-IDLEVNR         TO UT-IDLEVNR                                
015030     MOVE IN-KVAVROP         TO UT-KVAVROP                                
015040     MOVE IN-AVROPS-DAT      TO UT-AVROPS-DAT                             
015050     MOVE IN-KDERS           TO UT-KDERS                                  
015060     MOVE IN-KDAVT           TO UT-KDAVT                                  
015070*    MOVE IN-KVBR            TO UT-KVBR                                   
015080     MOVE IN-IDANSK          TO UT-IDANSK                                 
015081     PERFORM S02-SKRIV-W23510                                             
015090     .                                                                    
015100     EJECT                                                                
015200                                                                          
015500 Z-FINIT SECTION.                                                         
015700                                                                          
015800     CLOSE W23509                                                         
015810           W23510                                                         
015900     SKIP2                                                                
016000     MOVE 'S' TO POSTSUM-OPKOD                                            
016100     CALL POSTSUM USING POSTSUM-PARM                                      
016101                                                                          
016200     .                                                                    
016300     EJECT                                                                
016400 S01-LAES-W23509  SECTION.                                                
016500     SKIP2                                                                
016600     READ W23509 INTO IN-AREA                                             
016700     AT END                                                               
016900        SET END-OF-W23509 TO TRUE                                         
017000                                                                          
017100     NOT AT END                                                           
017200        MOVE 'W23509' TO POSTSUM-FDNAMN                                   
017300        MOVE 'W23509D1' TO POSTSUM-DDNAMN2                                
017500        CALL POSTSUM USING POSTSUM-PARM                                   
017600     END-READ                                                             
017700     .                                                                    
017800     EJECT                                                                
017810 S02-SKRIV-W23510 SECTION.                                                
017820                                                                          
017830     WRITE UT-POST FROM UT-AREA-2                                         
017840     .                                                                    
017850     EJECT                                                                
