000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2351000.                                                
000400*AUTHOR.         JOHAN NIHLBLAD.                                          
000500*DATE-WRITTEN.   JAN 2005.                                                
000600                                                                          
000700*    REMARKS                                                              
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        SKAPAR LISTA MED LEV.BESK. SOM SELEKTERATS PÅ BILD 2323.         
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
003000     SELECT W23508                     ASSIGN TO W23510D1.                
003001*          ---                                                            
003010     SELECT W23511                     ASSIGN TO W23510D2.                
003020*          --- UT-FIL                                                     
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP3                                                                
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
004010 FD  W23508                                                               
004020     LABEL RECORD STANDARD                                                
004030     RECORDING V                                                          
004040     BLOCK CONTAINS 0.                                                    
004050                                                                          
004051*01  POST -COPY W23508  -L.                                               
004061                                                                          
004070 FD  W23511                                                               
004080     LABEL RECORD STANDARD                                                
004090     RECORDING F                                                          
004091     BLOCK CONTAINS 0.                                                    
004092                                                                          
004093 01  UT-POST                 PIC X(422).                                  
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300     SKIP2                                                                
004301                                                                          
004310*    -- CHECKED BY WY2000                                                 
004400 77  IDPGM                       PIC X(8)    VALUE 'W2351000'.            
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
005200 77  W23508-EOF-SW               PIC X       VALUE 'N'.                   
005300     88  END-OF-W23508                       VALUE 'J'.                   
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
007400*01  AREA -COPY W23508     -PRE IN-                                       
007600*                                                                         
007760                                                                          
007770 01  UT-AREA-1.                                                           
007780     03  FILLER               PIC X(4)    VALUE 'PROC'.                   
007790     03  FILLER               PIC X       VALUE X'05'.                    
007791     03  FILLER               PIC X(5)    VALUE 'SUPPL'.                  
007792     03  FILLER               PIC X       VALUE X'05'.                    
007796     03  FILLER               PIC X(7)    VALUE 'PART NO'.                
007797     03  FILLER               PIC X       VALUE X'05'.                    
007798     03  FILLER               PIC X(8)    VALUE 'EXT TEXT'.               
007799     03  FILLER               PIC X       VALUE X'05'.                    
007800     03  FILLER               PIC X(16)   VALUE                           
007801                                          'TEXT INFO ENDING'.             
007802     03  FILLER               PIC X       VALUE X'05'.                    
007803     03  FILLER               PIC X(13)   VALUE 'DELIVERY INFO'.          
007804     03  FILLER               PIC X       VALUE X'05'.                    
007805     03  FILLER               PIC X(12)   VALUE 'PROMISED QTY'.           
007806     03  FILLER               PIC X       VALUE X'05'.                    
007807     03  FILLER               PIC X(10)   VALUE 'PREADVICED'.             
007808     03  FILLER               PIC X       VALUE X'05'.                    
007809     03  FILLER               PIC X(9)    VALUE 'STOCK BAL'.              
007810     03  FILLER               PIC X       VALUE X'05'.                    
007811     03  FILLER               PIC X(2)    VALUE 'AK'.                     
007812     03  FILLER               PIC X       VALUE X'05'.                    
007813     03  FILLER               PIC X(2)    VALUE 'BO'.                     
007814     03  FILLER               PIC X       VALUE X'05'.                    
007815     03  FILLER               PIC X(13)   VALUE 'NEXT CALL OFF'.          
007816     03  FILLER               PIC X       VALUE X'05'.                    
007817     03  FILLER               PIC X(8)    VALUE 'QUANTITY'.               
007818     03  FILLER               PIC X       VALUE X'05'.                    
007821     03  FILLER               PIC X(9)    VALUE 'AGREEMENT'.              
007822     03  FILLER               PIC X       VALUE X'05'.                    
007823     03  FILLER               PIC X(8)    VALUE 'SUP CODE'.               
007824     03  FILLER               PIC X       VALUE X'05'.                    
007825     EJECT                                                                
007826 01  UT-AREA-2.                                                           
007827     03  UT-IDANSK            PIC Z(3)    VALUE ZERO.                     
007828     03  FILLER               PIC X       VALUE X'05'.                    
007829     03  UT-IDLEVNR           PIC X(5)    VALUE SPACE.                    
007830     03  FILLER               PIC X       VALUE X'05'.                    
007832     03  UT-IDARTNR           PIC Z(8)9   VALUE ZERO.                     
007833     03  FILLER               PIC X       VALUE X'05'.                    
007834     03  UT-TELEVBSK-COMP     PIC X(323)  VALUE SPACE.                    
007835     03  FILLER               PIC X       VALUE X'05'.                    
007836     03  UT-TIBORT            PIC Z(7)    VALUE ZERO.                     
007837     03  FILLER               PIC X       VALUE X'05'.                    
007838     03  UT-DALEVBSK-AVS      PIC Z(7)9   VALUE ZERO.                     
007839     03  FILLER               PIC X       VALUE X'05'.                    
007840     03  UT-KVAVIS-BSKKVAR    PIC Z(6)9   VALUE ZERO.                     
007841     03  FILLER               PIC X       VALUE X'05'.                    
007842     03  UT-KVART-FORAVIS     PIC Z(7)    VALUE ZERO.                     
007843     03  FILLER               PIC X       VALUE X'05'.                    
007844     03  UT-KVLS              PIC Z(7)    VALUE ZERO.                     
007845     03  FILLER               PIC X       VALUE X'05'.                    
007846     03  UT-KVAKS             PIC Z(7)    VALUE ZERO.                     
007847     03  FILLER               PIC X       VALUE X'05'.                    
007848     03  UT-KVROS             PIC Z(7)    VALUE ZERO.                     
007849     03  FILLER               PIC X       VALUE X'05'.                    
007850     03  UT-AVROPS-DAT        PIC Z(7)    VALUE ZERO.                     
007851     03  FILLER               PIC X       VALUE X'05'.                    
007852     03  UT-KVAVROP           PIC Z(7)    VALUE ZERO.                     
007853     03  FILLER               PIC X       VALUE X'05'.                    
007854     03  UT-KDAVT             PIC Z(1)    VALUE ZERO.                     
007855     03  FILLER               PIC X       VALUE X'05'.                    
007856     03  UT-KDERS             PIC Z(2)9   VALUE ZERO.                     
007857     03  FILLER               PIC X       VALUE X'05'.                    
007858     EJECT                                                                
007860 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007900     SKIP3                                                                
012200 PROCEDURE DIVISION.                                                      
012400                                                                          
012600     PERFORM A-INIT                                                       
012700     PERFORM S01-LAES-W23508                                              
012800     PERFORM UNTIL END-OF-W23508                                          
012820                                                                          
012900       PERFORM B-BEHANDLA-POSTER                                          
013000       PERFORM S01-LAES-W23508                                            
013060                                                                          
013100     END-PERFORM                                                          
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
014100     OPEN INPUT  W23508                                                   
014110     OPEN OUTPUT W23511                                                   
014120     WRITE UT-POST FROM UT-AREA-1                                         
014200                                                                          
014300                                                                          
014400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014500     .                                                                    
014600     EJECT                                                                
014700 B-BEHANDLA-POSTER SECTION.                                               
014800                                                                          
014810     MOVE IN-IDANSK          TO UT-IDANSK                                 
014820     MOVE IN-IDLEVNR         TO UT-IDLEVNR                                
014900     MOVE IN-IDARTNR         TO UT-IDARTNR                                
015010     MOVE IN-TELEVBSK-COMP   TO UT-TELEVBSK-COMP                          
015020     MOVE IN-TIBORT          TO UT-TIBORT                                 
015030     MOVE IN-DALEVBSK-AVS    TO UT-DALEVBSK-AVS                           
015031     MOVE IN-KVAVIS-BSKKVAR  TO UT-KVAVIS-BSKKVAR                         
015032     MOVE IN-KVART-FORAVIS   TO UT-KVART-FORAVIS                          
015033     MOVE IN-KVLS            TO UT-KVLS                                   
015034     MOVE IN-KVAKS           TO UT-KVAKS                                  
015035     MOVE IN-KVROS           TO UT-KVROS                                  
015040     MOVE IN-AVROPS-DAT      TO UT-AVROPS-DAT                             
015050     MOVE IN-KVAVROP         TO UT-KVAVROP                                
015060     MOVE IN-KDAVT           TO UT-KDAVT                                  
015070     MOVE IN-KDERS           TO UT-KDERS                                  
015081     PERFORM S02-SKRIV-W23511                                             
015090     .                                                                    
015100     EJECT                                                                
015200                                                                          
015500 Z-FINIT SECTION.                                                         
015700                                                                          
015800     CLOSE W23508                                                         
015810           W23511                                                         
015900     SKIP2                                                                
016000     MOVE 'S' TO POSTSUM-OPKOD                                            
016100     CALL POSTSUM USING POSTSUM-PARM                                      
016101                                                                          
016200     .                                                                    
016300     EJECT                                                                
016400 S01-LAES-W23508  SECTION.                                                
016500     SKIP2                                                                
016600     READ W23508 INTO IN-AREA                                             
016700     AT END                                                               
016900        SET END-OF-W23508 TO TRUE                                         
017000                                                                          
017100     NOT AT END                                                           
017200        MOVE 'W23508' TO POSTSUM-FDNAMN                                   
017300        MOVE 'W23508D1' TO POSTSUM-DDNAMN2                                
017500        CALL POSTSUM USING POSTSUM-PARM                                   
017600     END-READ                                                             
017700     .                                                                    
017800     EJECT                                                                
017810 S02-SKRIV-W23511 SECTION.                                                
017820                                                                          
017830     WRITE UT-POST FROM UT-AREA-2                                         
017840     .                                                                    
017850     EJECT                                                                
