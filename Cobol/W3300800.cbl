000100 ID  DIVISION.                                                            
000200     SKIP2                                                                
000300 PROGRAM-ID.    W3300800.                                                 
000400 AUTHOR.        RONNY STENHOLM                                            
000500 DATE-WRITTEN.  AUG 1989.                                                 
000600     REMARKS.                                                             
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        HÄMTAR JUSTERING SOM SKAPATS FRÅN W30158-BILDEN OCH              
001000*        LIGGER PÅ WDG2-BASEN.                                            
001100*        SKRIVER POSTERNA FRÅN BASEN WDG2 PÅ                              
001200*        EN FIL (W33009).                                                 
001300*        OBS! EFTER 200 BEHANDLADE POSTER GÖRS 'GOBACK',                  
001400*        JMF CHECKPOINT!                                                  
001500*        PERIODICITET : VECKA                                             
001600*                                                                         
001700     EJECT                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*    --- UTFILER:                                                         
002500     SELECT W33009                       ASSIGN TO W33008D1.              
002700     SKIP2                                                                
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000     SKIP2                                                                
003100 FILE SECTION.                                                            
003200     SKIP2                                                                
003300 FD  W33009                                                               
003400     LABEL RECORD   STANDARD                                              
003500     RECORDING      V                                                     
003600     BLOCK CONTAINS 0.                                                    
003700     SKIP2                                                                
003800*01  POST -COPY W330200  -PRE UTREG-  -L.                                 
004000     EJECT                                                                
005100 WORKING-STORAGE SECTION.                                                 
005200     SKIP2                                                                
005201                                                                          
005210*    -- CHECKED BY WY2000                                                 
005300 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W3300800'.            
005400 77  JA                          PIC X       VALUE 'J'.                   
005500 77  NEJ                         PIC X       VALUE 'N'.                   
005600 77  BASEN-SLUT                  PIC X       VALUE 'N'.                   
005700 77  ANTAL-REC                   PIC S9(3)   VALUE +1 COMP-3.             
005800     SKIP3                                                                
005900                                                                          
006000 01  PACKA-UPP-KORT.                                                      
006100   03  P-UPP-IDPTYP               PIC X(3).                               
006200   03  P-UPP-IDARTNR              PIC 9(9).                               
006300   03  P-UPP-TIFSGVV              PIC 9(5).                               
006400   03  P-UPP-IDDISTR              PIC 9(5).                               
006500   03  P-UPP-KVLEVART             PIC S9(7).                              
006600   03  P-UPP-PRARTNTO             PIC S9(7)V9(2).                         
006700   03  P-UPP-PRARTSJK             PIC S9(7)V9(2).                         
006800   03  P-UPP-IDUSER               PIC X(8).                               
006900                                                                          
007000******************************************************************        
007100*   NYCKLAR TILL DLI                                                      
007200******************************************************************        
007300 01  FILLER                      PIC X(16)   VALUE 'DLI-NYCKLAR'.         
007400*                                                                         
007500 01  NYCKLAR-TILL-DLI.                                                    
007600*                               WDG2-FÖRÄLDER                             
007700     03  W-3131-X.                                                        
007800         05   W-HTYP             PIC X(4)    VALUE '3131'.                
007900         05   FILLER             PIC X(26)   VALUE LOW-VALUE.             
008000 01  DYNAMISKA-SUBPROGRAM.                                                
008100*                                                                         
008200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI'.             
008400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG '.             
008500     SKIP2                                                                
008600*- - - - - - - - - - - - - - - - PARAMETRAR TILL POSTSUM                  
008700*                                                                         
008800*01  -COPY W0005 -PRE  POSTSUM-                                           
009000                                                                          
009100******************************************************************        
009200*                                                                         
009300*          ARBETSAREOR TILL IMS-SEKTIONERNA                               
009400*                                                                         
009500*01  IMS-WS.                                                              
009600     03  FILLER                  PIC X(16) VALUE 'IMS-WS     '.           
009700     SKIP3                                                                
009800*         STATUSKOD FRÅN IMS                                              
009900     03  STATUS-WS               PIC XX.                                  
010000       88  SEGMENT-FINNS                   VALUE '  '.                    
010100       88  SEGMENT-SAKNAS                  VALUE 'GE'.                    
010200     SKIP2                                                                
010300     03  GODK-STATUSKODER.                                                
010400       05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
010500     SKIP2                                                                
010600 01  SSA1                        PIC X(64).                               
010700     EJECT                                                                
010800*--------------------IMS FUNKTIONSKODER                                   
010900*01  -COPY W0003                                                          
011100     EJECT                                                                
011200 01  FILLER                       PIC X(16)   VALUE                       
011300                                            'DLI-IO-AREA    '.            
011400*--------------------DLI INPUT-OUTPUT AREA                                
011500 01  DLI-IO-AREA.                                                         
011700*  03  WL313111 -COPY WDGX3132                                            
011900                                                                          
012000 01  UT-AREA-START                PIC X(24)   VALUE                       
012100                                            'UT-AREA-START  '.            
012200     SKIP3                                                                
012300 01  UT-AREA.                                                             
012400     03  FILLER                   PIC X(40).                              
012500     SKIP2                                                                
012600*01  FILLER  -PRE UT-  -COPY W330200 -RED UT-AREA                         
012800     EJECT                                                                
012900*    ----  AREA FÖR KONTROLLPOSTER                                        
013000 01  FILLER                  PIC X(24) VALUE 'KONTROLL-AREA'.             
013100     SKIP2                                                                
013200 01  KONT-AREA.                                                           
013400*    03  FILLER -COPY W092P001 -PRE W092-.                                
013500     03 KONTROLL-KORT             PIC X(76).                              
013600     EJECT                                                                
013700 LINKAGE SECTION.                                                         
013800*01    -COPY W0009  -PRE MSG-                                             
014000     EJECT                                                                
014100*01    -COPY W0008  -PRE 3131-                                            
014300     05  FILLER                      PIC X.                               
014400     EJECT                                                                
014500 PROCEDURE DIVISION USING MSG-PCB 3131-PCB.                               
014600     ENTRY  'DLITCBL' USING MSG-PCB 3131-PCB.                             
014700     SKIP2                                                                
014800 STYR SECTION.                                                            
014900                                                                          
015000     PERFORM A-INIT                                                       
015100     PERFORM IMS-LAES-ROT                                                 
015200     PERFORM E-LAS-BAS-WDG2                                               
015300     MOVE 1 TO ANTAL-REC                                                  
015400     PERFORM UNTIL BASEN-SLUT = JA OR ANTAL-REC = 201                     
015500       PERFORM B-BEHANDLA-POST                                            
015600       PERFORM C-SKRIV-POST                                               
015700       PERFORM D-RADERA-BAS-WDG2                                          
015800       PERFORM E-LAS-BAS-WDG2                                             
015900       ADD 1 TO ANTAL-REC                                                 
016000     END-PERFORM                                                          
016100     PERFORM Z-FINIT                                                      
016200     MOVE ZERO TO RETURN-CODE                                             
016300     GOBACK                                                               
016400     .                                                                    
016500     EJECT                                                                
016600                                                                          
016700 A-INIT SECTION.                                                          
016800     SKIP2                                                                
016900     OPEN OUTPUT W33009                                                   
017100     SKIP2                                                                
017200     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
017300     .                                                                    
017400     EJECT                                                                
017500                                                                          
017600 B-BEHANDLA-POST  SECTION.                                                
017700     SKIP2                                                                
017800     MOVE '200'         TO UT-IDPTYP                                      
017900     MOVE 3132-IDARTNR  TO UT-IDARTNR                                     
018000     MOVE 3132-IDDISTR  TO UT-IDDISTR                                     
018100     MOVE 3132-DAFSGVV (3:4)  TO UT-TIFSGVV                               
018200     MOVE 3132-KVLEVART TO UT-KVLEVART                                    
018300     MOVE 3132-PRARTNTO TO UT-PRARTNTO                                    
018400     MOVE 3132-PRARTSJK TO UT-PRARTSJK                                    
018500     MOVE 3132-IDUSER   TO UT-IDUSER                                      
018600                                                                          
018700     PERFORM BA-BEHANDLA-KONTROLLPOST                                     
018800     .                                                                    
018900                                                                          
019000 BA-BEHANDLA-KONTROLLPOST  SECTION.                                       
019100     SKIP2                                                                
019200     INITIALIZE KONT-AREA                                                 
019300                                                                          
019400     MOVE '200'         TO P-UPP-IDPTYP                                   
019500     MOVE 3132-IDARTNR  TO P-UPP-IDARTNR                                  
019600     MOVE 3132-IDDISTR  TO P-UPP-IDDISTR                                  
019700     MOVE 3132-DAFSGVV (3:4)  TO P-UPP-TIFSGVV                            
019800     MOVE 3132-KVLEVART TO P-UPP-KVLEVART                                 
019900     MOVE 3132-PRARTNTO TO P-UPP-PRARTNTO                                 
020000     MOVE 3132-PRARTSJK TO P-UPP-PRARTSJK                                 
020100     MOVE 3132-IDUSER   TO P-UPP-IDUSER                                   
020200                                                                          
020300     MOVE PACKA-UPP-KORT TO KONTROLL-KORT                                 
020400     .                                                                    
020500                                                                          
020600                                                                          
020700 C-SKRIV-POST  SECTION.                                                   
020800     SKIP2                                                                
020900     WRITE UTREG-POST FROM UT-AREA                                        
021000                                                                          
021100     MOVE SPACE TO POSTSUM-TRANSTYP                                       
021200     MOVE 'W33009' TO POSTSUM-FDNAMN                                      
021300     MOVE 'W33008D1' TO POSTSUM-DDNAMN2                                   
021400     CALL POSTSUM USING POSTSUM-PARM                                      
021500                                                                          
022200     .                                                                    
022300                                                                          
022400                                                                          
022500 D-RADERA-BAS-WDG2 SECTION.                                               
022600     SKIP2                                                                
022700     PERFORM IMS-DLET-BARN                                                
022800     .                                                                    
022900     EJECT                                                                
023000                                                                          
023100 E-LAS-BAS-WDG2 SECTION.                                                  
023200     SKIP2                                                                
023300     PERFORM IMS-LAES-BARN                                                
023400     IF SEGMENT-FINNS                                                     
023500       MOVE SPACE TO POSTSUM-TRANSTYP                                     
023600       MOVE 'WDG2  ' TO POSTSUM-FDNAMN                                    
023700       MOVE 'W33008DX' TO POSTSUM-DDNAMN2                                 
023800       CALL POSTSUM USING POSTSUM-PARM                                    
023900     ELSE                                                                 
024000       MOVE JA TO BASEN-SLUT                                              
024100     END-IF                                                               
024200     .                                                                    
024300                                                                          
024400                                                                          
024500 Z-FINIT SECTION.                                                         
024600     SKIP2                                                                
024700     CLOSE W33009                                                         
024900     MOVE 'S' TO POSTSUM-OPKOD                                            
025000     CALL POSTSUM USING POSTSUM-PARM                                      
025100     .                                                                    
025200     EJECT                                                                
025300                                                                          
025400***************************************************************           
025500*    IMS-SEKTIONER                                                        
025600***************************************************************           
025700                                                                          
025800 IMS-LAES-ROT SECTION.                                                    
025900                                                                          
026000     STRING 'WL313101(WDGXKEY  =' W-3131-X ')'                            
026100             DELIMITED BY SIZE INTO SSA1                                  
026200     MOVE '  ' TO GODK-STATUSKODER                                        
026300     CALL CBLTDLI USING GU 3131-PCB DLI-IO-AREA SSA1                      
026400     MOVE 3131-STATUS-CODE TO STATUS-WS                                   
026500     PERFORM IMS-STATUSKONTROLL                                           
026600     .                                                                    
026700                                                                          
026800                                                                          
026900 IMS-LAES-BARN SECTION.                                                   
027000                                                                          
027100     MOVE 'WL313111 ' TO SSA1                                             
027200     MOVE '  GBGE' TO GODK-STATUSKODER                                    
027300     CALL CBLTDLI USING GHNP 3131-PCB DLI-IO-AREA SSA1                    
027400     MOVE 3131-STATUS-CODE TO STATUS-WS                                   
027500     PERFORM IMS-STATUSKONTROLL                                           
027600     .                                                                    
027700     EJECT                                                                
027800                                                                          
027900 IMS-DLET-BARN SECTION.                                                   
028000                                                                          
028100     MOVE 'WL313111 ' TO SSA1                                             
028200     MOVE '  ' TO GODK-STATUSKODER                                        
028300     CALL CBLTDLI USING DLET 3131-PCB DLI-IO-AREA SSA1                    
028400     MOVE 3131-STATUS-CODE TO STATUS-WS                                   
028500     PERFORM IMS-STATUSKONTROLL                                           
028600     .                                                                    
028700                                                                          
028800                                                                          
028900 IMS-STATUSKONTROLL SECTION.                                              
029000     SET STATUS-IX TO 1                                                   
029100     SEARCH GODK-STATUS AT END CALL FELLOG                                
029200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
029300     END-SEARCH                                                           
029400     .                                                                    
029500     EJECT                                                                
