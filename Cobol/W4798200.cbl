000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W4798200.                                                
000400 AUTHOR.         JAN-ERIK FRANTZEN.                                       
000500 DATE-WRITTEN.   92/03/06.                                                
000510 DATE-COMPILED.                                                           
000600                                                                          
000900*    FUNKTION:                                                            
001000*        LÄSER NER WDQ4 OCH SELEKTERAR UT LITE DATA TILL EN FIL           
001100*                            SB                                           
001200*                                                                         
001300*        PROGRAMMET LÄSER    (WDQ4)                                       
001400*                                                                         
001500*                                                                         
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*          --- UTFIL                                                      
002500     SELECT W47982                     ASSIGN TO W47982D1.                
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP3                                                                
002900 FILE SECTION.                                                            
003000     SKIP3                                                                
003100 FD  W47982                                                               
003200     RECORDING       F                                                    
003300     BLOCK CONTAINS  0.                                                   
003400     SKIP2                                                                
003500*01  POST -COPY W47982 -PRE  UT-  -L.                                     
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800                                                                          
003801                                                                          
003810*    -- CHECKED BY WY2000                                                 
003900 77  IDPGM                       PIC X(8)    VALUE 'W4798200'.            
004000 77  JA                          PIC X       VALUE 'J'.                   
004100 77  NEJ                         PIC X       VALUE 'N'.                   
004110 77  W-ANTUT                     PIC S9(7)   VALUE ZERO.                  
004200     SKIP3                                                                
004210 77  PGM-POS                     PIC X(16)   VALUE SPACE.                 
004300 01  DYNAMISKA-SUBPROGRAM.                                                
004400                                                                          
004500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
004600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
004800     SKIP2                                                                
004900 01  FELTEXT.                                                             
005000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005200     EJECT                                                                
005700 01  UT-AREA-START               PIC X(24)   VALUE                        
005800                                 'UT-AREA-START  '.                       
006000                                                                          
006100*01  AREA -COPY W47982     -PRE UT-                                       
006200     EJECT                                                                
006300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
006400                                                                          
006600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
006700     SKIP3                                                                
006800*    --- STATUS-KOD FRÅN IMS                                              
006900 01  STATUS-WS                   PIC XX.                                  
007000     88  SEGMENT-FINNS                       VALUE '  '.                  
007100     88  BASEN-SLUT                          VALUE 'GB'.                  
007200     SKIP2                                                                
007300 01  GODK-STATUSKODER.                                                    
007400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
007500     SKIP3                                                                
007600 01  SSA1                        PIC X(64).                               
007800     EJECT                                                                
007900*    --- IMS FUNKTIONSKODER                                               
008000*01  -COPY W0003                                                          
008100     EJECT                                                                
008200*    ---  DLI INPUT-OUTPUT AREA                                           
008300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
008400     SKIP3                                                                
008500 01  DLI-IO-AREA.                                                         
008600*    03  -COPY WDQ401  -PRE ORQF-                                         
009000     EJECT                                                                
009100 LINKAGE SECTION.                                                         
009400*01  -COPY W0008  -PRE ORQF-                                              
009500     05  FILLER                  PIC X.                                   
009600     EJECT                                                                
009700 PROCEDURE DIVISION  USING ORQF-PCB.                                      
009710 MAIN SECTION.                                                            
009800     ENTRY 'DLITCBL' USING ORQF-PCB.                                      
009900                                                                          
010100     PERFORM A-INIT                                                       
010200     PERFORM IMS-GET-ORQF                                                 
010300     PERFORM UNTIL BASEN-SLUT                                             
010400        PERFORM B-FLYTTA-TILL-UTFIL                                       
010500        PERFORM S11-SKRIV-W47982                                          
010600        PERFORM IMS-GET-ORQF                                              
010700     END-PERFORM                                                          
010800                                                                          
010900                                                                          
011000     PERFORM Z-FINIT                                                      
011100                                                                          
011200     MOVE ZERO TO RETURN-CODE                                             
011300     GOBACK                                                               
011400     .                                                                    
011500     EJECT                                                                
011600 A-INIT SECTION.                                                          
011700                                                                          
011800     OPEN OUTPUT W47982                                                   
012100     .                                                                    
012200     EJECT                                                                
012300 B-FLYTTA-TILL-UTFIL SECTION.                                             
012400                                                                          
012500     MOVE ORQF-ORAD-IDORDER          TO UT-IDORDER                        
012600     MOVE ORQF-ORAD-IDDC             TO UT-IDDC                           
012610     MOVE ORQF-ORAD-IDDISTR          TO UT-IDDISTR                        
012620     MOVE ORQF-ORAD-IDKUNDNR         TO UT-IDKUNDNR                       
012630     MOVE ORQF-ORAD-IDKUNDRF         TO UT-IDKUNDRF                       
012640     MOVE ORQF-ORAD-IDARTNR          TO UT-IDARTNR                        
012650     MOVE ORQF-ORAD-KVBEART-Q        TO UT-KVBEART-Q                      
012660     MOVE ORQF-ORAD-KDORDKL          TO UT-KDORDKL                        
012670     MOVE ORQF-ORAD-TIREGDAT         TO UT-TIREGDAT                       
012700     MOVE ORQF-ORAD-ADLAGOMR         TO UT-ADLAGOMR                       
012710     MOVE ORQF-ORAD-IDLEVNR          TO UT-IDLEVNR                        
012720     MOVE ORQF-ORAD-VKART            TO UT-VKART                          
012730     MOVE ORQF-ORAD-VLARTNTO         TO UT-VLARTNTO                       
012740     MOVE ORQF-ORAD-PRARTNTO         TO UT-PRARTNTO                       
012752     MOVE ORQF-ORAD-PRARTNTO-LOC     TO UT-PRARTNTO-LOC                   
012771     MOVE ORQF-ORAD-PRARTNTO-LOCPREL TO UT-PRARTNTO-LOCPREL               
012772     MOVE ORQF-ORAD-KDVALISO         TO UT-KDVALISO                       
012800     MOVE ORQF-ORAD-KDPRODSL         TO UT-KDPRODSL                       
014900     .                                                                    
015000     EJECT                                                                
015100 Z-FINIT SECTION.                                                         
015200     CLOSE W47982                                                         
015300                                                                          
015400     DISPLAY 'ANTAL POSTER UT ' W-ANTUT                                   
015600     .                                                                    
015700     EJECT                                                                
015800 S11-SKRIV-W47982 SECTION.                                                
015900                                                                          
016000     WRITE UT-POST FROM UT-AREA                                           
016100                                                                          
016200     ADD +1 TO W-ANTUT                                                    
016600     .                                                                    
016700     EJECT                                                                
016800* --- IMS SEKTIONER ---                                                   
016900                                                                          
017100 IMS-GET-ORQF   SECTION.                                                  
017200                                                                          
017300     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
017400     CALL CBLTDLI USING GN ORQF-PCB DLI-IO-AREA                           
017500     MOVE ORQF-STATUS-CODE TO STATUS-WS                                   
017600     PERFORM IMS-STATUSKONTROLL                                           
017700     .                                                                    
017800     SKIP3                                                                
017900 IMS-STATUSKONTROLL SECTION.                                              
018000                                                                          
018100     SET STATUS-IX TO 1                                                   
018200     SEARCH GODK-STATUS                                                   
018300       AT END                                                             
018400         MOVE 'FEL STATUSKOD FRÅN IMS' TO FELTEXT-STR                     
018500         DISPLAY FELTEXT                                                  
018600         CALL FELLOG                                                      
018700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
018800         CONTINUE                                                         
018900     END-SEARCH                                                           
019000     .                                                                    
