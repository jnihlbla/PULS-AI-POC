001200 ID DIVISION.                                                             
001400 PROGRAM-ID.     W1111400.                                                
001500 AUTHOR.         STEFAN KIHLBERG.                                         
001600 DATE-WRITTEN.   93/08/02.                                                
001610 DATE-COMPILED.                                                           
001700                                                                          
002000*    FUNKTION:                                                            
002010*                                                                         
002020*        GEMENSAMMA ARTIKLAR                                              
002030*                                                                         
002100*        LÄSER INFIL W11113, ARTIKLAR DÄR FLGEMART SKALL                  
002110*        UPPDATERAS.                                                      
002120*                                                                         
002200*        UPPDATERAR 'FLGEMART' PÅ WDK611                                  
002300*                                                                         
002410*        PROGRAMMET UPPDATERAR WLARTC (WDK6)                              
002500*                                                                         
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     SKIP2                                                                
003400 INPUT-OUTPUT SECTION.                                                    
003500                                                                          
003600 FILE-CONTROL.                                                            
003701     SKIP2                                                                
003702*          --- ARTIKLAR DÄR FLGEMART SKALL UPPDATERAS                     
003710     SELECT W11113                     ASSIGN TO W11114D1.                
003900     EJECT                                                                
004000 DATA DIVISION.                                                           
004100     SKIP3                                                                
004200 FILE SECTION.                                                            
004301     SKIP3                                                                
004302 FD  W11113                                                               
004303     RECORDING       F                                                    
004304     BLOCK CONTAINS  0.                                                   
004305     SKIP2                                                                
004310*01  -COPY W11113      -L.                                                
004400     EJECT                                                                
004500 WORKING-STORAGE SECTION.                                                 
004600                                                                          
004601                                                                          
004610*    -- CHECKED BY WY2000                                                 
004700 77  IDPGM                       PIC X(8)    VALUE 'W1111400'.            
004800 01  CHKP-VAR.                                                            
004900 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
005000 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
005100 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
005200 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
005300 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
005400 03  CHKP-MAX                    PIC S9(3)   VALUE +100.                  
005500 77  JA                          PIC X       VALUE 'J'.                   
005600 77  NEJ                         PIC X       VALUE 'N'.                   
005700                                                                          
005800 01  FELTEXT.                                                             
005900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006201                                                                          
006202 77  W11113-EOF-SW               PIC X       VALUE 'N'.                   
006210     88  END-OF-W11113                       VALUE 'J'.                   
006500     SKIP3                                                                
007200 01  DYNAMISKA-SUBPROGRAM.                                                
007300*                                                                         
007400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007610     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007701     EJECT                                                                
007702*    --- PARAMETRAR TILL POSTSUM                                          
007703*                                                                         
007710*01  -COPY W0005   -PRE  POSTSUM-                                         
008001     EJECT                                                                
008002 01  W11113-AREA-START           PIC X(24)   VALUE                        
008003                                             'W11113-AREA-START'.         
008005                                                                          
008010*01  AREA -COPY W11113     -PRE W11113-                                   
008100*                                                                         
008200     EJECT                                                                
008300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008400                                                                          
008500 01  NYCKLAR-TILL-DLI.                                                    
008601     03  W-IDARTNR-X.                                                     
008602         05  W-IDARTNR           PIC S9(09)   VALUE ZERO COMP-3.          
008700     SKIP2                                                                
008800*    --- STATUS-KOD FRÅN IMS                                              
008900 01  STATUS-WS                   PIC XX.                                  
009000     88  SEGMENT-FINNS                       VALUE '  '.                  
009200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009400     88  IMS-EJ-OK                           VALUE 'XD'.                  
009500     SKIP2                                                                
009600 01  GODK-STATUSKODER.                                                    
009700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009800     SKIP3                                                                
009900 01  SSA1                        PIC X(64).                               
010100     EJECT                                                                
010200*    --- IMS FUNKTIONSKODER                                               
010300*01  -COPY W0003                                                          
010400     EJECT                                                                
010600*    ---  DLI INPUT-OUTPUT AREA                                           
010700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
010800     SKIP3                                                                
010900 01  DLI-IO-AREA.                                                         
011000     03  IO-AREA                 PIC X(900)  VALUE SPACE.                 
011101     SKIP3                                                                
011102     03  WLARTC01 REDEFINES IO-AREA.                                      
011103*        05  -COPY WDK601                                                 
011104     SKIP3                                                                
011105     03  WLARTC11 REDEFINES IO-AREA.                                      
011110*        05  -COPY WDK611                                                 
011200*    ---  DLI INPUT-OUTPUT AREA                                           
012100     EJECT                                                                
012200 LINKAGE SECTION.                                                         
012400*01  -COPY W0009   -PRE MSG-                                              
012501                                                                          
012502*01  -COPY W0008  -PRE ARTC-                                              
012510     05  FILLER                  PIC X.                                   
012901 PROCEDURE DIVISION  USING MSG-PCB ARTC-PCB.                              
012902 MAIN SECTION.                                                            
012910     ENTRY 'DLITCBL' USING MSG-PCB ARTC-PCB.                              
013000                                                                          
013300     PERFORM A-INIT                                                       
013410     PERFORM S01-LAES-W11113                                              
013500     PERFORM UNTIL END-OF-W11113                                          
013600       IF CHKP-ANT > CHKP-MAX                                             
013700         PERFORM X-TAG-CHECKPOINT                                         
013800       END-IF                                                             
014000       MOVE W11113-IDARTNR TO W-IDARTNR                                   
014100       PERFORM IMS-GET-ARTC01                                             
014200       IF SEGMENT-FINNS                                                   
014300          PERFORM IMS-GET-ARTC11                                          
014310          IF SEGMENT-FINNS                                                
014400             MOVE W11113-FLGEMART TO CLAG-FLGEMART                        
014500             PERFORM IMS-REPL-ARTC11                                      
014501             ADD +1 TO CHKP-ANT                                           
014503          END-IF                                                          
014504       END-IF                                                             
014510       PERFORM S01-LAES-W11113                                            
014600     END-PERFORM                                                          
014700                                                                          
014800                                                                          
014900     PERFORM Z-FINIT                                                      
015000                                                                          
015100     MOVE ZERO TO RETURN-CODE                                             
015200     GOBACK                                                               
015300     .                                                                    
015400     EJECT                                                                
015500 A-INIT SECTION.                                                          
015700                                                                          
015800     PERFORM IMS-RESTART                                                  
016001                                                                          
016010     OPEN INPUT W11113                                                    
016300                                                                          
016600                                                                          
016710     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
017000     .                                                                    
017200     EJECT                                                                
017300 Z-FINIT SECTION.                                                         
017400                                                                          
017810     CLOSE W11113                                                         
018001                                                                          
018002     MOVE 'S' TO POSTSUM-OPKOD                                            
018010     CALL POSTSUM USING POSTSUM-PARM                                      
018200     .                                                                    
018301     EJECT                                                                
018302 S01-LAES-W11113  SECTION.                                                
018303                                                                          
018304     READ W11113 INTO W11113-AREA                                         
018305     AT END                                                               
018307        SET END-OF-W11113 TO TRUE                                         
018308                                                                          
018309     NOT AT END                                                           
018310        MOVE 'W11113' TO POSTSUM-FDNAMN                                   
018311        MOVE 'W11114D1' TO POSTSUM-DDNAMN2                                
018313        CALL POSTSUM USING POSTSUM-PARM                                   
018314                                                                          
018316     END-READ                                                             
018320     .                                                                    
018600     EJECT                                                                
018700 X-TAG-CHECKPOINT   SECTION.                                              
018800                                                                          
019400     PERFORM IMS-CHECKPOINT                                               
019500     MOVE ZERO TO CHKP-ANT                                                
019700     .                                                                    
019800     EJECT                                                                
019900* --- IMS SEKTIONER ---                                                   
020000                                                                          
020102 IMS-GET-ARTC01 SECTION.                                                  
020103                                                                          
020104     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
020105          DELIMITED BY SIZE INTO SSA1                                     
020106     MOVE '  GE' TO GODK-STATUSKODER                                      
020107     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
020108     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
020109     PERFORM IMS-STATUSKONTROLL                                           
020110                                                                          
020111     .                                                                    
020112     SKIP3                                                                
020113 IMS-GET-ARTC11 SECTION.                                                  
020114                                                                          
020115     MOVE 'WLARTC11 ' TO SSA1                                             
020117     MOVE '  GE' TO GODK-STATUSKODER                                      
020118     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA SSA1                    
020119     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
020120     PERFORM IMS-STATUSKONTROLL                                           
020121     .                                                                    
020122                                                                          
020123     SKIP3                                                                
020124 IMS-REPL-ARTC11 SECTION.                                                 
020125                                                                          
020126     MOVE '  ' TO GODK-STATUSKODER                                        
020127     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA                         
020128     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
020129     PERFORM IMS-STATUSKONTROLL                                           
020130     .                                                                    
020200     EJECT                                                                
020300 IMS-RESTART SECTION.                                                     
020400                                                                          
020500     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
020600     MOVE '  ' TO GODK-STATUSKODER                                        
020700     CALL CBLTDLI USING XRST MSG-PCB                                      
020800                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
020900                        CHKP-AREA-LENGTH CHKP-AREA                        
021000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
021100     PERFORM IMS-STATUSKONTROLL                                           
021200     .                                                                    
021300     EJECT                                                                
021400 IMS-CHECKPOINT SECTION.                                                  
021500                                                                          
021600     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
021700     MOVE '  XD' TO GODK-STATUSKODER                                      
021800     CALL CBLTDLI USING CHKP MSG-PCB                                      
021900                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
022000                        CHKP-AREA-LENGTH CHKP-AREA                        
022100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
022200     PERFORM IMS-STATUSKONTROLL                                           
022300                                                                          
022400     IF IMS-EJ-OK                                                         
022500       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
022600       DISPLAY FELTEXT                                                    
022700       CALL FELLOG                                                        
022800     END-IF                                                               
022900     .                                                                    
023000     EJECT                                                                
023100 IMS-STATUSKONTROLL SECTION.                                              
023200                                                                          
023300     SET STATUS-IX TO 1                                                   
023400     SEARCH GODK-STATUS                                                   
023500       AT END                                                             
023600         MOVE 'FEL STATUS' TO FELTEXT-STR                                 
023700         DISPLAY FELTEXT                                                  
023800         CALL FELLOG                                                      
023900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
024000         CONTINUE                                                         
024100     END-SEARCH                                                           
024200     .                                                                    
