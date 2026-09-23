001000 ID DIVISION.                                                             
001100                                                                          
001200 PROGRAM-ID.     W4887000.                                                
001300 AUTHOR.         MÅNS SAMUELSSON.                                         
001400 DATE-WRITTEN.   93/09/17.                                                
001410 DATE-COMPILED.                                                           
001500                                                                          
001800*    FUNKTION:                                                            
001900*        RENSAR BOR ARTIKAR PÅ BUFFERTREGISTRET (WDD8) SOM INTE           
002000*        LÄNGRE FINNS PÅ ARTIKELREGISTRET.                                
002100*                                                                         
002201*        PROGRAMMET UPPDATERAR WLARTD (WDD8)                              
002300*                                                                         
002400*    ABENDKODER:                                                          
002500*        U0016 -  . . . .                                                 
002600*        U1000 -  . . . .                                                 
002700*                                                                         
002800                                                                          
002900     SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100     SKIP2                                                                
003200 INPUT-OUTPUT SECTION.                                                    
003300                                                                          
003400 FILE-CONTROL.                                                            
003501     SKIP2                                                                
003502*          --- FIL MED RENSADE ARTIKLAR                                   
003510     SELECT W12207                     ASSIGN TO W48870D1.                
003520     SELECT W48870                     ASSIGN TO W48870D2.                
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900     SKIP3                                                                
004000 FILE SECTION.                                                            
004101     SKIP3                                                                
004102 FD  W12207                                                               
004103     RECORDING       F                                                    
004104     BLOCK CONTAINS  0.                                                   
004105     SKIP2                                                                
004110*01  POST -COPY W12207   -PRE  IN-  -L.                                   
004200     EJECT                                                                
004210 FD  W48870                                                               
004220     RECORDING       F                                                    
004230     BLOCK CONTAINS  0.                                                   
004240     SKIP2                                                                
004250*01  POST -COPY W4887001 -PRE  UT-  -L.                                   
004260     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400     SKIP2                                                                
004401*    -- CHECKED BY WY2000                                                 
004410     SKIP3                                                                
004500 77  IDPGM                       PIC X(8)    VALUE 'W4887000'.            
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004700 77  W-DLET-WDD801               PIC 9(7)    VALUE ZERO.                  
004800 77  W12207-EOF-SW               PIC X       VALUE 'N'.                   
004900     88  END-OF-W12207                       VALUE 'J'.                   
005000     EJECT                                                                
005100 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005200 01  FILLER REDEFINES DAGENS-DATUM.                                       
005300     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005400     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005500     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005600     EJECT                                                                
005700 01  DYNAMISKA-SUBPROGRAM.                                                
005800*                                                                         
005900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006210     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006300     SKIP2                                                                
006400*    --- PARAMETRAR TILL ABEND                                            
006500                                                                          
006600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006800     SKIP2                                                                
006900 01  FELTEXT.                                                             
007000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007201     EJECT                                                                
007202*    --- PARAMETRAR TILL POSTSUM                                          
007203*                                                                         
007210*01  -COPY W0005   -PRE  POSTSUM-                                         
007401     EJECT                                                                
007402 01  IN-AREA-START               PIC X(24)   VALUE                        
007403                                 'IN-AREA-START  '.                       
007404     SKIP2                                                                
007405                                                                          
007410*01  -COPY W12207       -PRE IN-                                          
007500     EJECT                                                                
007510 01  UT-AREA-START               PIC X(24)   VALUE                        
007520                                 'UT-AREA-START  '.                       
007530     SKIP2                                                                
007540                                                                          
007550*01  -COPY W4887001     -PRE UT-                                          
007560     EJECT                                                                
007600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007700*                                                                         
007800     EJECT                                                                
007900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008000     SKIP3                                                                
008100 01  NYCKLAR-TILL-DLI.                                                    
008201     03  W-IDARTNR-X.                                                     
008202         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008300     SKIP2                                                                
008400*    --- STATUS-KOD FRÅN IMS                                              
008500 01  STATUS-WS                   PIC XX.                                  
008600     88  SEGMENT-FINNS                       VALUE '  '.                  
008800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008900     SKIP2                                                                
009000 01  GODK-STATUSKODER.                                                    
009100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009200     SKIP3                                                                
009300 01  SSA1                        PIC X(96).                               
009400 01  SSA2                        PIC X(96).                               
009500     EJECT                                                                
009600*    --- IMS FUNKTIONSKODER                                               
009700*01  -COPY W0003                                                          
009800     EJECT                                                                
010000*    ---  DLI INPUT-OUTPUT AREA                                           
010100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
010200     SKIP3                                                                
010300 01  DLI-IO-AREA-1.                                                       
010400     03  IO-AREA-1               PIC X(150)  VALUE SPACE.                 
010501     SKIP3                                                                
010502     03  WLARTD01 REDEFINES IO-AREA-1.                                    
010503*        05  -COPY WDD801  -PRE ARTD1-                                    
010504     SKIP3                                                                
010800     EJECT                                                                
010810 01  DLI-IO-AREA-2.                                                       
010820     03  IO-AREA-2               PIC X(150)  VALUE SPACE.                 
010830     SKIP3                                                                
010840     03  WLARTD01 REDEFINES IO-AREA-2.                                    
010850*        05  -COPY WDD801  -PRE ARTD-                                     
010860     SKIP3                                                                
010870     03  WLARTD11 REDEFINES IO-AREA-2.                                    
010880*        05  -COPY WDD811  -PRE ARTD-                                     
010893     EJECT                                                                
010900 LINKAGE SECTION.                                                         
011000                                                                          
011101     EJECT                                                                
011102*01  -COPY W0008  -PRE ARTD1-                                             
011103     05  FILLER                  PIC X.                                   
011104     EJECT                                                                
011105*01  -COPY W0008  -PRE ARTD2-                                             
011106     05  FILLER                  PIC X.                                   
011107     EJECT                                                                
011301 PROCEDURE DIVISION  USING ARTD1-PCB ARTD2-PCB.                           
011310     ENTRY 'DLITCBL' USING ARTD1-PCB ARTD2-PCB.                           
011400                                                                          
011600     SKIP2                                                                
011700     PERFORM A-INIT                                                       
011800     PERFORM S01-LAES-W12207                                              
011900     PERFORM UNTIL END-OF-W12207                                          
012000       MOVE IN-IDARTNR        TO W-IDARTNR                                
012100       PERFORM IMS-GHU-WLARTD01                                           
012200       IF SEGMENT-FINNS                                                   
012300         PERFORM B-SKAPA-UTFIL                                            
012410         PERFORM IMS-DLET-WLARTD01                                        
               ADD 1 TO W-DLET-WDD801                                           
012500       END-IF                                                             
012600       PERFORM S01-LAES-W12207                                            
012700     END-PERFORM                                                          
012900                                                                          
013000     PERFORM Z-FINIT                                                      
013100                                                                          
013200     MOVE ZERO TO RETURN-CODE                                             
013300     GOBACK                                                               
013400     .                                                                    
013500     EJECT                                                                
013600 A-INIT SECTION.                                                          
013801                                                                          
013810     OPEN INPUT  W12207                                                   
013820     OPEN OUTPUT W48870                                                   
013900     SKIP2                                                                
014000     ACCEPT DAGENS-DATUM  FROM DATE                                       
014110     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014300     .                                                                    
014400     EJECT                                                                
014500 Z-FINIT SECTION.                                                         
           DISPLAY 'ANTAL BORTTAGNA WDD801: ' W-DLET-WDD801                     
014610     CLOSE W12207                                                         
014620           W48870                                                         
014701     SKIP2                                                                
014702     MOVE 'S' TO POSTSUM-OPKOD                                            
014710     CALL POSTSUM USING POSTSUM-PARM                                      
014800     .                                                                    
015001     EJECT                                                                
015002 B-SKAPA-UTFIL  SECTION.                                                  
015004     PERFORM IMS-GU-WLARTD01-PCB2                                         
015005     PERFORM IMS-GNP-WLARTD11-PCB2                                        
015006     PERFORM UNTIL NOT SEGMENT-FINNS                                      
015007       MOVE W-IDARTNR TO UT-IDARTNR                                       
015008       MOVE ARTD-SALDO-IDDC      TO UT-IDDC                               
015009       MOVE ARTD-SALDO-ADBUFFOMR TO UT-ADBUFFOMR                          
015010       MOVE ARTD-SALDO-ADBUFFGANG TO UT-ADBUFFGANG                        
015011       MOVE ARTD-SALDO-ADBUFFPL TO UT-ADBUFFPL                            
015013       MOVE ARTD-SALDO-KVBUFF-F TO UT-KVBUFF-F                            
015014       MOVE ARTD-SALDO-KVBUFF-OF TO UT-KVBUFF-OF                          
015015       MOVE ARTD-SALDO-KVKOLLI-F TO UT-KVKOLLI-F                          
015016       MOVE ARTD-SALDO-KVKOLLI-OF TO UT-KVKOLLI-OF                        
015017       PERFORM S11-SKRIV-W48870                                           
015018       PERFORM IMS-GNP-WLARTD11-PCB2                                      
015023     END-PERFORM                                                          
015024     .                                                                    
015025     EJECT                                                                
015026 S01-LAES-W12207  SECTION.                                                
015028     SKIP2                                                                
015029     READ W12207 INTO IN-W12207 AT END                                    
015030       SET END-OF-W12207 TO TRUE                                          
015031     END-READ                                                             
015032                                                                          
015033     MOVE 'W12207' TO POSTSUM-FDNAMN                                      
015034     MOVE 'W48870D1' TO POSTSUM-DDNAMN2                                   
015035     CALL POSTSUM USING POSTSUM-PARM                                      
015040     .                                                                    
015200     EJECT                                                                
015300 S11-SKRIV-W48870 SECTION.                                                
015400     SKIP2                                                                
015500     WRITE UT-POST FROM UT-AREA                                           
015600                                                                          
015700     MOVE 'W48870' TO POSTSUM-FDNAMN                                      
015800     MOVE 'W48870D2' TO POSTSUM-DDNAMN2                                   
015810     CALL POSTSUM USING POSTSUM-PARM                                      
015820     .                                                                    
015830     EJECT                                                                
015900* --- IMS SEKTIONER ---                                                   
016000     SKIP3                                                                
016101     EJECT                                                                
016102 IMS-GHU-WLARTD01 SECTION.                                                
016103     STRING 'WLARTD01(IDARTNR  =' W-IDARTNR-X ')'                         
016104          DELIMITED BY SIZE INTO SSA1                                     
016105     MOVE '  GE' TO GODK-STATUSKODER                                      
016106     CALL CBLTDLI USING GHU ARTD1-PCB DLI-IO-AREA-1 SSA1                  
016107     MOVE ARTD1-STATUS-CODE TO STATUS-WS                                  
016108     PERFORM IMS-STATUSKONTROLL                                           
016109     .                                                                    
016110     SKIP3                                                                
016111 IMS-DLET-WLARTD01 SECTION.                                               
016112                                                                          
016113     MOVE '  ' TO GODK-STATUSKODER                                        
016114     CALL CBLTDLI USING DLET ARTD1-PCB DLI-IO-AREA-1                      
016115     MOVE ARTD1-STATUS-CODE TO STATUS-WS                                  
016116     PERFORM IMS-STATUSKONTROLL                                           
016117     .                                                                    
016118     EJECT                                                                
016119 IMS-GU-WLARTD01-PCB2 SECTION.                                            
016120     STRING 'WLARTD01(IDARTNR  =' W-IDARTNR-X ')'                         
016121          DELIMITED BY SIZE INTO SSA1                                     
016122     MOVE '  ' TO GODK-STATUSKODER                                        
016123     CALL CBLTDLI USING GU ARTD2-PCB DLI-IO-AREA-2 SSA1                   
016124     MOVE ARTD2-STATUS-CODE TO STATUS-WS                                  
016125     PERFORM IMS-STATUSKONTROLL                                           
016126     .                                                                    
016127     SKIP3                                                                
016128 IMS-GNP-WLARTD11-PCB2 SECTION.                                           
016129     MOVE '  GE' TO GODK-STATUSKODER                                      
016130     CALL CBLTDLI USING GNP ARTD2-PCB DLI-IO-AREA-2                       
016131     MOVE ARTD2-STATUS-CODE TO STATUS-WS                                  
016132     PERFORM IMS-STATUSKONTROLL                                           
016133     .                                                                    
016134     SKIP3                                                                
016300 IMS-STATUSKONTROLL SECTION.                                              
016400     SKIP2                                                                
016500     SET STATUS-IX TO 1                                                   
016600     SEARCH GODK-STATUS                                                   
016700       AT END                                                             
016800         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
016900         DISPLAY FELTEXT                                                  
017000         CALL FELLOG                                                      
017100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
017200         CONTINUE                                                         
017300     END-SEARCH                                                           
017400     .                                                                    
