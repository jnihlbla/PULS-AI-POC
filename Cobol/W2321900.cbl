000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W2321900.                                                
000400 AUTHOR.         PER-ANDERS HELGEGREN.                                    
000500 DATE-WRITTEN.   96/09/10.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        LÄSER NED WDL801 TILL EN FIL MEN EN POST PER IDARTNR             
001000*        DVS DE SISTA TRE VECKORNAS DAGLIGA ORDERINGÅNG                   
001100*                                                                         
001200*        PROGRAMMET LÄSER      WLOIGB (WDL8)                              
001300*                                                                         
001400*    ABENDKODER:                                                          
001500*        U0016 -  . . . .                                                 
001600*        U1000 -  . . . .                                                 
001700*                                                                         
001800                                                                          
001900     SKIP2                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*          --- WDL801 MED OI DE SISTA 3 VV                                
002700     SELECT W23221                     ASSIGN TO W23219D1.                
002710     SKIP2                                                                
002720*          --- WDL801 MED OI DE SISTA 3 VV                                
002730     SELECT W232WOI                    ASSIGN TO W23219D2.                
002800     SKIP2                                                                
002900*          --- SORTERINGSFIL                                              
003000     SELECT SORTFIL                    ASSIGN TO W23219DS.                
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP2                                                                
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
003600 FD  W23221                                                               
003700     RECORDING       F                                                    
003800     BLOCK CONTAINS  0.                                                   
003900                                                                          
004000*01  POST -COPY W23221 -PRE  UT-  -L.                                     
004010     SKIP2                                                                
004020 FD  W232WOI                                                              
004030     RECORDING       F                                                    
004040     BLOCK CONTAINS  0.                                                   
004050                                                                          
004060*01  POST -COPY W232VOI -PRE  WOI-  -L.                                   
004100     SKIP2                                                                
004200 SD  SORTFIL.                                                             
004300*01  POST -COPY WDL801 -PRE  SORTWS-                                      
004400     EJECT                                                                
004500 WORKING-STORAGE SECTION.                                                 
004600                                                                          
004601                                                                          
004610*    -- CHECKED BY WY2000                                                 
004700 77  IDPGM                       PIC X(8)    VALUE 'W2321900'.            
004800 77  JA                          PIC X       VALUE 'J'.                   
004900 77  NEJ                         PIC X       VALUE 'N'.                   
005000                                                                          
005100 77  SORTFIL-EOF-SW              PIC X       VALUE 'N'.                   
005200     88  END-OF-SORTFIL                      VALUE 'J'.                   
005300                                                                          
005400 77  SW-TEST-OI                  PIC X       VALUE 'N'.                   
005500     88  SW-TEST-OI-TRAFF                    VALUE 'J'.                   
005501                                                                          
005510 01  IX                          PIC S9(3)   COMP-3.                      
005600     SKIP3                                                                
005700 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005800 01  FILLER REDEFINES DAGENS-DATUM.                                       
005900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006200     EJECT                                                                
006300 01  DYNAMISKA-SUBPROGRAM.                                                
006400*                                                                         
006500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006900     SKIP2                                                                
007000*    --- PARAMETRAR TILL ABEND                                            
007100                                                                          
007200 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007300 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007500     SKIP2                                                                
007600 01  FELTEXT.                                                             
007700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007900     EJECT                                                                
008000*    --- PARAMETRAR TILL POSTSUM                                          
008100*                                                                         
008200*01  -COPY W0005   -PRE  POSTSUM-                                         
008300     EJECT                                                                
008400 01  UT-AREA-START               PIC X(24)   VALUE                        
008500                                 'UT-AREA-START  '.                       
008600     SKIP2                                                                
008700                                                                          
008800*01  AREA -COPY W23221     -PRE UT-                                       
008810     EJECT                                                                
008820 01  WOI-AREA-START               PIC X(24)   VALUE                       
008830                                 'WOI-AREA-START  '.                      
008840     SKIP2                                                                
008850                                                                          
008860*01  AREA -COPY W232VOI    -PRE WOI-                                      
008900     EJECT                                                                
009000*01  AREA -COPY WDL801     -PRE SORT-                                     
009100                                                                          
009200 01  SORT-RETURN-X               PIC X(2)  VALUE SPACE.                   
009300     EJECT                                                                
009400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009500*                                                                         
009700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009800     SKIP3                                                                
009900 01  NYCKLAR-TILL-DLI.                                                    
010000     03  W-IDARTNR-X.                                                     
010100         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
010200     SKIP2                                                                
010300*    --- STATUS-KOD FRÅN IMS                                              
010400 01  STATUS-WS                   PIC XX.                                  
010500     88  SEGMENT-FINNS                       VALUE '  '.                  
010600     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
010700     SKIP2                                                                
010800 01  GODK-STATUSKODER.                                                    
010900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011000     SKIP3                                                                
011100 01  SSA1                        PIC X(64).                               
011200 01  SSA2                        PIC X(64).                               
011300     EJECT                                                                
011400*    --- IMS FUNKTIONSKODER                                               
011500*01  -COPY W0003                                                          
011600     EJECT                                                                
011700*    ---  DLI INPUT-OUTPUT AREA                                           
011800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
011900     SKIP3                                                                
012000 01  DLI-IO-AREA.                                                         
012300     03  WLOIGB01.                                                        
012400*        05  -COPY WDL801                                                 
012500     EJECT                                                                
012600 LINKAGE SECTION.                                                         
012700                                                                          
012900*01  -COPY W0008  -PRE OIGB-                                              
013000     05  FILLER                  PIC X.                                   
013100     EJECT                                                                
013200 PROCEDURE DIVISION  USING OIGB-PCB.                                      
013300 MAIN SECTION.                                                            
013400     ENTRY 'DLITCBL' USING OIGB-PCB.                                      
013500                                                                          
013600     PERFORM A-INIT                                                       
013700                                                                          
013800     SORT SORTFIL ASCENDING KEY SORTWS-ART-IDARTNR                        
013900                  INPUT PROCEDURE B-SORT-INPUT                            
014000                  OUTPUT PROCEDURE C-SORT-OUTPUT                          
014100                                                                          
014200     IF SORT-RETURN NOT = 0                                               
014300       MOVE SORT-RETURN TO SORT-RETURN-X                                  
014400       STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT'                      
014500           DELIMITED BY SIZE                                              
014600           INTO FELTEXT-STR                                               
014700       DISPLAY FELTEXT                                                    
014800       PERFORM S99-ABEND                                                  
014900     ELSE                                                                 
015000       PERFORM Z-FINIT                                                    
015100                                                                          
015200       MOVE ZERO TO RETURN-CODE                                           
015300       GOBACK                                                             
015400     END-IF                                                               
015500                                                                          
015600     .                                                                    
015700     EJECT                                                                
015800 A-INIT SECTION.                                                          
015900                                                                          
016000     OPEN OUTPUT W23221                                                   
016010                 W232WOI                                                  
016100                                                                          
016200     ACCEPT DAGENS-DATUM  FROM DATE                                       
016300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
016400     .                                                                    
016500     EJECT                                                                
016600 B-SORT-INPUT SECTION.                                                    
016700                                                                          
016800     PERFORM IMS-GET-OIGB                                                 
016900     PERFORM UNTIL SEGMENT-SAKNAS                                         
017000        PERFORM BA-TEST-OI                                                
017100        IF SW-TEST-OI-TRAFF                                               
017200           MOVE ART-WDL801  TO SORT-AREA                                  
017300           PERFORM S31-SORT-RELEASE                                       
017400        END-IF                                                            
017500        PERFORM IMS-GET-OIGB                                              
017600     END-PERFORM                                                          
017700     .                                                                    
017800     EJECT                                                                
017900 BA-TEST-OI   SECTION.                                                    
018000                                                                          
018100     MOVE NEJ TO SW-TEST-OI                                               
018200     MOVE 1 TO IX                                                         
018300     PERFORM UNTIL IX > 21 OR SW-TEST-OI-TRAFF                            
018400        IF ART-KVOI-DIV     (IX)  NOT = ZERO  OR                          
018500           ART-KVOI-NDC     (IX)  NOT = ZERO  OR                          
018600           ART-KVOI-PROG    (IX)  NOT = ZERO  OR                          
018700           ART-KVOI-REFILL  (IX)  NOT = ZERO  OR                          
018800           ART-KVOI-SATS    (IX)  NOT = ZERO  OR                          
018900           ART-KVOI-SDC     (IX)  NOT = ZERO                              
019000           MOVE JA TO SW-TEST-OI                                          
019100        END-IF                                                            
019200        ADD +1 TO IX                                                      
019300     END-PERFORM                                                          
019400     .                                                                    
019500     EJECT                                                                
019600 C-SORT-OUTPUT SECTION.                                                   
019700                                                                          
019800     PERFORM S32-SORT-RETURN                                              
019900     PERFORM UNTIL END-OF-SORTFIL                                         
020000*******MOVE SORT-AREA TO UT-AREA                                          
020001       MOVE SORT-ART-IDARTNR             TO UT-IDARTNR                    
020002                                           WOI-IDARTNR                    
020010       MOVE 1 TO IX                                                       
020020       PERFORM UNTIL IX > 21                                              
020030          MOVE SORT-ART-TIVVD       (IX) TO UT-TIVVD       (IX)           
020031                                           WOI-TIVVD       (IX)           
020032          MOVE SORT-ART-KVOI-DIV    (IX) TO UT-KVOI-DIV    (IX)           
020033                                           WOI-KVOI-DIV    (IX)           
020040          MOVE SORT-ART-KVOI-NDC    (IX) TO UT-KVOI-NDC    (IX)           
020041                                           WOI-KVOI-NDC    (IX)           
020050          MOVE SORT-ART-KVOI-PROG   (IX) TO UT-KVOI-PROG   (IX)           
020051                                           WOI-KVOI-PROG   (IX)           
020060          MOVE SORT-ART-KVOI-REFILL (IX) TO UT-KVOI-REFILL (IX)           
020061                                           WOI-KVOI-REFILL (IX)           
020070          MOVE SORT-ART-KVOI-SATS   (IX) TO UT-KVOI-SATS   (IX)           
020071                                           WOI-KVOI-SATS   (IX)           
020080          MOVE SORT-ART-KVOI-SDC    (IX) TO UT-KVOI-SDC    (IX)           
020081                                           WOI-KVOI-SDC    (IX)           
020090          MOVE SORT-ART-KVOI-LEDTID (IX) TO UT-KVOI-LEDTID (IX)           
020091                                           WOI-KVOI-LEDTID (IX)           
020092          ADD +1 TO IX                                                    
020093       END-PERFORM                                                        
020100       PERFORM S11-SKRIV-W23221                                           
020110       PERFORM S12-SKRIV-W232WOI                                          
020200       PERFORM S32-SORT-RETURN                                            
020300     END-PERFORM                                                          
020400     .                                                                    
020500     EJECT                                                                
020600 Z-FINIT SECTION.                                                         
020610     CLOSE W23221                                                         
020620           W232WOI                                                        
020800     SKIP2                                                                
020900     MOVE 'S' TO POSTSUM-OPKOD                                            
021000     CALL POSTSUM USING POSTSUM-PARM                                      
021100     .                                                                    
021200     EJECT                                                                
021300 S11-SKRIV-W23221 SECTION.                                                
021400                                                                          
021500     WRITE UT-POST FROM UT-AREA                                           
021600                                                                          
021700     MOVE 'ART'     TO POSTSUM-TRANSTYP                                   
021800     MOVE 'W23221' TO POSTSUM-FDNAMN                                      
021900     MOVE 'W23219D1' TO POSTSUM-DDNAMN2                                   
022000     CALL POSTSUM USING POSTSUM-PARM                                      
022100     .                                                                    
022200     EJECT                                                                
022210 S12-SKRIV-W232WOI SECTION.                                               
022220                                                                          
022230     WRITE WOI-POST FROM WOI-AREA                                         
022240                                                                          
022250     MOVE 'ART'      TO POSTSUM-TRANSTYP                                  
022260     MOVE 'W232WOI'  TO POSTSUM-FDNAMN                                    
022270     MOVE 'W23219D2' TO POSTSUM-DDNAMN2                                   
022280     CALL POSTSUM USING POSTSUM-PARM                                      
022290     .                                                                    
022291     EJECT                                                                
022300 S31-SORT-RELEASE  SECTION.                                               
022400                                                                          
022500     RELEASE SORTWS-POST FROM SORT-AREA                                   
022600     .                                                                    
022700     EJECT                                                                
022800 S32-SORT-RETURN  SECTION.                                                
022900                                                                          
023000     RETURN SORTFIL INTO SORT-AREA                                        
023100     AT END                                                               
023200         SET END-OF-SORTFIL TO TRUE                                       
023300     .                                                                    
023400     EJECT                                                                
023500 S99-ABEND SECTION.                                                       
023600                                                                          
023700     SKIP2                                                                
023800     MOVE 'S' TO POSTSUM-OPKOD                                            
023900     CALL POSTSUM USING POSTSUM-PARM                                      
024000     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
024100     .                                                                    
024200     EJECT                                                                
024300* --- IMS SEKTIONER ---                                                   
024400     SKIP3                                                                
024500 IMS-GET-OIGB   SECTION.                                                  
024600                                                                          
024700     CALL CBLTDLI USING GN OIGB-PCB DLI-IO-AREA                           
024800     MOVE OIGB-STATUS-CODE TO STATUS-WS                                   
024900     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
025000     PERFORM IMS-STATUSKONTROLL                                           
025100     .                                                                    
025200     EJECT                                                                
025300 IMS-STATUSKONTROLL SECTION.                                              
025400                                                                          
025500     SET STATUS-IX TO 1                                                   
025600     SEARCH GODK-STATUS                                                   
025700       AT END                                                             
025800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
025900           DELIMITED BY SIZE INTO FELTEXT                                 
026000         DISPLAY FELTEXT                                                  
026100         CALL FELLOG                                                      
026200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
026300         CONTINUE                                                         
026400     END-SEARCH                                                           
026500     .                                                                    
