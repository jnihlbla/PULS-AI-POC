001000 ID DIVISION.                                                             
001100 PROGRAM-ID.     W4123900.                                                
001200 AUTHOR.         GERRY CARMICHAEL.                                        
001300 DATE-WRITTEN.   00/07/04.                                                
001400 DATE-COMPILED.                                                           
001500                                                                          
001600*    FUNKTION:                                                            
001700*        LÄSER WDQ4 FÖR LDC ORDRAR MED RFS OM X DAGAR                     
001800*                                                                         
001910*        PROGRAMMET LÄSER      WDQ4                                       
002000*                                                                         
002100*    ABENDKODER:                                                          
002200*        U0016 -  . . . .                                                 
002300*        U1000 -  . . . .                                                 
002400*                                                                         
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003201     SKIP2                                                                
003202*          --- INFIL                                                      
003203     SELECT W41238                     ASSIGN TO W41239D1.                
003204     SKIP2                                                                
003205*          --- UTFIL                                                      
003210     SELECT W41239                     ASSIGN TO W41239D2.                
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP2                                                                
003700 FILE SECTION.                                                            
003801     SKIP3                                                                
003802 FD  W41238                                                               
003803     RECORDING       F                                                    
003804     BLOCK CONTAINS  0.                                                   
003805                                                                          
003806*01  -COPY W41238      -L.                                                
003807     SKIP3                                                                
003808 FD  W41239                                                               
003809     RECORDING       F                                                    
003810     BLOCK CONTAINS  0.                                                   
003820                                                                          
003830*01  POST -COPY W41239 -PRE  UT- -L.                                      
003840     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100                                                                          
004101                                                                          
004110*    -- CHECKED BY WY2000                                                 
004200 77  IDPGM                       PIC X(8)    VALUE 'W4123900'.            
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004601                                                                          
004602 77  W41238-EOF-SW               PIC X       VALUE 'N'.                   
004610     88  END-OF-W41238                       VALUE 'J'.                   
004700     EJECT                                                                
004800 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004900 01  FILLER REDEFINES DAGENS-DATUM.                                       
005000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005100     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005200     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005300     EJECT                                                                
005400 01  DYNAMISKA-SUBPROGRAM.                                                
005500*                                                                         
005600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005910     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006000     SKIP2                                                                
006100*    --- PARAMETRAR TILL ABEND                                            
006200                                                                          
006300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006600     SKIP2                                                                
006700 01  FELTEXT.                                                             
006800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007001     EJECT                                                                
007002*    --- PARAMETRAR TILL POSTSUM                                          
007003*                                                                         
007010*01  -COPY W0005   -PRE  POSTSUM-                                         
007201     EJECT                                                                
007202 01  IN-AREA-START               PIC X(24)   VALUE                        
007203                                 'IN-AREA-START  '.                       
007204     SKIP2                                                                
007205                                                                          
007206*01  AREA -COPY W41238     -PRE IN-                                       
007207     EJECT                                                                
007208 01  UT-AREA-START               PIC X(24)   VALUE                        
007209                                 'UT-AREA-START  '.                       
007210     SKIP2                                                                
007220*01  AREA -COPY W41239     -PRE UT-                                       
007230     EJECT                                                                
007400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007500*                                                                         
007600     EJECT                                                                
007700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007800     SKIP3                                                                
007900 01  NYCKLAR-TILL-DLI.                                                    
008000     03  W-WDQ401KY-MIN-X.                                                
008001         05  W-IDORDER-MIN        PIC S9(7)    VALUE ZERO COMP-3.         
008002         05  W-IDDC-MIN           PIC X(2)     VALUE SPACE.               
008003         05  FILLER               PIC X(14)    VALUE SPACE.               
008004     03  W-WDQ401KY-MAX-X.                                                
008005         05  W-IDORDER-MAX        PIC S9(7)    VALUE ZERO COMP-3.         
008006         05  W-IDDC-MAX           PIC X(2)     VALUE SPACE.               
008007         05  FILLER               PIC X(14)    VALUE SPACE.               
008100     SKIP2                                                                
008200*    --- STATUS-KOD FRÅN IMS                                              
008300 01  STATUS-WS                   PIC XX.                                  
008400     88  SEGMENT-FINNS                       VALUE '  '.                  
008500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
008600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008700     SKIP2                                                                
008800 01  GODK-STATUSKODER.                                                    
008900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009000     SKIP3                                                                
009100 01  SSA1                        PIC X(96).                               
009300     EJECT                                                                
009400*    --- IMS FUNKTIONSKODER                                               
009500*01  -COPY W0003                                                          
009600     EJECT                                                                
009800*    ---  DLI INPUT-OUTPUT AREA                                           
009901 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ401'.                      
009902 01  DLI-IO-WDQ401.                                                       
009910*    03  -COPY WDQ401                                                     
010200     EJECT                                                                
010300 LINKAGE SECTION.                                                         
010400                                                                          
010501                                                                          
010502*01  -COPY W0008  -PRE WDQ4-                                              
010510     05  FILLER                  PIC X.                                   
010600     EJECT                                                                
010701 PROCEDURE DIVISION  USING WDQ4-PCB.                                      
010702 MAIN SECTION.                                                            
010710     ENTRY 'DLITCBL' USING WDQ4-PCB.                                      
010800                                                                          
011000                                                                          
011100     PERFORM A-INIT                                                       
011200                                                                          
011310     PERFORM S01-LAES-W41238                                              
011400     PERFORM UNTIL END-OF-W41238                                          
011500                                                                          
011600       PERFORM B-SKAPA-UTFIL                                              
012000                                                                          
012110       PERFORM S01-LAES-W41238                                            
012120                                                                          
012200     END-PERFORM                                                          
012300                                                                          
012400                                                                          
012500     PERFORM Z-FINIT                                                      
012600                                                                          
012700     MOVE ZERO TO RETURN-CODE                                             
012800     GOBACK                                                               
012900     .                                                                    
013000     EJECT                                                                
013100 A-INIT SECTION.                                                          
013201                                                                          
013210     OPEN INPUT  W41238                                                   
013301                                                                          
013310     OPEN OUTPUT W41239                                                   
013400                                                                          
013500     ACCEPT DAGENS-DATUM  FROM DATE                                       
013610     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
013800     .                                                                    
013900     EJECT                                                                
013910 B-SKAPA-UTFIL SECTION.                                                   
013911     MOVE LOW-VALUE  TO W-WDQ401KY-MIN-X                                  
013912     MOVE HIGH-VALUE TO W-WDQ401KY-MAX-X                                  
013913                                                                          
013914     MOVE IN-IDORDER TO W-IDORDER-MIN                                     
013915                        W-IDORDER-MAX                                     
013916     MOVE IN-IDDC    TO W-IDDC-MIN                                        
013917                        W-IDDC-MAX                                        
013918                                                                          
013919     PERFORM IMS-GU-WDQ401                                                
013920     IF SEGMENT-FINNS                                                     
013921       PERFORM UNTIL SEGMENT-SAKNAS                                       
013922         MOVE IN-IDORDER        TO UT-IDORDER                             
013923         MOVE ORAD-IDARTNR      TO UT-IDARTNR                             
013924         MOVE ORAD-IDDC         TO UT-IDDC                                
013925         MOVE IN-DARFS          TO UT-DARFS                               
013926         MOVE ORAD-IDDISTR      TO UT-IDDISTR                             
013927         MOVE ORAD-IDKUNDNR     TO UT-IDKUNDNR                            
013928         MOVE ORAD-IDORDNR7     TO UT-IDORDNR7                            
013929         MOVE ORAD-IDSYSTEM     TO UT-IDSYSTEM                            
013930         MOVE ORAD-KVBEART      TO UT-KVBEART                             
013931         MOVE ORAD-KVBEART-Q    TO UT-KVBEART-Q                           
013932         MOVE ORAD-KDKVBRYT     TO UT-KDKVBRYT                            
013933         MOVE ORAD-KDDSP        TO UT-KDDSP                               
013934         MOVE ORAD-KDORDKL      TO UT-KDORDKL                             
013935         MOVE ORAD-BERADREF     TO UT-BERADREF                            
013936         MOVE ZERO              TO UT-KVANT-ART                           
013937         MOVE ORAD-PRARTNTO-LOCPREL TO UT-PRARTNTO-LOCPREL                
013938         MOVE ORAD-IDPRQUES     TO UT-IDPRQUES                            
013939         MOVE IN-BEKUNDRF       TO UT-BEKUNDRF                            
013940         MOVE IN-FLEMBORD       TO UT-FLEMBORD                            
013941         MOVE IN-FLFORBI        TO UT-FLFORBI                             
013942         MOVE ORAD-FLRESTN      TO UT-FLRESTN                             
013943         MOVE ORAD-FLSDCLEV     TO UT-FLSDCLEV                            
013944         MOVE IN-FLORDSPE       TO UT-FLORDSPE                            
013945         MOVE IN-FLOVRLEV       TO UT-FLOVRLEV                            
013946         MOVE ORAD-IDBIL        TO UT-IDBIL                               
013948         MOVE ORAD-IDKUNDRF-RO  TO UT-IDKUNDRF-RO                         
013949         MOVE IN-KDFAKTYP       TO UT-KDFAKTYP                            
013951         MOVE ORAD-KDPRTYP      TO UT-KDPRTYP                             
013952         MOVE ORAD-KDTPOTYP     TO UT-KDTPOTYP                            
013953         MOVE ORAD-TIRODAT      TO UT-TIRODAT                             
013954         MOVE ORAD-TITPO        TO UT-TITPO                               
013955         IF ORAD-IDSYSTEM      = 'ECOM'                                   
013955            MOVE ORAD-PRARTNTO-LOC TO UT-PRARTNTO-LOC                     
013985         ELSE                                                             
013955            MOVE ZERO             TO UT-PRARTNTO-LOC                      
013956         END-IF                                                           
013982         PERFORM S11-SKRIV-W41239                                         
013983         PERFORM IMS-GN-WDQ401                                            
013984       END-PERFORM                                                        
013985     END-IF                                                               
013986     .                                                                    
013990     EJECT                                                                
014000 Z-FINIT SECTION.                                                         
014101     CLOSE W41238                                                         
014110           W41239                                                         
014201     SKIP2                                                                
014202     MOVE 'S' TO POSTSUM-OPKOD                                            
014210     CALL POSTSUM USING POSTSUM-PARM                                      
014300     .                                                                    
014401     EJECT                                                                
014402 S01-LAES-W41238  SECTION.                                                
014403     READ W41238 INTO IN-AREA                                             
014404     AT END                                                               
014405        MOVE HIGH-VALUE TO IN-AREA                                        
014406        SET END-OF-W41238 TO TRUE                                         
014407                                                                          
014408     NOT AT END                                                           
014409        MOVE 'W41238' TO POSTSUM-FDNAMN                                   
014410        MOVE 'W41239D1' TO POSTSUM-DDNAMN2                                
014411*       -- ÄNDRA TILL MOVE SPACE OM FILEN SAKNAR POSTTYP                  
014412*       -- CHANGE TO MOVE SPACE IF THE FILE LACKS RECORD TYPES            
014413        MOVE '001'     TO POSTSUM-TRANSTYP                                
014414        CALL POSTSUM USING POSTSUM-PARM                                   
014415     END-READ                                                             
014420     .                                                                    
014501     EJECT                                                                
014502 S11-SKRIV-W41239 SECTION.                                                
014503                                                                          
014504     WRITE UT-POST FROM UT-AREA                                           
014505                                                                          
014506     MOVE '002'     TO POSTSUM-TRANSTYP                                   
014507     MOVE 'W41239' TO POSTSUM-FDNAMN                                      
014508     MOVE 'W41239D2' TO POSTSUM-DDNAMN2                                   
014509     CALL POSTSUM USING POSTSUM-PARM                                      
014510     .                                                                    
014700     EJECT                                                                
014800 S99-ABEND SECTION.                                                       
014900                                                                          
015001     SKIP2                                                                
015002     MOVE 'S' TO POSTSUM-OPKOD                                            
015010     CALL POSTSUM USING POSTSUM-PARM                                      
015100     CALL ABEND USING RKOD-ABEND                                          
015200     .                                                                    
015300     EJECT                                                                
015400* --- IMS SEKTIONER ---                                                   
015500                                                                          
015601     EJECT                                                                
015710 IMS-GU-WDQ401 SECTION.                                                   
015720     STRING 'WDQ401  (WDQ401KY>=' W-WDQ401KY-MIN-X                        
015730                    '&WDQ401KY<=' W-WDQ401KY-MAX-X ')'                    
015740          DELIMITED BY SIZE INTO SSA1                                     
015750     MOVE '  GE' TO GODK-STATUSKODER                                      
015760     CALL CBLTDLI USING GU WDQ4-PCB DLI-IO-WDQ401 SSA1                    
015770     MOVE WDQ4-STATUS-CODE TO STATUS-WS                                   
015780     PERFORM IMS-STATUSKONTROLL                                           
015790     .                                                                    
015791     EJECT                                                                
015792 IMS-GN-WDQ401 SECTION.                                                   
015793     STRING 'WDQ401  (WDQ401KY>=' W-WDQ401KY-MIN-X                        
015794                    '&WDQ401KY<=' W-WDQ401KY-MAX-X ')'                    
015795          DELIMITED BY SIZE INTO SSA1                                     
015796     MOVE '  GBGE' TO GODK-STATUSKODER                                    
015797     CALL CBLTDLI USING GN WDQ4-PCB DLI-IO-WDQ401 SSA1                    
015798     MOVE WDQ4-STATUS-CODE TO STATUS-WS                                   
015799     PERFORM IMS-STATUSKONTROLL                                           
015800     .                                                                    
015801     EJECT                                                                
015810 IMS-STATUSKONTROLL SECTION.                                              
015900                                                                          
016000     SET STATUS-IX TO 1                                                   
016100     SEARCH GODK-STATUS                                                   
016200       AT END                                                             
016300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
016400           DELIMITED BY SIZE INTO FELTEXT                                 
016500         DISPLAY FELTEXT                                                  
016600         CALL FELLOG                                                      
016700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
016800         CONTINUE                                                         
016900     END-SEARCH                                                           
017000     .                                                                    
