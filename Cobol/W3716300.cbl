000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3716300.                                                
000300 AUTHOR.         BO HAMMARIN.                                             
000400 DATE-WRITTEN.   OKT-99.                                                  
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000810*      - PGM LÄSER BYTES POÄNGPRM (WDGX3156)                              
000820*      - PGM KONTROLLERAR OM RAPPORTEN SKALL FRAMSTÄLLAS ELLER EJ         
000831*      - PGM MATCHAR BYTESFIL MOT ARTIKELREG (WDK601/611) FÖR             
000832*            UTSÖKNING AV CORE-PARTS                                      
000840*      - PGM MATCHAR BYTESFIL MOT BENÄMNINGSREG (WDD311) FÖR              
000850*            KOMPLETTERING MED BENÄMNING                                  
000900*      - PGM SKAPAR RAPPORT FÖR BYTESARTIKLAR SOM DISTRIBUERAS VIA        
001100*            MEMO TILL DEN MOTTAGARE SOM FINNS ANGIVEN I POÄNGPRM         
001200*                                                                         
001300*    ABENDKODER:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800     EJECT                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000                                                                          
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002401                                                                          
002408*          --- UPPDATERINGSPOSTER ETC. BYTES                              
002410     SELECT W37164                     ASSIGN TO W37163D1.                
002420                                                                          
002430*          --- RAPPORTPOSTER MEMO                                         
002440     SELECT W37163                     ASSIGN TO W37163D2.                
002450                                                                          
002460*          --- HEADERPOSTER MEMO                                          
002470     SELECT W37165                     ASSIGN TO W37163D3.                
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800                                                                          
002900 FILE SECTION.                                                            
003001                                                                          
003002 FD  W37164                                                               
003003     RECORDING       F                                                    
003004     BLOCK CONTAINS  0.                                                   
003005                                                                          
003006*01  -COPY W37164      -L.                                                
003007                                                                          
003014 FD  W37163                                                               
003015     RECORDING       F                                                    
003016     BLOCK CONTAINS  0.                                                   
003017                                                                          
003020*01  UT-POST -COPY W37163  -L.                                            
003021                                                                          
003030 FD  W37165                                                               
003040     RECORDING       F                                                    
003050     BLOCK CONTAINS  0.                                                   
003060                                                                          
003070 01  HEAD-POST                   PIC X(80).                               
003100     EJECT                                                                
003200 WORKING-STORAGE SECTION.                                                 
003301                                                                          
003310*    -- CHECKED BY WY2000                                                 
003400 77  IDPGM                       PIC X(8)    VALUE 'W3716300'.            
003500 77  JA                          PIC X       VALUE 'J'.                   
003600 77  NEJ                         PIC X       VALUE 'N'.                   
003801                                                                          
003802 77  W37164-EOF-SW               PIC X       VALUE 'N'.                   
003803     88  END-OF-W37164                       VALUE 'J'.                   
003900                                                                          
004600 01  W-DIVERSE.                                                           
004900     03  WS-PRARTBTO             PIC 9(7).9(2).                           
004901     03  WS-PRARTSJK             PIC 9(7).9(2).                           
005100                                                                          
005101 01  W-TITLE.                                                             
005102     03  FILLER                  PIC X(8) VALUE 'CORE-ID'.                
005103     03  FILLER                  PIC X(25) VALUE 'DESCRIPTION'.           
005104     03  FILLER                  PIC X(4) VALUE 'FGRP'.                   
005105     03  FILLER                  PIC X(2) VALUE 'SC'.                     
005106     03  FILLER                  PIC X(3) VALUE 'ACC'.                    
005107     03  FILLER                  PIC X(7) VALUE 'ST.BAL.'.                
005108     03  FILLER                  PIC X(6) VALUE 'POINTS'.                 
005109     03  FILLER                  PIC X(10) VALUE 'BASE PRICE'.            
005110     03  FILLER                  PIC X(10) VALUE 'COST PRICE'.            
005111     03  FILLER                  PIC X(5) VALUE 'SCDAT'.                  
005112                                                                          
005120 01  DYNAMISKA-SUBPROGRAM.                                                
005130     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005140     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI'.             
005150     03  FELLOG                  PIC X(8)    VALUE 'FELLOG'.              
005160     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005170                                                                          
005200*    --- PARAMETRAR TILL ABEND                                            
005300                                                                          
005400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
005600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
005601                                                                          
005800 01  FELTEXT.                                                             
005900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006100     EJECT                                                                
006110                                                                          
006903*    --- PARAMETRAR TILL POSTSUM                                          
006904*                                                                         
006910*01  -COPY W0005   -PRE  POSTSUM-                                         
007101     EJECT                                                                
007103                                                                          
007109 01  BYT-AREA-START               PIC X(24)   VALUE                       
007110                                 'BYT-AREA-START '.                       
007113*01  AREA -COPY W37164     -PRE BYT-                                      
007114     EJECT                                                                
007124                                                                          
007125 01  RAPP-AREA-START              PIC X(24)   VALUE                       
007126                                 'RAPP-AREA-START '.                      
007127*01  AREA -COPY W37163     -PRE RAPP-                                     
007130     EJECT                                                                
007131                                                                          
007132 01  HEAD-AREA-START              PIC X(24)   VALUE                       
007133                                 'HEAD-AREA-START '.                      
007134 01  HEAD-AREA.                                                           
007135     03  FILLER                   PIC X(80).                              
007136     EJECT                                                                
007140*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007150*                                                                         
007160     EJECT                                                                
007170 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007180                                                                          
007190 01  NYCKLAR-TILL-DLI.                                                    
007212     03  W-WDGXKEY-3155-X.                                                
007213         05  W-IDHTYP-3155       PIC X(4)    VALUE '3155'.                
007214         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
007215     03  W-WDGXKEY-3156-X.                                                
007216         05  W-KDSEGKEY-3156     PIC X(1)    VALUE '1'.                   
007217     03  W-IDARTNR-X.                                                     
007218         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
007219     03  W-IDSKYLT-X.                                                     
007220         05  W-IDSKYLT           PIC X(3).                                
007227                                                                          
007228     EJECT                                                                
007229*    --- STATUS-KOD FRÅN IMS                                              
007230 01  STATUS-WS                   PIC XX.                                  
007231     88  SEGMENT-FINNS                       VALUE '  '.                  
007232     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
007233                                                                          
007234 01  GODK-STATUSKODER.                                                    
007235     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
007236                                                                          
007237 01  SSA1                        PIC X(64).                               
007238 01  SSA2                        PIC X(64).                               
007243     EJECT                                                                
007244*    --- IMS FUNKTIONSKODER                                               
007245*01  -COPY W0003                                                          
007246     EJECT                                                                
007247*    ---  DLI INPUT-OUTPUT AREA                                           
007269 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3156'.                    
007270 01  DLI-IO-WDGX3156.                                                     
007271*    03  -COPY WDGX3156                                                   
007272     EJECT                                                                
007273 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601  '.                    
007274 01  DLI-IO-WDK601.                                                       
007276*    03  -COPY WDK601                                                     
007277     EJECT                                                                
007278 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611  '.                    
007279 01  DLI-IO-WDK611.                                                       
007280*    03  -COPY WDK611                                                     
007281     EJECT                                                                
007282 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311  '.                    
007283 01  DLI-IO-WDD311.                                                       
007284*    03  -COPY WDD311                                                     
007290     EJECT                                                                
007500 LINKAGE SECTION.                                                         
007510                                                                          
007592*01  -COPY W0008  -PRE 3156-                                              
007593     05  FILLER                  PIC X.                                   
007594                                                                          
007595*01  -COPY W0008  -PRE WDK6-                                              
007596     05  FILLER                  PIC X.                                   
007597                                                                          
007598*01  -COPY W0008  -PRE WDD3-                                              
007599     05  FILLER                  PIC X.                                   
007600                                                                          
007601     EJECT                                                                
007602 PROCEDURE DIVISION  USING                                                
007603                           3156-PCB WDK6-PCB WDD3-PCB.                    
007604 MAIN SECTION.                                                            
007605     ENTRY 'DLITCBL' USING                                                
007606                           3156-PCB WDK6-PCB WDD3-PCB.                    
007607                                                                          
007800     PERFORM A-INIT                                                       
007900                                                                          
007901     IF 3156-FLEXCREP = 'J' OR 'Y'                                        
007902       PERFORM S01-LAES-W37164                                            
007920                                                                          
008000       PERFORM UNTIL END-OF-W37164                                        
008120         PERFORM B-BYGG-RAPPORT                                           
008720         PERFORM S01-LAES-W37164                                          
008800       END-PERFORM                                                        
008810     END-IF                                                               
008900                                                                          
009000     PERFORM C-BYGG-HEADER                                                
009010                                                                          
009100     PERFORM Z-FINIT                                                      
009200                                                                          
009300     MOVE ZERO TO RETURN-CODE                                             
009400     GOBACK                                                               
009500     .                                                                    
009600     EJECT                                                                
009700 A-INIT SECTION.                                                          
009801                                                                          
009802     OPEN INPUT  W37164                                                   
009910     OPEN OUTPUT W37163                                                   
009920                 W37165                                                   
010000                                                                          
010510     MOVE IDPGM     TO  POSTSUM-PROGNAMN                                  
010511                                                                          
010512     WRITE UT-POST FROM W-TITLE                                           
010520                                                                          
010530     PERFORM IMS-GU-WDGX3156                                              
010600     .                                                                    
010700     EJECT                                                                
010800 B-BYGG-RAPPORT SECTION.                                                  
010900                                                                          
010901     MOVE BYT-IDARTNR                TO W-IDARTNR                         
010903     MOVE 'GB'                       TO W-IDSKYLT                         
010904                                                                          
010905     PERFORM IMS-GU-WDD311-BSEQ                                           
010906                                                                          
010907     MOVE BYT-IDARTNR                TO RAPP-IDARTNR                      
010908     MOVE TEXT-BEART                 TO RAPP-BEART                        
010909     MOVE BYT-IDFKNGRP               TO RAPP-IDFKNGRP                     
010910     MOVE BYT-KDEXCHA                TO RAPP-KDEXCHA                      
010911     MOVE BYT-KVLS                   TO RAPP-KVLS                         
010912     MOVE BYT-KVPOINT                TO RAPP-KVPOINT                      
010913     MOVE BYT-PRREF                  TO WS-PRARTBTO                       
010914     MOVE WS-PRARTBTO                TO RAPP-PRARTBTO                     
010915     MOVE 0                          TO WS-PRARTSJK                       
010916     MOVE WS-PRARTSJK                TO RAPP-PRARTSJK                     
010917                                                                          
010918     IF BYT-KDEXCHA =  16                                                 
010919     COMPUTE W-IDARTNR = W-IDARTNR - 1000                                 
010920     END-COMPUTE                                                          
010921     ELSE                                                                 
010922     COMPUTE W-IDARTNR = W-IDARTNR - 6000                                 
010923     END-COMPUTE                                                          
010924     END-IF                                                               
010925     PERFORM IMS-GU-WDK601                                                
010926     IF SEGMENT-SAKNAS                                                    
010927       MOVE ZERO                     TO RAPP-TIERSDAT                     
010928                                        RAPP-KDERS                        
010929     ELSE                                                                 
010930       IF ART-KDERS-UTG >  0                                              
010931          MOVE ART-KDERS-UTG             TO RAPP-KDERS                    
010932          MOVE ART-TIERSDAT              TO RAPP-TIERSDAT                 
010933       ELSE                                                               
010934           PERFORM IMS-GNP-WDK611                                         
010935           IF SEGMENT-SAKNAS                                              
010936             MOVE ZERO                   TO RAPP-TIERSDAT                 
010937                                            RAPP-KDERS                    
010938           ELSE                                                           
010940                                                                          
010957             MOVE ART-TIERSDAT           TO RAPP-TIERSDAT                 
010958             MOVE CLAG-KDERS             TO RAPP-KDERS                    
010959             MOVE CLAG-PRARTSJK      TO WS-PRARTSJK                       
010960             MOVE WS-PRARTSJK       TO RAPP-PRARTSJK                      
010965           END-IF                                                         
010966       END-IF                                                             
010967     END-IF                                                               
010968                                                                          
010970     PERFORM S11-SKRIV-W37163                                             
011100     .                                                                    
011201     EJECT                                                                
011202 C-BYGG-HEADER SECTION.                                                   
011203                                                                          
011207     MOVE ')SEND'                    TO HEAD-AREA                         
011208     PERFORM S12-SKRIV-W37165                                             
011209                                                                          
011210     MOVE 'TITLE POINT REPORT'       TO HEAD-AREA                         
011211     PERFORM S12-SKRIV-W37165                                             
011212                                                                          
011213     MOVE 'OPTION FORCE'             TO HEAD-AREA                         
011214     PERFORM S12-SKRIV-W37165                                             
011215                                                                          
011216     MOVE 'LINESIZE 80'              TO HEAD-AREA                         
011217     PERFORM S12-SKRIV-W37165                                             
011218                                                                          
011220     STRING 'DEST ' 3156-IDMAIL DELIMITED BY SIZE INTO HEAD-AREA          
011222     PERFORM S12-SKRIV-W37165                                             
011223                                                                          
011224     MOVE 'MEMO'                     TO HEAD-AREA                         
011225     PERFORM S12-SKRIV-W37165                                             
011262     .                                                                    
011263     EJECT                                                                
011264 Z-FINIT SECTION.                                                         
011265                                                                          
011266     CLOSE W37164                                                         
011267           W37163                                                         
011268           W37165                                                         
011269                                                                          
011270     MOVE 'S' TO POSTSUM-OPKOD                                            
011271     CALL POSTSUM USING POSTSUM-PARM                                      
011272     .                                                                    
011273     EJECT                                                                
011274 S01-LAES-W37164  SECTION.                                                
011275                                                                          
011276     READ W37164 INTO BYT-AREA                                            
011277     AT END                                                               
011278        MOVE HIGH-VALUE   TO BYT-AREA                                     
011279        SET END-OF-W37164 TO TRUE                                         
011280                                                                          
011281     NOT AT END                                                           
011282        MOVE 'W37164'   TO POSTSUM-FDNAMN                                 
011283        MOVE 'W37163D1' TO POSTSUM-DDNAMN2                                
011284        MOVE 'BYT'      TO POSTSUM-TRANSTYP                               
011285        CALL POSTSUM USING POSTSUM-PARM                                   
011286     END-READ                                                             
011287     .                                                                    
011290     EJECT                                                                
011302 S11-SKRIV-W37163 SECTION.                                                
011303                                                                          
011304     WRITE UT-POST FROM RAPP-AREA                                         
011305                                                                          
011306     MOVE 'MEMR'     TO POSTSUM-TRANSTYP                                  
011307     MOVE 'W37163'   TO POSTSUM-FDNAMN                                    
011308     MOVE 'W37163D2' TO POSTSUM-DDNAMN2                                   
011309     CALL POSTSUM USING POSTSUM-PARM                                      
011310     .                                                                    
011500     EJECT                                                                
011600 S12-SKRIV-W37165 SECTION.                                                
011610                                                                          
011611     WRITE HEAD-POST FROM HEAD-AREA                                       
011612                                                                          
011613     MOVE 'MEMH'     TO POSTSUM-TRANSTYP                                  
011614     MOVE 'W37163'   TO POSTSUM-FDNAMN                                    
011615     MOVE 'W37163D3' TO POSTSUM-DDNAMN2                                   
011616     CALL POSTSUM USING POSTSUM-PARM                                      
011617     .                                                                    
011618     EJECT                                                                
011620 S99-ABEND SECTION.                                                       
011801                                                                          
011802     MOVE 'S' TO POSTSUM-OPKOD                                            
011810     CALL POSTSUM USING POSTSUM-PARM                                      
011900     CALL ABEND USING RKOD-ABEND                                          
012000     .                                                                    
012010     EJECT                                                                
012100* --- IMS SEKTIONER ---                                                   
012200                                                                          
017000 IMS-GU-WDGX3156 SECTION.                                                 
017100                                                                          
017200     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-3155-X ')'                    
017300            DELIMITED BY SIZE INTO SSA1                                   
017400     STRING 'WDGX3156(KDSEGKEY =' W-WDGXKEY-3156-X ')'                    
017500            DELIMITED BY SIZE INTO SSA2                                   
017600     MOVE '  '             TO GODK-STATUSKODER                            
017700     CALL CBLTDLI USING GU  3156-PCB DLI-IO-WDGX3156 SSA1 SSA2            
017800     MOVE 3156-STATUS-CODE TO STATUS-WS                                   
017900     PERFORM IMS-STATUSKONTROLL                                           
018000     .                                                                    
019300     EJECT                                                                
019310 IMS-GU-WDK601 SECTION.                                                   
019320                                                                          
019350     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
019360          DELIMITED BY SIZE INTO SSA1                                     
019380     MOVE '  GE'           TO GODK-STATUSKODER                            
019390     CALL CBLTDLI USING GU  WDK6-PCB DLI-IO-WDK601 SSA1                   
019391     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
019392     PERFORM IMS-STATUSKONTROLL                                           
019393     .                                                                    
019394     EJECT                                                                
019395 IMS-GNP-WDK611 SECTION.                                                  
019396                                                                          
019397     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
019398          DELIMITED BY SIZE INTO SSA1                                     
019399     MOVE 'WDK611   '      TO SSA2                                        
019400     MOVE '  GE'           TO GODK-STATUSKODER                            
019401     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
019402     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
019403     PERFORM IMS-STATUSKONTROLL                                           
019404     .                                                                    
019405     EJECT                                                                
019406 IMS-GU-WDD311-BSEQ SECTION.                                              
019407                                                                          
019408     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
019409                                    DELIMITED BY SIZE INTO SSA1           
019410     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
019411                                    DELIMITED BY SIZE INTO SSA2           
019412     MOVE '  '             TO GODK-STATUSKODER                            
019413     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
019414     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
019415     PERFORM IMS-STATUSKONTROLL                                           
019416     .                                                                    
019417     EJECT                                                                
019420 IMS-STATUSKONTROLL SECTION.                                              
019500                                                                          
019600     SET STATUS-IX TO 1                                                   
019700     SEARCH GODK-STATUS                                                   
019800       AT END                                                             
019900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
020000           DELIMITED BY SIZE INTO FELTEXT                                 
020100         DISPLAY FELTEXT                                                  
020200         CALL FELLOG                                                      
020300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
020400         CONTINUE                                                         
020500     END-SEARCH                                                           
020600     .                                                                    
