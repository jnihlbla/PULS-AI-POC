000100 ID DIVISION.                                                             
000201                                                                          
000301 PROGRAM-ID.             W1147600.                                        
000401 AUTHOR.                 STEFAN KIHLBERG                                  
000501     DATE-WRITTEN.       JULI 2012.                                       
000601*                                                                         
000701     REMARKS.                                                             
000801*                                                                         
000901*    FUNKTION:                                                            
001001*            IMS BMP/BATCH HUVUDPROGRAM.                                  
001002*            PGM SKICKAR KÖPANMODAN TILL SI+ FÖR NYA ARTIKLAR             
001101*            MED LOKAL ANSKAFFNING I KINA OCH USA.                        
001102*                                                                         
001201*            LÄSER WDC901 KDANSKQ = 4                                     
001301*                                                                         
001401*            FÖR GODKÄNDA ARTIKLAR:                                       
001402*            WDK611-KDEMBKOD-2 = 20, 25, 30, 35, 40, 45, 50 OCH 80        
001403*            WDK712-BEFT       = 93, 95, 98 OCH 99                        
001404*                                                                         
001405*            UPPDATERA KDANSKQ = 2                                        
001406*                                                                         
001407*                                                                         
001501*                                                                         
001601*            PLOCKAR INFO FRÅN ARTIKELREGISTREN (WDK6, WDK7               
001701*                              WDP3 (ANSK-INFO/WANSNAME).                 
001801*                                                                         
001802*            UPPDATERAR WDC9                                              
001803*            LÄSER      WDK6                                              
001804*            LÄSER      WDK7                                              
001805*                                                                         
001901                                                                          
002001 ENVIRONMENT DIVISION.                                                    
002201 INPUT-OUTPUT SECTION.                                                    
002301                                                                          
002401 FILE-CONTROL.                                                            
002501                                                                          
002601* UTFILER:                                                                
002701*            TRANSAR TILL INKÖP SI+ CHINA / USA                           
002801     SELECT  W11476      ASSIGN    UT-S-W11476D1.                         
002802     SELECT  W11477      ASSIGN    UT-S-W11476D2.                         
002803     SELECT  W11478      ASSIGN    UT-S-W11476D3.                         
002804     SELECT  W1147A      ASSIGN    UT-S-W11476D4.                         
002805     SELECT  W1147B      ASSIGN    UT-S-W11476D5.                         
002806     SELECT  W1147C      ASSIGN    UT-S-W11476D6.                         
002807     SELECT  W1147D      ASSIGN    UT-S-W11476D7.                         
002808     SELECT  W1147E      ASSIGN    UT-S-W11476D8.                         
002809     SELECT  W1147F      ASSIGN    UT-S-W11476D9.                         
002901                                                                          
002902                                                                          
003001 DATA DIVISION.                                                           
003201 FILE SECTION.                                                            
003301                                                                          
003401 FD  W11476                                                               
003501     LABEL RECORD STANDARD                                                
003601     RECORDING      V                                                     
003701     BLOCK CONTAINS 0.                                                    
003901*01  UTPOST-71     -COPY T335R301 -L                                      
004001                                                                          
004002                                                                          
004003 FD  W11477                                                               
004004     LABEL RECORD STANDARD                                                
004005     RECORDING      V                                                     
004006     BLOCK CONTAINS 0.                                                    
004009*01  UTPOST-72     -COPY T335R301 -L                                      
004010                                                                          
004011                                                                          
004012 FD  W11478                                                               
004013     LABEL RECORD STANDARD                                                
004014     RECORDING      V                                                     
004015     BLOCK CONTAINS 0.                                                    
004050*01  UTPOST-73     -COPY T335R301 -L                                      
004060                                                                          
004061 FD  W1147A                                                               
004062     LABEL RECORD STANDARD                                                
004063     RECORDING      V                                                     
004064     BLOCK CONTAINS 0.                                                    
004065*01  UTPOST-41     -COPY T335R301 -L                                      
004066                                                                          
004067 FD  W1147B                                                               
004068     LABEL RECORD STANDARD                                                
004069     RECORDING      V                                                     
004070     BLOCK CONTAINS 0.                                                    
004071*01  UTPOST-43     -COPY T335R301 -L                                      
004072                                                                          
004073 FD  W1147C                                                               
004074     LABEL RECORD STANDARD                                                
004075     RECORDING      V                                                     
004076     BLOCK CONTAINS 0.                                                    
004077*01  UTPOST-44     -COPY T335R301 -L                                      
004078                                                                          
004079 FD  W1147D                                                               
004080     LABEL RECORD STANDARD                                                
004081     RECORDING      V                                                     
004082     BLOCK CONTAINS 0.                                                    
004083*01  UTPOST-45     -COPY T335R301 -L                                      
004084                                                                          
004085 FD  W1147E                                                               
004086     LABEL RECORD STANDARD                                                
004087     RECORDING      V                                                     
004088     BLOCK CONTAINS 0.                                                    
004089*01  UTPOST-46     -COPY T335R301 -L                                      
004090                                                                          
004091 FD  W1147F                                                               
004092     LABEL RECORD STANDARD                                                
004093     RECORDING      V                                                     
004094     BLOCK CONTAINS 0.                                                    
004095*01  UTPOST-47     -COPY T335R301 -L                                      
004096                                                                          
004100                                                                          
004101 WORKING-STORAGE SECTION.                                                 
004201                                                                          
004401 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W1147600'.            
004402 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
004403 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
004501 77  JA                          PIC X       VALUE 'J'.                   
004601 77  NEJ                         PIC X       VALUE 'N'.                   
004701 77  IX                          PIC 9(3)    VALUE ZERO.                  
004801 77  IX-MAX                      PIC 9(3)    VALUE ZERO.                  
004901 77  KOLL-IX                     PIC 9(3)    VALUE ZERO.                  
005201 77  WS-IDHANDLR                 PIC 9(3)    VALUE ZERO.                  
005202 77  WS-IDINK                    PIC 9(3).                                
005203 77  WS-IDLEVNR-DC               PIC X(5).                                
005204 01  WS-IDANSK                   PIC 9(3).                                
005205                                                                          
005301 77  SW-TRAFF                    PIC X       VALUE 'N'.                   
005401                                                                          
005501 01  ARTIKEL-SW                  PIC X       VALUE 'N'.                   
005502     88  ARTIKEL-OK                          VALUE 'J'.                   
005503     88  ARTIKEL-FEL                         VALUE 'N'.                   
005504                                                                          
005505 01  ARTIKELNR.                                                           
005601     03 BLANKA-X                 PIC X(11) VALUE SPACE.                   
005701     03 ARTNR-ALFA               PIC X(9).                                
005801     03 ARTNR-NUM REDEFINES ARTNR-ALFA PIC 9(9).                          
005901                                                                          
006001 01  WS-IDUSER.                                                           
006101     03 WS-IDMAIL                PIC X(9).                                
006201     03 FILLER                   PIC X(51).                               
006301                                                                          
006401 01  KOLL-IDUSER                 PIC X(9).                                
006501 01  FILLER REDEFINES KOLL-IDUSER.                                        
006601     03  KOLL-TKN                PIC X  OCCURS 9.                         
008001                                                                          
008101 01  DAGENS-DATUM.                                                        
008201     03  AAMMDD-DAGENS           PIC 9(6)    VALUE ZERO.                  
008301     03  AAVV                    PIC 9(4)    VALUE ZERO.                  
008401     03  FILLER    REDEFINES AAVV.                                        
008501         05  AA                  PIC 9(2).                                
008601         05  VV                  PIC 9(2).                                
008701                                                                          
009203*01  -COPY WWDCKONS                                                       
009301                                                                          
009302                                                                          
009401*    ---- SUBPROGRAM OCH PARAMETERAREOR                                   
009501                                                                          
009601 01  DYNAMISKA-SUBPROGRAM.                                                
009701   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
009801   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
009901   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
010001   03  WDATKONV                  PIC X(8)    VALUE 'WDATKONV'.            
010201                                                                          
010701*    ---- PARAMETRAR TILL WDATKONV                                        
010901*01  -COPY WDATAREA.                                                      
011001                                                                          
011101*    ---- PARAMETRAR TILL POSTSUM                                         
011301*01  -COPY W0005       -PRE POSTSUM-.                                     
011401                                                                          
011402                                                                          
011501*01  AREA     -COPY T335R301   -PRE UT-                                   
011601                                                                          
011602                                                                          
011701*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA                                
011801 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011901                                                                          
012001*    ---- STATUSKOD FRÅN IMS                                              
012101 01  STATUS-WS                   PIC XX.                                  
012201     88  SEGMENT-FINNS                       VALUE '  '.                  
012301     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012401     88  SEGMENT-SLUT                        VALUE 'GB'.                  
012501     88  IMS-EJ-OK                           VALUE 'XD'.                  
012601                                                                          
012701 01  GODK-STATUSKODER.                                                    
012801   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
012901                                                                          
013001 01  ALL-SSA.                                                             
013002     03 SSA1                     PIC X(64).                               
013101     03 SSA2                     PIC X(64).                               
013102     03 SSA3                     PIC X(64).                               
013201                                                                          
013202                                                                          
013301*    ---- NYCKLAR OCH SÖKFÄLT TILL DLI                                    
013501 01  NYCKLAR-TILL-DLI.                                                    
014101     03  W-KDANSKQ-X.                                                     
014201         05  W-KDANSKQ           PIC X(1)    VALUE '4'.                   
014202     03  W-IDARTNR-X.                                                     
014203         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
014204     03  W-IDLAND-X.                                                      
014206         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
015001     03  W-KDARBTYP-X.                                                    
015101         05  W-KDARBTYP          PIC X(08)   VALUE 'ANSK'.                
015201     03  W-IDPERSON-X.                                                    
015301         05  W-IDPERSON          PIC S9(3)   VALUE ZERO COMP-3.           
015302     03  W-IDDC-X.                                                        
015303         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
015304                                                                          
015401                                                                          
015501*01  -COPY W0003                                                          
015601                                                                          
015701*    ---  DLI INPUT-OUTPUT AREA                                           
016301 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDC901'.         
016401 01  DLI-IO-WDC901.                                                       
016501*    03  -COPY WDC901                                                     
016502                                                                          
016503 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK601'.         
016504 01  DLI-IO-WDK601.                                                       
016505*    03  -COPY WDK601                                                     
016601 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK611'.         
016701 01  DLI-IO-WDK611.                                                       
016801*    03  -COPY WDK611                                                     
016802                                                                          
016803 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK712'.         
016804 01  DLI-IO-WDK712.                                                       
016805*    03  -COPY WDK712                                                     
016806                                                                          
016807 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK722'.         
016808 01  DLI-IO-WDK722.                                                       
016809*    03  -COPY WDK722                                                     
016810                                                                          
017603 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDP311'.         
017701 01  DLI-IO-WDP311.                                                       
017801*    03  -COPY WDP311 -PRE WDP3-                                          
017901                                                                          
017902                                                                          
017903 01  FILLER                      PIC X(16)   VALUE 'WDB601 AREA'.         
017904 01   DLI-IO-AREA-B601.                                                   
017905*     03  -COPY WDB601                                                    
017906                                                                          
018001 LINKAGE SECTION.                                                         
018101                                                                          
018201*01  -COPY W0009     -PRE MSG-                                            
018301                                                                          
018701*01  -COPY W0008     -PRE WDC9-                                           
018801      05 FILLER       PIC X.                                              
018901                                                                          
018902*01  -COPY W0008     -PRE WDK6-                                           
018903      05 FILLER       PIC X.                                              
018904                                                                          
018905*01  -COPY W0008     -PRE WDK7-                                           
018906      05 FILLER       PIC X.                                              
018907                                                                          
019301*01  -COPY W0008     -PRE WDP3-                                           
019401      05  FILLER      PIC X.                                              
019501                                                                          
019502*01  -COPY W0008     -PRE WDB6-                                           
019503      05  FILLER      PIC X.                                              
019504                                                                          
019601 PROCEDURE DIVISION USING  MSG-PCB  WDC9-PCB WDK6-PCB WDK7-PCB            
019701                           WDP3-PCB WDB6-PCB.                             
019801     ENTRY 'DLITCBL' USING MSG-PCB  WDC9-PCB WDK6-PCB WDK7-PCB            
019901                           WDP3-PCB WDB6-PCB.                             
020001                                                                          
020101     PERFORM A-INIT                                                       
020201     PERFORM IMS-GHN-WDC901                                               
020401                                                                          
020502     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
020602        MOVE KART-IDDC        TO W-IDDC                                   
020603        MOVE KART-IDARTNR     TO W-IDARTNR                                
020604                                                                          
020605        IF (KART-KDANSKQ = '2' OR '4')                                    
020606        AND KART-TIINKOP = ZERO                                           
020609           PERFORM IMS-GU-WDB601                                          
020610           IF DCS-NDC-CN OR (DCS-NDC-NA AND DCS-USA)                      
020611              MOVE DCS-IDLANDX2 TO W-IDLAND                               
020612                                                                          
020701              PERFORM B-KOLLA-ARTIKEL                                     
020702              IF ARTIKEL-OK                                               
020705*  SKAPA INKOPSBEGARAN                                                    
020706                 PERFORM C-KOLLA-IDINK                                    
020707                 PERFORM D-KOLLA-SUPPLIER                                 
020708                 PERFORM E-SKAPA-UTPOST                                   
023302                                                                          
023303                 IF KART-KVPROG = ZERO                                    
023304                    PERFORM IMS-DLET-WDC901                               
023305                 ELSE                                                     
023306*  MARKERA INKOPSBEGARAN SÄND TILL INKÖP                                  
023307                    MOVE '2'        TO KART-KDANSKQ                       
023308                    MOVE AAMMDD-DAGENS TO KART-TIINKOP                    
023309                    PERFORM IMS-REPL-WDC901                               
023310                 END-IF                                                   
023311                                                                          
023312              END-IF                                                      
023313           END-IF                                                         
023314        END-IF                                                            
023315        PERFORM IMS-GHN-WDC901                                            
023320                                                                          
023901     END-PERFORM                                                          
024201                                                                          
024301     PERFORM Z-FINIT                                                      
024401                                                                          
025101     MOVE ZERO TO RETURN-CODE                                             
025201     GOBACK                                                               
025301     .                                                                    
025401                                                                          
025402                                                                          
025501 A-INIT SECTION.                                                          
025502     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
025601                                                                          
025901     MOVE 'IDAG'  TO DAT-KDDATFORM                                        
026001     MOVE  ZERO   TO DAT-I-TIDATUM                                        
026101     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
026201                         DAT-O-TIDATUM DAT-KDSVAR                         
026301     IF DAT-KDSVAR-OK                                                     
026401       MOVE DAT-TIAAMMDD    TO  AAMMDD-DAGENS                             
026501       MOVE DAT-TIAA-VECKA  TO  AA                                        
026601       MOVE DAT-TIVV        TO  VV                                        
026701     END-IF                                                               
026801                                                                          
026901     OPEN OUTPUT W11476 W11477 W11478                                     
026902                 W1147A W1147B W1147C                                     
026903                 W1147D W1147E W1147F                                     
027301     .                                                                    
027401                                                                          
027402                                                                          
030600 B-KOLLA-ARTIKEL      SECTION.                                            
030610     MOVE 'B-KOLLA-ARTIKEL ' TO CURRENT-SECTION                           
030700                                                                          
030701     MOVE NEJ         TO ARTIKEL-SW                                       
030702                                                                          
030703     PERFORM IMS-GU-WDK601                                                
030704     IF SEGMENT-FINNS                                                     
030711        PERFORM IMS-GU-WDK712                                             
030712        IF  SEGMENT-FINNS                                                 
030713           IF  LART-BEFT          = 93 OR 95 OR 98 OR 99                  
030714                MOVE JA  TO ARTIKEL-SW                                    
030715           END-IF                                                         
030716        END-IF                                                            
030717        IF ARTIKEL-FEL                                                    
030718           PERFORM IMS-GU-WDK611                                          
030719           IF SEGMENT-FINNS                                               
030720           AND (CLAG-BEFT       = 93 OR 95 OR 98 OR 99                    
030721               OR                                                         
030722               CLAG-KDEMBKOD-2 = 20 OR 25 OR 30 OR 35 OR 40 OR            
030723                                 45 OR 50 OR 80)                          
030724                                                                          
030725             MOVE JA TO ARTIKEL-SW                                        
030726           END-IF                                                         
030727        END-IF                                                            
030728     END-IF                                                               
030729     .                                                                    
030730                                                                          
030740                                                                          
030774 C-KOLLA-IDINK SECTION.                                                   
030775     MOVE 'C-KOLLA-IDINK   ' TO CURRENT-SECTION                           
030776                                                                          
030777     PERFORM IMS-GU-WDK722                                                
030778     IF XLAG-IDINK (1:3) NUMERIC                                          
030779        MOVE XLAG-IDINK (1:3)    TO WS-IDINK                              
030780     ELSE                                                                 
030781        IF XLAG-IDINK (2:3) NUMERIC                                       
030782           MOVE XLAG-IDINK (2:3) TO WS-IDINK                              
030783        ELSE                                                              
030784            MOVE ZERO            TO WS-IDINK                              
030785        END-IF                                                            
030786     END-IF                                                               
030788                                                                          
030789     MOVE ZERO TO WS-IDHANDLR                                             
030790     IF (WS-IDINK > +99 AND < +790) OR                                    
030791        (WS-IDINK > +799 AND < +987) OR                                   
030792        (WS-IDINK > +987 AND <= +999)                                     
030793         MOVE WS-IDINK TO WS-IDHANDLR                                     
030794     END-IF                                                               
030795     .                                                                    
030796                                                                          
030797                                                                          
030798 D-KOLLA-SUPPLIER SECTION.                                                
030799     MOVE 'D-KOLLA-SUPPL   ' TO CURRENT-SECTION                           
030800                                                                          
030801     IF DCS-NDC-CN                                                        
030802       MOVE DCS-IDLEVNR-DC TO WS-IDLEVNR-DC                               
030803     ELSE                                                                 
030804       MOVE DCS-IDLEVNR-EMB TO WS-IDLEVNR-DC                              
030805     END-IF                                                               
030818     .                                                                    
030820                                                                          
030821 E-SKAPA-UTPOST SECTION.                                                  
030822     MOVE 'E-SKAPA-UTPOST  ' TO CURRENT-SECTION                           
030823                                                                          
030830     MOVE '301'              TO UT-IDRT                                   
030900     MOVE SPACE              TO UT-COMMON-AREA                            
031000                                UT-NP-AREA                                
031100     MOVE WS-IDHANDLR        TO UT-IDHANDLR                               
031200     MOVE ART-IDARTNR        TO ARTNR-NUM                                 
031300     INSPECT ARTNR-ALFA REPLACING LEADING ZEROES BY SPACE                 
031400     MOVE ARTIKELNR          TO UT-IDPITEM                                
031500     MOVE 'PR'               TO UT-CDTYPE-REQ                             
031600     MOVE 'V'                TO UT-CD-IDPITEM                             
031700     MOVE 'VCC '             TO UT-IDPORG                                 
031801     MOVE SPACE              TO UT-TXNOTES-REQ1                           
031900     MOVE ART-KDSORT         TO UT-NP-CDUOM                               
032000*******                                                                   
032100     IF ART-KDSORT = 'ST' OR 'SA' OR 'SW' OR 'TM' OR 'HW' OR 'PA'         
032300        MOVE 'PCE'           TO UT-NP-CDUOM                               
032400     ELSE                                                                 
032500        IF ART-KDSORT = 'MM'                                              
032600           MOVE 'MMT'        TO UT-NP-CDUOM                               
032700        END-IF                                                            
032800        IF ART-KDSORT = 'M'                                               
032900           MOVE 'MTR'        TO UT-NP-CDUOM                               
033000        END-IF                                                            
033100        IF ART-KDSORT = 'M2'                                              
033200           MOVE 'MTK'        TO UT-NP-CDUOM                               
033300        END-IF                                                            
033400        IF ART-KDSORT = 'M3'                                              
033500           MOVE 'MTQ'        TO UT-NP-CDUOM                               
033600        END-IF                                                            
033700        IF ART-KDSORT = 'ML'                                              
033800           MOVE 'MLT'        TO UT-NP-CDUOM                               
033900        END-IF                                                            
034000        IF ART-KDSORT = 'L'                                               
034100           MOVE 'LTR'        TO UT-NP-CDUOM                               
034200        END-IF                                                            
034300        IF ART-KDSORT = 'G'                                               
034400           MOVE 'GRM'        TO UT-NP-CDUOM                               
034500        END-IF                                                            
034600        IF ART-KDSORT = 'KG'                                              
034700           MOVE 'KGM'        TO UT-NP-CDUOM                               
034800        END-IF                                                            
034900        IF ART-KDSORT = 'C2'                                              
035000           MOVE 'C2 '        TO UT-NP-CDUOM                               
035100        END-IF                                                            
035200     END-IF                                                               
035300*******                                                                   
035400                                                                          
035500     IF XLAG-IDANSK > 0                                                   
035600        MOVE XLAG-IDANSK     TO WS-IDANSK                                 
035700        MOVE WS-IDANSK       TO W-IDPERSON                                
035800        PERFORM IMS-GU-WDP311                                             
035900        IF SEGMENT-SAKNAS                                                 
036000           DISPLAY ' IDANSK SAKNAS PÅ P311: ' WS-IDANSK                   
036100        ELSE                                                              
036200           MOVE WDP3-PERS-IDNAMN TO UT-NMHANDLR-ISSUER                    
036300           MOVE WDP3-PERS-IDTFN  TO UT-IDPHONE-ISSUER                     
036400           MOVE WDP3-PERS-IDAVD  TO UT-IDSECTN-ISSUER                     
036500           MOVE WDP3-PERS-IDMAIL TO WS-IDUSER                             
036600           PERFORM EA-KOLLA-IDUSER                                        
036700        END-IF                                                            
036800     END-IF                                                               
036900                                                                          
038720     MOVE KART-TILEVBEG        TO UT-NP-TIPROD-DATE                       
038740     MOVE KART-KVPROG          TO UT-NP-QTPITEM-YEAR                      
038800                                                                          
038900     MOVE 'N'                  TO UT-FLPLANT-BUYER                        
039000     MOVE 'Y'                  TO UT-FLCOBL-ALLOWED                       
039100                                  UT-NP-FLCOBL-REQUIRED                   
039201     MOVE WS-IDLEVNR-DC        TO UT-NP-IDUSER                            
039300     MOVE ZERO                 TO UT-IDCONSIG                             
039400                                  UT-NP-QTPITEM-ORDER                     
039500                                  UT-NP-TIPRE-DEL-DATE-1                  
039600                                  UT-NP-QTPRE-DEL-1                       
039700                                  UT-NP-TIPRE-DEL-2                       
039800                                  UT-NP-TIPRE-DEL-DATE-2                  
039900                                  UT-NP-QTPRE-DEL-2                       
040000                                  UT-NP-TIPRE-DEL-3                       
040100                                  UT-NP-TIPRE-DEL-DATE-3                  
040200                                  UT-NP-QTPRE-DEL-3                       
040300                                  UT-NP-TISAMPLE                          
040400                                  UT-NP-TISAMPLE-DATE                     
040500                                  UT-NP-QTSAMPLE                          
040600                                                                          
040700     EVALUATE W-IDDC                                                      
040800       WHEN WC-NDC-CN-71                                                  
040900         PERFORM S01-SKRIV-UTPOST-71                                      
041000       WHEN WC-NDC-CN-72                                                  
041002         PERFORM S01-SKRIV-UTPOST-72                                      
041003       WHEN WC-NDC-CN-73                                                  
041004         PERFORM S01-SKRIV-UTPOST-73                                      
041005       WHEN WC-NDC-US-RU                                                  
041006         PERFORM S01-SKRIV-UTPOST-41                                      
041007       WHEN WC-NDC-US-LA                                                  
041008         PERFORM S01-SKRIV-UTPOST-43                                      
041009       WHEN WC-NDC-US-SE                                                  
041010         PERFORM S01-SKRIV-UTPOST-44                                      
041011       WHEN WC-NDC-US-CH                                                  
041012         PERFORM S01-SKRIV-UTPOST-45                                      
041013       WHEN WC-NDC-US-JA                                                  
041014         PERFORM S01-SKRIV-UTPOST-46                                      
041015       WHEN WC-NDC-US-DA                                                  
041016         PERFORM S01-SKRIV-UTPOST-47                                      
041020     END-EVALUATE                                                         
041400     .                                                                    
041500                                                                          
041510                                                                          
041600 EA-KOLLA-IDUSER SECTION.                                                 
041610     MOVE 'EA-KOLLA-IDUSER ' TO CURRENT-SECTION                           
041700                                                                          
041800     MOVE NEJ       TO SW-TRAFF                                           
041900     MOVE WS-IDMAIL TO KOLL-IDUSER                                        
042000     MOVE 1         TO IX                                                 
042100     MOVE 9         TO IX-MAX                                             
042200     PERFORM UNTIL IX > IX-MAX                                            
042300        IF KOLL-TKN(IX) = '@'                                             
042400           MOVE IX  TO KOLL-IX                                            
042500           MOVE 10  TO IX                                                 
042600           MOVE JA  TO SW-TRAFF                                           
042700        ELSE                                                              
042800           ADD 1    TO IX                                                 
042900        END-IF                                                            
043000     END-PERFORM                                                          
043100                                                                          
043200     IF SW-TRAFF = NEJ                                                    
043300        MOVE KOLL-IDUSER(1:8)  TO UT-IDUSERID-ISSUER                      
043400     ELSE                                                                 
043500       ADD -1 TO KOLL-IX                                                  
043600                                                                          
043700       IF KOLL-IX = 1                                                     
043800        MOVE KOLL-IDUSER(1:1)  TO UT-IDUSERID-ISSUER                      
043900       ELSE                                                               
044000        IF KOLL-IX = 2                                                    
044100         MOVE KOLL-IDUSER(1:2) TO UT-IDUSERID-ISSUER                      
044200        ELSE                                                              
044300         IF KOLL-IX = 3                                                   
044400          MOVE KOLL-IDUSER(1:3) TO UT-IDUSERID-ISSUER                     
044500         ELSE                                                             
044600          IF KOLL-IX = 4                                                  
044700           MOVE KOLL-IDUSER(1:4) TO UT-IDUSERID-ISSUER                    
044800          ELSE                                                            
044900           IF KOLL-IX = 5                                                 
045000            MOVE KOLL-IDUSER(1:5) TO UT-IDUSERID-ISSUER                   
045100           ELSE                                                           
045200            IF KOLL-IX = 6                                                
045300             MOVE KOLL-IDUSER(1:6) TO UT-IDUSERID-ISSUER                  
045400            ELSE                                                          
045500             IF KOLL-IX = 7                                               
045600              MOVE KOLL-IDUSER(1:7) TO UT-IDUSERID-ISSUER                 
045700             ELSE                                                         
045800              IF KOLL-IX = 8                                              
045900               MOVE KOLL-IDUSER(1:8) TO UT-IDUSERID-ISSUER                
046000              END-IF                                                      
046100             END-IF                                                       
046200            END-IF                                                        
046300           END-IF                                                         
046400          END-IF                                                          
046500         END-IF                                                           
046600        END-IF                                                            
046700       END-IF                                                             
046800     END-IF                                                               
046900     .                                                                    
047000                                                                          
047010                                                                          
047100 Z-FINIT   SECTION.                                                       
047110     MOVE 'Z-FINIT         ' TO CURRENT-SECTION                           
047200                                                                          
047301     CLOSE W11476 W11477 W11478                                           
047302           W1147A W1147B W1147C                                           
047303           W1147D W1147E W1147F                                           
047400                                                                          
047500     MOVE 'S' TO POSTSUM-OPKOD                                            
047600     CALL POSTSUM USING POSTSUM-PARM                                      
047700     .                                                                    
047800                                                                          
047900                                                                          
049401 S01-SKRIV-UTPOST-71 SECTION.                                             
049402     MOVE 'SKRIV-UTPOST-71 ' TO CURRENT-SECTION                           
049403                                                                          
049500     MOVE SPACE      TO  POSTSUM-TRANSTYP                                 
049700     WRITE UTPOST-71 FROM UT-AREA                                         
049710     MOVE 'W11476'   TO  POSTSUM-PROGNAMN                                 
049720                         POSTSUM-FDNAMN                                   
049730     MOVE 'W11476D1' TO  POSTSUM-DDNAMN2                                  
049731     CALL POSTSUM USING POSTSUM-PARM                                      
049740                                                                          
049800     .                                                                    
049900                                                                          
050000 S01-SKRIV-UTPOST-72 SECTION.                                             
050001     MOVE 'SKRIV-UTPOST-72 ' TO CURRENT-SECTION                           
050002                                                                          
050003     MOVE SPACE      TO  POSTSUM-TRANSTYP                                 
050005     WRITE UTPOST-72 FROM UT-AREA                                         
050006     MOVE 'W11477'   TO  POSTSUM-PROGNAMN                                 
050007                         POSTSUM-FDNAMN                                   
050008     MOVE 'W11476D2' TO  POSTSUM-DDNAMN2                                  
050009     CALL POSTSUM USING POSTSUM-PARM                                      
050010                                                                          
050011     .                                                                    
050012                                                                          
050013 S01-SKRIV-UTPOST-73 SECTION.                                             
050014     MOVE 'SKRIV-UTPOST-73 ' TO CURRENT-SECTION                           
050015                                                                          
050016     MOVE SPACE      TO  POSTSUM-TRANSTYP                                 
050018     WRITE UTPOST-73 FROM UT-AREA                                         
050019     MOVE 'W11478'   TO  POSTSUM-PROGNAMN                                 
050020                         POSTSUM-FDNAMN                                   
050021     MOVE 'W11476D3' TO  POSTSUM-DDNAMN2                                  
050022     CALL POSTSUM USING POSTSUM-PARM                                      
050023     .                                                                    
050024                                                                          
050027 S01-SKRIV-UTPOST-41 SECTION.                                             
050028     MOVE 'SKRIV-UTPOST-41 ' TO CURRENT-SECTION                           
050029                                                                          
050030     MOVE SPACE      TO  POSTSUM-TRANSTYP                                 
050040     WRITE UTPOST-41 FROM UT-AREA                                         
050050     MOVE 'W1147A'   TO  POSTSUM-PROGNAMN                                 
050060                         POSTSUM-FDNAMN                                   
050070     MOVE 'W11476D4' TO  POSTSUM-DDNAMN2                                  
050080     CALL POSTSUM USING POSTSUM-PARM                                      
050090     .                                                                    
050300                                                                          
050400 S01-SKRIV-UTPOST-43 SECTION.                                             
050500     MOVE 'SKRIV-UTPOST-43 ' TO CURRENT-SECTION                           
050600                                                                          
050700     MOVE SPACE      TO  POSTSUM-TRANSTYP                                 
050800     WRITE UTPOST-43 FROM UT-AREA                                         
050900     MOVE 'W1147B'   TO  POSTSUM-PROGNAMN                                 
050901                         POSTSUM-FDNAMN                                   
050902     MOVE 'W11476D5' TO  POSTSUM-DDNAMN2                                  
050903     CALL POSTSUM USING POSTSUM-PARM                                      
050904     .                                                                    
050907                                                                          
050908 S01-SKRIV-UTPOST-44 SECTION.                                             
050909     MOVE 'SKRIV-UTPOST-44 ' TO CURRENT-SECTION                           
050910                                                                          
050911     MOVE SPACE      TO  POSTSUM-TRANSTYP                                 
050912     WRITE UTPOST-44 FROM UT-AREA                                         
050913     MOVE 'W1147C'   TO  POSTSUM-PROGNAMN                                 
050914                         POSTSUM-FDNAMN                                   
050915     MOVE 'W11476D6' TO  POSTSUM-DDNAMN2                                  
050916     CALL POSTSUM USING POSTSUM-PARM                                      
050917     .                                                                    
050920                                                                          
050921 S01-SKRIV-UTPOST-45 SECTION.                                             
050922     MOVE 'SKRIV-UTPOST-45 ' TO CURRENT-SECTION                           
050923                                                                          
050924     MOVE SPACE      TO  POSTSUM-TRANSTYP                                 
050925     WRITE UTPOST-45 FROM UT-AREA                                         
050926     MOVE 'W1147D'   TO  POSTSUM-PROGNAMN                                 
050927                         POSTSUM-FDNAMN                                   
050928     MOVE 'W11476D7' TO  POSTSUM-DDNAMN2                                  
050929     CALL POSTSUM USING POSTSUM-PARM                                      
050930     .                                                                    
050933                                                                          
050934 S01-SKRIV-UTPOST-46 SECTION.                                             
050935     MOVE 'SKRIV-UTPOST-46 ' TO CURRENT-SECTION                           
050936                                                                          
050937     MOVE SPACE      TO  POSTSUM-TRANSTYP                                 
050938     WRITE UTPOST-46 FROM UT-AREA                                         
050939     MOVE 'W1147E'   TO  POSTSUM-PROGNAMN                                 
050940                         POSTSUM-FDNAMN                                   
050941     MOVE 'W11476D8' TO  POSTSUM-DDNAMN2                                  
050942     CALL POSTSUM USING POSTSUM-PARM                                      
050943     .                                                                    
050944 S01-SKRIV-UTPOST-47 SECTION.                                             
050945     MOVE 'SKRIV-UTPOST-47 ' TO CURRENT-SECTION                           
050946                                                                          
050947     MOVE SPACE      TO  POSTSUM-TRANSTYP                                 
050948     WRITE UTPOST-47 FROM UT-AREA                                         
050949     MOVE 'W1147F'   TO  POSTSUM-PROGNAMN                                 
050950                         POSTSUM-FDNAMN                                   
050951     MOVE 'W11476D9' TO  POSTSUM-DDNAMN2                                  
050952     CALL POSTSUM USING POSTSUM-PARM                                      
050953     .                                                                    
050954     EJECT                                                                
050955* --- IMS SEKTIONER ---                                                   
050956                                                                          
050957 IMS-GHN-WDC901    SECTION.                                               
050958     MOVE 'IMS-GHN-WDC901  ' TO CURRENT-IMS-SECTION                       
050959                                                                          
050960     MOVE SPACE                 TO ALL-SSA                                
051020     MOVE 'WDC901 '             TO SSA1                                   
051030     MOVE '  GEGB'              TO GODK-STATUSKODER                       
051200     CALL CBLTDLI USING GHN WDC9-PCB DLI-IO-WDC901 SSA1                   
051300     MOVE WDC9-STATUS-CODE      TO STATUS-WS                              
051400     PERFORM IMS-STATUSKONTROLL                                           
051500     .                                                                    
051600                                                                          
051700 IMS-REPL-WDC901    SECTION.                                              
051701     MOVE 'IMS-REPL-WDC901 ' TO CURRENT-IMS-SECTION                       
051702                                                                          
051703     MOVE SPACE                 TO ALL-SSA                                
051704     MOVE '    '                TO GODK-STATUSKODER                       
051705     CALL CBLTDLI USING REPL WDC9-PCB DLI-IO-WDC901                       
051706     MOVE WDC9-STATUS-CODE      TO STATUS-WS                              
051707     PERFORM IMS-STATUSKONTROLL                                           
051708     .                                                                    
051709                                                                          
051710                                                                          
051720 IMS-DLET-WDC901    SECTION.                                              
051730     MOVE 'IMS-DLET-WDC901 ' TO CURRENT-IMS-SECTION                       
051740                                                                          
051750     MOVE SPACE                 TO ALL-SSA                                
051760     MOVE '    '                TO GODK-STATUSKODER                       
051770     CALL CBLTDLI USING DLET WDC9-PCB DLI-IO-WDC901                       
051780     MOVE WDC9-STATUS-CODE      TO STATUS-WS                              
051790     PERFORM IMS-STATUSKONTROLL                                           
051800     .                                                                    
051900                                                                          
052000                                                                          
052401 IMS-GU-WDK601 SECTION.                                                   
052402     MOVE 'IMS-GU-WDK601   ' TO CURRENT-IMS-SECTION                       
052403                                                                          
052404     MOVE SPACE                 TO ALL-SSA                                
052500     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
052600            DELIMITED BY SIZE INTO SSA1                                   
052700     MOVE '  GE'                TO GODK-STATUSKODER                       
052801     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
052901     MOVE WDK6-STATUS-CODE      TO STATUS-WS                              
053000     PERFORM IMS-STATUSKONTROLL                                           
053100     .                                                                    
053200                                                                          
053210                                                                          
053300 IMS-GU-WDK611 SECTION.                                                   
053310     MOVE 'IMS-GU-WDK611   ' TO CURRENT-IMS-SECTION                       
053320                                                                          
053330     MOVE SPACE                 TO ALL-SSA                                
053400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
053401            DELIMITED BY SIZE INTO SSA1                                   
053402     MOVE  'WDK611   '          TO SSA2                                   
053500     MOVE '  GE'                TO GODK-STATUSKODER                       
053601     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
053701     MOVE WDK6-STATUS-CODE      TO STATUS-WS                              
053800     PERFORM IMS-STATUSKONTROLL                                           
053900     .                                                                    
054000                                                                          
054100                                                                          
054200 IMS-GU-WDK712 SECTION.                                                   
054300     MOVE 'IMS-GU-WDK712   ' TO CURRENT-IMS-SECTION                       
054400                                                                          
054410     MOVE SPACE                 TO ALL-SSA                                
054500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
054600            DELIMITED BY SIZE INTO SSA1                                   
054610     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
054620            DELIMITED BY SIZE INTO SSA2                                   
054700     MOVE '  GE'                TO GODK-STATUSKODER                       
054800     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2               
054900     MOVE WDK7-STATUS-CODE      TO STATUS-WS                              
054910     PERFORM IMS-STATUSKONTROLL                                           
054920     .                                                                    
054930                                                                          
054940                                                                          
054950 IMS-GU-WDK722 SECTION.                                                   
054960     MOVE 'IMS-GU-WDK722   ' TO CURRENT-IMS-SECTION                       
054970                                                                          
054971     MOVE SPACE                 TO ALL-SSA                                
054980     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
054990            DELIMITED BY SIZE INTO SSA1                                   
054991     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
054992            DELIMITED BY SIZE INTO SSA2                                   
054993     MOVE 'WDK722 '             TO SSA3                                   
054994     MOVE '    '                TO GODK-STATUSKODER                       
054995     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3          
054996     MOVE WDK7-STATUS-CODE      TO STATUS-WS                              
054997     PERFORM IMS-STATUSKONTROLL                                           
054998     .                                                                    
054999                                                                          
055000                                                                          
055001 IMS-GU-WDP311      SECTION.                                              
055010     MOVE 'IMS-GU-WDBP311  ' TO CURRENT-IMS-SECTION                       
055020                                                                          
055030     MOVE SPACE                 TO ALL-SSA                                
055100     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
055200          DELIMITED BY SIZE INTO SSA1                                     
055300     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
055400          DELIMITED BY SIZE INTO SSA2                                     
055500     MOVE '  GE'              TO GODK-STATUSKODER                         
055600     CALL CBLTDLI USING GU  WDP3-PCB DLI-IO-WDP311 SSA1 SSA2              
055700     MOVE WDP3-STATUS-CODE    TO STATUS-WS                                
055800     PERFORM IMS-STATUSKONTROLL                                           
055900     .                                                                    
056000                                                                          
056001                                                                          
056010 IMS-GU-WDB601    SECTION.                                                
056011     MOVE 'IMS-GU-WDB601   ' TO CURRENT-IMS-SECTION                       
056012                                                                          
056013     MOVE SPACE                 TO ALL-SSA                                
056020     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
056030          DELIMITED BY SIZE INTO SSA1                                     
056040     MOVE '  GE'              TO GODK-STATUSKODER                         
056050     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
056060     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
056070     PERFORM IMS-STATUSKONTROLL                                           
056080     IF SEGMENT-SAKNAS                                                    
056090        MOVE SPACE TO DCS-KDDC                                            
056091     END-IF                                                               
056092     .                                                                    
056093                                                                          
056094                                                                          
056100 IMS-STATUSKONTROLL SECTION.                                              
056200     SET STATUS-IX TO 1                                                   
056300     SEARCH GODK-STATUS                                                   
056400       AT END CALL FELLOG                                                 
056500       WHEN GODK-STATUS(STATUS-IX) = STATUS-WS CONTINUE                   
056600     END-SEARCH                                                           
056700     .                                                                    
