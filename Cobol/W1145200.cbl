000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1145200.                                                
000300 AUTHOR.         BODIL LINDAHL.                                           
000400 DATE-WRITTEN.   JANUARI 2006                                             
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        LÄSER FIL ANNULLATIONER OCH SKAPAR FIL TILL PV INKÖP.            
001000*                                                                         
001100*                                                                         
001200*    ABENDKODER:                                                          
001300*        U0016 -  . . . .                                                 
001400*        U1000 -  . . . .                                                 
001500*                                                                         
001500* CCID 10277114 -> NEW MAIL ADDRESSES                                     
001500*                                                                         
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*          --- ANNULATIONER                                               
002500     SELECT INFIL                      ASSIGN TO W11452D1.                
002501     SKIP2                                                                
002502*          --- UTFIL                                                      
002503     SELECT W11452                     ASSIGN TO W11452D2.                
002504*          --- UTFIL MAIL DATA                                            
002505     SELECT W11453                     ASSIGN TO W11452D3.                
002506*          --- UTFIL MAIL ADR                                             
002507     SELECT W11457                     ASSIGN TO W11452D4.                
002508     EJECT                                                                
002509 DATA DIVISION.                                                           
002510                                                                          
002511 FILE SECTION.                                                            
002512     SKIP2                                                                
002513 FD  INFIL                                                                
002514     RECORDING       F                                                    
002520     BLOCK CONTAINS  0.                                                   
002530     SKIP3                                                                
002540 01  IN-POST         PIC X(80).                                           
002550     SKIP3                                                                
002560 FD  W11452                                                               
002570     RECORDING       V                                                    
002580     BLOCK CONTAINS  0.                                                   
002590*01  POST -COPY T335R301 -PRE  UT-  -L.                                   
002591     SKIP3                                                                
002592 FD  W11453                                                               
002593     RECORDING       F                                                    
002594     BLOCK CONTAINS  0.                                                   
002595*01  POST -COPY W11453   -PRE  UT2-  -L.                                  
002600     SKIP3                                                                
002610 FD  W11457                                                               
002620     RECORDING       F                                                    
002630     BLOCK CONTAINS  0.                                                   
002640 01  UT3-POST           PIC X(80).                                        
002650     EJECT                                                                
002700 WORKING-STORAGE SECTION.                                                 
002710*    -COPY WY2000W1                                                       
002720 77  PROGRAM-NAMN                PIC X(8) VALUE 'W1145200'.               
002730 77  JA                          PIC X       VALUE 'J'.                   
002740 77  NEJ                         PIC X       VALUE 'N'.                   
002750 77  IX                          PIC 9(3)    VALUE ZERO.                  
002760 77  IX-MAX                      PIC 9(3)    VALUE ZERO.                  
002770 77  KOLL-IX                     PIC 9(3)    VALUE ZERO.                  
002792 77  WS-IDARTNR                  PIC 9(8)    VALUE ZERO.                  
002793 77  SPAR-ARTC-IDANSK            PIC 9(3)    VALUE ZERO.                  
002794 77  SPAR-ARTG-TEANSINK          PIC X(50)   VALUE SPACE.                 
002795 77  SPAR-KDERS                  PIC 9(2)    VALUE ZERO.                  
002796 77  SW-TRAFF                    PIC X       VALUE 'N'.                   
002797                                                                          
002798 01  ARTIKELNR.                                                           
002799     03 BLANKA-X                 PIC X(11) VALUE SPACE.                   
002800     03 ARTNR-ALFA               PIC X(9).                                
002801     03 ARTNR-NUM REDEFINES ARTNR-ALFA PIC 9(9).                          
002802                                                                          
002803 01  WS-IDUSER.                                                           
002804     03 WS-IDMAIL                PIC X(9).                                
002805     03 FILLER                   PIC X(51).                               
002806                                                                          
002807 01  KOLL-IDUSER                 PIC X(9).                                
002808 01  FILLER REDEFINES KOLL-IDUSER.                                        
002809     03  KOLL-TKN                PIC X  OCCURS 9.                         
002810                                                                          
002818 01  WS-DATUM.                                                            
002819     03  AAMMDD                  PIC 9(6)    VALUE ZERO.                  
002820     03  AAVV                    PIC 9(4)    VALUE ZERO.                  
002821     03  FILLER    REDEFINES AAVV.                                        
002822         05  AA                  PIC 9(2).                                
002823         05  VV                  PIC 9(2).                                
002824                                                                          
002825 01  W-ANSK-TEL.                                                          
002826     03  W-IDANSK                PIC 9(3).                                
002827     03  FILLER                  PIC X.                                   
002828     03  W-EXTTEL.                                                        
002829         05  W-IDTFN             PIC X(16).                               
002830                                                                          
002831 01  MAIL-RAD.                                                            
002832     03 FILLER                   PIC X(5) VALUE 'DEST '.                  
002833     03 MAIL-ADRESS              PIC X(60).                               
002840                                                                          
003700 77  INFIL-EOF-SW                PIC X       VALUE 'N'.                   
003800     88  END-OF-INFIL                        VALUE 'J'.                   
003900                                                                          
004500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
004600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
004700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
004800                                                                          
004900 01  FELTEXT.                                                             
005000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005200     EJECT                                                                
005400*01  -COPY W0005           -PRE  POSTSUM-                                 
005401     EJECT                                                                
005410*01  -COPY WDATAREA.                                                      
005420     EJECT                                                                
005500     EJECT                                                                
005600 01  IN-AREA-START                PIC X(24)   VALUE                       
005700                                 'IN-AREA-START  '.                       
006300*01  AREA  -COPY A310TB65  -PRE IN-                                       
006400     EJECT                                                                
006500 01  UT-AREA-START               PIC X(24)   VALUE                        
006600                                 'UT-AREA-START  '.                       
006700*01  AREA  -COPY T335R301  -PRE UT-                                       
006701     EJECT                                                                
006710 01  UT2-AREA-START               PIC X(24)   VALUE                       
006720                                 'UT2-AREA-START  '.                      
006730*01  AREA  -COPY W11453    -PRE UT2-                                      
006740     EJECT                                                                
006750 01  UT3-AREA-START               PIC X(24)   VALUE                       
006760                                 'UT3-AREA-START  '.                      
006770 01  UT3-AREA                     PIC X(80).                              
006800     EJECT                                                                
006830 01  DYNAMISKA-SUBPROGRAM.                                                
006840   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
006850   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
006860   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
006870   03  WDATKONV                  PIC X(8)    VALUE 'WDATKONV'.            
006890     EJECT                                                                
006905*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA                                
006906 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
006907     SKIP3                                                                
006908*    ---- STATUSKOD FRÅN IMS                                              
006909 01  STATUS-WS                   PIC XX.                                  
006910     88  SEGMENT-FINNS                       VALUE '  '.                  
006911     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
006912     88  SEGMENT-SLUT                        VALUE 'GE'.                  
006913     88  IMS-EJ-OK                           VALUE 'XD'.                  
006914     SKIP3                                                                
006915 01  GODK-STATUSKODER.                                                    
006916   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
006917     SKIP3                                                                
006918 01  SSA1                        PIC X(64).                               
006919 01  SSA2                        PIC X(64).                               
006920     EJECT                                                                
006921*    ---- NYCKLAR TILL DLI                                                
006922 01  NYCKLAR-TILL-DLI.                                                    
006923   03  W-IDARTNR-X.                                                       
006924     05  W-IDARTNR               PIC S9(9)                COMP-3.         
006928   03  W-KDARBTYP-X.                                                      
006929     05  W-KDARBTYP              PIC X(08)    VALUE 'ANSK'.               
006930   03  W-IDPERSON-X.                                                      
006931     05  W-IDPERSON              PIC S9(3)    VALUE ZERO COMP-3.          
006932     EJECT                                                                
006933*01  -COPY W0003                                                          
006934     EJECT                                                                
006935 01  DLI-IO-AREA.                                                         
006936   03 IO-AREA                    PIC X(900)  VALUE SPACE.                 
006937     SKIP3                                                                
006940*  03  WLARTC01 -COPY WDK601              -RED IO-AREA.                   
006941     EJECT                                                                
006942*  03  WLARTC11 -COPY WDK611              -RED IO-AREA.                   
006943     EJECT                                                                
006944 01  DLI-IO-AREA-2.                                                       
006945   03 IO-AREA-2                  PIC X(600)  VALUE SPACE.                 
006946     SKIP3                                                                
006947*  03  WLARTG01 -COPY WDD201 -PRE ARTG- -RED IO-AREA-2.                   
006948     EJECT                                                                
006949 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP311'.                      
006950 01  DLI-IO-WDP311.                                                       
006951*    03  -COPY WDP311                                                     
006952     EJECT                                                                
006953 LINKAGE SECTION.                                                         
006954     SKIP3                                                                
006960*01      -COPY W0008     -PRE ARTC-                                       
006961      05 FILLER          PIC X.                                           
006962     EJECT                                                                
006963*01      -COPY W0008     -PRE ARTG-                                       
006964      05 FILLER          PIC X.                                           
006965     EJECT                                                                
006966*01      -COPY W0008     -PRE WDP3-                                       
006967     05  FILLER                  PIC X.                                   
006968     EJECT                                                                
006990 PROCEDURE DIVISION USING ARTC-PCB ARTG-PCB WDP3-PCB.                     
006992     ENTRY 'DLITCBL' USING ARTC-PCB ARTG-PCB WDP3-PCB.                    
006994                                                                          
007000 MAIN SECTION.                                                            
007100                                                                          
007200     PERFORM A-INIT                                                       
007300                                                                          
007400     PERFORM S01-LAES-INFIL                                               
008100     PERFORM UNTIL END-OF-INFIL                                           
008110                                                                          
008120        MOVE IN-ARTNR TO WS-IDARTNR                                       
008130        MOVE WS-IDARTNR TO W-IDARTNR                                      
008131        PERFORM IMS-GU-ARTG01                                             
008133        IF SEGMENT-FINNS                                                  
008134           MOVE ARTG-ART-TEANSINK TO SPAR-ARTG-TEANSINK                   
008135        ELSE                                                              
008136           MOVE SPACE TO SPAR-ARTG-TEANSINK                               
008137        END-IF                                                            
008138        PERFORM IMS-GU-ARTC01                                             
008139        IF SEGMENT-FINNS                                                  
008140           MOVE ZERO TO SPAR-ARTC-IDANSK                                  
008141                        SPAR-KDERS                                        
008142           PERFORM IMS-GNP-ARTC11                                         
008143           IF SEGMENT-FINNS                                               
008144              MOVE CLAG-IDANSK TO SPAR-ARTC-IDANSK                        
008145              MOVE CLAG-KDERS  TO SPAR-KDERS                              
008149           END-IF                                                         
008150***        SORTERA BORT NAP-POSTER (=004) IGEN,(ENDAST MAIL)              
008151           IF IN-BESTPREF = '004'                                         
008152              PERFORM C-SKAPA-SKRIV-UTPOST2                               
008153           ELSE                                                           
008154              PERFORM B-SKAPA-SKRIV-UTPOST                                
008155           END-IF                                                         
008156        END-IF                                                            
008160                                                                          
008162        PERFORM S01-LAES-INFIL                                            
008170     END-PERFORM                                                          
008300                                                                          
008400     PERFORM Z-FINIT                                                      
008500     MOVE ZERO TO RETURN-CODE                                             
008600     GOBACK                                                               
008700     .                                                                    
008800     EJECT                                                                
008900 A-INIT SECTION.                                                          
009000                                                                          
009100     OPEN INPUT  INFIL                                                    
009200     OPEN OUTPUT W11452                                                   
009300                 W11453                                                   
009400                 W11457                                                   
009410                                                                          
009500     MOVE SPACE      TO UT2-AREA                                          
009800     .                                                                    
009900     EJECT                                                                
040200 B-SKAPA-SKRIV-UTPOST SECTION.                                            
040300                                                                          
040400     MOVE '301'       TO UT-IDRT                                          
040500     MOVE SPACE       TO UT-COMMON-AREA                                   
040600                         UT-NP-AREA                                       
040610                         UT-CC-AREA                                       
040640                                                                          
040800     MOVE W-IDARTNR TO ARTNR-NUM                                          
040900     INSPECT ARTNR-ALFA REPLACING LEADING ZEROES BY SPACE                 
041000     MOVE ARTIKELNR   TO UT-IDPITEM                                       
041100     MOVE 'CC'        TO UT-CDTYPE-REQ                                    
041200     MOVE 'V'         TO UT-CD-IDPITEM                                    
041300     MOVE 'VCAS'      TO UT-IDPORG                                        
041310     MOVE 'Y'         TO UT-FLCOBL-ALLOWED                                
041320     MOVE SPAR-ARTG-TEANSINK TO UT-TXNOTES-REQ1                           
041401                                                                          
041410     IF IN-BESTPREF NUMERIC                                               
041420        MOVE IN-BESTPREF TO UT-IDHANDLR                                   
041430     ELSE                                                                 
041440        MOVE ZERO TO UT-IDHANDLR                                          
041450     END-IF                                                               
041460                                                                          
041600     IF SPAR-ARTC-IDANSK > 0                                              
041610        MOVE 'ANSK'           TO W-KDARBTYP                               
041700        MOVE SPAR-ARTC-IDANSK TO W-IDANSK                                 
041800        MOVE W-IDANSK         TO W-IDPERSON                               
041900        PERFORM IMS-GET-WDP3-IDANSK                                       
042000        IF SEGMENT-SAKNAS                                                 
042100           DISPLAY ' IDANSK SAKNAS PÅ P311: ' W-IDANSK                    
042200        ELSE                                                              
042300           MOVE PERS-IDNAMN TO UT-NMHANDLR-ISSUER                         
042400           MOVE PERS-IDTFN  TO UT-IDPHONE-ISSUER                          
042500           MOVE PERS-IDAVD  TO UT-IDSECTN-ISSUER                          
042600           MOVE PERS-IDMAIL TO WS-IDUSER                                  
042700           PERFORM BA-KOLLA-IDUSER                                        
042800        END-IF                                                            
042900     END-IF                                                               
043000                                                                          
043230     MOVE 'BP2TW'        TO UT-CC-IDUSER                                  
043400     MOVE 'N'            TO UT-CC-FLMTRL                                  
043401     MOVE 'Y'            TO UT-CC-FLTOTAL-CANCEL                          
043410     MOVE 'BL'           TO UT-CC-CDMODE-ORDER                            
043411                                                                          
043413     MOVE 'AAMMDD'        TO DAT-KDDATFORM                                
043414     MOVE  IN-DATUM-UTSKR TO DAT-I-TIDATUM                                
043415     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
043416                         DAT-O-TIDATUM DAT-KDSVAR                         
043417     IF DAT-KDSVAR-OK                                                     
043419       MOVE DAT-TIAAVV-GRP  TO  AAVV                                      
043421     END-IF                                                               
043422                                                                          
043430     MOVE AAVV  TO UT-CC-TIEXIT-WEEK                                      
043500     MOVE ZERO  TO UT-CC-IDSUPPL                                          
043600                   UT-CC-QTPITEM-CANCEL                                   
043700                   UT-CC-QTPITEM-LAST-DEL                                 
043800                   UT-CC-QTPITEM-REMAIN                                   
043900                   UT-CC-QTPITEM-STOCK                                    
044000                   UT-CC-IDSUFFIX-ORDER                                   
045100     PERFORM S02-SKRIV-UTPOST                                             
045300     .                                                                    
045400     EJECT                                                                
045500 BA-KOLLA-IDUSER SECTION.                                                 
045600                                                                          
045625     MOVE NEJ TO SW-TRAFF                                                 
045630                                                                          
045700     MOVE WS-IDMAIL TO KOLL-IDUSER                                        
045800     MOVE 1 TO IX                                                         
045900     MOVE 9 TO IX-MAX                                                     
046000     PERFORM UNTIL IX > IX-MAX                                            
046100        IF KOLL-TKN(IX) = '@'                                             
046200           MOVE IX TO KOLL-IX                                             
046300           MOVE 10 TO IX                                                  
046310           MOVE JA TO SW-TRAFF                                            
046400        ELSE                                                              
046500           ADD 1 TO IX                                                    
046600        END-IF                                                            
046700     END-PERFORM                                                          
046800                                                                          
046810     IF SW-TRAFF = NEJ                                                    
046811        MOVE KOLL-IDUSER(1:8) TO UT-IDUSERID-ISSUER                       
046830     ELSE                                                                 
046900       ADD -1 TO KOLL-IX                                                  
047000                                                                          
047100       IF KOLL-IX = 1                                                     
047200        MOVE KOLL-IDUSER(1:1) TO UT-IDUSERID-ISSUER                       
047300       ELSE                                                               
047400        IF KOLL-IX = 2                                                    
047500         MOVE KOLL-IDUSER(1:2) TO UT-IDUSERID-ISSUER                      
047600        ELSE                                                              
047700         IF KOLL-IX = 3                                                   
047800          MOVE KOLL-IDUSER(1:3) TO UT-IDUSERID-ISSUER                     
047900         ELSE                                                             
048000          IF KOLL-IX = 4                                                  
048100           MOVE KOLL-IDUSER(1:4) TO UT-IDUSERID-ISSUER                    
048200          ELSE                                                            
048300           IF KOLL-IX = 5                                                 
048400            MOVE KOLL-IDUSER(1:5) TO UT-IDUSERID-ISSUER                   
048500           ELSE                                                           
048600            IF KOLL-IX = 6                                                
048700             MOVE KOLL-IDUSER(1:6) TO UT-IDUSERID-ISSUER                  
048800            ELSE                                                          
048900             IF KOLL-IX = 7                                               
049000              MOVE KOLL-IDUSER(1:7) TO UT-IDUSERID-ISSUER                 
049100             ELSE                                                         
049200              IF KOLL-IX = 8                                              
049300               MOVE KOLL-IDUSER(1:8) TO UT-IDUSERID-ISSUER                
049400              END-IF                                                      
049500             END-IF                                                       
049600            END-IF                                                        
049700           END-IF                                                         
049800          END-IF                                                          
049900         END-IF                                                           
050000        END-IF                                                            
050100       END-IF                                                             
050110     END-IF                                                               
050120     .                                                                    
050130     EJECT                                                                
050140 C-SKAPA-SKRIV-UTPOST2 SECTION.                                           
050150                                                                          
050160     IF SPAR-KDERS > 20                                                   
050161        IF UT2-AREA = SPACE                                               
050162           PERFORM S04-SKRIV-UTPOST3                                      
050163        END-IF                                                            
050170        MOVE SPACE      TO UT2-AREA                                       
050171        MOVE IN-ARTNR   TO UT2-IDARTNR                                    
050180        MOVE SPAR-KDERS TO UT2-KDERS                                      
050190                                                                          
050191        PERFORM S03-SKRIV-UTPOST2                                         
050192     END-IF                                                               
050200     .                                                                    
050300     EJECT                                                                
050400 Z-FINIT   SECTION.                                                       
050500                                                                          
050600     CLOSE W11452                                                         
050610           W11453                                                         
050620           W11457                                                         
050700                                                                          
050800     MOVE 'S' TO POSTSUM-OPKOD                                            
050900     CALL POSTSUM USING POSTSUM-PARM                                      
051000     .                                                                    
051100     EJECT                                                                
051200 S01-LAES-INFIL SECTION.                                                  
051300                                                                          
051400     READ INFIL INTO IN-AREA                                              
051500     AT END                                                               
051600        SET END-OF-INFIL TO TRUE                                          
051700                                                                          
051800     NOT AT END                                                           
051900        MOVE 'IN'         TO POSTSUM-FDNAMN                               
052000        MOVE 'W11452D1'   TO POSTSUM-DDNAMN2                              
052100        MOVE SPACE        TO POSTSUM-TRANSTYP                             
052200        CALL POSTSUM USING POSTSUM-PARM                                   
052300     END-READ                                                             
052400     .                                                                    
052500     EJECT                                                                
052600 S02-SKRIV-UTPOST SECTION.                                                
052700                                                                          
053000     WRITE UT-POST FROM UT-AREA                                           
053100     .                                                                    
053110     EJECT                                                                
053120 S03-SKRIV-UTPOST2 SECTION.                                               
053130                                                                          
053140     WRITE UT2-POST FROM UT2-AREA                                         
053150     .                                                                    
053160     EJECT                                                                
053170 S04-SKRIV-UTPOST3 SECTION.                                               
053180                                                                          
053191     MOVE ')SEND'                TO UT3-AREA                              
053192     WRITE UT3-POST FROM UT3-AREA                                         
053194     MOVE 'TITLE ANNULLATIONANM. NAP ' TO UT3-AREA                        
053195     WRITE UT3-POST FROM UT3-AREA                                         
053197     MOVE 'OPTION FORCE'         TO UT3-AREA                              
053198     WRITE UT3-POST FROM UT3-AREA                                         
053199     MOVE 'WSYST@VOLVOCARS.COM ' TO MAIL-ADRESS                           
053200     MOVE 'INK '                 TO W-KDARBTYP                            
053202     MOVE 499                    TO W-IDPERSON                            
053203     PERFORM IMS-GET-WDP3-IDANSK                                          
053204     IF SEGMENT-SAKNAS                                                    
053205        DISPLAY ' IDINK  SAKNAS PÅ P311: ' '499'                          
053206     ELSE                                                                 
053210        MOVE PERS-IDMAIL         TO MAIL-ADRESS                           
053212     END-IF                                                               
053215     MOVE MAIL-RAD               TO UT3-AREA                              
053216     WRITE UT3-POST FROM UT3-AREA                                         
053217     MOVE 'MEMO SEND'            TO UT3-AREA                              
053218     WRITE UT3-POST FROM UT3-AREA                                         
053219     MOVE ')END'                 TO UT3-AREA                              
053220     WRITE UT3-POST FROM UT3-AREA                                         
053221     .                                                                    
053230     EJECT                                                                
053300*********** IMS SEKTIONER                                                 
055700 IMS-GU-ARTC01 SECTION.                                                   
055800     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
055900            DELIMITED BY SIZE INTO SSA1                                   
056000     MOVE '  ' TO GODK-STATUSKODER                                        
056100     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
056200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
056600     PERFORM IMS-STATUSKONTROLL                                           
056700     .                                                                    
056800     SKIP3                                                                
056900 IMS-GNP-ARTC11 SECTION.                                                  
057000     MOVE  'WLARTC11 ' TO SSA1                                            
057100     MOVE '  GE'   TO GODK-STATUSKODER                                    
057200     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1                     
057300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
057400     PERFORM IMS-STATUSKONTROLL                                           
057500     .                                                                    
057600     SKIP2                                                                
057700 IMS-GU-ARTG01 SECTION.                                                   
057800     STRING 'WLARTG01(IDARTNR  =' W-IDARTNR-X ')'                         
057900            DELIMITED BY SIZE INTO SSA1                                   
058000     MOVE '  GE' TO GODK-STATUSKODER                                      
058100     CALL CBLTDLI USING GU ARTG-PCB DLI-IO-AREA-2 SSA1                    
058200     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
058300     PERFORM IMS-STATUSKONTROLL                                           
058400     .                                                                    
058500     EJECT                                                                
058600 IMS-GET-WDP3-IDANSK   SECTION.                                           
058700     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
058800          DELIMITED BY SIZE INTO SSA1                                     
058900     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
059000          DELIMITED BY SIZE INTO SSA2                                     
059100     MOVE '  GE' TO GODK-STATUSKODER                                      
059200     CALL CBLTDLI USING GU  WDP3-PCB DLI-IO-WDP311 SSA1 SSA2              
059300     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
059400     PERFORM IMS-STATUSKONTROLL                                           
059500     .                                                                    
059600     SKIP3                                                                
059700 IMS-STATUSKONTROLL SECTION.                                              
059800     SET STATUS-IX TO 1                                                   
059900     SEARCH GODK-STATUS                                                   
060000       AT END CALL FELLOG                                                 
060100       WHEN GODK-STATUS(STATUS-IX) = STATUS-WS CONTINUE                   
060200     END-SEARCH                                                           
060300     .                                                                    
