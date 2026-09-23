000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1053100.                                                
000300 AUTHOR.         SUSANNE ENEGARD.                                         
000400 DATE-WRITTEN.   NOVEMBER 1984.                                           
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION.                                                            
000800*        PROGRAMMETS FRÅGEDELEN HÄMTAR INFORMATION OM KATALOG-            
000900*                    IDENTITET.                                           
001000*        PROGRAMMETS UPPDATERINGSDEL NYREGISTRERAR OCH ÄNDRAR             
001100*                    KATALOGIDENTITET.                                    
001200*                                                                         
001300*        PROGRAMMET LÄSER      WDP7 (USER)                                
001400*        PROGRAMMET UPPDATERAR WDN1                                       
001500*        PROGRAMMET LÄSER      WDN5                                       
001600*        PROGRAMMET LÄSER      WDN7                                       
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSAKTION: W1T531                                              
002000*        MID:         W1I53101                                            
002100*                                                                         
002200*    UTDATA.                                                              
002300*        MOD:         W1O53101                                            
002310*                                                                         
002320*    ÄNDRINGAR:                                                           
002330*        2005-03-23  I systemlösning NEVIS (Semcon) skall red.            
002340*        fortfarande uppdatera nya katalogid i puls på denna              
002350*        bild.                                                            
002351*        TIOMBRYT-F som i VCS sattes vid en ombrytning, skall nu          
002352*        sättas här online manuellt. Detta kräver att man har kon-        
002353*        troll på att samma datum inte tas ut mer än en gång.             
002354*        Ett nytt sekundärindex på WDN1 med ingång på TIOMBRYT-F          
002355*        & KDFORDON skapas för att lättare lösa denna uppgift.            
002356*                                                                         
002360*                                                                         
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP3                                                                
002700 DATA DIVISION.                                                           
002800     EJECT                                                                
002900 WORKING-STORAGE SECTION.                                                 
003000                                                                          
003100*    -- CHECKED BY WY2000                                                 
003200 77  IDPGM                       PIC X(8)    VALUE 'W1053100'.            
003300 77  JA                          PIC X       VALUE 'J'.                   
003400 77  NEJ                         PIC X       VALUE 'N'.                   
003500 77  INDX                        PIC S9(9)   VALUE +1   COMP SYNC.        
003600 77  DOIX                        PIC S9(9)   VALUE +1   COMP SYNC.        
003700 77  SUBIX                       PIC S9(9)   VALUE +1   COMP SYNC.        
003800 77  MAXSUBIX                    PIC S9(9)   VALUE +1   COMP SYNC.        
003900 77  RELIX                       PIC S9(9)   VALUE +1   COMP SYNC.        
004000 77  SPRAAK-IX                   PIC S9(9)   VALUE +1   COMP SYNC.        
004100 77  INDATA-FEL                  PIC X(1)    VALUE 'N'.                   
004200 77  KDFORDON-OK                 PIC X(1)    VALUE 'N'.                   
004300 77  WS-KDFORDON                 PIC X(2)    VALUE 'N'.                   
004400 77  C-RAK-1                     PIC S9(4)   VALUE ZERO COMP-3.           
004500 77  C-RAK-2                     PIC S9(4)   VALUE ZERO COMP-3.           
004600 77  BECAT-RAKN                  PIC S9(4)   VALUE ZERO COMP-3.           
004610 77  SW-KAT-FINNS                PIC X       VALUE 'N'.                   
004620     88  KAT-FINNS                           VALUE 'J'.                   
004630     88  KATALOG-EJ-UPPLAGD                  VALUE 'N'.                   
004700                                                                          
004800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
004900     88  EGEN-MID                            VALUE '1531'.                
005000     88  GODK-MID                            VALUE                        
005100         '1511' '1512' '1513' '1514' '1515' '1517' '1518' '1519'          
005200         '1521' '1525' '1531' '1532' '1533' '1551'.                       
005300     88  HELP-MID                            VALUE '0551'.                
005400     EJECT                                                                
005500 01  DYNAMISKA-SUBPROGRAM.                                                
005600   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
005700   03  WDATKONV                  PIC X(8)    VALUE 'WDATKONV'.            
005710   03  WMEDKONV                  PIC X(8)    VALUE 'WMEDKONV'.            
005800   03  W005INIT                  PIC X(8)    VALUE 'W005INIT'.            
005900   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
006000                                                                          
006100 01  FILLER                      PIC X(16)   VALUE 'ABEND-TEXT'.          
006200 01  ABEND-TEXT                  PIC X(73)   VALUE SPACE.                 
006300                                                                          
006400 01  INDATA-VARIABLER.                                                    
006500     03  IDCATNR-WS              PIC X(5)    VALUE SPACE.                 
006600     03  FILLER       REDEFINES IDCATNR-WS.                               
006700         05 KEY-IDCATNR          PIC 9(5).                                
006800                                                                          
006900     03  GEMENA                  PIC X(3)    VALUE 'åäö'.                 
007000     03  VERSALER                PIC X(3)    VALUE 'ÅÄÖ'.                 
007100                                                                          
007200     03  DAGENS-DATUM            PIC 9(6).                                
007300     03  W-TIOMBRYT-6            PIC 9(6)  VALUE ZERO.                    
007400     03  W-TIOMBRYT-7            PIC 9(7)  VALUE ZERO.                    
007500                                                                          
007600 01  LITEN-BOKSTAV               PIC X(29)                                
007700           VALUE 'abcdefghijklmnopqrstuvwxyzåäö'.                         
007800                                                                          
007900 01  STOR-BOKSTAV                PIC X(29)                                
008000           VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZÅÄÖ'.                         
008100     EJECT                                                                
008200* - - - - - - - - - - - - - - - - - - -  NYCKLAR TILL DLI                 
008300 01  FILLER                      PIC X(16)  VALUE 'NYCKLAR-T-DLI'.        
008400 01  NYCKLAR-TILL-DLI.                                                    
008500   03  W-IDCATNR-X.                                                       
008600     05  W-IDCATNR               PIC 9(5)   VALUE ZERO .                  
008700   03  W-IDCATGRP-X.                                                      
008800     05  W-IDCATGRP              PIC 9(2)   VALUE ZERO.                   
008900   03  W-IDCATAVS-X.                                                      
009000     05  W-IDCATAVS              PIC 9(4)   VALUE 1.                      
009100   03  W-IDILLU-X.                                                        
009200     05  W-IDILLU                PIC S9(5)   VALUE ZERO  COMP-3.          
009201                                                                          
009210   03  W-WDN1A1KY-MIN-X.                                                  
009220     05  W-TIOMBRYT-F-MIN        PIC S9(7)   VALUE ZERO  COMP-3.          
009230     05  W-KDFORDON-MIN          PIC X(2)    VALUE LOW-VALUE.             
009240   03  W-WDN1A1KY-MAX-X.                                                  
009250     05  W-TIOMBRYT-F-MAX        PIC S9(7)   VALUE ZERO  COMP-3.          
009260     05  W-KDFORDON-MAX          PIC X(2)    VALUE HIGH-VALUE.            
009300     EJECT                                                                
009310*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
009320*01 -COPY WMEDAREA                                                        
009330     EJECT                                                                
009340 01  MESSAGE-CODES.                                                       
009380     03  INF-UPD-DONE            PIC X(3)    VALUE '101'.                 
009390     03  ERR-CORR-HIGHLIT-FIELD  PIC X(3)    VALUE '001'.                 
009391     03  ERR-NOT-NUMERIC         PIC X(3)    VALUE '020'.                 
009392     03  ERR-DO-NOT-SPACE        PIC X(3)    VALUE '336'.                 
009393     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
009394     EJECT                                                                
009400* - - - - - - - - - - - - - - - - - - -  MEDDELANDEN                      
009500 01  FILLER                      PIC X(16)   VALUE 'MEDDELANDEN'.         
009600 01  MEDDELANDEN.                                                         
011800     03 FILLER-4.                                                         
011900          05 FILLER              PIC X(40)                                
012000              VALUE '    *                                   '.           
012100          05 FILLER              PIC X(40)                                
012200              VALUE '    *                                   '.           
012300     03 FILLER REDEFINES FILLER-4.                                        
012400          05 FEL-4   OCCURS 2    PIC X(40).                               
013900     03 FILLER-12.                                                        
014000          05 FILLER              PIC X(40)                                
014100              VALUE '    KATALOGNR SAKNAS                    '.           
014200          05 FILLER              PIC X(40)                                
014300              VALUE '    CATALOGUE ID. NOT FOUND             '.           
014400     03 FILLER REDEFINES FILLER-12.                                       
014500          05 MED-2   OCCURS 2    PIC X(40).                               
014600     03 FILLER-13.                                                        
014700          05 FILLER              PIC X(40)                                
014800              VALUE '    MAX 40 TECKEN, ANDRA RADEN INRÄKNAD '.           
014900          05 FILLER              PIC X(40)                                
015000              VALUE '    MAX 40 CHARS, SECOND LINE INCLUDED  '.           
015100     03 FILLER REDEFINES FILLER-13.                                       
015200          05 MED-3   OCCURS 2    PIC X(40).                               
015300     03 FILLER-14.                                                        
015400          05 FILLER              PIC X(40)                                
015500              VALUE '    MAX 20 TKN PÅ RAD 1 NÄR RAD 2 FINNS '.           
015600          05 FILLER              PIC X(40)                                
015700              VALUE '    MAX 20 CHR ON ROW 1 WHEN ROW 2 EXIST'.           
015800     03 FILLER REDEFINES FILLER-14.                                       
015900          05 MED-4   OCCURS 2    PIC X(40).                               
015901                                                                          
015980                                                                          
015981     03 FILLER-15.                                                        
015982          05 FILLER              PIC X(40)                                
015983              VALUE ' Fordonsslag måste fyllas i          '.              
015984          05 FILLER              PIC X(40)                                
015985              VALUE ' Vehicle "belongs to" is missing     '.              
015986     03 FILLER REDEFINES FILLER-15.                                       
015987          05 MED-5   OCCURS 2    PIC X(40).                               
015988                                                                          
015989     03 FILLER-16.                                                        
015990          05 FILLER              PIC X(28)                                
015991              VALUE 'Detta datum finns redan för '.                       
015992          05 MED-6-KAT-1         PIC X(12)  VALUE SPACE.                  
015994          05 FILLER              PIC X(28)                                
015995              VALUE 'This date already alloc.for '.                       
015996          05 MED-6-KAT-2         PIC X(12)  VALUE SPACE.                  
015997     03 FILLER REDEFINES FILLER-16.                                       
015998          05 MED-6   OCCURS 2    PIC X(40).                               
015999                                                                          
016000                                                                          
016001     03 FILLER-17.                                                        
016002          05 FILLER              PIC X(40)                                
016003              VALUE ' Måste vara ett riktigt datum'.                      
016005          05 FILLER              PIC X(40)                                
016006              VALUE ' Must be a valid date        '.                      
016008     03 FILLER REDEFINES FILLER-17.                                       
016009          05 MED-7   OCCURS 2    PIC X(40).                               
016010                                                                          
016011     03 FILLER-18.                                                        
016012          05 FILLER              PIC X(40)                                
016013              VALUE ' Får EJ ändras. Registrerat i MASTER !  '.           
016014          05 FILLER              PIC X(40)                                
016015              VALUE ' May NOT be altered.  Reg.in MASTER !   '.           
016016     03 FILLER REDEFINES FILLER-18.                                       
016017          05 MED-8   OCCURS 2    PIC X(40).                               
016018                                                                          
016019     03 FILLER-19.                                                        
016020          05 FILLER              PIC X(40)                                
016021              VALUE ' Endast fordonskoder PV RE NL godkända  '.           
016022          05 FILLER              PIC X(40)                                
016023              VALUE ' Only Veh.codes PV RE NL are permitted  '.           
016024     03 FILLER REDEFINES FILLER-19.                                       
016025          05 MED-9   OCCURS 2    PIC X(40).                               
016026                                                                          
016027     03 FILLER-20.                                                        
016028          05 FILLER              PIC X(40)                                
016029              VALUE ' Emblem måste fyllas i          '.                   
016030          05 FILLER              PIC X(40)                                
016031              VALUE ' Emblem is missing     '.                            
016032     03 FILLER REDEFINES FILLER-20.                                       
016033          05 MED-10  OCCURS 2    PIC X(40).                               
016034                                                                          
016035     03 FILLER-21.                                                        
016036          05 FILLER              PIC X(40)                                
016037              VALUE ' Katalog Master måste fyllas i '.                    
016038          05 FILLER              PIC X(40)                                
016039              VALUE ' Cat.Mastername is missing     '.                    
016040     03 FILLER REDEFINES FILLER-21.                                       
016041          05 MED-11  OCCURS 2    PIC X(40).                               
016042                                                                          
016044     03 FILLER-22.                                                        
016045          05 FILLER              PIC X(40)                                
016046              VALUE ' Maila nya modell-ID till System-avd.'.              
016047          05 FILLER              PIC X(40)                                
016048              VALUE ' Email Sys Dep. about new Model ID´s  '.             
016049     03 FILLER REDEFINES FILLER-22.                                       
016050          05 MED-12  OCCURS 2    PIC X(40).                               
016051                                                                          
016052                                                                          
016060     EJECT                                                                
016100*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
016200 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
016300     SKIP3                                                                
016400*01 -COPY WMSGINIT                                                        
016500     EJECT                                                                
016600* - - - - - - - - - - - - - - - - - - - - FORDONSKONTROLL                 
016700 01  FILLER                      PIC X(16) VALUE 'FORDONS-KONTR'.         
016800*01  -COPY WWFORDON.                                                      
016900     EJECT                                                                
017000* - - - - - - - - - - - - - - - - - - - - DATUMKONTROLL                   
017100 01  FILLER                      PIC X(16) VALUE 'DATUM-KONTR'.           
017200*01  -COPY WDATAREA.                                                      
017300     EJECT                                                                
017400* - - - - - - - - - - - - - - - - - - - - MID-AREA                        
017500 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
017600*01  -COPY W1I53101.                                                      
017700     EJECT                                                                
017800* - - - - - - - - - - - - - - - - - - - - MSG-AREA                        
017900 01  FILLER                      PIC X(16)   VALUE 'MSG-AREA'.            
018000*01  -COPY WMSGAREA                                                       
018100     EJECT                                                                
018200*    03   -COPY W1O53101 -RED MSG-AREA.                                   
018300     EJECT                                                                
018400* - - - - - - - - - - - - - - - - - - -  MFS-AREA                         
018500 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
018600*01  -COPY WMFSAREA                                                       
018700     EJECT                                                                
018800* - - - - - - - - - - - - - - - - - - -  IMS-WS                           
018900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
019000 01  IMS-WS.                                                              
019100*                        **** STATUS-KOD FRÅN IMS                         
019200   03  STATUS-WS                 PIC XX.                                  
019300     88  SEGMENT-FINNS                       VALUE '  '.                  
019400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
019500     88  BASEN-SLUT                          VALUE 'GB'.                  
019600     SKIP2                                                                
019700   03  GODK-STATUSKODER.                                                  
019800     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
019900     SKIP2                                                                
020000 01  SSA1                        PIC X(128).                              
020100 01  SSA2                        PIC X(128).                              
020200     EJECT                                                                
020300*                            IMS FUNKTIONSKODER                           
020400*01    -COPY W0003                                                        
020500     EJECT                                                                
020600* - - - - - - - - - - - - - - - - - - -  DLI-IO-AREA                      
020700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
020800 01  DLI-IO-AREA.                                                         
020900     03 IO-WDN1A-AREA            PIC X(64)  VALUE SPACE.                  
021000     SKIP3                                                                
021100*    03  WDN1A    -COPY WDN1A1        -RED IO-WDN1A-AREA.                 
021200     EJECT                                                                
021210     03  IO-AREA-1               PIC X(500)  VALUE SPACE.                 
021220     SKIP3                                                                
021230*    03  WLKATM01 -COPY WDN101        -RED IO-AREA-1.                     
021240     EJECT                                                                
021300     03  IO-AREA-2               PIC X(500)  VALUE SPACE.                 
021400     SKIP3                                                                
021500*    03  WLKATH01 -COPY WDN501        -RED IO-AREA-2                      
021600     EJECT                                                                
021700*    03  WLKATH11 -COPY WDN511        -RED IO-AREA-2  -PRE AVS-.          
021800     EJECT                                                                
021900*    03  WLKATH12 -COPY WDN512        -RED IO-AREA-2  -PRE AVS-.          
022000     EJECT                                                                
022100*    03  WLKATK01 -COPY WDN701        -RED IO-AREA-2.                     
022200     EJECT                                                                
022300 LINKAGE SECTION.                                                         
022400     SKIP2                                                                
022500*01  -COPY W0009          -PRE MSG-                                       
022600     EJECT                                                                
022700*01  -COPY W0008          -PRE USEA-                                      
022800     05  FILLER                  PIC X.                                   
022900                                                                          
023000*01  -COPY W0008          -PRE KAT-                                       
023100     05  FILLER                  PIC X.                                   
023200     EJECT                                                                
023300*01  -COPY W0008          -PRE AVS-                                       
023400     05  FILLER                  PIC X.                                   
023500     EJECT                                                                
023600*01  -COPY W0008          -PRE ILLU-                                      
023700     05  FILLER                  PIC X.                                   
023800     EJECT                                                                
023810*01  -COPY W0008          -PRE WDN1A-                                     
023820     05  FILLER                  PIC X.                                   
023830     EJECT                                                                
023900 PROCEDURE DIVISION USING MSG-PCB USEA-PCB KAT-PCB AVS-PCB                
024000                          ILLU-PCB WDN1A-PCB.                             
024100 STYR SECTION.                                                            
024200     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB KAT-PCB AVS-PCB               
024300                           ILLU-PCB WDN1A-PCB.                            
024400     PERFORM IMS-GET-MSG                                                  
024500                                                                          
024600     IF SEGMENT-FINNS                                                     
024700       PERFORM A-INIT                                                     
024800       IF IDCATNR-WS NOT NUMERIC                                          
024810         MOVE ERR-WRONG-KEY     TO MED-IDMFSMED                           
024820         CALL WMEDKONV USING MED-WMEDAREA                                 
024830         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
024840                                                                          
025000         PERFORM F-RENSA-BILD                                             
025100       ELSE                                                               
025200         MOVE KEY-IDCATNR TO W-IDCATNR                                    
025300         IF MFS-UPDATE                                                    
025400           PERFORM B-KOLLA-INDATA                                         
025500           IF INDATA-FEL = JA                                             
025600             MOVE ERR-CORR-HIGHLIT-FIELD TO MED-IDMFSFEL                  
025610             CALL WMEDKONV USING MED-WMEDAREA                             
025620             MOVE MED-MFSFEL TO MOD-TEMFSFEL                              
025630                                                                          
025700             PERFORM E-VISA-BILD-IGEN                                     
025800           ELSE                                                           
025900             PERFORM C-UPPDATERA                                          
026000             IF INDATA-FEL = JA                                           
026100               PERFORM F-RENSA-BILD                                       
026200             ELSE                                                         
026300               PERFORM E-VISA-BILD-IGEN                                   
026310*              PERFORM D-LAS-KATALOGID                                    
026400               MOVE KAT-TIREGDAT TO MOD-TIREGDAT                          
026500             END-IF                                                       
026600           END-IF                                                         
026700         ELSE                                                             
026800           PERFORM D-LAS-KATALOGID                                        
026900         END-IF                                                           
027000       END-IF                                                             
027100       MOVE LENGTH OF MOD-W1O53101 TO MSG-KVLL                            
027200       ADD  +4                     TO MSG-KVLL                            
027300       PERFORM IMS-INSERT-MSG                                             
027400     END-IF                                                               
027500     MOVE ZERO TO RETURN-CODE                                             
027600     GOBACK                                                               
027700     .                                                                    
027800     EJECT                                                                
027900 A-INIT SECTION.                                                          
028000     SKIP2                                                                
028100     IF MSG-DUBBLA-TRANSKODER                                             
028200       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I53101                 
028300       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
028400       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
028500                                                                          
028600       IF MFS-IDTRANS = '1531'                                            
028700         MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                         
028800       ELSE                                                               
028900         MOVE SPACE TO MFS-KDTRTYP                                        
029000       END-IF                                                             
029100     ELSE                                                                 
029200       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W1I53101                  
029300       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
029400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
029500     END-IF                                                               
029600     INSPECT MID-W1I53101 REPLACING ALL '>' BY SPACE                      
029700     INSPECT MID-W1I53101 REPLACING ALL '<' BY SPACE                      
029800                                                                          
029900     MOVE LOW-VALUE   TO MSG-AREA                                         
030000     MOVE '1531'      TO MOD-IDTRANS                                      
030100     MOVE 'W1O531N1'  TO MFS-IDMOD                                        
030110     MOVE MFS-IDTRANS TO W-IDTRANS                                        
030200                                                                          
030210     MOVE ALL '+'           TO MSGI-WMSGINIT                              
030220     MOVE '001'             TO MSGI-KDCALL                                
030230     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
030240     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
030250     MOVE '1531'            TO MSGI-IDTRANS                               
030260     IF GODK-MID                                                          
030280         MOVE MID-IDCATNR-IN     TO MSGI-IDCATNR                          
030290     END-IF                                                               
030291     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
030296                                                                          
030297     IF MSGI-IDLAND-SPR = 'SE'                                            
030298       MOVE 'S  ' TO MED-IDSKYLT                                          
030299       MOVE +1 TO SPRAAK-IX                                               
030300     ELSE                                                                 
030301       MOVE 'GB ' TO MED-IDSKYLT                                          
030302       MOVE +2 TO SPRAAK-IX                                               
030303     END-IF                                                               
030901                                                                          
030910*    -- KONTROLL AV IDCATNR                                               
030920     MOVE MFS-RENSA-FAELT TO MOD-IDCATNR-IN                               
030930                                                                          
030940     IF MID-IDCATNR-IN NOT = ALL '+'                                      
030950       MOVE '7'         TO MFS-IDPFK                                      
030960       MOVE SPACE       TO MFS-KDTRTYP                                    
030970     END-IF                                                               
030980     INSPECT MSGI-IDCATNR REPLACING LEADING SPACE BY ZERO                 
030991     MOVE MSGI-IDCATNR TO IDCATNR-WS                                      
031000                                                                          
031110     MOVE IDCATNR-WS     TO MOD-IDCATNR-UT                                
031200     INSPECT MOD-IDCATNR-UT REPLACING LEADING ZERO BY SPACE               
031300     MOVE MFS-RENSA-FAELT TO MOD-IDCATNR-IN                               
031400                             MOD-TEMFSFEL                                 
031500                             MOD-TEMFSINF                                 
031700     IF ENGLISH-TEXT                                                      
031800       MOVE +2 TO SPRAAK-IX                                               
031900     ELSE                                                                 
032000       MOVE +1 TO SPRAAK-IX                                               
032100     END-IF                                                               
032200                                                                          
032300     ACCEPT DAGENS-DATUM FROM DATE                                        
032400     .                                                                    
032500     EJECT                                                                
032600 B-KOLLA-INDATA SECTION.                                                  
032700     SKIP2                                                                
032800     PERFORM BB-TRANSFORM-BOKSTAV                                         
032900                                                                          
033000     MOVE NEJ TO INDATA-FEL                                               
033100     MOVE ZERO TO BECAT-RAKN                                              
033110     PERFORM IMS-GU-KAT                                                   
033120     IF SEGMENT-FINNS                                                     
033121       SET KAT-FINNS TO TRUE                                              
033130     END-IF                                                               
033200                                                                          
033300     IF MID-BECAT NOT = ALL '+'                                           
033400       IF MID-BECAT-RAD2 NOT = ALL '+'                                    
033500         IF MID-BECAT-RAD1 = ALL '+' OR = SPACE                           
033600           MOVE MFS-ALFA-FAELT-FEL TO MOD-BECAT-RAD1-ATTR                 
033700                                      MOD-BECAT-RAD2-ATTR                 
033800           MOVE JA TO INDATA-FEL                                          
033900         ELSE                                                             
034000           MOVE ZERO TO C-RAK-1 C-RAK-2                                   
034100           INSPECT MID-BECAT-RAD1 TALLYING C-RAK-1 FOR CHARACTERS         
034200                                   BEFORE INITIAL HIGH-VALUE              
034300           MOVE C-RAK-1 TO BECAT-RAKN                                     
034400           INSPECT MID-BECAT-RAD2 TALLYING C-RAK-2 FOR CHARACTERS         
034500                                   BEFORE INITIAL HIGH-VALUE              
034600           ADD C-RAK-2  TO BECAT-RAKN                                     
034700           IF BECAT-RAKN > 40                                             
034800             MOVE MFS-ALFA-FAELT-FEL TO MOD-BECAT-RAD1-ATTR               
034900                                        MOD-BECAT-RAD2-ATTR               
035000             MOVE JA TO INDATA-FEL                                        
035100             MOVE MED-4(SPRAAK-IX) TO MOD-TEMFSINF                        
035200           ELSE                                                           
035300             IF C-RAK-1 > 20                                              
035400               MOVE MFS-ALFA-FAELT-FEL TO MOD-BECAT-RAD1-ATTR             
035500                                          MOD-BECAT-RAD2-ATTR             
035600               MOVE JA TO INDATA-FEL                                      
035700               MOVE MED-3(SPRAAK-IX) TO MOD-TEMFSINF                      
035800             ELSE                                                         
035900               MOVE MFS-ALFA-FAELT-RAETT TO MOD-BECAT-RAD1-ATTR           
036000                                            MOD-BECAT-RAD2-ATTR           
036100             END-IF                                                       
036200           END-IF                                                         
036300         END-IF                                                           
036400       ELSE                                                               
036500         MOVE MFS-ALFA-FAELT-RAETT TO MOD-BECAT-RAD1-ATTR                 
036600                                      MOD-BECAT-RAD2-ATTR                 
036700       END-IF                                                             
036710     ELSE                                                                 
036720*      MID-BECAT är all '+'  (oifylld)                                    
036730       IF KATALOG-EJ-UPPLAGD                                              
036750*        --- Man får inte UTELÄMNA katalogbeteckning vid nyuppl.          
036760         MOVE MFS-ALFA-FAELT-FEL TO MOD-BECAT-RAD1-ATTR                   
036761                                    MOD-BECAT-RAD2-ATTR                   
036770         MOVE JA TO INDATA-FEL                                            
036771       ELSE                                                               
036772         IF KAT-BECAT = SPACE                                             
036773*          --- Katalogbeteckning saknas i basen! Fyll i beteckning        
036774           MOVE MFS-ALFA-FAELT-FEL TO MOD-BECAT-RAD1-ATTR                 
036775                                      MOD-BECAT-RAD2-ATTR                 
036776           MOVE JA TO INDATA-FEL                                          
036781         END-IF                                                           
036790       END-IF                                                             
036800     END-IF                                                               
036810                                                                          
036900     IF MID-BEEMBLEM NOT = ALL '+' AND SPACE                              
036910       IF KAT-FINNS                                                       
036920         IF KAT-TIOMBRYT-SEN = ZERO                                       
036930*          -- OK, att ändra innan första masterfil mottagen               
037000           MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEEMBLEM-ATTR                 
037001         ELSE                                                             
037002           MOVE MFS-ALFA-FAELT-FEL TO MOD-BEEMBLEM-ATTR                   
037003           MOVE MED-8(SPRAAK-IX)   TO MOD-TEMFSINF                        
037004           MOVE JA TO INDATA-FEL                                          
037005*          --- Egentligen borde skapas en trans för att                   
037006*          --- skapa en W15465-fil-post. ( i C-UPPDATERA isåfall)         
037007*          --- En sådan 65-post behandlas i W1560300 i W156P3             
037008*          --- och omfattar även dataelement BEMASTER                     
037009         END-IF                                                           
037010       ELSE                                                               
037011         MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEEMBLEM-ATTR                   
037012       END-IF                                                             
037013     ELSE                                                                 
037014       IF MID-BEEMBLEM = ALL '+'                                          
037015       OR MID-BEEMBLEM = SPACE                                            
037020         IF KATALOG-EJ-UPPLAGD                                            
037030*          --- Man får inte UTELÄMNA katalogemblem vid nyuppl.            
037040           MOVE MFS-ALFA-FAELT-FEL TO MOD-BEEMBLEM-ATTR                   
037050           MOVE MED-10(SPRAAK-IX) TO MOD-TEMFSINF                         
037060           MOVE JA TO INDATA-FEL                                          
037070         ELSE                                                             
037080           IF KAT-BEEMBLEM = SPACE                                        
037090*            --- Katalogemblem måste finnas.  Saknas på basen!            
037091             MOVE MFS-ALFA-FAELT-FEL TO MOD-BEEMBLEM-ATTR                 
037092             MOVE MED-10(SPRAAK-IX) TO MOD-TEMFSINF                       
037093             MOVE JA TO INDATA-FEL                                        
037094           ELSE                                                           
037095             IF KAT-TIOMBRYT-SEN = ZERO                                   
037096*              -- OK, innan första masterfil mottagen                     
037097               MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEEMBLEM-ATTR             
037098             ELSE                                                         
037099               IF MID-BEEMBLEM = SPACE                                    
037100*                -- Försök att blanka ut befintligt reg. EMBLEM           
037101                 MOVE MFS-ALFA-FAELT-FEL TO MOD-BEEMBLEM-ATTR             
037102                 MOVE MED-8(SPRAAK-IX) TO MOD-TEMFSINF                    
037103                 MOVE JA TO INDATA-FEL                                    
037104               END-IF                                                     
037105             END-IF                                                       
037106           END-IF                                                         
037107         END-IF                                                           
037108       END-IF                                                             
037109     END-IF                                                               
037110     IF INDATA-FEL = NEJ                                                  
037200     IF MID-BEMASTER NOT = ALL '+' AND SPACE                              
037210       IF KAT-FINNS                                                       
037220         IF KAT-TIOMBRYT-SEN = ZERO                                       
037230*          -- OK, att ändra innan första masterfil mottagen               
037240           MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEMASTER-ATTR                 
037250         ELSE                                                             
037260           MOVE MFS-ALFA-FAELT-FEL TO MOD-BEMASTER-ATTR                   
037261           MOVE MED-8(SPRAAK-IX) TO MOD-TEMFSINF                          
037270           MOVE JA TO INDATA-FEL                                          
037271*          --- Egentligen borde skapas en trans för att                   
037272*          --- skapa en W15465-fil-post. ( i C-UPPDATERA isåfall)         
037273*          --- En sådan 65-post behandlas i W1560300 i W156P3             
037274*          --- och omfattar även dataelement BEEMBLEM                     
037294         END-IF                                                           
037295       ELSE                                                               
037296         MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEMASTER-ATTR                   
037297       END-IF                                                             
037298     ELSE                                                                 
037299       IF MID-BEMASTER = ALL '+'                                          
037300       OR MID-BEMASTER = SPACE                                            
037301         IF KATALOG-EJ-UPPLAGD                                            
037302*          --- Man får inte UTELÄMNA MASTERNAMN vid nyupplägg.            
037303           MOVE MFS-ALFA-FAELT-FEL TO MOD-BEMASTER-ATTR                   
037304           MOVE MED-11(SPRAAK-IX) TO MOD-TEMFSINF                         
037305           MOVE JA TO INDATA-FEL                                          
037306         ELSE                                                             
037307           IF KAT-BEMASTER = SPACE                                        
037308*            --- Masternamn måste finnas. Saknas på basen!                
037309             MOVE MFS-ALFA-FAELT-FEL TO MOD-BEMASTER-ATTR                 
037310             MOVE MED-11(SPRAAK-IX) TO MOD-TEMFSINF                       
037311             MOVE JA TO INDATA-FEL                                        
037312           ELSE                                                           
037313             IF KAT-TIOMBRYT-SEN = ZERO                                   
037314*              -- OK, innan första masterfil mottagen                     
037315               MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEMASTER-ATTR             
037316             ELSE                                                         
037317               IF MID-BEMASTER = SPACE                                    
037318*                -- Försök att blanka ut befintligt reg. BEMASTER         
037319                 MOVE MFS-ALFA-FAELT-FEL TO MOD-BEMASTER-ATTR             
037320                 MOVE MED-8(SPRAAK-IX) TO MOD-TEMFSINF                    
037321                 MOVE JA TO INDATA-FEL                                    
037322               END-IF                                                     
037323             END-IF                                                       
037324           END-IF                                                         
037325         END-IF                                                           
037326       END-IF                                                             
037400     END-IF                                                               
037401     END-IF                                                               
037410                                                                          
037500     IF MID-KDFORDON NOT = ALL '+'                                        
037600       IF MID-KDFORDON NUMERIC                                            
037700         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDFORDON-ATTR                     
037710         MOVE MED-9(SPRAAK-IX) TO MOD-TEMFSINF                            
037800         MOVE JA TO INDATA-FEL                                            
037900       ELSE                                                               
037910         IF KAT-FINNS                                                     
037912           IF KAT-TIOMBRYT-SEN = ZERO                                     
037913             CONTINUE                                                     
037914           ELSE                                                           
037915*            -- Man får inte ändra fordonslag för producerad kat.         
037916             MOVE MFS-ALFA-FAELT-FEL TO MOD-KDFORDON-ATTR                 
037917             MOVE MED-8(SPRAAK-IX) TO MOD-TEMFSINF                        
037918             MOVE JA TO INDATA-FEL                                        
037919           END-IF                                                         
037930         END-IF                                                           
037940                                                                          
037950         IF INDATA-FEL = NEJ                                              
038000           MOVE MID-KDFORDON TO WS-KDFORDON                               
038100           PERFORM BA-KOLLA-KDFORDON                                      
038200           IF KDFORDON-OK = JA                                            
038300             MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDFORDON-ATTR               
038400           ELSE                                                           
038500             MOVE MFS-ALFA-FAELT-FEL TO MOD-KDFORDON-ATTR                 
038510             MOVE MED-9(SPRAAK-IX) TO MOD-TEMFSINF                        
038600             MOVE JA TO INDATA-FEL                                        
038700           END-IF                                                         
038710         END-IF                                                           
038800       END-IF                                                             
038900     END-IF                                                               
038910                                                                          
039000     IF MID-FLKOPIE NOT = ALL '+'                                         
039100       IF SWEDISH-TEXT                                                    
039200         IF MID-FLKOPIE = 'J' OR 'N' OR ' '                               
039300           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLKOPIE-ATTR                  
039400         ELSE                                                             
039500           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLKOPIE-ATTR                    
039600           MOVE JA TO INDATA-FEL                                          
039700         END-IF                                                           
039800       ELSE                                                               
039900         IF MID-FLKOPIE = 'Y' OR 'N' OR ' '                               
040000           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLKOPIE-ATTR                  
040100         ELSE                                                             
040200           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLKOPIE-ATTR                    
040300           MOVE JA TO INDATA-FEL                                          
040400         END-IF                                                           
040500       END-IF                                                             
040600     END-IF                                                               
040610                                                                          
040700     IF MID-IDILLU NOT = ALL '+'                                          
040800       IF MID-IDILLU NOT NUMERIC                                          
040900         MOVE MFS-NUM-FAELT-FEL TO MOD-IDILLU-ATTR                        
041000         MOVE JA TO INDATA-FEL                                            
041100       ELSE                                                               
041200         IF MID-IDILLU = ZERO                                             
041300           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDILLU-ATTR                    
041400         ELSE                                                             
041500           MOVE MID-IDILLU TO W-IDILLU                                    
041600           PERFORM IMS-GU-ILLU                                            
041700           IF SEGMENT-FINNS                                               
041800             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDILLU-ATTR                  
041900           ELSE                                                           
042000             MOVE MFS-NUM-FAELT-FEL TO MOD-IDILLU-ATTR                    
042100             MOVE JA TO INDATA-FEL                                        
042200           END-IF                                                         
042300         END-IF                                                           
042400       END-IF                                                             
042500     END-IF                                                               
042510                                                                          
042600     MOVE +1 TO INDX                                                      
042700     PERFORM UNTIL INDX > +17                                             
042800       IF MID-TIOMBRYT(INDX) NOT = ALL '+'                                
042900         IF MID-TIOMBRYT(INDX) NOT NUMERIC                                
043000           MOVE MFS-NUM-FAELT-FEL TO MOD-TIOMBRYT-ATTR(INDX)              
043010           MOVE MED-7(SPRAAK-IX)  TO MOD-TEMFSINF                         
043100           MOVE JA TO INDATA-FEL                                          
043200         ELSE                                                             
043300           IF MID-TIOMBRYT(INDX) > ZERO                                   
043400             MOVE   'AAMMDD'       TO DAT-KDDATFORM                       
043500             MOVE MID-TIOMBRYT(INDX) TO DAT-I-TIDATUM                     
043600             CALL WDATKONV USING DAT-KDDATFORM                            
043700                  DAT-I-TIDATUM DAT-O-TIDATUM DAT-KDSVAR                  
043800             IF DAT-KDSVAR-FEL                                            
043900               MOVE MFS-NUM-FAELT-FEL TO MOD-TIOMBRYT-ATTR(INDX)          
043910               MOVE MED-7(SPRAAK-IX)  TO MOD-TEMFSINF                     
044000               MOVE JA TO INDATA-FEL                                      
044100             ELSE                                                         
044200               MOVE MFS-NUM-FAELT-RAETT TO MOD-TIOMBRYT-ATTR(INDX)        
044300             END-IF                                                       
044400           ELSE                                                           
044500             MOVE MFS-NUM-FAELT-RAETT TO MOD-TIOMBRYT-ATTR(INDX)          
044600           END-IF                                                         
044700         END-IF                                                           
044800       END-IF                                                             
044900       ADD +1 TO INDX                                                     
045000     END-PERFORM                                                          
045010                                                                          
045100     IF MID-TENOTE NOT = ALL '+'                                          
045200       MOVE MFS-ALFA-FAELT-RAETT TO MOD-TENOTE-ATTR                       
045300     END-IF                                                               
045301                                                                          
045310* TIOMBRYT-F                                                              
045311                                                                          
045320     IF MID-TIOMBRYT-F NOT = ALL '+'                                      
045321*      --- Formell kontroll                                               
045324       IF MID-TIOMBRYT-F NOT NUMERIC                                      
045325         MOVE MFS-NUM-FAELT-FEL TO MOD-TIOMBRYT-F-ATTR                    
045326         MOVE MED-7(SPRAAK-IX)      TO MOD-TEMFSINF                       
045327         MOVE JA TO INDATA-FEL                                            
045328       ELSE                                                               
045329         IF MID-TIOMBRYT-F  > ZERO                                        
045330           MOVE   'AAMMDD'       TO DAT-KDDATFORM                         
045331           MOVE MID-TIOMBRYT-F     TO DAT-I-TIDATUM                       
045332           CALL WDATKONV USING DAT-KDDATFORM                              
045333                DAT-I-TIDATUM DAT-O-TIDATUM DAT-KDSVAR                    
045334           IF DAT-KDSVAR-FEL                                              
045335             MOVE MFS-NUM-FAELT-FEL TO MOD-TIOMBRYT-F-ATTR                
045336             MOVE MED-7(SPRAAK-IX)  TO MOD-TEMFSINF                       
045337             MOVE JA TO INDATA-FEL                                        
045338           ELSE                                                           
045339             MOVE MFS-NUM-FAELT-RAETT TO MOD-TIOMBRYT-F-ATTR              
045340           END-IF                                                         
045341         ELSE                                                             
045342           MOVE MFS-NUM-FAELT-RAETT TO MOD-TIOMBRYT-F-ATTR                
045343         END-IF                                                           
045344       END-IF                                                             
045345                                                                          
045346       IF INDATA-FEL = NEJ                                                
045347*        --- Affärslogisk kontroll                                        
045348         PERFORM IMS-GU-KAT                                               
045349         IF SEGMENT-FINNS                                                 
045350           IF KAT-TIOMBRYT-SEN > ZERO                                     
045351*            --- Man får inte ändra efter första master-uppdat            
045352             MOVE MFS-NUM-FAELT-FEL TO MOD-TIOMBRYT-F-ATTR                
045353             MOVE MED-8(SPRAAK-IX)  TO MOD-TEMFSINF                       
045354             MOVE JA TO INDATA-FEL                                        
045355           ELSE                                                           
045356             MOVE MFS-NUM-FAELT-RAETT TO MOD-TIOMBRYT-F-ATTR              
045357           END-IF                                                         
045358           MOVE KAT-KDFORDON  TO WS-KDFORDON                              
045359         END-IF                                                           
045360                                                                          
045361         IF INDATA-FEL = NEJ                                              
045362           IF WS-KDFORDON = SPACE                                         
045363             MOVE MFS-ALFA-FAELT-FEL TO MOD-KDFORDON-ATTR                 
045364             MOVE MED-5(SPRAAK-IX)   TO MOD-TEMFSINF                      
045365             MOVE JA TO INDATA-FEL                                        
045366           ELSE                                                           
045367*            --- Kolla om värdet är unikt på basen.                       
045369                                                                          
045370*            -- Konvertera FÖRST till Y2K-format på datumfält             
045371             MOVE MID-TIOMBRYT-F     TO W-TIOMBRYT-6                      
045372             PERFORM S51-Y2K-TIOMBRYT                                     
045375             MOVE W-TIOMBRYT-7   TO W-TIOMBRYT-F-MAX                      
045376                                    W-TIOMBRYT-F-MIN                      
045379*            Söker med LO- och HI-value i W-KDFORDON-MIN o MAX            
045380             PERFORM IMS-GN-WDN1A                                         
045381             IF SEGMENT-FINNS                                             
045382*              --- Fanns redan                                            
045383               MOVE MFS-NUM-FAELT-FEL TO MOD-TIOMBRYT-F-ATTR              
045384               MOVE KATA-IDCATNR      TO MED-6-KAT-1                      
045385                                         MED-6-KAT-2                      
045386               MOVE MED-6(SPRAAK-IX)  TO MOD-TEMFSINF                     
045387               MOVE JA TO INDATA-FEL                                      
045388             ELSE                                                         
045389               MOVE MFS-NUM-FAELT-RAETT TO MOD-TIOMBRYT-F-ATTR            
045390             END-IF                                                       
045391           END-IF                                                         
045392         END-IF                                                           
045393       END-IF                                                             
045394     END-IF                                                               
045400     .                                                                    
045500     EJECT                                                                
045600 BA-KOLLA-KDFORDON SECTION.                                               
045700     SKIP2                                                                
045800     SET IX TO +1                                                         
045900     SEARCH W-FORDONSLAG                                                  
046000            AT END MOVE NEJ TO KDFORDON-OK                                
046100            WHEN W-KDFORDON(IX) = WS-KDFORDON                             
046200            MOVE JA TO KDFORDON-OK                                        
046300     END-SEARCH                                                           
046400     .                                                                    
046500     EJECT                                                                
046600 BB-TRANSFORM-BOKSTAV SECTION.                                            
046700     SKIP2                                                                
046800     INSPECT MID-BEEMBLEM CONVERTING LITEN-BOKSTAV TO STOR-BOKSTAV        
046900     INSPECT MID-BEMASTER CONVERTING LITEN-BOKSTAV TO STOR-BOKSTAV        
047000     INSPECT MID-KDFORDON CONVERTING LITEN-BOKSTAV TO STOR-BOKSTAV        
047100     INSPECT MID-FLKOPIE CONVERTING LITEN-BOKSTAV TO STOR-BOKSTAV         
047200     INSPECT MID-TENOTE CONVERTING LITEN-BOKSTAV TO STOR-BOKSTAV          
047300     .                                                                    
047400     EJECT                                                                
047500 C-UPPDATERA SECTION.                                                     
047600     SKIP2                                                                
047700     ACCEPT DAGENS-DATUM FROM DATE                                        
047800     INSPECT MID-BECAT-RAD1 REPLACING ALL HIGH-VALUE BY SPACE             
047900     INSPECT MID-BECAT-RAD2 REPLACING ALL HIGH-VALUE BY SPACE             
048000                                                                          
048100     PERFORM IMS-GHU-KAT                                                  
048200                                                                          
048300     IF SEGMENT-FINNS                                                     
048400***                               UPPDATERING                             
048500       PERFORM CA-UPPDATERA-BEF-KAT                                       
048600       IF INDATA-FEL = NEJ                                                
048700         PERFORM CB-UPPDATERA-ILLU                                        
048800       END-IF                                                             
048900       IF INDATA-FEL = JA                                                 
049010         MOVE ERR-DO-NOT-SPACE  TO MED-IDMFSMED                           
049020         CALL WMEDKONV USING MED-WMEDAREA                                 
049030         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
049100       ELSE                                                               
049200         PERFORM IMS-REPL-KAT                                             
049210                                                                          
049220         MOVE INF-UPD-DONE   TO MED-IDMFSMED                              
049230         CALL WMEDKONV USING MED-WMEDAREA                                 
049240         MOVE MED-MFSMED TO MOD-TEMFSINF                                  
049400       END-IF                                                             
049500     ELSE                                                                 
049510***         INSERT AV NY KATALOG                                          
049700       PERFORM CC-NYUPPLAGG-KAT                                           
049801                                                                          
049810       MOVE INF-UPD-DONE     TO MED-IDMFSMED                              
049820       CALL WMEDKONV USING MED-WMEDAREA                                   
049830       MOVE MED-MFSMED TO MOD-TEMFSINF                                    
049840*        --- Även nedan INFO-meddelande till felraden                     
049850       MOVE MED-12 (SPRAAK-IX) TO MOD-TEMFSFEL                            
050600     END-IF                                                               
050700     .                                                                    
050800     EJECT                                                                
050900 CA-UPPDATERA-BEF-KAT SECTION.                                            
051000     SKIP2                                                                
051100     IF MID-BECAT NOT = ALL '+'                                           
051200                                                                          
051300       IF KAT-BECAT NOT = SPACE                                           
051400         IF MID-BECAT = SPACE                                             
051500                                                                          
051600*----------- IFYLLT FÄLT FÅR ALDRIG BLANKAS UT, isåfall EOF !             
051700           MOVE MFS-ALFA-FAELT-FEL TO MOD-BECAT-RAD1-ATTR                 
051800                                      MOD-BECAT-RAD2-ATTR                 
051900         ELSE                                                             
052000           IF MID-BECAT-RAD1 = ALL '+'                                    
052100             MOVE SPACE          TO KAT-BECAT-RAD1                        
052200           ELSE                                                           
052300             MOVE MID-BECAT-RAD1 TO KAT-BECAT-RAD1                        
052400           END-IF                                                         
052500           IF MID-BECAT-RAD2 = ALL '+'                                    
052600             MOVE SPACE          TO KAT-BECAT-RAD2                        
052700           ELSE                                                           
052800             MOVE MID-BECAT-RAD2 TO KAT-BECAT-RAD2                        
052900           END-IF                                                         
053000         END-IF                                                           
053100       ELSE                                                               
053200         IF MID-BECAT-RAD1 = ALL '+'                                      
053300           MOVE SPACE          TO KAT-BECAT-RAD1                          
053400         ELSE                                                             
053500           MOVE MID-BECAT-RAD1 TO KAT-BECAT-RAD1                          
053600         END-IF                                                           
053700         IF MID-BECAT-RAD2 = ALL '+'                                      
053800           MOVE SPACE          TO KAT-BECAT-RAD2                          
053900         ELSE                                                             
054000           MOVE MID-BECAT-RAD2 TO KAT-BECAT-RAD2                          
054100         END-IF                                                           
054200       END-IF                                                             
054300     END-IF                                                               
054310                                                                          
054400     IF MID-BEEMBLEM NOT = ALL '+'                                        
054500       IF (MID-BEEMBLEM = ALL SPACE) AND                                  
054600       (KAT-BEEMBLEM NOT = SPACE)                                         
054700                                                                          
054800*------------------ IFYLLT FÄLT FÅR ALDRIG BLANKAS UT                     
054900         MOVE MFS-ALFA-FAELT-FEL TO MOD-BEEMBLEM-ATTR                     
055000         MOVE JA TO INDATA-FEL                                            
055100       ELSE                                                               
055110         IF KAT-BEEMBLEM = SPACE                                          
055120         OR KAT-TIOMBRYT-SEN = ZERO                                       
055130*          --- Man vill ändra EMBLEM innan FÖRSTA masteruppdat            
055140*          --- Övr. kontroller är gjorda i B-KOLLA...                     
055200           INSPECT MID-BEEMBLEM CONVERTING GEMENA TO VERSALER             
055300           MOVE MID-BEEMBLEM TO KAT-BEEMBLEM                              
055400         END-IF                                                           
055410       END-IF                                                             
055500     END-IF                                                               
055510                                                                          
055600     IF MID-BEMASTER NOT = ALL '+'                                        
055700       IF (MID-BEMASTER = ALL SPACE) AND                                  
055800       (KAT-BEMASTER NOT = SPACE)                                         
055900                                                                          
056000*------------------ IFYLLT FÄLT FÅR ALDRIG BLANKAS UT                     
056100         MOVE MFS-ALFA-FAELT-FEL TO MOD-BEMASTER-ATTR                     
056200         MOVE JA TO INDATA-FEL                                            
056300       ELSE                                                               
056310         IF KAT-BEMASTER = SPACE                                          
056320         OR KAT-TIOMBRYT-SEN = ZERO                                       
056330*          --- Man vill ändra BEMASTER innan FÖRSTA masteruppdat          
056340*          --- Övr. kontroller är gjorda i B-KOLLA...                     
056400           INSPECT MID-BEMASTER CONVERTING GEMENA TO VERSALER             
056500           MOVE MID-BEMASTER TO KAT-BEMASTER                              
056600         END-IF                                                           
056610       END-IF                                                             
056700     END-IF                                                               
056710                                                                          
056800     IF MID-FLKOPIE NOT = ALL '+'                                         
056900       IF MID-FLKOPIE = SPACE                                             
057000         MOVE 'N' TO KAT-FLKOPIE                                          
057100       ELSE                                                               
057200         IF ENGLISH-TEXT    AND MID-FLKOPIE = 'Y'                         
057300           MOVE 'J' TO KAT-FLKOPIE                                        
057400         ELSE                                                             
057500           MOVE MID-FLKOPIE TO KAT-FLKOPIE                                
057600         END-IF                                                           
057700       END-IF                                                             
057800     END-IF                                                               
057810                                                                          
057900     MOVE +1 TO INDX                                                      
058000     PERFORM UNTIL INDX > +17                                             
058100       IF MID-TIOMBRYT(INDX) NOT = ALL '+'                                
058200         MOVE MID-TIOMBRYT(INDX) TO W-TIOMBRYT-6                          
058300         PERFORM S51-Y2K-TIOMBRYT                                         
058400         MOVE W-TIOMBRYT-7       TO KAT-TIOMBRYT(INDX)                    
058500       END-IF                                                             
058600       ADD +1 TO INDX                                                     
058700     END-PERFORM                                                          
058800     PERFORM X1-KOMPRIMERA-TIOMBRYT-TABELL                                
058900                                                                          
059000     IF MID-TENOTE NOT = ALL '+'                                          
059100       INSPECT MID-TENOTE CONVERTING GEMENA TO VERSALER                   
059200       MOVE MID-TENOTE TO KAT-TENOTE                                      
059300     END-IF                                                               
059301                                                                          
059305     IF MID-KDFORDON NOT = ALL '+'                                        
059306       IF KAT-TIOMBRYT-SEN = ZERO                                         
059307*        --- Man vill ändra KDFORDON innan FÖRSTA masteruppdat            
059308         INSPECT MID-KDFORDON CONVERTING GEMENA TO VERSALER               
059309         MOVE MID-KDFORDON TO KAT-KDFORDON                                
059310       END-IF                                                             
059311     END-IF                                                               
059312                                                                          
059320     IF MID-TIOMBRYT-F NOT = ALL '+'                                      
059323       IF KAT-TIOMBRYT-SEN = ZERO                                         
059326*        --- Man vill ändra TIOMBRYT-F innan FÖRSTA masteruppdat          
059327*        --- Alla kontroller är gjorda i B-KOLLA...                       
059328         MOVE MID-TIOMBRYT-F     TO W-TIOMBRYT-6                          
059329         PERFORM S51-Y2K-TIOMBRYT                                         
059330         MOVE W-TIOMBRYT-7       TO KAT-TIOMBRYT-F                        
059332       END-IF                                                             
059334     END-IF                                                               
059340                                                                          
060000     .                                                                    
060100     EJECT                                                                
060200 CB-UPPDATERA-ILLU SECTION.                                               
060300     SKIP2                                                                
060400*    VINJETTILLUSTRATION FÖR KATALOGEN  (OMSLAGSBILD)                     
060500     IF MID-IDILLU NOT = ALL '+'                                          
060600       IF MID-IDILLU = ZERO                                               
060700         PERFORM IMS-GHU-AVS-ILLU                                         
060800         IF SEGMENT-FINNS                                                 
060900           PERFORM IMS-DLET-AVS-ILLU                                      
061000         END-IF                                                           
061100       ELSE                                                               
061200         PERFORM IMS-GHU-AVS-ILLU                                         
061300         IF SEGMENT-FINNS                                                 
061400           PERFORM IMS-DLET-AVS-ILLU                                      
061500           MOVE MID-IDILLU TO AVS-ILLU-IDILLU                             
061600           MOVE LOW-VALUE  TO AVS-ILLU-KDCATPUB-FOM                       
061700           PERFORM IMS-ISRT-AVS-ILLU                                      
061800         ELSE                                                             
061900           MOVE MID-IDILLU TO AVS-ILLU-IDILLU                             
062000           MOVE LOW-VALUE  TO AVS-ILLU-KDCATPUB-FOM                       
062100           PERFORM IMS-ISRT-AVS-ILLU                                      
062200         END-IF                                                           
062300       END-IF                                                             
062400     END-IF                                                               
062500     .                                                                    
062600     EJECT                                                                
062700 CC-NYUPPLAGG-KAT SECTION.                                                
062800     SKIP2                                                                
062900     MOVE W-IDCATNR    TO KAT-IDCATNR                                     
063000                                                                          
063100     IF MID-BECAT NOT = ALL '+'                                           
063200       IF MID-BECAT-RAD1 NOT = ALL '+'                                    
063300*          TRANSFORM MID-BECAT-RAD1 FROM GEMENA TO VERSALER               
063400         MOVE MID-BECAT-RAD1 TO KAT-BECAT-RAD1                            
063500       ELSE                                                               
063600         MOVE SPACE       TO KAT-BECAT-RAD1                               
063700       END-IF                                                             
063800       IF MID-BECAT-RAD2 NOT = ALL '+'                                    
063900*          TRANSFORM MID-BECAT-RAD2 FROM GEMENA TO VERSALER               
064000         MOVE MID-BECAT-RAD2 TO KAT-BECAT-RAD2                            
064100       ELSE                                                               
064200         MOVE SPACE       TO KAT-BECAT-RAD2                               
064300       END-IF                                                             
064400     ELSE                                                                 
064500       MOVE SPACE     TO KAT-BECAT                                        
064600     END-IF                                                               
064610                                                                          
064700     IF MID-BEEMBLEM NOT = ALL '+'                                        
064800       INSPECT MID-BEEMBLEM CONVERTING GEMENA TO VERSALER                 
064900       MOVE MID-BEEMBLEM TO KAT-BEEMBLEM                                  
065000     ELSE                                                                 
065100       MOVE SPACE     TO KAT-BEEMBLEM                                     
065200     END-IF                                                               
065300                                                                          
065400     IF MID-BEMASTER NOT = ALL '+'                                        
065500       INSPECT MID-BEMASTER CONVERTING GEMENA TO VERSALER                 
065600       MOVE MID-BEMASTER TO KAT-BEMASTER                                  
065700     ELSE                                                                 
065800       MOVE SPACE     TO KAT-BEMASTER                                     
065900     END-IF                                                               
066000*    FLKATVAD SÄTTS INITIALT TILL NEJ, JA-SÄTTS PÅ 1532                   
066100     MOVE NEJ TO KAT-FLKATVAD                                             
066200                                                                          
066300     IF MID-FLKOPIE = ' ' OR MID-FLKOPIE = ALL '+'                        
066400       MOVE 'J'            TO KAT-FLKOPIE                                 
066500     ELSE                                                                 
066600       IF ENGLISH-TEXT    AND MID-FLKOPIE = 'Y'                           
066700         MOVE 'J'         TO KAT-FLKOPIE                                  
066800       ELSE                                                               
066900         MOVE MID-FLKOPIE TO KAT-FLKOPIE                                  
067000       END-IF                                                             
067100     END-IF                                                               
067200                                                                          
067300     IF MID-TENOTE NOT = ALL '+'                                          
067400       INSPECT MID-TENOTE CONVERTING GEMENA TO VERSALER                   
067500       MOVE MID-TENOTE TO KAT-TENOTE                                      
067600     ELSE                                                                 
067700       MOVE SPACE      TO KAT-TENOTE                                      
067800     END-IF                                                               
067900                                                                          
068000     IF MID-KDFORDON NOT = ALL '+'                                        
068100       MOVE MID-KDFORDON TO KAT-KDFORDON                                  
068200     ELSE                                                                 
068300       MOVE SPACE        TO KAT-KDFORDON                                  
068400     END-IF                                                               
068500*    KDCATPUB SÄTTS INITIALT TILL BLANK. UPPDATERAS I 1525.               
068600*    VISAR DEN TIDKOD SOM GÄLLDE FÖR DEN SENASTE OMBRYTNINGEN.            
068700     MOVE SPACE TO KAT-KDCATPUB-FOM                                       
068800                                                                          
068900     MOVE +1 TO INDX                                                      
069000     PERFORM UNTIL INDX > +17                                             
069100       IF MID-TIOMBRYT(INDX) NOT = ALL '+'                                
069200         MOVE MID-TIOMBRYT(INDX) TO W-TIOMBRYT-6                          
069300         PERFORM S51-Y2K-TIOMBRYT                                         
069400         MOVE W-TIOMBRYT-7       TO KAT-TIOMBRYT(INDX)                    
069500       ELSE                                                               
069600         MOVE ZERO               TO KAT-TIOMBRYT(INDX)                    
069700       END-IF                                                             
069800       ADD +1 TO INDX                                                     
069900     END-PERFORM                                                          
070000     PERFORM X1-KOMPRIMERA-TIOMBRYT-TABELL                                
070100                                                                          
070200     MOVE DAGENS-DATUM TO KAT-TIREGDAT                                    
070300     MOVE ZERO         TO KAT-TIHIST                                      
070600*         (PUBLICERINGSDATUM) TIOMBRYT-PUBL SATTES I W10525               
070700                          KAT-TIOMBRYT-PUBL                               
070800*         (OMBRYTNINGS ORDER) TIOMBRYT-ORD SATTES I W10525                
070900                          KAT-TIOMBRYT-ORD                                
071000*         (SENASTE OMBR.DAT.) TIOMBRYT-SEN SATTES I W15450                
071010*                             numera sätts det i W15964                   
071100                          KAT-TIOMBRYT-SEN                                
071110*         (FÖRSTA OMBR.DATUM) TIOMBRYT-F SÄTTS NUMERA HÄR                 
071120*                             tidigare sattes det i W15450                
071130                          KAT-TIOMBRYT-F                                  
071140     IF MID-TIOMBRYT-F NOT = ALL '+'                                      
071141*      -- Byt till Y2K-format på ombryt-datum                             
071142       MOVE MID-TIOMBRYT-F TO W-TIOMBRYT-6                                
071143       PERFORM S51-Y2K-TIOMBRYT                                           
071144       MOVE W-TIOMBRYT-7   TO KAT-TIOMBRYT-F                              
071150     END-IF                                                               
071200                                                                          
071300     MOVE +1 TO INDX                                                      
071400     PERFORM UNTIL INDX > +10                                             
071500       MOVE SPACE TO KAT-IDMODELL     (INDX)                              
071600                     KAT-IDVARIANT    (INDX)                              
071700       MOVE ZERO  TO KAT-TIMODAAR-STA (INDX)                              
071800                     KAT-TIMODAAR-STO (INDX)                              
071900       IF INDX < +7                                                       
072000         MOVE SPACE TO KAT-IDPARTGRP  (INDX)                              
072100       END-IF                                                             
072200       ADD +1 TO INDX                                                     
072300     END-PERFORM                                                          
072400                                                                          
072500     PERFORM IMS-ISRT-KAT                                                 
072510         MOVE INF-UPD-DONE   TO MED-IDMFSMED                              
072520         CALL WMEDKONV USING MED-WMEDAREA                                 
072530         MOVE MED-MFSMED TO MOD-TEMFSINF                                  
072700     .                                                                    
072800     EJECT                                                                
077500 D-LAS-KATALOGID SECTION.                                                 
077600     SKIP2                                                                
077700     PERFORM IMS-GU-KAT                                                   
077800     IF SEGMENT-FINNS                                                     
077900       MOVE KAT-BECAT-RAD1 TO MOD-BECAT-RAD1                              
078000       MOVE KAT-BECAT-RAD2 TO MOD-BECAT-RAD2                              
078100       MOVE KAT-BEEMBLEM   TO MOD-BEEMBLEM                                
078200       MOVE KAT-BEMASTER   TO MOD-BEMASTER                                
078300       MOVE KAT-KDFORDON   TO MOD-KDFORDON                                
078400       MOVE KAT-FLKATVAD   TO MOD-FLKATVAD                                
078500       IF KAT-FLKOPIE = 'J' AND ENGLISH-TEXT                              
078600         MOVE     'Y'      TO MOD-FLKOPIE                                 
078700       ELSE                                                               
078800         MOVE KAT-FLKOPIE  TO MOD-FLKOPIE                                 
078900       END-IF                                                             
079000       MOVE KAT-TIOMBRYT-F   TO MOD-TIOMBRYT-F                            
079100       MOVE KAT-TIREGDAT     TO MOD-TIREGDAT                              
079200       MOVE KAT-TIHIST       TO MOD-TIHIST                                
079300       MOVE KAT-TIOMBRYT-SEN TO MOD-TIOMBRYT-SEN                          
079400                                                                          
079500       IF KAT-TIOMBRYT-ORD > ZERO                                         
079600*        NY OMBRYTNING BESTÄLLD IDAG. INFO FÅR INTE VISAS ÄN.             
079700*        KAT-TIOMBRYT-ORD NOLLAS ISLUTET AV BATCHEN W154J09H.             
079800         MOVE 'BES'               TO MOD-KDCATPUB-R-FOM                   
079900         MOVE  ZERO               TO MOD-TIAAVV-PUBL                      
080000       ELSE                                                               
080100         MOVE KAT-KDCATPUB-FOM (4:3)                                      
080200                                  TO MOD-KDCATPUB-R-FOM                   
080300         IF KAT-TIOMBRYT-PUBL > ZERO                                      
080400           MOVE KAT-TIOMBRYT-PUBL TO DAT-I-TIDATUM                        
080500           MOVE 'AAMMDD'          TO DAT-KDDATFORM                        
080600           CALL WDATKONV USING DAT-KDDATFORM  DAT-I-TIDATUM               
080700                              DAT-O-TIDATUM DAT-KDSVAR                    
080800           IF DAT-KDSVAR-FEL                                              
080900             MOVE 'FEL DATUM I TIOMBRYT-PUBL I WDN101'                    
081000                                  TO ABEND-TEXT                           
081100             CALL FELLOG                                                  
081200           ELSE                                                           
081300             MOVE DAT-TIAAVV-GRP  tO MOD-TIAAVV-PUBL                      
081400           END-IF                                                         
081500         ELSE                                                             
081600           MOVE  ZERO TO MOD-TIAAVV-PUBL                                  
081700         END-IF                                                           
081800       END-IF                                                             
081900                                                                          
082000       MOVE +1 TO INDX                                                    
082100       PERFORM UNTIL INDX > +17                                           
082200         MOVE KAT-TIOMBRYT(INDX) TO MOD-TIOMBRYT(INDX)                    
082300         ADD +1 TO INDX                                                   
082400       END-PERFORM                                                        
082500       MOVE KAT-TENOTE       TO MOD-TENOTE                                
082600                                                                          
082700       PERFORM IMS-GU-AVS-ILLU                                            
082800       IF SEGMENT-FINNS                                                   
082900         MOVE ILLU-IDILLU TO MOD-IDILLU                                   
083000       ELSE                                                               
083100         MOVE MFS-RENSA-FAELT TO MOD-IDILLU                               
083200       END-IF                                                             
083300     ELSE                                                                 
083400       PERFORM F-RENSA-BILD                                               
083500       MOVE MED-2 (SPRAAK-IX) TO MOD-TEMFSINF                             
083600     END-IF                                                               
083700     .                                                                    
083800     EJECT                                                                
083900 E-VISA-BILD-IGEN SECTION.                                                
084000     SKIP2                                                                
084100     MOVE MFS-ROER-EJ-FAELT TO MOD-BECAT-RAD1                             
084200                               MOD-BECAT-RAD2                             
084300                               MOD-BEEMBLEM                               
084400                               MOD-BEMASTER                               
084500                               MOD-KDFORDON                               
084600                               MOD-FLKOPIE                                
084700                               MOD-TIOMBRYT-F                             
084800                               MOD-TIOMBRYT-SEN                           
084900                               MOD-TIAAVV-PUBL                            
085000                               MOD-KDCATPUB-R-FOM                         
085100                               MOD-FLKATVAD                               
085200                               MOD-TIREGDAT                               
085300                               MOD-IDILLU                                 
085400                               MOD-TIHIST                                 
085500                               MOD-TENOTE                                 
085600     MOVE +1 TO INDX                                                      
085700     PERFORM UNTIL  INDX > +17                                            
085800       MOVE MFS-ROER-EJ-FAELT TO MOD-TIOMBRYT(INDX)                       
085900       ADD +1 TO INDX                                                     
086000     END-PERFORM                                                          
086100     .                                                                    
086200     EJECT                                                                
086300 F-RENSA-BILD SECTION.                                                    
086400     SKIP2                                                                
086500     MOVE MFS-RENSA-FAELT TO MOD-BECAT-RAD1                               
086600                             MOD-BECAT-RAD2                               
086700                             MOD-BEEMBLEM                                 
086800                             MOD-BEMASTER                                 
086900                             MOD-KDFORDON                                 
087000                             MOD-FLKOPIE                                  
087100                             MOD-TIOMBRYT-F                               
087200                             MOD-TIOMBRYT-SEN                             
087300                             MOD-TIAAVV-PUBL                              
087400                             MOD-KDCATPUB-R-FOM                           
087500                             MOD-FLKATVAD                                 
087600                             MOD-TIREGDAT                                 
087700                             MOD-IDILLU                                   
087800                             MOD-TIHIST                                   
087900                             MOD-TENOTE                                   
088000     MOVE +1 TO INDX                                                      
088100     PERFORM UNTIL  INDX > +17                                            
088200       MOVE MFS-RENSA-FAELT TO MOD-TIOMBRYT(INDX)                         
088300       ADD +1 TO INDX                                                     
088400     END-PERFORM                                                          
088500     .                                                                    
088600     EJECT                                                                
088700 X1-KOMPRIMERA-TIOMBRYT-TABELL SECTION.                                   
088800     SKIP2                                                                
088900***  KOMPRIMERAR TIOMBRYT I OCCURSSATSEN SÅ ATT ALLA DATUM KOMMER         
089000***  EFTER VARANDRA UTAN MELLANLIGGANDE DATUM MED NOLL I SIG              
089100***  OBEROENDE AV HUR MAN FYLLT I DEM PÅ BILDEN.                          
089200                                                                          
089300     MOVE +1 TO DOIX                                                      
089400     PERFORM UNTIL  DOIX > +16                                            
089500       MOVE +1 TO SUBIX                                                   
089600       COMPUTE MAXSUBIX = +17 - DOIX                                      
089700       PERFORM UNTIL  KAT-TIOMBRYT(DOIX)  >  ZERO                         
089800                  OR  SUBIX  >=  MAXSUBIX                                 
089900         COMPUTE RELIX = DOIX + SUBIX                                     
090000         MOVE KAT-TIOMBRYT(RELIX) TO KAT-TIOMBRYT(DOIX)                   
090100         MOVE ZERO TO KAT-TIOMBRYT(RELIX)                                 
090200         ADD +1 TO SUBIX                                                  
090300       END-PERFORM                                                        
090400       ADD +1 TO DOIX                                                     
090500     END-PERFORM                                                          
090600     .                                                                    
090700     EJECT                                                                
090800*                                                                         
090900* SECTION S51-Y2K-TIOMBRYT LIGGER I                                       
091000* COPYTEXT W.PROD.COBOL.W150Y2K2                                          
091100*                                                                         
091200*    -COPY W150Y2K2                                                       
091300     EJECT                                                                
091400* IMS SEKTIONER                                                           
091500     SKIP1                                                                
091600 IMS-GET-MSG SECTION.                                                     
091700     MOVE '  QC' TO GODK-STATUSKODER                                      
091800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
091900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
092000     PERFORM IMS-STATUSKONTROLL                                           
092100     .                                                                    
092200     SKIP3                                                                
092300 IMS-INSERT-MSG SECTION.                                                  
092400     IF MSGI-IDLAND-SPR = 'SE'                                            
092500       MOVE '0' TO MFS-KDHUVOMR                                           
092600     END-IF                                                               
092700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
092800     MOVE SPACE TO GODK-STATUSKODER                                       
092900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
093000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
093100     PERFORM IMS-STATUSKONTROLL                                           
093200     .                                                                    
093300     EJECT                                                                
093400 IMS-GN-WDN1A    SECTION.                                                 
093500     STRING 'WDN1A1  (WDN1A1KY>=' W-WDN1A1KY-MIN-X                        
093510                    '&WDN1A1KY<=' W-WDN1A1KY-MAX-X ')'                    
093600            DELIMITED BY SIZE INTO SSA1                                   
093700     MOVE '  GEGB' TO GODK-STATUSKODER                                    
093800     CALL CBLTDLI USING GN WDN1A-PCB IO-WDN1A-AREA SSA1                   
093900     MOVE WDN1A-STATUS-CODE TO STATUS-WS                                  
094000     PERFORM IMS-STATUSKONTROLL                                           
094100     .                                                                    
094200     SKIP2                                                                
094210 IMS-GU-KAT SECTION.                                                      
094220     STRING 'WLKATM01(IDCATNR  =' W-IDCATNR-X ')'                         
094230            DELIMITED BY SIZE INTO SSA1                                   
094240     MOVE '  GEGB' TO GODK-STATUSKODER                                    
094250     CALL CBLTDLI USING GU KAT-PCB IO-AREA-1 SSA1                         
094260     MOVE KAT-STATUS-CODE TO STATUS-WS                                    
094270     PERFORM IMS-STATUSKONTROLL                                           
094280     .                                                                    
094290     SKIP2                                                                
094300 IMS-GHU-KAT SECTION.                                                     
094400     STRING 'WLKATM01(IDCATNR  =' W-IDCATNR-X ')'                         
094500            DELIMITED BY SIZE INTO SSA1                                   
094600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
094700     CALL CBLTDLI USING GHU KAT-PCB IO-AREA-1 SSA1                        
094800     MOVE KAT-STATUS-CODE TO STATUS-WS                                    
094900     PERFORM IMS-STATUSKONTROLL                                           
095000     .                                                                    
095100     EJECT                                                                
095200 IMS-REPL-KAT SECTION.                                                    
095300     MOVE '  ' TO GODK-STATUSKODER                                        
095400     CALL CBLTDLI USING REPL KAT-PCB IO-AREA-1                            
095500     MOVE KAT-STATUS-CODE TO STATUS-WS                                    
095600     PERFORM IMS-STATUSKONTROLL                                           
095700     .                                                                    
095800     SKIP2                                                                
095900 IMS-ISRT-KAT SECTION.                                                    
096000     MOVE 'WLKATM01 ' TO SSA1                                             
096100     MOVE '  ' TO GODK-STATUSKODER                                        
096200     CALL CBLTDLI USING ISRT KAT-PCB IO-AREA-1 SSA1                       
096300     MOVE KAT-STATUS-CODE TO STATUS-WS                                    
096400     PERFORM IMS-STATUSKONTROLL                                           
096500     .                                                                    
096600     EJECT                                                                
097500 IMS-GU-AVS-ILLU SECTION.                                                 
097600     STRING 'WLKATH01(WDN501KY =' W-IDCATNR-X W-IDCATGRP-X                
097700                      W-IDCATAVS-X ')'                                    
097800            DELIMITED BY SIZE INTO SSA1                                   
097900     MOVE 'WLKATH11 ' TO SSA2                                             
098000     MOVE '  GE' TO GODK-STATUSKODER                                      
098100     CALL CBLTDLI USING GU AVS-PCB IO-AREA-2 SSA1 SSA2                    
098200     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
098300     PERFORM IMS-STATUSKONTROLL                                           
098400     .                                                                    
098500     SKIP3                                                                
098600 IMS-GHU-AVS-ILLU SECTION.                                                
098700     STRING 'WLKATH01(WDN501KY =' W-IDCATNR-X W-IDCATGRP-X                
098800                      W-IDCATAVS-X ')'                                    
098900            DELIMITED BY SIZE INTO SSA1                                   
099000     MOVE 'WLKATH11 ' TO SSA2                                             
099100     MOVE '  GE' TO GODK-STATUSKODER                                      
099200     CALL CBLTDLI USING GHU AVS-PCB IO-AREA-2 SSA1 SSA2                   
099300     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
099400     PERFORM IMS-STATUSKONTROLL                                           
099500     .                                                                    
099600     SKIP3                                                                
099700 IMS-DLET-AVS-ILLU SECTION.                                               
099800     MOVE SPACE TO GODK-STATUSKODER                                       
099900     CALL CBLTDLI USING DLET AVS-PCB IO-AREA-2                            
100000     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
100100     PERFORM IMS-STATUSKONTROLL                                           
100200     .                                                                    
100300     SKIP3                                                                
100400 IMS-ISRT-AVS-ILLU SECTION.                                               
100500     STRING 'WLKATH01(WDN501KY =' W-IDCATNR-X W-IDCATGRP-X                
100600                      W-IDCATAVS-X ')'                                    
100700            DELIMITED BY SIZE INTO SSA1                                   
100800     MOVE 'WLKATH11 ' TO SSA2                                             
100900     MOVE '  ' TO GODK-STATUSKODER                                        
101000     CALL CBLTDLI USING ISRT AVS-PCB IO-AREA-2 SSA1 SSA2                  
101100     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
101200     PERFORM IMS-STATUSKONTROLL                                           
101300     .                                                                    
101400     EJECT                                                                
102600 IMS-GU-ILLU SECTION.                                                     
102700     STRING 'WLKATK01(IDILLU   =' W-IDILLU-X ')'                          
102800            DELIMITED BY SIZE INTO SSA1                                   
102900     MOVE '  GE' TO GODK-STATUSKODER                                      
103000     CALL CBLTDLI USING GU ILLU-PCB IO-AREA-2 SSA1                        
103100     MOVE ILLU-STATUS-CODE TO STATUS-WS                                   
103200     PERFORM IMS-STATUSKONTROLL                                           
103300     .                                                                    
103400     EJECT                                                                
103500                                                                          
103600 IMS-STATUSKONTROLL SECTION.                                              
103700     SET STATUS-IX TO 1                                                   
103800     SEARCH GODK-STATUS                                                   
103900       AT END                                                             
104000         CALL FELLOG                                                      
104100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
104200         CONTINUE                                                         
104300     END-SEARCH                                                           
104400     .                                                                    
