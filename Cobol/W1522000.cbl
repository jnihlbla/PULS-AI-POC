000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W1522000.                                                
000400 AUTHOR.         CONNY EGHOLT.                                            
000500 DATE-WRITTEN.   97/09/04.                                                
000600 DATE-COMPILED.                                                           
000700*                                                                         
000800*    OBSERVERA / NOTE THIS !                                              
000900*        Before deployment and compilation in PROD/QASE,                  
001000*        Be sure to swich to "PROD" directory setting                     
001100*        the WS-field  COM-CBG-DIR  to right value.                       
001200*                                                                         
001300*                                                                         
001400*    FUNKTION:                                                            
001500*        FÖRSTA PROGRAMMET I W152V2 SOM SKAPAR FILER MED                  
001600*        ÖVERSÄTTNINGSBESTÄLLNING FRÅN SVENSKA/ENGELSKA                   
001700*        TILL 16 ANDRA SPRÅK.                                             
001800*        FÖR TECHLA-BENÄMNINGAR BESTÄLLES ÄVEN ÖVERSÄTTNING               
001900*        Till de nya språken                                              
001910*        J, RUS, KOR, RC, T, TR, RCN                                      
002000*        där TECHLA inte har översättning.                                
002100*                                                                         
002200*        Benämningar som skall översättas läses in från WL1220            
002300*        som påfylls av MPP W10526.                                       
002400*        WL1220 läses endast om 1220-IDORDNR-VCTS > +0                    
002500*                                                                         
002600*        PACKAR IHOP FOTNOTSTEXTER OCH RUBRIKER SOM FÖREKOMMER            
002700*        PÅ FLER ÄN ETT SEGMENT TILL EN ENDA LÅNG STRÄNG, VILKET          
002800*        ÖVERSÄTTNINGSBYRÅN HELST VILL HA.                                
002900*        DE VILL OCKSÅ HA BÅDE SVENSKA OCH ENGELSKA I SAMMA POST.         
003000*        DEN MAXIMALA LÄNGDEN MÅSTE AVKORTAS MED 2 BYTE PER SEGM          
003100*        FÖR ATT GE PLATS ÅT "SHIFT-IN" OCH "SHIFT-OUT" TECKEN.           
003200*                                                                         
003400*        PROGRAMMET UPPDATERAR WL1219 (WDR2)                              
003900*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
003910*        PROGRAMMET LÄSER      WDK6                                       
004000*                                                                         
004300*        PROGRAMMET SKRIVER DISPLAYER PÅ FIL W15221                       
004400*                                                                         
004500*    ÄNDRINGAR:                                                           
004600* 99-12-10: ÄT C-99010 till Rel. 00-1                                     
004700*           Redaktörens "note" på 1503,1506,1509 som lagrats på           
004800*           WL120511 skickas med till översättningsbyrån.                 
004900*                                                /Conny                   
005000*                                                                         
005100* 00-10-31: ÄT C-00005 till Rel. 00-9                                     
005200*           Türkiska språket tillkommer                                   
005300*                                                /Conny                   
005400* 03-06-16: AKUT-ÄT                                                       
005500*           Malaysia (Bahasa) språket utgår                               
005600*                                                /Conny                   
005700* 05-09-05: AKUT-ändring eTracker 2424800                                 
005800*           ny adress till directoryn i gbwux006                          
005900*                                                /Conny                   
006000* 05-09-22: ÄT ändring eTracker 2053029                                   
006100*           Språk 'cn' 'RCN' simplified Chinese tillkommer                
006200*           för Nevis VIDA.                                               
006300*                                                /Conny                   
006400* 06-05-15: LAPS Q&P projektet.                                           
006500*           IDBENNR förlängs till S9(7).                                  
006600*           Läsning av Kataloglexikon tas bort. Används ej.               
006700*                                                /Conny                   
006800* 07-04-10: Add functiongroup and Planner notes in the translation        
006900*           files.  CCID 3890472                                          
006910*           comment "ADDED INFO TO TRANSLATION FILES"                     
007000*           Tillkommer läsning av WDD312 för artikelnummer.               
007010*           Tillkommer läsning av WDK601 (funkgrp)                        
007100*                                                /Conny                   
007200*                                                                         
007210* 07-07-24: Changes needed for communication through Warley server        
007211*                                                                         
007212* 14-12-10: Ta bort all kod mot WL1205 och WL1206                         
007220*                                                                         
007230* 15-03   : Rensning av död kod och justering för parametrar              
007231*           via sysin som försvann när rutinen ändrades från              
007232*           beställnings- till veckorutin                                 
007240*                                                                         
007300*    ABENDKODER:                                                          
007400*        U0016 -  . . . .                                                 
007500*        U1000 -  . . . .                                                 
007600*                                                                         
007700*    EXCEPTIONKOD:                                                        
007800*        U0004 -  DISPLAY-FIL W15221 SKAPAD I b- SECTION.                 
007900                                                                          
008000                                                                          
008100 ENVIRONMENT DIVISION.                                                    
008300 INPUT-OUTPUT SECTION.                                                    
008400                                                                          
008500 FILE-CONTROL.                                                            
008600                                                                          
008700*          --- KOMMANDOFIL                                                
008800     SELECT W1522K                    ASSIGN TO W15220D1.                 
008900                                                                          
009000*          --- ÖVERSÄTTNINGSFIL   PÅ ENGELSKA                             
009100*          ---                         TILL SVENSKA                       
009200     SELECT W152TOSV                   ASSIGN TO W152TOSV.                
009300                                                                          
009400*          --- ÖVERSÄTTNINGSFIL   PÅ SVENSKA                              
009500*          ---                         TILL ENGELSKA                      
009600     SELECT W152TOEN                   ASSIGN TO W152TOEN.                
009700                                                                          
009800*          --- ÖVERSÄTTNINGSFILER PÅ ENGELSKA OCH SVENSKA                 
009900*          ---                         TILL AMERIKANSKA                   
010000     SELECT W152TOUS                   ASSIGN TO W152TOUS.                
010100                                                                          
010200*          ---                         TILL TYSKA                         
010300     SELECT W152TODE                   ASSIGN TO W152TODE.                
010400     SKIP2                                                                
010500*          ---                         TILL FRANSKA                       
010600     SELECT W152TOFR                   ASSIGN TO W152TOFR.                
010700     SKIP2                                                                
010800*          ---                         TILL SPANSKA                       
010900     SELECT W152TOES                   ASSIGN TO W152TOES.                
011000     SKIP2                                                                
011100*          ---                         TILL PORTUGISKA                    
011200     SELECT W152TOPT                   ASSIGN TO W152TOPT.                
011300     SKIP2                                                                
011400*          ---                         TILL NEDERLÄNDSKA                  
011500     SELECT W152TONL                   ASSIGN TO W152TONL.                
011600     SKIP2                                                                
011700*          ---                         TILL ITALIENSKA                    
011800     SELECT W152TOIT                   ASSIGN TO W152TOIT.                
011900     SKIP2                                                                
012000*          ---                         TILL FINSKA                        
012100     SELECT W152TOFI                   ASSIGN TO W152TOFI.                
012200     SKIP2                                                                
012300*          ---                         TILL JAPANSKA                      
012400     SELECT W152TOJA                   ASSIGN TO W152TOJA.                
012500     SKIP2                                                                
012600*          ---                         TILL KOREANSKA                     
012700     SELECT W152TOKO                   ASSIGN TO W152TOKO.                
012800     SKIP2                                                                
012900*          ---                         TILL KINESISKA (TRAD.)             
013000     SELECT W152TOZH                   ASSIGN TO W152TOZH.                
013100     SKIP2                                                                
013200*          ---                         TILL KINESISKA (SIMP.)             
013300     SELECT W152TOCN                   ASSIGN TO W152TOCN.                
013400     SKIP2                                                                
013500*          ---                         TILL RYSKA                         
013600     SELECT W152TORU                   ASSIGN TO W152TORU.                
013700     SKIP2                                                                
013800*          ---                         TILL THAI                          
013900     SELECT W152TOTH                   ASSIGN TO W152TOTH.                
014000     SKIP2                                                                
014100*          ---                         TILL TURKISKA                      
014200     SELECT W152TOTR                   ASSIGN TO W152TOTR.                
014210     SKIP2                                                                
014220*          ---                         TILL POLISKA                       
014230     SELECT W152TOPL                   ASSIGN TO W152TOPL.                
014300                                                                          
014400                                                                          
014500 DATA DIVISION.                                                           
014600     SKIP3                                                                
014700 FILE SECTION.                                                            
014800     SKIP3                                                                
014900 FD  W1522K                                                               
015000     RECORDING       F                                                    
015100     BLOCK CONTAINS  0.                                                   
015200                                                                          
015300 01  KOMMANDO-POST    PIC X(80).                                          
015400     SKIP3                                                                
015500 FD  W152TOSV                                                             
015600     RECORDING       V                                                    
015700     BLOCK CONTAINS  0.                                                   
015800*01  POST -COPY W152PBX  -PRE  TOSVB-  -L.                                
015900     SKIP3                                                                
016000 FD  W152TOEN                                                             
016100     RECORDING       V                                                    
016200     BLOCK CONTAINS  0.                                                   
016300*01  POST -COPY W152PBX  -PRE  TOENB-  -L.                                
016400     SKIP3                                                                
016500 FD  W152TOUS                                                             
016600     RECORDING       V                                                    
016700     BLOCK CONTAINS  0.                                                   
016800*01  POST -COPY W152PBX  -PRE  TOUSB-  -L.                                
016900     SKIP3                                                                
017000 FD  W152TODE                                                             
017100     RECORDING       V                                                    
017200     BLOCK CONTAINS  0.                                                   
017300*01  POST -COPY W152PBX  -PRE  TODEB-  -L.                                
017400     SKIP3                                                                
017500 FD  W152TOFR                                                             
017600     RECORDING       V                                                    
017700     BLOCK CONTAINS  0.                                                   
017800*01  POST -COPY W152PBX  -PRE  TOFRB-  -L.                                
017900     SKIP3                                                                
018000 FD  W152TOES                                                             
018100     RECORDING       V                                                    
018200     BLOCK CONTAINS  0.                                                   
018300*01  POST -COPY W152PBX  -PRE  TOESB-  -L.                                
018400     SKIP3                                                                
018500 FD  W152TOPT                                                             
018600     RECORDING       V                                                    
018700     BLOCK CONTAINS  0.                                                   
018800*01  POST -COPY W152PBX  -PRE  TOPTB-  -L.                                
018900     SKIP3                                                                
019000 FD  W152TONL                                                             
019100     RECORDING       V                                                    
019200     BLOCK CONTAINS  0.                                                   
019300*01  POST -COPY W152PBX  -PRE  TONLB-  -L.                                
019400     SKIP3                                                                
019500 FD  W152TOIT                                                             
019600     RECORDING       V                                                    
019700     BLOCK CONTAINS  0.                                                   
019800*01  POST -COPY W152PBX  -PRE  TOITB-  -L.                                
019900     SKIP3                                                                
020000 FD  W152TOFI                                                             
020100     RECORDING       V                                                    
020200     BLOCK CONTAINS  0.                                                   
020300*01  POST -COPY W152PBX  -PRE  TOFIB-  -L.                                
020400     SKIP3                                                                
020500 FD  W152TOJA                                                             
020600     RECORDING       V                                                    
020700     BLOCK CONTAINS  0.                                                   
020800*01  POST -COPY W152PBX  -PRE  TOJAB-  -L.                                
020900     SKIP3                                                                
021000 FD  W152TOKO                                                             
021100     RECORDING       V                                                    
021200     BLOCK CONTAINS  0.                                                   
021300*01  POST -COPY W152PBX  -PRE  TOKOB-  -L.                                
021400     SKIP3                                                                
021500 FD  W152TOZH                                                             
021600     RECORDING       V                                                    
021700     BLOCK CONTAINS  0.                                                   
021800*01  POST -COPY W152PBX  -PRE  TOZHB-  -L.                                
021900     SKIP3                                                                
022000 FD  W152TOCN                                                             
022100     RECORDING       V                                                    
022200     BLOCK CONTAINS  0.                                                   
022300*01  POST -COPY W152PBX  -PRE  TOCNB-  -L.                                
022400     SKIP3                                                                
022500 FD  W152TORU                                                             
022600     RECORDING       V                                                    
022700     BLOCK CONTAINS  0.                                                   
022800*01  POST -COPY W152PBX  -PRE  TORUB-  -L.                                
022900     SKIP3                                                                
023000 FD  W152TOTH                                                             
023100     RECORDING       V                                                    
023200     BLOCK CONTAINS  0.                                                   
023300*01  POST -COPY W152PBX  -PRE  TOTHB-  -L.                                
023400     SKIP3                                                                
023500 FD  W152TOTR                                                             
023600     RECORDING       V                                                    
023700     BLOCK CONTAINS  0.                                                   
023800*01  POST -COPY W152PBX  -PRE  TOTRB-  -L.                                
023900     SKIP3                                                                
023910 FD  W152TOPL                                                             
023920     RECORDING       V                                                    
023930     BLOCK CONTAINS  0.                                                   
023940*01  POST -COPY W152PBX  -PRE  TOPLB-  -L.                                
023950     SKIP3                                                                
024000                                                                          
024100 WORKING-STORAGE SECTION.                                                 
024200     SKIP2                                                                
024300                                                                          
024400*    -- CHECKED BY WY2000                                                 
024600 77  IDPGM                       PIC X(8)    VALUE 'W1522000'.            
024700 77  JA                          PIC X       VALUE 'J'.                   
024800 77  NEJ                         PIC X       VALUE 'N'.                   
024900 77  OCH                         PIC X       VALUE '&'.                   
024910 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
024920 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
025000*    --- TAB innehåller hex'05'                                           
025100 77  TAB                         PIC X       VALUE '	'.                   
025400 77  WS-KDLEX                    PIC X       VALUE ' '.                   
025710 77  WS-TEARTNOT                 PIC X(40)   VALUE SPACE.                 
025720 77  WS-IDFKNGRP                 PIC 9(4)   VALUE Zero.                   
025800 77  WS-KDERS                    PIC S9(2)   COMP VALUE +99.              
025810 77  PGM-RETURKOD                PIC S9(9)   COMP VALUE ZERO.             
025900     SKIP2                                                                
026000 01  FELTEXT.                                                             
026100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
026200     03  FELTEXT-STR             PIC X(80)   VALUE SPACE.                 
026300                                                                          
026400                                                                          
026800 77  BESTAELL-SW                PIC X      VALUE 'N'.                     
026900     88 BENAMNING-BESTALLD                 VALUE 'J'.                     
027000     88 BENAMNING-EJ-BESTALLD              VALUE 'N'.                     
027100                                                                          
027200 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
027300 01  FILLER REDEFINES DAGENS-DATUM.                                       
027400     03  DAGENS-DATUM-AAR        PIC 9(2).                                
027500     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
027600     03  DAGENS-DATUM-DAG        PIC 9(2).                                
027700                                                                          
027710                                                                          
027800 01  DYNAMISKA-SUBPROGRAM.                                                
027900*                                                                         
028000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
028100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
028200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
028210     03  VIMSID                  PIC X(8)    VALUE 'VIMSID '.             
028300                                                                          
028310                                                                          
028400*    --- PARAMETRAR TILL POSTSUM                                          
028500*                                                                         
028600*01  -COPY W0005   -PRE  POSTSUM-                                         
028700                                                                          
028701                                                                          
028702*    --- PARAMETERS TO VIMSID                                             
028703 01  VIMSID-PARM.                                                         
028704   03  IMSID4                    PIC X(4)    VALUE SPACE.                 
028705   03  FILLER                    PIC X(4)    VALUE SPACE.                 
028710                                                                          
028800                                                                          
028900 01  SEGM                        PIC S9(9) VALUE ZERO COMP SYNC.          
029000                                                                          
031500 01  FILLER                      PIC X(16) VALUE 'SVENSK BEN'.            
031600 01  SVENSK-BENAMNING.                                                    
031700         05  WS-SV-BEARTEXT      PIC X(100).                              
031800                                                                          
031900 01  FILLER                      PIC X(16) VALUE 'ENGELSK BEN'.           
032000 01  ENGELSK-BENAMNING.                                                   
032100         05  WS-EN-BEARTEXT      PIC X(100).                              
032200                                                                          
032300                                                                          
032600                                                                          
035300* -----------------------------------------------------------             
035400*     Här nedan är tabellen för alla beställda språk (inkl ben)           
035500* -----------------------------------------------------------             
035600                                                                          
035700 01  TO-IX                     PIC S9(9)   VALUE +0   COMP SYNC.          
035800 01  TO-IX-MAX                 PIC S9(9)   VALUE +18  COMP SYNC.          
035900                                                                          
036000 01  WS-UTFIL-SPRAK-TABELL.                                               
036100     03 WS-UTFIL-SPRAKTAB-INNEHALL.                                       
036200       05 FILLER      VALUE 'S  SVsv  '   PIC X(9).                       
036300       05 FILLER      VALUE 'GB ENen  '   PIC X(9).                       
036400       05 FILLER      VALUE 'USAUSus  '   PIC X(9).                       
036500       05 FILLER      VALUE 'D  DEde  '   PIC X(9).                       
036600       05 FILLER      VALUE 'F  FRfr  '   PIC X(9).                       
036700       05 FILLER      VALUE 'E  ESes  '   PIC X(9).                       
036800       05 FILLER      VALUE 'P  PTpt  '   PIC X(9).                       
036900       05 FILLER      VALUE 'NL NLnl  '   PIC X(9).                       
037000       05 FILLER      VALUE 'I  ITit  '   PIC X(9).                       
037100       05 FILLER      VALUE 'SF FIfi  '   PIC X(9).                       
037200       05 FILLER      VALUE 'J  JAja  '   PIC X(9).                       
037300       05 FILLER      VALUE 'KORKOko  '   PIC X(9).                       
037400       05 FILLER      VALUE 'RC ZHzh  '   PIC X(9).                       
037500       05 FILLER      VALUE 'RUSRUru  '   PIC X(9).                       
037600       05 FILLER      VALUE 'T  THth  '   PIC X(9).                       
037700       05 FILLER      VALUE 'TR TRtr  '   PIC X(9).                       
037800       05 FILLER      VALUE 'RCNCNcn  '   PIC X(9).                       
037810       05 FILLER      VALUE 'PL PLpl  '   PIC X(9).                       
037900*    --- Tabell för flaggning av önskad översättning                      
038000*    --- ej användbara index i tabellen är (1 1)                          
038100     03 FILLER REDEFINES WS-UTFIL-SPRAKTAB-INNEHALL.                      
038200       05 TABELL-RAD  OCCURS 18.                                          
038300         07 IDSKYLT-TO-VERSAL       PIC X(3).                             
038400         07 ISO-TO-VERSAL           PIC X(2).                             
038500         07 ISO-TO-GEMEN            PIC X(2).                             
038600         07 SKRIV-KOMMANDO          PIC X.                                
038700         07 TIDIGARE-BESTAELLD      PIC X.                                
038800                                                                          
038810                                                                          
038900 01  UT-AREA-START               PIC X(24)   VALUE                        
039000                                             'UT-AREA-START'.             
039100     SKIP2                                                                
039200 01  UT-AREA.                                                             
039300     03  UT-AREA-0.                                                       
039400*        --- OBS! nedanstående FILLERS innehåller x'05' (TAB)             
039500         05  UT-IDPTYP           PIC X(4).                                
039600         05  FILLER              PIC X      VALUE '	'.                    
039700         05  UT-DIFAELT          PIC 9(3).                                
039800         05  FILLER              PIC X      VALUE '	'.                    
039900         05  UT-IDBENNR          PIC 9(7).                                
040000         05  FILLER              PIC X      VALUE '	'.                    
040100         05  UT-IDSPRAK-ISO-TO   PIC X(2).                                
040200         05  FILLER              PIC X      VALUE '	'.                    
040300         05  UT-TEXTAREA         PIC X(247).                              
040400*      --- EN +TAB +SV +TAB +FKNGRP +TAB +TENOTE =247                     
040500*   03  PBEN-AREA -COPY W152PBX   -PRE UT-  -RED  UT-AREA-0               
040600                                                                          
040610                                                                          
040700 01  COMMAND-AREA-START          PIC X(24)   VALUE                        
040800                                             'COMMAND-AREA-START'.        
040900 01  COMMAND-AREA                PIC X(80) VALUE SPACE.                   
041000                                                                          
041100 01  COM-VARIABLER.                                                       
041200     03 COM-INDUTJ               PIC X(30)   VALUE 'W152.W152V2'.         
041300     03 COM-DAGID                PIC 9(6).                                
041500     03 COM-CBG-DIR              PIC X(14).                               
041510     03 COM-CBG-PROD-DIR         PIC X(14)                                
041700                 VALUE '/PROD/inbound/'.                                  
041710     03 COM-CBG-TEST-DIR         PIC X(14)                                
041810                 VALUE '/TEST/inbound/'.                                  
042000                                                                          
042100                                                                          
042200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
042300 01  NYCKLAR-TILL-DLI.                                                    
043300     03  W-KY1219-X.                                                      
043400         05 W-1219-IDHTYP        PIC X(4)    VALUE '1219'.                
043500         05 FILLER               PIC X(26)   VALUE LOW-VALUE.             
043600                                                                          
046500     03  W-IDBENNR-X.                                                     
046600         05  W-IDBENNR           PIC S9(7)   COMP-3 VALUE +0.             
046700                                                                          
046800     03  W-IDARTNR-X.                                                     
046810         05  W-IDARTNR           PIC S9(9)   COMP-3 VALUE +0.             
046820                                                                          
046900     03  W-IDSKYLT-B             PIC X(3)    VALUE SPACE.                 
047000                                                                          
047100                                                                          
047200*    --- STATUS-KOD FRÅN IMS                                              
047300 01  STATUS-WS                   PIC XX.                                  
047400     88  SEGMENT-FINNS                       VALUE '  '.                  
047600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
047900                                                                          
048000 01  GODK-STATUSKODER.                                                    
048100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
048200     SKIP3                                                                
048300 01  ALL-SSA.                                                             
048310     03 SSA1                     PIC X(128).                              
048400     03 SSA2                     PIC X(128).                              
048500                                                                          
048510                                                                          
048600*    --- IMS FUNKTIONSKODER                                               
048700*01  -COPY W0003                                                          
048800                                                                          
048900*    ---  DLI INPUT-OUTPUT AREA                                           
049300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WL121911'.                    
049400 01  DLI-IO-WL121911.                                                     
049500*    03  -COPY WDGX1220 -PRE 1219-                                        
049510                                                                          
049600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WL121921'.                    
049700 01  DLI-IO-WL121921.                                                     
049800*    03  -COPY WDGX1221 -PRE 1219-                                        
049810                                                                          
051200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLBENA01'.                    
051300 01  DLI-IO-WLBENA01.                                                     
051400*    03  -COPY WDD301  -PRE BENA-                                         
051500                                                                          
051600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLBENA11'.                    
051700 01  DLI-IO-WLBENA11.                                                     
051800*    03  -COPY WDD311  -PRE BENA-                                         
051900                                                                          
051901 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLBENA12'.                    
051902 01  DLI-IO-WLBENA12.                                                     
051903*    03  -COPY WDD312  -PRE BENA-                                         
051904                                                                          
051910 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
051920 01  DLI-IO-WDK601.                                                       
051930*    03  -COPY WDK601                                                     
051940                                                                          
051950 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
051960 01  DLI-IO-WDK611.                                                       
051970*    03  -COPY WDK611                                                     
051980                                                                          
051990                                                                          
052000 LINKAGE SECTION.                                                         
052100                                                                          
052200*01  -COPY W0009   -PRE MSG-                                              
052300                                                                          
052700*01  -COPY W0008  -PRE 1219-                                              
052800     05  FILLER                  PIC X.                                   
052900                                                                          
054200*01  -COPY W0008  -PRE BENA-                                              
054300     05  FILLER                  PIC X.                                   
054400                                                                          
054410*01  -COPY W0008  -PRE WDK6-                                              
054420     05  FILLER                  PIC X.                                   
054430                                                                          
054440                                                                          
054500 PROCEDURE DIVISION  USING MSG-PCB  1219-PCB                              
054600                           BENA-PCB WDK6-PCB.                             
054700 MAIN SECTION.                                                            
054800     ENTRY 'DLITCBL' USING MSG-PCB  1219-PCB                              
054900                           BENA-PCB WDK6-PCB.                             
055000     SKIP2                                                                
055100     PERFORM A-INIT-OCH-KOLLA-BEST                                        
055200                                                                          
055300     IF BENAMNING-BESTALLD                                                
056700        PERFORM D-SKRIV-KOMMANDOFIL                                       
056800        PERFORM E-RENSA-WL121921                                          
056810     END-IF                                                               
056900                                                                          
057000     PERFORM Z-FINIT                                                      
057100                                                                          
057200     MOVE PGM-RETURKOD TO RETURN-CODE                                     
057300     GOBACK                                                               
057400     .                                                                    
057500                                                                          
057510                                                                          
057600 A-INIT-OCH-KOLLA-BEST SECTION.                                           
057610     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
057700                                                                          
057800                                                                          
057900     OPEN OUTPUT W1522K                                                   
058000                 W152TOSV W152TOIT                                        
058100                 W152TOEN W152TOFI                                        
058200                 W152TOUS W152TOJA                                        
058300                 W152TODE W152TOKO                                        
058400                 W152TOFR W152TOPL                                        
058500                 W152TOES W152TOZH                                        
058600                 W152TOPT W152TORU                                        
058700                 W152TONL W152TOTH                                        
058800                 W152TOTR W152TOCN                                        
058900                                                                          
059000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
060210                                                                          
060300*    --- Grundställ fil-skriv-kontrollen i tabellen                       
060400     MOVE +1 TO TO-IX                                                     
060500     PERFORM UNTIL TO-IX > TO-IX-MAX                                      
060600       MOVE SPACE TO SKRIV-KOMMANDO (TO-IX)                               
060700       ADD +1 TO TO-IX                                                    
060800     END-PERFORM                                                          
060801     PERFORM AA-KOLLA-BENAMNINGAR                                         
060802                                                                          
060803     ACCEPT COM-DAGID     FROM DATE                                       
060804                                                                          
060805     DISPLAY 'INDUT=' COM-INDUTJ                                          
060806     DISPLAY ' DAGID=' COM-DAGID                                          
060807             ' ORDBEN=' 1219-1220-IDORDER-VCTS-BEN                        
060808             ' ORDLEX=' 1219-1220-IDORDER-VCTS-LEXIKON                    
060809     DISPLAY ' '                                                          
060810                                                                          
060820*    -- FIND OUT WHICH IMS SYSTEM WE ARE USING                            
060830     CALL VIMSID USING VIMSID-PARM                                        
060900     .                                                                    
060910                                                                          
061000                                                                          
120500 AA-KOLLA-BENAMNINGAR  SECTION.                                           
120510     MOVE 'AA-KOLLA-BEN    ' TO CURRENT-SECTION                           
120600                                                                          
120700     SET BENAMNING-EJ-BESTALLD TO TRUE                                    
120800*    --- Läs actionfil med ev. nybest. benämningsöversättningar           
120900     PERFORM IMS-GHU-1219-1220                                            
121000     IF 1219-1220-IDORDER-VCTS-BEN = ZERO                                 
121100       CONTINUE                                                           
121200*      --- Benämningarna är inte klara att översättas                     
121300     ELSE                                                                 
121400       PERFORM IMS-GNP-1219-1221                                          
121500                                                                          
121600       MOVE 'PBEN' TO UT-IDPTYP   POSTSUM-TRANSTYP                        
121700       MOVE 'B'    TO WS-KDLEX                                            
121800       MOVE    60  TO UT-DIFAELT                                          
122000                                                                          
122100       PERFORM UNTIL NOT SEGMENT-FINNS                                    
122200         MOVE SPACE TO WS-SV-BEARTEXT                                     
122300                       WS-EN-BEARTEXT                                     
122400         MOVE 1219-1221-IDBENNR TO W-IDBENNR  UT-IDBENNR                  
122500*        --- Hämta först Benämning på Engelska och Svenska,               
122600*        --- vilka används som från-språk för översättning.               
122700         PERFORM IMS-GU-BENA01                                            
122800*        --- Här ABENDAS om IDBENNR inte finns på BENA01                  
122900                                                                          
123000         MOVE 'GB ' TO W-IDSKYLT-B                                        
123100         PERFORM IMS-GU-BENA11                                            
123200         IF SEGMENT-FINNS                                                 
123300           MOVE BENA-TEXT-BEARTEXT TO WS-EN-BEARTEXT                      
123400         END-IF                                                           
123500                                                                          
123600         MOVE 'S  ' TO W-IDSKYLT-B                                        
123700         PERFORM IMS-GU-BENA11                                            
123800         IF SEGMENT-FINNS                                                 
123900           MOVE BENA-TEXT-BEARTEXT TO WS-SV-BEARTEXT                      
124000         END-IF                                                           
124100         MOVE ZERO TO TO-IX                                               
124200                                                                          
124300         IF  WS-EN-BEARTEXT = SPACE                                       
124400         AND WS-SV-BEARTEXT = SPACE                                       
124500           CONTINUE                                                       
124600         ELSE                                                             
124700*          --- Förbered  FKNGRP                                           
124701           Move Zeroes            To WS-IDFKNGRP                          
124702           Move Spaces            To WS-TEARTNOT                          
124703           Move 99                To WS-KDERS                             
124704           Perform IMS-GU-BENA12-FIRST                                    
124705                                                                          
124706           Perform Until SEGMENT-SAKNAS                                   
124707                   Or WS-KDERS = Zero                                     
124708             If SEGMENT-FINNS                                             
124709               Move BENA-ART-IDARTNR   To W-IDARTNR                       
124710                                                                          
124711               Perform IMS-GU-WDK601                                      
124712               If SEGMENT-FINNS                                           
124713                 Move ART-IDFKNGRP     To WS-IDFKNGRP                     
124714                                                                          
124715                 Perform IMS-GNP-WDK611                                   
124716*                --- Väljer med fördel oersatta artiklar                  
124718                 If SEGMENT-FINNS                                         
124719                   Move CLAG-KDERS     To WS-KDERS                        
124720                 End-If                                                   
124721               End-If                                                     
124722             End-If                                                       
124723             Perform IMS-GNP-BENA12                                       
124724           End-Perform                                                    
124725                                                                          
124730*          --- S och/eller GB finns ! Beställ det som saknas              
124800           MOVE SPACE             TO UT-TEXTAREA                          
124810                                     UT-PBEN-TENOTE                       
124820           Move Zeroes            To UT-PBEN-IDFKNGRP                     
124900           MOVE TAB               TO UT-PBEN-KOMMA5                       
125000                                     UT-PBEN-KOMMA6                       
125010                                     UT-PBEN-KOMMA7                       
125100           IF WS-EN-BEARTEXT = SPACE                                      
125200             MOVE +2 TO TO-IX                                             
125300             MOVE ISO-TO-VERSAL (TO-IX) TO UT-IDSPRAK-ISO-TO              
125400             MOVE WS-SV-BEARTEXT TO UT-PBEN-BEARTEXT-SV                   
125410             Move WS-IDFKNGRP    To UT-PBEN-IDFKNGRP                      
125500             MOVE JA TO SKRIV-KOMMANDO (TO-IX)                            
125600                                                                          
125700             WRITE TOENB-POST FROM UT-PBEN-AREA                           
125800             DISPLAY UT-PBEN-AREA                                         
125900             MOVE UT-IDPTYP TO POSTSUM-TRANSTYP                           
126000             MOVE 'W152TOEN' TO POSTSUM-FDNAMN                            
126100             MOVE 'W152TOEN' TO POSTSUM-DDNAMN2                           
126200             CALL POSTSUM USING POSTSUM-PARM                              
126300             SET BENAMNING-BESTALLD TO TRUE                               
126400           ELSE                                                           
126500             IF WS-SV-BEARTEXT = SPACE                                    
126600               MOVE +1 TO TO-IX                                           
126700               MOVE ISO-TO-VERSAL (TO-IX) TO UT-IDSPRAK-ISO-TO            
126800               MOVE WS-EN-BEARTEXT TO UT-PBEN-BEARTEXT-EN                 
126810               Move WS-IDFKNGRP  To UT-PBEN-IDFKNGRP                      
126900               MOVE JA TO SKRIV-KOMMANDO (TO-IX)                          
127000                                                                          
127100               WRITE TOSVB-POST FROM UT-PBEN-AREA                         
127200               DISPLAY UT-PBEN-AREA                                       
127300               MOVE UT-IDPTYP TO POSTSUM-TRANSTYP                         
127400               MOVE 'W152TOSV' TO POSTSUM-FDNAMN                          
127500               MOVE 'W152TOSV' TO POSTSUM-DDNAMN2                         
127600               CALL POSTSUM USING POSTSUM-PARM                            
127700               SET BENAMNING-BESTALLD TO TRUE                             
127800             ELSE                                                         
127900               MOVE +3 TO TO-IX                                           
128000*              --- Både SV och EN finns !                                 
128100             END-IF                                                       
128200           END-IF                                                         
128300         END-IF                                                           
128400                                                                          
128500         IF TO-IX > ZERO                                                  
128600*          --- Kolla nu de övriga språken,                                
128700*          --- Dock måste Engelska finnas isåfall.                        
128800           IF WS-EN-BEARTEXT = SPACE                                      
128900              CONTINUE                                                    
129000           ELSE                                                           
129100             MOVE +3 TO TO-IX                                             
129200             PERFORM UNTIL TO-IX > TO-IX-MAX                              
129310*               --- Översättning beställs för samtliga språk              
129320                                                                          
130200                MOVE ISO-TO-VERSAL (TO-IX) TO UT-IDSPRAK-ISO-TO           
130300                MOVE WS-EN-BEARTEXT        TO UT-PBEN-BEARTEXT-EN         
130400                MOVE TAB                   TO UT-PBEN-KOMMA5              
130500                MOVE WS-SV-BEARTEXT        TO UT-PBEN-BEARTEXT-SV         
130600                MOVE TAB                   TO UT-PBEN-KOMMA6              
130710                Move WS-IDFKNGRP           TO UT-PBEN-IDFKNGRP            
130711                MOVE TAB                   TO UT-PBEN-KOMMA7              
130720                MOVE WS-TEARTNOT           TO UT-PBEN-TENOTE              
130800                                                                          
130900                MOVE JA TO SKRIV-KOMMANDO(TO-IX)                          
131000                PERFORM S11-SKRIV-UTFILPOST                               
131100                SET BENAMNING-BESTALLD TO TRUE                            
131200                                                                          
131300                ADD +1 TO TO-IX                                           
131400             END-PERFORM                                                  
131500           END-IF                                                         
131600         END-IF                                                           
131700*        --- Hämta nästa beställning                                      
131800         PERFORM IMS-GNP-1219-1221                                        
131900       END-PERFORM                                                        
132000     END-IF                                                               
132100     .                                                                    
132200                                                                          
132210                                                                          
132300 D-SKRIV-KOMMANDOFIL    SECTION.                                          
132310     MOVE 'D-SKRIV-KOM-FIL ' TO CURRENT-SECTION                           
132400                                                                          
132410     IF IMSID4 = 'IMG0'                                                   
132411       MOVE COM-CBG-PROD-DIR TO COM-CBG-DIR                               
132420     ELSE                                                                 
132421       MOVE COM-CBG-TEST-DIR TO COM-CBG-DIR                               
132430     END-IF                                                               
132440                                                                          
132500     MOVE +1 TO TO-IX                                                     
132600     PERFORM UNTIL TO-IX > TO-IX-MAX                                      
132700         IF SKRIV-KOMMANDO(TO-IX) = JA                                    
132800           STRING 'PUT'             DELIMITED BY SIZE                     
132900                  ' '''             DELIMITED BY SIZE                     
133000                  COM-INDUTJ        DELIMITED BY SPACE                    
133100                  '.W152TO'         DELIMITED BY SIZE                     
133200                  ISO-TO-VERSAL(TO-IX) DELIMITED BY SIZE                  
133300                  '(+0)'' '         DELIMITED BY SIZE                     
133400                  COM-CBG-DIR       DELIMITED BY SIZE                     
133500                  'P-'              DELIMITED BY SIZE                     
133600                  COM-DAGID         DELIMITED BY SIZE                     
133700                  '.'               DELIMITED BY SIZE                     
133800                  ISO-TO-GEMEN(TO-IX) DELIMITED BY SIZE                   
133900                                                                          
134000             INTO COMMAND-AREA                                            
134100                                                                          
134200             ON OVERFLOW                                                  
134300                STRING 'KOMMANDOPOST KUNDE EJ SKAPAS FöR '                
134400                                          DELIMITED BY SIZE               
134500                COM-INDUTJ                DELIMITED BY SPACE              
134600                '.W152TO'                 DELIMITED BY SIZE               
134700                ISO-TO-VERSAL(TO-IX)   DELIMITED BY SIZE                  
134800                '(+0)'                    DELIMITED BY SIZE               
134900                INTO FELTEXT-STR                                          
135000                                                                          
135100                DISPLAY FELTEXT-STR                                       
135200                MOVE +4 TO PGM-RETURKOD                                   
135300             NOT ON OVERFLOW                                              
135301                                                                          
135400                WRITE KOMMANDO-POST FROM COMMAND-AREA                     
135500                MOVE 'COM'      TO POSTSUM-TRANSTYP                       
135600                MOVE 'W1522K'  TO POSTSUM-FDNAMN                          
135700                MOVE 'W1522K'  TO POSTSUM-DDNAMN2                         
135800                CALL POSTSUM USING POSTSUM-PARM                           
135820                                                                          
135900           END-STRING                                                     
136000         END-IF                                                           
136100         ADD +1 TO TO-IX                                                  
136200     END-PERFORM                                                          
136300     .                                                                    
136400                                                                          
136410                                                                          
136500 E-RENSA-WL121921  SECTION.                                               
136510     MOVE 'E-RENSA-WL121921' TO CURRENT-SECTION                           
136600                                                                          
136800*    --- Alla benämningar är nu beställda                                 
136900*    --- Rensa ALLA segment WL121921 genom att ta bort                    
137000*    --- föräldrasegmentet och inserta ett nytt.                          
137100     PERFORM IMS-GHU-1219-1220                                            
137200     PERFORM IMS-DLET-1219                                                
137300*    --- Nolla ordernumret för benämningar.                               
137400     MOVE ZERO TO 1219-1220-IDORDER-VCTS-BEN                              
137500     PERFORM IMS-ISRT-1219-1220                                           
137700     .                                                                    
137800                                                                          
137810                                                                          
137900 S11-SKRIV-UTFILPOST  SECTION.                                            
137910     MOVE 'S11-SKRIV-UTFIL ' TO CURRENT-SECTION                           
138000     SKIP2                                                                
138400*    --- SV och EN ben förbereds och SKRIVS i C-  (TO-IX +1  +2)          
138500*    --- Övriga benämn.-språk förbereds i C-      (TO-IX   > +2)          
138600                                                                          
138700*    --- DISPLAY:er hamnar på fil W15221, för logg-ändamål                
138800     EVALUATE TO-IX                                                       
138900       WHEN +3                                                            
139000*          --- Amerikanska                                                
139100           EVALUATE WS-KDLEX                                              
139200           WHEN 'B'                                                       
139300                 WRITE TOUSB-POST FROM UT-PBEN-AREA                       
139400*                DISPLAY UT-PBEN-AREA                                     
139500              WHEN OTHER CONTINUE                                         
139600           END-EVALUATE                                                   
139700           MOVE 'W152TOUS' TO POSTSUM-FDNAMN                              
139800           MOVE 'W152TOUS' TO POSTSUM-DDNAMN2                             
139900           CALL POSTSUM USING POSTSUM-PARM                                
140000       WHEN +4                                                            
140100*          --- Tyska                                                      
140200          EVALUATE WS-KDLEX                                               
140300              WHEN 'B'                                                    
140400                 WRITE TODEB-POST FROM UT-PBEN-AREA                       
140500*                DISPLAY UT-PBEN-AREA                                     
140600              WHEN OTHER CONTINUE                                         
140700           END-EVALUATE                                                   
140800           MOVE 'W152TODE' TO POSTSUM-FDNAMN                              
140900           MOVE 'W152TODE' TO POSTSUM-DDNAMN2                             
141000           CALL POSTSUM USING POSTSUM-PARM                                
141100       WHEN +5                                                            
141200*          --- Franska                                                    
141300           EVALUATE WS-KDLEX                                              
141400              WHEN 'B'                                                    
141500                 WRITE TOFRB-POST FROM UT-PBEN-AREA                       
141600*                DISPLAY UT-PBEN-AREA                                     
141700              WHEN OTHER CONTINUE                                         
141800           END-EVALUATE                                                   
141900           MOVE 'W152TOFR' TO POSTSUM-FDNAMN                              
142000           MOVE 'W152TOFR' TO POSTSUM-DDNAMN2                             
142100           CALL POSTSUM USING POSTSUM-PARM                                
142200       WHEN +6                                                            
142300*          --- Spanska                                                    
142400           EVALUATE WS-KDLEX                                              
142500              WHEN 'B'                                                    
142600                 WRITE TOESB-POST FROM UT-PBEN-AREA                       
142700*                DISPLAY UT-PBEN-AREA                                     
142800              WHEN OTHER CONTINUE                                         
142900           END-EVALUATE                                                   
143000           MOVE 'W152TOES' TO POSTSUM-FDNAMN                              
143100           MOVE 'W152TOES' TO POSTSUM-DDNAMN2                             
143200           CALL POSTSUM USING POSTSUM-PARM                                
143300       WHEN +7                                                            
143400*          --- Portugisiska                                               
143500           EVALUATE WS-KDLEX                                              
143600              WHEN 'B'                                                    
143700                 WRITE TOPTB-POST FROM UT-PBEN-AREA                       
143800*                DISPLAY UT-PBEN-AREA                                     
143900              WHEN OTHER CONTINUE                                         
144000           END-EVALUATE                                                   
144100           MOVE 'W152TOPT' TO POSTSUM-FDNAMN                              
144200           MOVE 'W152TOPT' TO POSTSUM-DDNAMN2                             
144300           CALL POSTSUM USING POSTSUM-PARM                                
144400       WHEN +8                                                            
144500*          --- Nederländska                                               
144600           EVALUATE WS-KDLEX                                              
144700              WHEN 'B'                                                    
144800                 WRITE TONLB-POST FROM UT-PBEN-AREA                       
144900*                DISPLAY UT-PBEN-AREA                                     
145000              WHEN OTHER CONTINUE                                         
145100           END-EVALUATE                                                   
145200           MOVE 'W152TONL' TO POSTSUM-FDNAMN                              
145300           MOVE 'W152TONL' TO POSTSUM-DDNAMN2                             
145400           CALL POSTSUM USING POSTSUM-PARM                                
145500       WHEN +9                                                            
145600*          --- Italienska                                                 
145700           EVALUATE WS-KDLEX                                              
145800              WHEN 'B'                                                    
145900                 WRITE TOITB-POST FROM UT-PBEN-AREA                       
146000*                DISPLAY UT-PBEN-AREA                                     
146100              WHEN OTHER CONTINUE                                         
146200           END-EVALUATE                                                   
146300           MOVE 'W152TOIT' TO POSTSUM-FDNAMN                              
146400           MOVE 'W152TOIT' TO POSTSUM-DDNAMN2                             
146500           CALL POSTSUM USING POSTSUM-PARM                                
146600       WHEN +10                                                           
146700*          --- Finska                                                     
146800           EVALUATE WS-KDLEX                                              
146900              WHEN 'B'                                                    
147000                 WRITE TOFIB-POST FROM UT-PBEN-AREA                       
147100*                DISPLAY UT-PBEN-AREA                                     
147200              WHEN OTHER CONTINUE                                         
147300           END-EVALUATE                                                   
147400           MOVE 'W152TOFI' TO POSTSUM-FDNAMN                              
147500           MOVE 'W152TOFI' TO POSTSUM-DDNAMN2                             
147600           CALL POSTSUM USING POSTSUM-PARM                                
147700       WHEN +11                                                           
147800*          --- Japanska                                                   
147900           EVALUATE WS-KDLEX                                              
148000              WHEN 'B'                                                    
148100                 WRITE TOJAB-POST FROM UT-PBEN-AREA                       
148200*                DISPLAY UT-PBEN-AREA                                     
148300              WHEN OTHER CONTINUE                                         
148400           END-EVALUATE                                                   
148500           MOVE 'W152TOJA' TO POSTSUM-FDNAMN                              
148600           MOVE 'W152TOJA' TO POSTSUM-DDNAMN2                             
148700           CALL POSTSUM USING POSTSUM-PARM                                
148800       WHEN +12                                                           
148900*          --- Koreanska                                                  
149000           EVALUATE WS-KDLEX                                              
149100              WHEN 'B'                                                    
149200                 WRITE TOKOB-POST FROM UT-PBEN-AREA                       
149300*                DISPLAY UT-PBEN-AREA                                     
149400              WHEN OTHER CONTINUE                                         
149500           END-EVALUATE                                                   
149600           MOVE 'W152TOKO' TO POSTSUM-FDNAMN                              
149700           MOVE 'W152TOKO' TO POSTSUM-DDNAMN2                             
149800           CALL POSTSUM USING POSTSUM-PARM                                
149900       WHEN +13                                                           
150000*          --- Kinesiska, traditionell                                    
150100           EVALUATE WS-KDLEX                                              
150200              WHEN 'B'                                                    
150300                 WRITE TOZHB-POST FROM UT-PBEN-AREA                       
150400*                DISPLAY UT-PBEN-AREA                                     
150500              WHEN OTHER CONTINUE                                         
150600           END-EVALUATE                                                   
150700           MOVE 'W152TOZH' TO POSTSUM-FDNAMN                              
150800           MOVE 'W152TOZH' TO POSTSUM-DDNAMN2                             
150900           CALL POSTSUM USING POSTSUM-PARM                                
151000       WHEN +14                                                           
151100*          --- Ryska                                                      
151200           EVALUATE WS-KDLEX                                              
151300              WHEN 'B'                                                    
151400                 WRITE TORUB-POST FROM UT-PBEN-AREA                       
151500*                DISPLAY UT-PBEN-AREA                                     
151600              WHEN OTHER CONTINUE                                         
151700           END-EVALUATE                                                   
151800           MOVE 'W152TORU' TO POSTSUM-FDNAMN                              
151900           MOVE 'W152TORU' TO POSTSUM-DDNAMN2                             
152000           CALL POSTSUM USING POSTSUM-PARM                                
152100       WHEN +15                                                           
152200*          --- Thai                                                       
152300           EVALUATE WS-KDLEX                                              
152400              WHEN 'B'                                                    
152500                 WRITE TOTHB-POST FROM UT-PBEN-AREA                       
152600*                DISPLAY UT-PBEN-AREA                                     
152700              WHEN OTHER CONTINUE                                         
152800           END-EVALUATE                                                   
152900           MOVE 'W152TOTH' TO POSTSUM-FDNAMN                              
153000           MOVE 'W152TOTH' TO POSTSUM-DDNAMN2                             
153100           CALL POSTSUM USING POSTSUM-PARM                                
153200       WHEN +16                                                           
153300*          --- Turkiska                                                   
153400           EVALUATE WS-KDLEX                                              
153500              WHEN 'B'                                                    
153600                 WRITE TOTRB-POST FROM UT-PBEN-AREA                       
153700*                DISPLAY UT-PBEN-AREA                                     
153800              WHEN OTHER CONTINUE                                         
153900           END-EVALUATE                                                   
154000           MOVE 'W152TOTR' TO POSTSUM-FDNAMN                              
154100           MOVE 'W152TOTR' TO POSTSUM-DDNAMN2                             
154200           CALL POSTSUM USING POSTSUM-PARM                                
154300       WHEN +17                                                           
154400*          --- Simplified Chinese                                         
154500           EVALUATE WS-KDLEX                                              
154600              WHEN 'B'                                                    
154700                 WRITE TOCNB-POST FROM UT-PBEN-AREA                       
154800*                DISPLAY UT-PBEN-AREA                                     
154900              WHEN OTHER CONTINUE                                         
155000           END-EVALUATE                                                   
155100           MOVE 'W152TOCN' TO POSTSUM-FDNAMN                              
155200           MOVE 'W152TOCN' TO POSTSUM-DDNAMN2                             
155300           CALL POSTSUM USING POSTSUM-PARM                                
155310       WHEN +18                                                           
155320*          --- Polish - POLAND                                   0        
155330           EVALUATE WS-KDLEX                                              
155340              WHEN 'B'                                                    
155350                 WRITE TOPLB-POST FROM UT-PBEN-AREA                       
155360                 DISPLAY UT-PBEN-AREA                                     
155370              WHEN OTHER CONTINUE                                         
155380           END-EVALUATE                                                   
155390           MOVE 'W152TOPL' TO POSTSUM-FDNAMN                              
155391           MOVE 'W152TOPL' TO POSTSUM-DDNAMN2                             
155392           CALL POSTSUM USING POSTSUM-PARM                                
155400                                                                          
155500       WHEN OTHER    CONTINUE                                             
155600                                                                          
155700     END-EVALUATE                                                         
155800     .                                                                    
155900                                                                          
155910                                                                          
156000 Z-FINIT SECTION.                                                         
156010     MOVE 'Z-FINIT         ' TO CURRENT-SECTION                           
156100                                                                          
156200     CLOSE       W1522K                                                   
156300                 W152TOSV                                                 
156400                 W152TOEN                                                 
156500                 W152TOUS                                                 
156600                 W152TODE                                                 
156700                 W152TOFR                                                 
156800                 W152TOES                                                 
156900                 W152TOPT                                                 
157000                 W152TONL                                                 
157100                 W152TOIT                                                 
157200                 W152TOFI                                                 
157300                 W152TOJA                                                 
157400                 W152TOKO                                                 
157500                 W152TOZH                                                 
157600                 W152TORU                                                 
157700                 W152TOTH                                                 
157800                 W152TOTR                                                 
157900                 W152TOCN                                                 
157910                 W152TOPL                                                 
158000                                                                          
158100     MOVE 'S' TO POSTSUM-OPKOD                                            
158200     CALL POSTSUM USING POSTSUM-PARM                                      
158300     .                                                                    
158400                                                                          
158410                                                                          
158500* --- IMS SEKTIONER ---                                                   
158600                                                                          
161200 IMS-GHU-1219-1220 SECTION.                                               
161210     MOVE 'GHU-1219-1220   ' TO CURRENT-IMS-SECTION                       
161220                                                                          
161230     MOVE SPACE TO ALL-SSA                                                
161300     STRING 'WL121901(WDGXKEY  =' W-KY1219-X ')'                          
161400          DELIMITED BY SIZE INTO SSA1                                     
161500     STRING 'WL121911(KDSEGKEY =1)'                                       
161600          DELIMITED BY SIZE INTO SSA2                                     
161700     MOVE '  GE' TO GODK-STATUSKODER                                      
161800     CALL CBLTDLI USING GHU 1219-PCB DLI-IO-WL121911 SSA1 SSA2            
161900     MOVE 1219-STATUS-CODE TO STATUS-WS                                   
162000     PERFORM IMS-STATUSKONTROLL                                           
162100     .                                                                    
162200                                                                          
162300 IMS-GNP-1219-1221 SECTION.                                               
162310     MOVE 'GNP-1219-1221   ' TO CURRENT-IMS-SECTION                       
162320                                                                          
162330     MOVE SPACE TO ALL-SSA                                                
162400     STRING 'WL121921 '                                                   
162500          DELIMITED BY SIZE INTO SSA1                                     
162600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
162700     CALL CBLTDLI USING GNP 1219-PCB DLI-IO-WL121921 SSA1                 
162800     MOVE 1219-STATUS-CODE TO STATUS-WS                                   
162900     PERFORM IMS-STATUSKONTROLL                                           
163000     .                                                                    
163100                                                                          
163200 IMS-DLET-1219  SECTION.                                                  
163210     MOVE 'DLET-1219       ' TO CURRENT-IMS-SECTION                       
163220                                                                          
163230     MOVE SPACE TO ALL-SSA                                                
163300     MOVE '  ' TO GODK-STATUSKODER                                        
163400     CALL CBLTDLI USING DLET 1219-PCB DLI-IO-WL121911                     
163500     MOVE 1219-STATUS-CODE TO STATUS-WS                                   
163600     PERFORM IMS-STATUSKONTROLL                                           
163700     .                                                                    
163800                                                                          
163900 IMS-ISRT-1219-1220 SECTION.                                              
163910     MOVE 'ISRT-1219-1220  ' TO CURRENT-IMS-SECTION                       
163920                                                                          
163930     MOVE SPACE TO ALL-SSA                                                
164000     STRING 'WL121901(WDGXKEY  =' W-KY1219-X ')'                          
164100          DELIMITED BY SIZE INTO SSA1                                     
164200     MOVE   'WL121911  '      TO SSA2                                     
164300     MOVE '  ' TO GODK-STATUSKODER                                        
164400     CALL CBLTDLI USING ISRT 1219-PCB DLI-IO-WL121911 SSA1 SSA2           
164500     MOVE 1219-STATUS-CODE TO STATUS-WS                                   
164600     PERFORM IMS-STATUSKONTROLL                                           
164700     .                                                                    
164800                                                                          
170600 IMS-GU-BENA01    SECTION.                                                
170610     MOVE 'GU-BENA01       ' TO CURRENT-IMS-SECTION                       
170620                                                                          
170630     MOVE SPACE TO ALL-SSA                                                
170700     STRING 'WLBENA01(IDBENNR  =' W-IDBENNR-X ')'                         
170800          DELIMITED BY SIZE INTO SSA1                                     
170900     MOVE '  GE' TO GODK-STATUSKODER                                      
171000     CALL CBLTDLI USING GU BENA-PCB DLI-IO-WLBENA01 SSA1                  
171100     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
171200     PERFORM IMS-STATUSKONTROLL                                           
171300     .                                                                    
171400                                                                          
171500 IMS-GU-BENA11           SECTION.                                         
171510     MOVE 'GU-BENA11       ' TO CURRENT-IMS-SECTION                       
171520                                                                          
171530     MOVE SPACE TO ALL-SSA                                                
171600     STRING 'WLBENA01(IDBENNR  =' W-IDBENNR-X ')'                         
171700          DELIMITED BY SIZE INTO SSA1                                     
171800     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-B ')'                         
171900          DELIMITED BY SIZE INTO SSA2                                     
172000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
172100     CALL CBLTDLI USING GU BENA-PCB DLI-IO-WLBENA11 SSA1 SSA2             
172200     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
172300     PERFORM IMS-STATUSKONTROLL                                           
172400     .                                                                    
172500                                                                          
172510 IMS-GU-BENA12-FIRST     SECTION.                                         
172511     MOVE 'GU-BENA12-FIRST ' TO CURRENT-IMS-SECTION                       
172512                                                                          
172513     MOVE SPACE TO ALL-SSA                                                
172520     STRING 'WLBENA01*P(IDBENNR  =' W-IDBENNR-X ')'                       
172530          DELIMITED BY SIZE INTO SSA1                                     
172540     Move   'WLBENA12*F '     TO SSA2                                     
172560     MOVE '  GEGB' TO GODK-STATUSKODER                                    
172570     CALL CBLTDLI USING GU BENA-PCB DLI-IO-WLBENA12 SSA1 SSA2             
172580     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
172590     PERFORM IMS-STATUSKONTROLL                                           
172591     .                                                                    
172592                                                                          
172593 IMS-GNP-BENA12          SECTION.                                         
172594     MOVE 'GNP-BENA12      ' TO CURRENT-IMS-SECTION                       
172595                                                                          
172596     MOVE SPACE TO ALL-SSA                                                
172597     Move   'WLBENA12   '     TO SSA1                                     
172598     MOVE '  GEGB' TO GODK-STATUSKODER                                    
172599     CALL CBLTDLI USING GNP BENA-PCB DLI-IO-WLBENA12 SSA1                 
172600     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
172601     PERFORM IMS-STATUSKONTROLL                                           
172602     .                                                                    
172603                                                                          
172604 IMS-GU-WDK601            SECTION.                                        
172605     MOVE 'GU-WDK601       ' TO CURRENT-IMS-SECTION                       
172606                                                                          
172607     MOVE SPACE TO ALL-SSA                                                
172608     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
172609          DELIMITED BY SIZE INTO SSA1                                     
172610     MOVE '  GE' TO GODK-STATUSKODER                                      
172611     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
172612     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
172613     PERFORM IMS-STATUSKONTROLL                                           
172614     .                                                                    
172615                                                                          
172616 IMS-GNP-WDK611           SECTION.                                        
172617     MOVE 'GNP-WDK611      ' TO CURRENT-IMS-SECTION                       
172618                                                                          
172619     MOVE SPACE TO ALL-SSA                                                
172620     MOVE   'WDK611    '      TO SSA1                                     
172621     MOVE '  GEGB' TO GODK-STATUSKODER                                    
172622     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
172623     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
172624     PERFORM IMS-STATUSKONTROLL                                           
172625     .                                                                    
172626                                                                          
172640 IMS-STATUSKONTROLL SECTION.                                              
172700     SKIP2                                                                
172800     SET STATUS-IX TO 1                                                   
172900     SEARCH GODK-STATUS                                                   
173000       AT END                                                             
173100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
173200           DELIMITED BY SIZE INTO FELTEXT                                 
173300         DISPLAY FELTEXT                                                  
173400         CALL FELLOG                                                      
173500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
173600         CONTINUE                                                         
173700     END-SEARCH                                                           
173800     .                                                                    
