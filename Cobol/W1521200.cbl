000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W1521200.                                                
000400 AUTHOR.         CONNY EGHOLT.                                            
000500 DATE-WRITTEN.   97/06/04.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*    FUNKTION:                                                            
001000*                                                                         
001100*        UPPDATERAR WDN2, WDN3, WDN4, WDD3 MED NYÖVERSATTA                
001200*        RUBRIKER, FOTNOTER, TILLÄGGSTEXTER OCH BENÄMNINGAR.              
001300*        UPPDATERAR HÄNDELSEBAS WDR2 MED FLAGGA FÖR ÖVERSATT SPRÅK        
001400*        UPPDATERAR WDN5-AVSNITT MED FLAVSUST=JA, DÄR TEXTEN              
001500*        FÖREKOMMER FÖR ATT AVSNITTET SKALL KOMMA MED TILL VADIS          
001600*                                                                         
001700*     -- UNDANTAG:                                                        
001800*        FÖR RUB, FOT OCH TTEXT SOM UPPDATERAS PÅ ENGELSKA (GB),          
001900*        GÄLLER ATT MAN INTE SKALL SLÄCKA HÄNDELSEN PÅ WL1206             
002000*        DÅ DET INTE FINNS FLER BESTÄLLNINGAR KVAR.                       
002100*        NÄR ENGELSKA LÄGGS IN DENNA VÄG, BETYDER DET ATT DET             
002200*        ENDAST HAR FUNNITS SVENSKA NÄR BESTÄLLNINGEN GJORDES.            
002300*        EFTERSOM ÖVERSÄTTNINGSBYRÅN BEHÖVER ENGELSKAN FÖR ÖVRIGA         
002400*        SPRÅK, KOMMER DE ATT BESTÄLLAS EFTER ATT ENGELSKAN               
002500*        BLIVIT UPPDATERAD.  ("TVÅ-STEGS-RAKET")                          
002600*                                                                         
002700*     -- MEMOFIL SKAPAS TILL PARTS KATALOGREDAKTION NÄR EN                
002800*        ENGELSK ÖVERSÄTTNING LAGTS UPP.                                  
002900*        SKAPAR EN RAD MED INFO OM VARJE TEXT.                            
003000* 990423                                                                  
003100*     -- epostfil skapas till översättningsbyrån när en text              
003200*        är för lång och blivit uppdaterad med trunkering.                
003300* 021120 ÄT029                                                            
003400*     -- IN-area förlängd p.g.a. ny CTX W152PBEN med nya BEARTEXT         
003500* 021120 Projekt NEVIS                                                    
003600*     -- WDD301 skall uppdateras med FLAENDR=JA, då något segment         
003700*        blivit ändrat.                                                   
003800* 2005-10-11                                                              
003900*        Kolla att minst en av tillhörande artiklar (WDD312) har          
004000*        KDPRODSL = 11-29 innan WDD3-BEN-FLAENDR sätts till 'J'           
004100* 051027 eTracker 2571884                                                 
004200*     -- WDD311 skall uppdateras med Engelskan i övriga Europeiska        
004300*     -- språk då Engelskan uppdateras och annat språk ej är              
004400*     -- flaggat som översatt.                                            
004500*     -- samt att man låter FLOVERSATT vidare vara 'N' för att            
004600*     -- möjliggöra uppdatering på rätt språk från W152D2-rutinen.        
004700*     -- Detta är exakt samma funktion som i W1012200.                    
004800* 070821 eTracker 5506833                                                 
004900*     -- Benämningar som kommer från CBG skall behandlas på samma         
005000*     -- sätt som benämningar från Techla. 'S ' och 'GB' versala.         
005100*     -- Efter ACPT test upptäcks att bakvända GB, " BG" inte upp-        
005200*     -- dateras när ny GB kommer från CBG. Kod från W1012200 kop-        
005300*     -- ieras in för denna funktion i EB-UPPDAT-GB...                    
005400*                                                                         
005500*                                                                         
005600*        PROGRAMMET UPPDATERAR WDN2                                       
005700*        PROGRAMMET UPPDATERAR WDN3                                       
005800*        PROGRAMMET UPPDATERAR WDN4                                       
005900*        PROGRAMMET UPPDATERAR WDD3                                       
006000*        PROGRAMMET UPPDATERAR WDR2 (WDGX1206 ) HTYP 1205                 
006100*        PROGRAMMET UPPDATERAR WDN5                                       
006200*        PROGRAMMET LÄSER      WDN5D  (SEQ IDTTEXTNR PÅ RAD)              
006300*        PROGRAMMET LÄSER      WDN5E  (SEQ IDRUBNR   PÅ RAD)              
006400*        PROGRAMMET LÄSER      WDN5F  (SEQ IDFOTNR   PÅ RAD)              
006500*        PROGRAMMET LÄSER      WDK6                                       
006600*                                                                         
006700*    ABENDKODER:                                                          
006800*        U0016 -  . . . .                                                 
006900*        U1000 -  . . . .                                                 
007000*                                                                         
007100                                                                          
007200     SKIP3                                                                
007300 ENVIRONMENT DIVISION.                                                    
007400     SKIP2                                                                
007500 INPUT-OUTPUT SECTION.                                                    
007600                                                                          
007700 FILE-CONTROL.                                                            
007800     SKIP2                                                                
007900*          --- UPPDATERINGSPOSTER                                         
008000     SELECT W15210                     ASSIGN TO W15212D1.                
008100*          --- TOTALFEL-POSTER                                            
008200     SELECT W15212                     ASSIGN TO W15212D2.                
008300*          --- MEMO-POSTER                                                
008400     SELECT W15214                     ASSIGN TO W15212D3.                
008500*          --- FEL-POSTER TILL ÖVERSÄTTARE                                
008600     SELECT W15215                     ASSIGN TO W15212D4.                
008700*                                                                         
008800     EJECT                                                                
008900 DATA DIVISION.                                                           
009000     SKIP3                                                                
009100 FILE SECTION.                                                            
009200     SKIP3                                                                
009300 FD  W15210                                                               
009400     RECORDING       V                                                    
009500     BLOCK CONTAINS  0.                                                   
009600*01  -COPY W152PRUB      -L.                                              
009700*01  -COPY W152PFOT      -L.                                              
009800*01  -COPY W152PTIL      -L.                                              
009900*01  -COPY W152PBEN      -L.                                              
010000     EJECT                                                                
010100 FD  W15212                                                               
010200     RECORDING    F                                                       
010300     BLOCK CONTAINS 0.                                                    
010400     SKIP2                                                                
010500*01  POST  -COPY W15212   -L  -PRE FEL-.                                  
010600                                                                          
010700     EJECT                                                                
010800*                                                                         
010900 FD  W15214                                                               
011000     LABEL RECORD STANDARD                                                
011100     BLOCK CONTAINS 0.                                                    
011200     SKIP2                                                                
011300 01  W15214-MEMO-POST          PIC X(80).                                 
011400     EJECT                                                                
011500                                                                          
011600 FD  W15215                                                               
011700     LABEL RECORD STANDARD                                                
011800     BLOCK CONTAINS  0.                                                   
011900 01  W15215-POST               PIC X(80).                                 
012000                                                                          
012100     EJECT                                                                
012200 WORKING-STORAGE SECTION.                                                 
012300     SKIP2                                                                
012400                                                                          
012500*    -- CHECKED BY WY2000                                                 
012600 77  IDPGM                       PIC X(8)    VALUE 'W1521200'.            
012700 01  FELTEXT.                                                             
012800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
012900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
013000     SKIP3                                                                
013100 01  FILLER                      PIC X(08)   VALUE 'INDEX'.               
013200 01  INDEXES.                                                             
013300     03 SPR-IX                   PIC S9(9)   VALUE +0 COMP SYNC.          
013400     03 SEG-IX                   PIC S9(9)   VALUE +0 COMP SYNC.          
013500     03 SPRAAK-IX                PIC S9(9)   VALUE +0 COMP SYNC.          
013600     03 MAX-SPRAAK-PLUS-1        PIC S9(9)   VALUE +27 COMP SYNC.         
013700                                                                          
013800 01  FILLER                      PIC X(16)   VALUE 'CHKP-AREA'.           
013900 01  CHKP-VAR.                                                            
014000     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
014100     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
014200     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
014300     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
014400     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
014500     03 CHKP-MAX                 PIC S9(3)   VALUE +200 COMP-3.           
014600                                                                          
014700 01  FILLER                      PIC X(16)   VALUE 'WORK-FALT'.           
014800 77  JA                          PIC X       VALUE 'J'.                   
014900 77  NEJ                         PIC X       VALUE 'N'.                   
015000 77  OCH                         PIC X       VALUE '&'.                   
015100 77  ELLER                       PIC X       VALUE '!'.                   
015200 77  WS-IDSPRAK                  PIC X(2)    VALUE SPACE.                 
015300 77  WS-ACTION-KDLEX             PIC X       VALUE SPACE.                 
015400 77  WS-NEVIS-ART                PIC X       VALUE SPACE.                 
015500 77  WS-ACTION-IDLEXNR           PIC 9(7)    VALUE ZERO.                  
015600 77  SPAR-KDLEX                  PIC X(1)    VALUE SPACE.                 
015700 77  SPAR-IDBENNR                PIC 9(7)    VALUE ZERO.                  
015800 77  TA-BORT-TRANS               PIC X       VALUE 'N'.                   
015900 77  WS-GEMEN          PIC X(26)                                          
016000                       VALUE 'abcdefghijklmnopqrstuvwxyz'.                
016100 77  WS-VERSAL         PIC X(26)                                          
016200                       VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.                
016300 77  WS-GEMEN-SPECIAL  PIC X(25)                                          
016400                       VALUE 'åâáàäçêéèëîíìïñôóòöõüûúùý'.                 
016500 77  WS-VERSAL-SPECIAL PIC X(25)                                          
016600                       VALUE 'ÅÂÁÀÄÇÊÉÈËÎÍÌÏÑÔÓÒÖÕÜÛÚÙÝ'.                 
016700                                                                          
016800 77  SW-WDD3-UPDATE              PIC X       VALUE 'N'.                   
016900     88 WDD3-UNTOUCHED                       VALUE 'N'.                   
017000     88 WDD3-UPDATED                         VALUE 'J'.                   
017100                                                                          
017200 77  WS-WDD311-LANG              PIC X(3).                                
017300     88 WDD311-WESTERN-LANG                  VALUE 'D  '                  
017400                                                   'E  '                  
017500                                                   'F  '                  
017600                                                   'I  '                  
017700                                                   'MAL'                  
017800                                                   'NL '                  
017900                                                   'P  '                  
018000                                                   'S  '                  
018100                                                   'SF '                  
018200                                                   'USA'                  
018300                                                   'DK '                  
018400                                                   'PL '.                 
018500                                                                          
018600 01  FILLER                      PIC X(16)   VALUE 'SPRAK-TABELL'.        
018700 77  MAX-ANTAL-SPRAK             PIC S9(4)   VALUE +26 COMP.              
018800 01  SPRAK-AREA.                                                          
018900     03 ISO-TYSKA                PIC X(2) VALUE 'DE'.                     
019000     03 KATALOG-TYSKA            PIC X(3)       VALUE 'D  '.              
019100     03 ISO-SPANSKA              PIC X(2) VALUE 'ES'.                     
019200     03 KATALOG-SPANSKA          PIC X(3)       VALUE 'E  '.              
019300     03 ISO-FRANSKA              PIC X(2) VALUE 'FR'.                     
019400     03 KATALOG-FRANSKA          PIC X(3)       VALUE 'F  '.              
019500     03 ISO-ENGELSKA             PIC X(2) VALUE 'EN'.                     
019600     03 KATALOG-ENGELSKA         PIC X(3)       VALUE 'GB '.              
019700     03 ISO-ITALIENSKA           PIC X(2) VALUE 'IT'.                     
019800     03 KATALOG-ITALIENSKA       PIC X(3)       VALUE 'I  '.              
019900     03 ISO-HOLLANDSKA           PIC X(2) VALUE 'NL'.                     
020000     03 KATALOG-HOLLANDSKA       PIC X(3)       VALUE 'NL '.              
020100     03 ISO-PORTUGISISKA         PIC X(2) VALUE 'PT'.                     
020200     03 KATALOG-PORTUGISISKA     PIC X(3)       VALUE 'P  '.              
020300     03 ISO-SVENSKA              PIC X(2) VALUE 'SV'.                     
020400     03 KATALOG-SVENSKA          PIC X(3)       VALUE 'S  '.              
020500     03 ISO-FINSKA               PIC X(2) VALUE 'FI'.                     
020600     03 KATALOG-FINSKA           PIC X(3)       VALUE 'SF '.              
020700     03 ISO-AMERIKANSKA          PIC X(2) VALUE 'US'.                     
020800     03 KATALOG-AMERIKANSKA      PIC X(3)       VALUE 'USA'.              
020900*    --- DE NYA SPRÅKEN                                                   
021000     03 ISO-JAPANSKA             PIC X(2) VALUE 'JA'.                     
021100     03 KATALOG-JAPANSKA         PIC X(3)       VALUE 'J  '.              
021200     03 ISO-KOREANSKA            PIC X(2) VALUE 'KO'.                     
021300     03 KATALOG-KOREANSKA        PIC X(3)       VALUE 'KOR'.              
021400     03 ISO-MALAYSISKA           PIC X(2) VALUE 'MS'.                     
021500     03 KATALOG-MALAYSISKA       PIC X(3)       VALUE 'MAL'.              
021600     03 ISO-TRAD-KINESISKA       PIC X(2) VALUE 'ZH'.                     
021700     03 KATALOG-TRAD-KINESISKA   PIC X(3)       VALUE 'RC '.              
021800     03 ISO-RYSKA                PIC X(2) VALUE 'RU'.                     
021900     03 KATALOG-RYSKA            PIC X(3)       VALUE 'RUS'.              
022000     03 ISO-THAILANDSKA          PIC X(2) VALUE 'TH'.                     
022100     03 KATALOG-THAILANDSKA      PIC X(3)       VALUE 'T  '.              
022200     03 ISO-TURKISKA             PIC X(2) VALUE 'TR'.                     
022300     03 KATALOG-TURKISKA         PIC X(3)       VALUE 'TR '.              
022400*    --- DE NYASTE SPRÅKEN  (2005)                                        
022500     03 ISO-TJECKISKA            PIC X(2) VALUE 'CS'.                     
022600     03 KATALOG-TJECKISKA        PIC X(3)       VALUE 'CZ '.              
022700     03 ISO-DANSKA               PIC X(2) VALUE 'DA'.                     
022800     03 KATALOG-DANSKA           PIC X(3)       VALUE 'DK '.              
022900     03 ISO-GREKISKA             PIC X(2) VALUE 'EL'.                     
023000     03 KATALOG-GREKISKA         PIC X(3)       VALUE 'GR '.              
023100     03 ISO-UNGERSKA             PIC X(2) VALUE 'HU'.                     
023200     03 KATALOG-UNGERSKA         PIC X(3)       VALUE 'H  '.              
023300     03 ISO-FARSI                PIC X(2) VALUE 'FA'.                     
023400     03 KATALOG-FARSI            PIC X(3)       VALUE 'IR '.              
023500     03 ISO-POLSKA               PIC X(2) VALUE 'PL'.                     
023600     03 KATALOG-POLSKA           PIC X(3)       VALUE 'PL '.              
023700     03 ISO-SIMP-KINESISKA       PIC X(2) VALUE 'CN'.                     
023800     03 KATALOG-SIMP-KINESISKA   PIC X(3)       VALUE 'RCN'.              
023900     03 ISO-SIMP-RUMANSKA        PIC X(2) VALUE 'RO'.                     
024000     03 KATALOG-SIMP-RUMANSKA    PIC X(3)       VALUE 'RO '.              
024100     03 ISO-SIMP-SERBISKA        PIC X(2) VALUE 'SR'.                     
024200     03 KATALOG-SIMP-SERBISKA    PIC X(3)       VALUE 'YU '.              
024300                                                                          
024400 01  SPRAK-TAB REDEFINES SPRAK-AREA.                                      
024500     03 FILLER    OCCURS 26.                                              
024600       05 TAB-IDSPRAK             PIC X(2).                               
024700       05 TAB-IDSKYLT             PIC X(3).                               
024800 01  FILLER                      PIC X(16) VALUE 'SWITCHAR'.              
024900     SKIP2                                                                
025000 77  W15210-EOF-SW               PIC X       VALUE 'N'.                   
025100     88  END-OF-W15210                       VALUE 'J'.                   
025200                                                                          
025300 77  INDATA-SW                   PIC X       VALUE 'J'.                   
025400     88  INDATA-OK                           VALUE 'J'.                   
025500     88  INDATA-FEL                          VALUE 'N'.                   
025600     EJECT                                                                
025700 01  FILLER                      PIC X(16) VALUE 'DAGENS-DATUM'.          
025800 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
025900 01  FILLER REDEFINES DAGENS-DATUM.                                       
026000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
026100     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
026200     03  DAGENS-DATUM-DAG        PIC 9(2).                                
026300     EJECT                                                                
026400*                                                                         
026500*01  -COPY WWPRODSL                                                       
026600     EJECT                                                                
026700 01  DYNAMISKA-SUBPROGRAM.                                                
026800*                                                                         
026900     03  WREVERSE                PIC X(8)    VALUE 'WREVERSE'.            
027000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
027100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
027200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
027300*                                                                         
027400*    --- PARAMETRAR TILL POSTSUM                                          
027500*                                                                         
027600*01  -COPY W0005   -PRE  POSTSUM-                                         
027700     EJECT                                                                
027800 01  IN-AREA-START               PIC X(16)   VALUE                        
027900                                             'IN-AREA-START'.             
028000 01  IN-AREA.                                                             
028100     03  IN-AREA-0.                                                       
028200         05 IN-IDPTYP.                                                    
028300           07 FILLER             PIC X        VALUE 'X'.                  
028400           07 IN-KDLEX           PIC X        VALUE 'Y'.                  
028500           07 FILLER             PIC X(3)     VALUE 'ZZ,'.                
028600         05  IN-DIFAELT          PIC 999      VALUE ZERO.                 
028700         05  FILLER              PIC X        VALUE ','.                  
028800         05  IN-IDBENNR          PIC 9(7)     VALUE 9999999.              
028900         05  FILLER              PIC X        VALUE ','.                  
029000         05  IN-IDSPRAK          PIC X(2)     VALUE 'XX'.                 
029100         05  FILLER              PIC X(101)   VALUE HIGH-VALUE.           
029200*    03  FILLER -COPY W152PBEN  -PRE IN-  -RED  IN-AREA-0                 
029300*                                                                         
029400     EJECT                                                                
029500 01  W15212-AREA-START               PIC X(24)   VALUE                    
029600                                         'W15212-AREA-START'.             
029700*01  AREA  -COPY W15212  -PRE W15212- .                                   
029800     EJECT                                                                
029900 01  SPAR-RUBRIKDATA-AREA.                                                
030000     03 FILLER OCCURS 3.                                                  
030100*       05 -COPY W152PRUB -PRE SPAR-                                      
030200     SKIP3                                                                
030300 01  SPAR-FOTNOTDATA-AREA.                                                
030400     03 FILLER OCCURS 3.                                                  
030500*       05 -COPY W152PFOT -PRE SPAR-                                      
030600                                                                          
030700     EJECT                                                                
030800*01   -COPY WREVAREA                                                      
030900     EJECT                                                                
031000                                                                          
031100 01  FILLER             PIC X(16)    VALUE 'W15214-AREA'.                 
031200 01  W15214-AREA.                                                         
031300     03  FILLER              PIC X(1)     VALUE SPACE.                    
031400     03  W15214-KDLEX        PIC X(1)     VALUE 'X'.                      
031500     03  FILLER              PIC X(1)     VALUE ' '.                      
031600     03  W15214-IDBENNR      PIC 9(7)     VALUE 9999999.                  
031700     03  FILLER              PIC X(1)     VALUE ' '.                      
031800     03  W15214-IDSKYLT      PIC X(3)     VALUE 'XXX'.                    
031900     03  FILLER              PIC X(1)     VALUE ' '.                      
032000     03  W15214-TEXT         PIC X(67)    VALUE SPACE.                    
032100     EJECT                                                                
032200                                                                          
032300                                                                          
032400 01  FILLER             PIC X(16)    VALUE 'W15215-AREA'.                 
032500 01  W15215-AREA.                                                         
032600     03  W15215-ID-DEL.                                                   
032700       05 W15215-IDPTYP      PIC X(4)     VALUE SPACE.                    
032800       05 FILLER             PIC X        VALUE SPACE.                    
032900       05 W15215-DIFAELT     PIC 9(3)     VALUE ZERO.                     
033000       05 FILLER             PIC X        VALUE SPACE.                    
033100       05 W15215-IDLEXNR     PIC 9(5)     VALUE ZERO.                     
033200       05 FILLER             PIC X        VALUE SPACE.                    
033300       05 W15215-IDSPRAK     PIC X(2)     VALUE SPACE.                    
033400       05 FILLER             PIC X        VALUE SPACE.                    
033500     03  W15215-TEXT         PIC X(60)    VALUE SPACE.                    
033600     EJECT                                                                
033700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
033800     SKIP3                                                                
033900 01  NYCKLAR-TILL-DLI.                                                    
034000                                                                          
034100*    RUBRIK                                                               
034200     03  W-IDRUBNR-X.                                                     
034300         05  W-IDRUBNR           PIC S9(5)   VALUE ZERO COMP-3.           
034400                                                                          
034500     03  W-WDN211KY-X.                                                    
034600         05  W-IDSKYLT-RUB       PIC X(3)    VALUE SPACE.                 
034700         05  W-IDSEGMNR-RUB      PIC S9      VALUE +0   COMP-3.           
034800                                                                          
034900*    FOTNOT                                                               
035000     03  W-IDFOTNR-X.                                                     
035100         05  W-IDFOTNR           PIC S9(5)   VALUE ZERO COMP-3.           
035200                                                                          
035300     03  W-KDFORDON-X.                                                    
035400         05  W-KDFORDON          PIC X(2)    VALUE SPACE.                 
035500                                                                          
035600     03  W-WDN321KY-X.                                                    
035700         05  W-IDSKYLT-FOT       PIC X(3)    VALUE SPACE.                 
035800         05  W-IDSEGMNR-FOT      PIC S9      VALUE +0   COMP-3.           
035900                                                                          
036000*    TILLÄGGSTEXT                                                         
036100     03  W-IDTTEXNR-X.                                                    
036200         05  W-IDTTEXNR          PIC S9(5)   VALUE ZERO COMP-3.           
036300                                                                          
036400                                                                          
036500*    BENÄMNING                                                            
036600     03  W-IDBENNR-X.                                                     
036700         05  W-IDBENNR           PIC S9(7)   VALUE ZERO COMP-3.           
036800                                                                          
036900     03  W-IDSKYLT-X.                                                     
037000         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
037100                                                                          
037200     03  W-IDARTNR-X.                                                     
037300         05  W-IDARTNR           PIC S9(9)  VALUE ZERO  COMP-3.           
037400                                                                          
037500*    ÖVERSÄTTNINGSBEVAKNING                                               
037600     03  W-WDGXKEY-X.                                                     
037700         05  FILLER              PIC X(4)    VALUE '1205'.                
037800         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
037900                                                                          
038000     03  W-WDGX1206-X.                                                    
038100         05  W-IDCATNR           PIC 9(5)    VALUE ZERO.                  
038200         05  W-KDLEX-X.                                                   
038300             07 W-KDLEX          PIC X       VALUE SPACE.                 
038400         05  W-IDLEXNR-X.                                                 
038500             07 W-IDLEXNR        PIC 9(7)    VALUE ZERO.                  
038600                                                                          
038700* WDN5 KATALOG-ROT                                                        
038800     03 W-WDN501KY-X.                                                     
038900       05 W-IDCATNR-N5-X.                                                 
039000         07 W-IDCATNR-N5         PIC 9(5)    VALUE ZERO.                  
039100       05 W-IDCATGRP-N5-X.                                                
039200         07 W-IDCATGRP-N5        PIC 9(2)    VALUE ZERO.                  
039300       05 W-IDCATAVS-N5-X.                                                
039400         07 W-IDCATAVS-N5        PIC 9(4)    VALUE ZERO.                  
039500                                                                          
039600* WDN5 KATALOG-RAD                                                        
039700     03 W-WDN512KY-X.                                                     
039800       05 W-IDCATRAD-N5-X.                                                
039900         07  W-IDCATRAD-N5         PIC 9(4)    VALUE ZERO.                
040000       05 W-KDCATPUB-N5-X.                                                
040100         07  W-KDCATPUB-N5         PIC X(6)    VALUE SPACE.               
040200                                                                          
040300* WDN5D  SEKUNDÄR-INDEX  TILLÄGGSTEXTFÖREKOMST                            
040400 01  W-WDN5D1KY-X.                                                        
040500     03  W-IDTTEXNR-D-X.                                                  
040600       05  W-IDTTEXNR-D         PIC S9(5) COMP-3 VALUE ZERO.              
040700     03  W-IDCATNR-D-X.                                                   
040800       05  W-IDCATNR-D          PIC 9(5)    VALUE ZERO.                   
040900     03  W-IDCATGRP-D-X.                                                  
041000       05  W-IDCATGRP-D         PIC 9(2)    VALUE ZERO.                   
041100     03  W-IDCATAVS-D-X.                                                  
041200       05  W-IDCATAVS-D         PIC 9(4)    VALUE ZERO.                   
041300     03  W-IDCATRAD-D-X.                                                  
041400       05  W-IDCATRAD-D         PIC 9(4)    VALUE ZERO.                   
041500     03  W-KDCATPUF-D-X.                                                  
041600       05  W-KDCATPUF-D         PIC X(6)    VALUE SPACE.                  
041700                                                                          
041800* WDN5E  SEKUNDÄR-INDEX  RUBRIKFÖREKOMST                                  
041900 01  W-WDN5E1KY-X.                                                        
042000     03  W-IDRUBNR-E-X.                                                   
042100       05  W-IDRUBNR-E          PIC S9(5) COMP-3 VALUE ZERO.              
042200     03  W-IDCATNR-E-X.                                                   
042300       05  W-IDCATNR-E          PIC 9(5)    VALUE ZERO.                   
042400     03  W-IDCATGRP-E-X.                                                  
042500       05  W-IDCATGRP-E         PIC 9(2)    VALUE ZERO.                   
042600     03  W-IDCATAVS-E-X.                                                  
042700       05  W-IDCATAVS-E         PIC 9(4)    VALUE ZERO.                   
042800     03  W-IDCATRAD-E-X.                                                  
042900       05  W-IDCATRAD-E         PIC 9(4)    VALUE ZERO.                   
043000     03  W-KDCATPUF-E-X.                                                  
043100       05  W-KDCATPUF-E         PIC X(6)    VALUE SPACE.                  
043200                                                                          
043300* WDN5F  SEKUNDÄR-INDEX  FOTNOTFÖREKOMST                                  
043400 01  W-WDN5F1KY-X.                                                        
043500     03  W-IDFOTNR-F-X.                                                   
043600         05  W-IDFOTNR-F        PIC S9(5) COMP-3 VALUE ZERO.              
043700     03 W-IDCATNR-F-X.                                                    
043800         05 W-IDCATNR-F         PIC 9(5)    VALUE ZERO.                   
043900     03 W-IDCATGRP-F-X.                                                   
044000         05 W-IDCATGRP-F        PIC 9(2)    VALUE ZERO.                   
044100     03 W-IDCATAVS-F-X.                                                   
044200         05 W-IDCATAVS-F        PIC 9(4)    VALUE ZERO.                   
044300     03 W-IDCATRAD-F-X.                                                   
044400         05 W-IDCATRAD-F        PIC 9(4)    VALUE ZERO.                   
044500     03 W-KDCATPUB-F-X.                                                   
044600         05 W-KDCATPUB-F        PIC X(6)    VALUE SPACE.                  
044700                                                                          
044800     03 21-LOW-VALUE            PIC X(21)  VALUE LOW-VALUE.               
044900     03 21-HIGH-VALUE           PIC X(21)  VALUE HIGH-VALUE.              
045000     SKIP2                                                                
045100*    --- STATUS-KOD FRÅN IMS                                              
045200 01  STATUS-WS                   PIC XX.                                  
045300     88  SEGMENT-FINNS                       VALUE '  '.                  
045400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
045500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
045600     88  SEGMENT-SLUT                        VALUE 'GB'.                  
045700     88  IMS-EJ-OK                           VALUE 'XD'.                  
045800     SKIP2                                                                
045900 01  GODK-STATUSKODER.                                                    
046000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
046100     SKIP3                                                                
046200 01  SSA1                        PIC X(120).                              
046300 01  SSA2                        PIC X(120).                              
046400 01  SSA3                        PIC X(120).                              
046500     EJECT                                                                
046600*    --- IMS FUNKTIONSKODER                                               
046700*01  -COPY W0003                                                          
046800     EJECT                                                                
046900*    ---  DLI INPUT-OUTPUT AREA                                           
047000                                                                          
047100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDN201'.                      
047200 01  DLI-IO-WDN201.                                                       
047300*    03  -COPY WDN201  -PRE WDN2-                                         
047400     EJECT                                                                
047500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDN211'.                      
047600 01  DLI-IO-WDN211.                                                       
047700*    03  -COPY WDN211  -PRE WDN2-                                         
047800     EJECT                                                                
047900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDN301'.                      
048000 01  DLI-IO-WDN301.                                                       
048100*    03  -COPY WDN301  -PRE WDN3-                                         
048200     EJECT                                                                
048300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDN311'.                      
048400 01  DLI-IO-WDN311.                                                       
048500*    03  -COPY WDN311  -PRE WDN3-                                         
048600     EJECT                                                                
048700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDN321'.                      
048800 01  DLI-IO-WDN321.                                                       
048900*    03  -COPY WDN321  -PRE WDN3-                                         
049000     EJECT                                                                
049100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDN401'.                      
049200 01  DLI-IO-WDN401.                                                       
049300*    03  -COPY WDN401  -PRE WDN4-                                         
049400     EJECT                                                                
049500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDN411'.                      
049600 01  DLI-IO-WDN411.                                                       
049700*    03  -COPY WDN411  -PRE WDN4-                                         
049800     EJECT                                                                
049900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD301'.                      
050000 01  DLI-IO-WDD301.                                                       
050100*    03  -COPY WDD301  -PRE WDD3-                                         
050200     EJECT                                                                
050300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311'.                      
050400 01  DLI-IO-WDD311.                                                       
050500*    03  -COPY WDD311  -PRE WDD3-                                         
050600     EJECT                                                                
050700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD312'.                      
050800 01  DLI-IO-WDD312.                                                       
050900*    03  -COPY WDD312  -PRE WDD3-                                         
051000     EJECT                                                                
051100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR201'.                      
051200 01  DLI-IO-WDR201.                                                       
051300*    03  -COPY WDGX01   -PRE WDR2-                                        
051400     EJECT                                                                
051500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX1206'.                    
051600 01  DLI-IO-WDGX1206.                                                     
051700*    03  -COPY WDGX1206 -PRE WDR2-                                        
051800     EJECT                                                                
051900                                                                          
052000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDN5'.                        
052100 01  DLI-IO-WDN5.                                                         
052200     03  WDN5-IO-AREA                 PIC X(32)  VALUE SPACE.             
052300*    03  -COPY WDN501 -PRE WDN5- -RED WDN5-IO-AREA.                       
052400*    03  -COPY WDN512 -PRE WDN5- -RED WDN5-IO-AREA.                       
052500     EJECT                                                                
052600                                                                          
052700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDN5D1'.                      
052800 01  DLI-IO-WDN5D1.                                                       
052900*    03  -COPY WDN5D1 -PRE WDN5D-                                         
053000     EJECT                                                                
053100                                                                          
053200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDN5E1'.                      
053300 01  DLI-IO-WDN5E1.                                                       
053400*    03  -COPY WDN5E1 -PRE WDN5E-                                         
053500     EJECT                                                                
053600                                                                          
053700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDN5F1'.                      
053800 01  DLI-IO-WDN5F1.                                                       
053900*    03  -COPY WDN5F1 -PRE WDN5F-                                         
054000                                                                          
054100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
054200 01  DLI-IO-WDK601.                                                       
054300*    03  -COPY WDK601 -PRE WDK6-                                          
054400     EJECT                                                                
054500 LINKAGE SECTION.                                                         
054600                                                                          
054700*01  -COPY W0009   -PRE MSG-                                              
054800     EJECT                                                                
054900*01  -COPY W0008  -PRE WDN2-                                              
055000     05  FILLER                  PIC X.                                   
055100     EJECT                                                                
055200*01  -COPY W0008  -PRE WDN3-                                              
055300     05  FILLER                  PIC X.                                   
055400     EJECT                                                                
055500*01  -COPY W0008  -PRE WDN4-                                              
055600     05  FILLER                  PIC X.                                   
055700     EJECT                                                                
055800*01  -COPY W0008  -PRE WDD3-                                              
055900     05  FILLER                  PIC X.                                   
056000     EJECT                                                                
056100*01  -COPY W0008  -PRE WDR2-                                              
056200     05  FILLER                  PIC X.                                   
056300     EJECT                                                                
056400*01  -COPY W0008  -PRE WDN5-                                              
056500     05  FILLER                  PIC X.                                   
056600     EJECT                                                                
056700*01  -COPY W0008  -PRE WDN5D-                                             
056800     05  FILLER                  PIC X.                                   
056900     EJECT                                                                
057000*01  -COPY W0008  -PRE WDN5E-                                             
057100     05  FILLER                  PIC X.                                   
057200     EJECT                                                                
057300*01  -COPY W0008  -PRE WDN5F-                                             
057400     05  FILLER                  PIC X.                                   
057500     EJECT                                                                
057600*01  -COPY W0008  -PRE WDK6-                                              
057700     05  FILLER                  PIC X.                                   
057800     EJECT                                                                
057900 PROCEDURE DIVISION  USING MSG-PCB  WDN2-PCB                              
058000           WDN3-PCB WDN4-PCB WDD3-PCB WDR2-PCB                            
058100           WDN5-PCB WDN5D-PCB WDN5E-PCB WDN5F-PCB WDK6-PCB.               
058200 MAIN SECTION.                                                            
058300     ENTRY 'DLITCBL' USING MSG-PCB  WDN2-PCB                              
058400           WDN3-PCB WDN4-PCB WDD3-PCB WDR2-PCB                            
058500           WDN5-PCB WDN5D-PCB WDN5E-PCB WDN5F-PCB WDK6-PCB.               
058600                                                                          
058700     SKIP2                                                                
058800     PERFORM A-INIT                                                       
058900     PERFORM S11-LAES-W15210                                              
059000     PERFORM UNTIL END-OF-W15210                                          
059100       IF CHKP-ANT > CHKP-MAX                                             
059200         PERFORM X-TAG-CHECKPOINT                                         
059300       END-IF                                                             
059400                                                                          
059500       EVALUATE IN-KDLEX                                                  
059600*        WHEN 'R'                                                         
059700*          PERFORM X-KONV-SPRAK-IDSKYLT                                   
059800*          PERFORM B-UPPDATERA-RUBRIK                                     
059900*        WHEN 'F'                                                         
060000*          PERFORM X-KONV-SPRAK-IDSKYLT                                   
060100*          PERFORM C-UPPDATERA-FOTNOT                                     
060200*        WHEN 'T'                                                         
060300*          PERFORM X-KONV-SPRAK-IDSKYLT                                   
060400*          PERFORM D-UPPDATERA-TILLAGGSTEXT                               
060500*          PERFORM S11-LAES-W15210                                        
060600*          CONTINUE                                                       
060700         WHEN 'B'                                                         
060800           PERFORM X-KONV-SPRAK-IDSKYLT                                   
060900           PERFORM E-UPPDATERA-BENAMNING                                  
061000           PERFORM S11-LAES-W15210                                        
061100           CONTINUE                                                       
061200         WHEN OTHER                                                       
061300           PERFORM S11-LAES-W15210                                        
061400           CONTINUE                                                       
061500       END-EVALUATE                                                       
061600     END-PERFORM                                                          
061700                                                                          
061800                                                                          
061900     PERFORM Z-FINIT                                                      
062000                                                                          
062100     MOVE ZERO TO RETURN-CODE                                             
062200     GOBACK                                                               
062300     .                                                                    
062400     EJECT                                                                
062500 A-INIT SECTION.                                                          
062600     SKIP2                                                                
062700                                                                          
062800     PERFORM IMS-RESTART                                                  
062900                                                                          
063000     OPEN INPUT W15210                                                    
063100     OPEN OUTPUT W15212                                                   
063200     OPEN OUTPUT W15214                                                   
063300     OPEN OUTPUT W15215                                                   
063400                                                                          
063500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
063600                                                                          
063700     ACCEPT DAGENS-DATUM FROM DATE                                        
063800     .                                                                    
063900     SKIP3                                                                
064000                                                                          
064100*B-UPPDATERA-RUBRIK        SECTION.                                       
064200*    SKIP2                                                                
064300*    --- HEADER RECORDS CAN COME IN GROUPS OF ONE TO THREE                
064400*    SET INDATA-OK  TO TRUE                                               
064500*    INITIALIZE  SPAR-PRUB-W152PRUB(1)                                    
064600*                SPAR-PRUB-W152PRUB(2)                                    
064700*                SPAR-PRUB-W152PRUB(3)                                    
064800*                                                                         
064900*    MOVE IN-KDLEX  TO W15214-KDLEX  SPAR-KDLEX                           
065000*                                                                         
065100*    PERFORM BA-LAGRA-RUBRIKRADER                                         
065200*                                                                         
065300*    PERFORM IMS-GHU-WDN201                                               
065400*    IF SEGMENT-FINNS                                                     
065500*                                                                         
065600*      IF WDN2-RUB-FLKOMBINERAS = JA                                      
065700*        IF SPAR-PRUB-BERUBTXT(2) NOT = SPACE                             
065800*          SET INDATA-FEL TO TRUE                                         
065900*          --- ENDAST 1 SEGMENT OM 30 BYTES TILLÅTET                      
066000*          --- NÄR FLKOMBINAERAS =JA.                                     
066100*          --- FELPOST TILL ÖVERS.BYRÅN SKICKAS                           
066200*        END-IF                                                           
066300*                                                                         
066400*        MOVE +0    TO W-IDSEGMNR-RUB                                     
066500*        PERFORM IMS-GHNP-KVAL-WDN211                                     
066600*        IF SEGMENT-FINNS                                                 
066700*          MOVE SPAR-PRUB-BERUBTXT(1) TO WDN2-TEXT-BERUBTXT               
066800*          PERFORM IMS-REPL-WDN211                                        
066900*          ADD +1 TO W-IDSEGMNR-RUB                                       
067000*          PERFORM IMS-GHNP-KVAL-WDN211                                   
067100*          IF SEGMENT-FINNS                                               
067200*            PERFORM IMS-DLET-WDN211                                      
067300*          END-IF                                                         
067400*          ADD    +1 TO W-IDSEGMNR-RUB                                    
067500*          PERFORM IMS-GHNP-KVAL-WDN211                                   
067600*          IF SEGMENT-FINNS                                               
067700*            PERFORM IMS-DLET-WDN211                                      
067800*          END-IF                                                         
067900*        ELSE                                                             
068000*          MOVE W-IDSKYLT-RUB TO WDN2-TEXT-IDSKYLT                        
068100*          MOVE      +0        TO WDN2-TEXT-IDSEGMNR                      
068200*          MOVE SPAR-PRUB-BERUBTXT(1) TO WDN2-TEXT-BERUBTXT               
068300*          PERFORM IMS-ISRT-WDN211                                        
068400*        END-IF                                                           
068500*                                                                         
068600*        IF INDATA-FEL                                                    
068700*          --- 30 BYTE TRUNKERAD RUBRIK ÄR UPPDATERAD PÅ BASEN            
068800*          --- SKICKA BEGÄRAN OM RÄTTELSE TILL ÖVERS.BYRÅN.               
068900*          MOVE 'FLKOMBINERAS=J,  ENDAST EN RAD MED 30 TKN GODK.'         
069000*                                 TO W15212-FEL-FELTEXT                   
069100*          MOVE +1 TO SEG-IX                                              
069200*          PERFORM UNTIL SEG-IX > +3                                      
069300*            MOVE SPAR-PRUB-W152PRUB(SEG-IX) TO W15212-FEL-DATADEL        
069400*            PERFORM S12-SKRIV-FELFIL-W15212                              
069500*            IF SEG-IX > +1                                               
069600*              IF SPAR-PRUB-BERUBTXT(SEG-IX) NOT = SPACE                  
069700*                --- SKICKA POST TILL SPS                                 
069800*                MOVE SPAR-PRUB-BERUBTXT(SEG-IX) TO W15215-TEXT           
069900*                PERFORM S15-SKRIV-SPSFIL-W15215                          
070000*              END-IF                                                     
070100*            END-IF                                                       
070200*            ADD +1 TO SEG-IX                                             
070300*          END-PERFORM                                                    
070400*        ELSE                                                             
070500*          --- TAG BORT POSTEN PÅ ÖVERSÄTTNINGSBEVAKNINGEN                
070600*          MOVE SPAR-IDBENNR TO WS-ACTION-IDLEXNR                         
070700*          MOVE 'R'        TO WS-ACTION-KDLEX                             
070800*          PERFORM S02-DELETE-ACTION-FILE                                 
070900*          PERFORM S03-UPPDATERA-WDN5                                     
071000*        END-IF                                                           
071100*      ELSE                                                               
071200*        --- 3 SEGMENT OM 30 BYTE KAN UPPDATERAS                          
071300*        --- OM DET FANNS MER INDATA, HAR FELPOST SKRIVITS I BA-          
071400*        MOVE +1 TO SEG-IX  W-IDSEGMNR-RUB                                
071500*        PERFORM IMS-GHNP-KVAL-WDN211                                     
071600*                                                                         
071700*        PERFORM UNTIL SEG-IX > +3                                        
071800*          IF SPAR-PRUB-BERUBTXT(SEG-IX) = SPACE                          
071900*            IF SEGMENT-FINNS                                             
072000*              IF SEG-IX > +1                                             
072100*                PERFORM IMS-DLET-WDN211                                  
072200*              ELSE                                                       
072300*                --- NEITHER INDATA NOR MORE OLD SEGMENTS LEFT            
072400*                --- IF SEG-IX =1, THE OLD DATA SHOULD REMAIN             
072500*                MOVE +4 TO SEG-IX                                        
072600*              END-IF                                                     
072700*            END-IF                                                       
072800*          ELSE                                                           
072900*            MOVE       SEG-IX    TO WDN2-TEXT-IDSEGMNR                   
073000*            MOVE SPAR-PRUB-BERUBTXT(SEG-IX)                              
073100*                                 TO WDN2-TEXT-BERUBTXT                   
073200*            IF SEGMENT-FINNS                                             
073300*              PERFORM IMS-REPL-WDN211                                    
073400*            ELSE                                                         
073500*              MOVE W-IDSKYLT-RUB TO WDN2-TEXT-IDSKYLT                    
073600*              PERFORM IMS-ISRT-WDN211                                    
073700*            END-IF                                                       
073800*          END-IF                                                         
073900*          ADD +1 TO SEG-IX,  W-IDSEGMNR-RUB                              
074000*          PERFORM IMS-GHNP-KVAL-WDN211                                   
074100*        END-PERFORM                                                      
074200*        IF INDATA-OK                                                     
074300*          --- TAG BORT POSTEN PÅ ÖVERSÄTTNINGSBEVAKNINGEN                
074400*          MOVE 'R' TO WS-ACTION-KDLEX                                    
074500*          PERFORM S02-DELETE-ACTION-FILE                                 
074600*          PERFORM S03-UPPDATERA-WDN5                                     
074700*        END-IF                                                           
074800*      END-IF                                                             
074900*      IF INDATA-OK AND ( W-IDSKYLT = 'GB ' )                             
075000*        --- SKRIV EN MEMOPOST TILL REDAKTÖRER                            
075100*        MOVE SPAR-IDBENNR       TO W15214-IDBENNR                        
075200*        MOVE W-IDSKYLT          TO W15214-IDSKYLT                        
075300*        STRING SPAR-PRUB-BERUBTXT(1) DELIMITED BY '  '                   
075400*              '                   '  DELIMITED BY SIZE                   
075500*               '(SE 1503 och 1521)'  DELIMITED BY SIZE                   
075600*                                   INTO W15214-TEXT                      
075700*        END-STRING                                                       
075800*        PERFORM S14-SKRIV-MEMOFIL-W15214                                 
075900*      END-IF                                                             
076000*    ELSE                                                                 
076100*      --- HEADER ID SHOULD BE FOUND IN DATABASE                          
076200*      MOVE 'IDRUBNR FANNS EJ PÅ BASEN. (WDN2)  '                         
076300*                             TO W15212-FEL-FELTEXT                       
076400*      MOVE +1 TO SEG-IX                                                  
076500*      PERFORM UNTIL SEG-IX > +3                                          
076600*        MOVE SPAR-PRUB-W152PRUB(SEG-IX) TO W15212-FEL-DATADEL            
076700*        PERFORM S12-SKRIV-FELFIL-W15212                                  
076800*        ADD +1 TO SEG-IX                                                 
076900*      END-PERFORM                                                        
077000*    END-IF                                                               
077100*    .                                                                    
077200*    EJECT                                                                
077300*BA-LAGRA-RUBRIKRADER  SECTION.                                           
077400*    SKIP2                                                                
077500*    MOVE IN-IDBENNR TO SPAR-IDBENNR                                      
077600*                       W-IDRUBNR                                         
077700*                       W15215-IDLEXNR                                    
077800*    MOVE IN-IDPTYP  TO W15215-IDPTYP                                     
077900*    MOVE IN-DIFAELT TO W15215-DIFAELT                                    
078000*    MOVE IN-IDSPRAK TO W15215-IDSPRAK                                    
078100*                                                                         
078200*    MOVE +1 TO SEG-IX                                                    
078300*                                                                         
078400*    PERFORM UNTIL END-OF-W15210 OR SEG-IX > +3                           
078500*    OR NOT ( IN-KDLEX = 'R' AND IN-IDBENNR = SPAR-IDBENNR )              
078600*                                                                         
078700*        MOVE IN-PRUB-W152PRUB TO SPAR-PRUB-W152PRUB (SEG-IX)             
078800*        PERFORM S11-LAES-W15210                                          
078900*        ADD +1 TO SEG-IX                                                 
079000*    END-PERFORM                                                          
079100*    IF SEG-IX > +3 AND NOT END-OF-W15210                                 
079200*      IF IN-IDBENNR = SPAR-IDBENNR                                       
079300*        DISPLAY SPAR-IDBENNR ' RUB1= ' SPAR-PRUB-W152PRUB(1)             
079400*        DISPLAY SPAR-IDBENNR ' RUB2= ' SPAR-PRUB-W152PRUB(2)             
079500*        DISPLAY SPAR-IDBENNR ' RUB3= ' SPAR-PRUB-W152PRUB(3)             
079600*        --- LÄS FÖRBI DESSA EXTRAPOSTER                                  
079700*        PERFORM UNTIL IN-IDBENNR NOT = SPAR-IDBENNR                      
079800*                OR END-OF-W15210                                         
079900*          --- DET FANNS MER ÄN TRE IN-RADER. SKRIV FELPOST.              
080000*          SET INDATA-FEL TO TRUE                                         
080100*                                                                         
080200*          MOVE ' > 3 ÖVERS.RADER  DATABAS UPPDATERAD MED 3'              
080300*                                TO W15212-FEL-FELTEXT                    
080400*          MOVE IN-PRUB-W152PRUB TO W15212-FEL-DATADEL                    
080500*          PERFORM S12-SKRIV-FELFIL-W15212                                
080600*                                                                         
080700*          --- SKICKA POST TILL SPS                                       
080800*          MOVE IN-PRUB-BERUBTXT TO W15215-TEXT                           
080900*          PERFORM S15-SKRIV-SPSFIL-W15215                                
081000*                                                                         
081100*          PERFORM S11-LAES-W15210                                        
081200*        END-PERFORM                                                      
081300*      END-IF                                                             
081400*    END-IF                                                               
081500*                                                                         
081600*    MOVE W-IDSKYLT    TO W-IDSKYLT-RUB                                   
081700*    .                                                                    
081800*    EJECT                                                                
081900*C-UPPDATERA-FOTNOT        SECTION.                                       
082000*    SKIP2                                                                
082100*    --- FOOTNOTE RECORDS CAN COME IN GROUPS OF ONE TO THREE              
082200*    SET INDATA-OK  TO TRUE                                               
082300*                                                                         
082400*    INITIALIZE  SPAR-PFOT-W152PFOT(1)                                    
082500*                SPAR-PFOT-W152PFOT(2)                                    
082600*                SPAR-PFOT-W152PFOT(3)                                    
082700*                                                                         
082800*    MOVE IN-KDLEX     TO W15214-KDLEX  SPAR-KDLEX                        
082900*                                                                         
083000*    PERFORM CA-LAGRA-FOTNOTRADER                                         
083100*                                                                         
083200*    PERFORM IMS-GHU-WDN301                                               
083300*                                                                         
083400*    IF SEGMENT-FINNS                                                     
083500*      PERFORM IMS-GNP-WDN311                                             
083600*                                                                         
083700*      IF SEGMENT-FINNS                                                   
083800*        MOVE WDN3-FORD-KDFORDON   TO W-KDFORDON                          
083900*                                                                         
084000*        IF WDN3-FORD-FLOVERSATT = JA                                     
084100*          --- FOOTNOT TEXT SHALL BE TRANSLATED TO ALL LANGUAGES          
084200*          PERFORM IMS-GHU-WDN311                                         
084300*                                                                         
084400*          MOVE +1 TO W-IDSEGMNR-FOT, SEG-IX                              
084500*          PERFORM IMS-GHNP-KVAL-WDN321                                   
084600*                                                                         
084700*          PERFORM UNTIL SEG-IX > +3                                      
084800*            IF SPAR-PFOT-BEFOTNOT(SEG-IX) = SPACE                        
084900*              IF SEGMENT-FINNS                                           
085000*                IF SEG-IX > +1                                           
085100*                  PERFORM IMS-DLET-WDN321                                
085200*                  ADD +1 TO SEG-IX, W-IDSEGMNR-FOT                       
085300*                  PERFORM IMS-GHNP-KVAL-WDN321                           
085400*                END-IF                                                   
085500*              ELSE                                                       
085600*                --- NEITHER INDATA NOR MORE OLD SEGMENTS LEFT            
085700*                --- IF SEG-IX =1, THE OLD DATA SHOULD REMAIN             
085800*                MOVE +4 TO SEG-IX                                        
085900*              END-IF                                                     
086000*            ELSE                                                         
086100*              MOVE SEG-IX        TO WDN3-TEXT-IDSEGMNR                   
086200*              MOVE SPAR-PFOT-BEFOTNOT (SEG-IX)                           
086300*                               TO WDN3-TEXT-BEFOTNOT                     
086400*              IF SEGMENT-FINNS                                           
086500*                PERFORM IMS-REPL-WDN321                                  
086600*              ELSE                                                       
086700*                MOVE W-IDSKYLT-FOT TO WDN3-TEXT-IDSKYLT                  
086800*                PERFORM IMS-ISRT-WDN321                                  
086900*              END-IF                                                     
087000*              ADD +1 TO SEG-IX, W-IDSEGMNR-FOT                           
087100*              PERFORM IMS-GHNP-KVAL-WDN321                               
087200*            END-IF                                                       
087300*          END-PERFORM                                                    
087400*                                                                         
087500*          IF INDATA-OK                                                   
087600*            --- SLÄCK FOTNOTSNUMRET PÅ ÖVERSÄTTNINGSBEVAKNINGEN          
087700*            MOVE 'F' TO WS-ACTION-KDLEX                                  
087800*            PERFORM S02-DELETE-ACTION-FILE                               
087900*            PERFORM S03-UPPDATERA-WDN5                                   
088000*          END-IF                                                         
088100*          IF INDATA-OK AND ( W-IDSKYLT = 'GB ' )                         
088200*            --- SKRIV EN MEMOPOST TILL REDAKTÖRERNA                      
088300*            MOVE SPAR-IDBENNR   TO W15214-IDBENNR                        
088400*            MOVE W-IDSKYLT      TO W15214-IDSKYLT                        
088500*            STRING SPAR-PFOT-BEFOTNOT(1) DELIMITED BY '  '               
088600*                   '   (SE 1509)'        DELIMITED BY SIZE               
088700*                                       INTO W15214-TEXT                  
088800*            END-STRING                                                   
088900*            PERFORM S14-SKRIV-MEMOFIL-W15214                             
089000*          END-IF                                                         
089100*        ELSE                                                             
089200*          --- FOOTNOT TEXT OCCURS ONLY ON LANG-CODE 'S  '                
089300*          --- AND IS VALID FOR ALL OTHER LANG'S AS WELL                  
089400*          --- släck fotnotsnumret på översättningsbevakningen            
089500*          --- ifall den trots allt skulle finnas där                     
089600*          MOVE 'F' TO WS-ACTION-KDLEX                                    
089700*          PERFORM S02-DELETE-ACTION-FILE                                 
089800*          PERFORM S03-UPPDATERA-WDN5                                     
089900*        END-IF                                                           
090000*      ELSE                                                               
090100*        --- FOOTNOTE TYPE SEGMENT NOT FOUND IN DATABASE                  
090200*        MOVE 'WDN311  FANNS EJ             '                             
090300*                               TO W15212-FEL-FELTEXT                     
090400*        MOVE +1 TO SEG-IX                                                
090500*        PERFORM UNTIL SEG-IX > +3                                        
090600*          MOVE SPAR-PFOT-W152PFOT(SEG-IX) TO W15212-FEL-DATADEL          
090700*          PERFORM S12-SKRIV-FELFIL-W15212                                
090800*          ADD +1 TO SEG-IX                                               
090900*        END-PERFORM                                                      
091000*      END-IF                                                             
091100*    ELSE                                                                 
091200*      --- FOOTNOTE ID SHOULD BE FOUND IN DATABASE                        
091300*      MOVE 'IDFOTNR FANNS EJ PÅ BASEN.  (WDN3) '                         
091400*                             TO W15212-FEL-FELTEXT                       
091500*      MOVE +1 TO SEG-IX                                                  
091600*      PERFORM UNTIL SEG-IX > +3                                          
091700*        MOVE SPAR-PFOT-W152PFOT(SEG-IX) TO W15212-FEL-DATADEL            
091800*        PERFORM S12-SKRIV-FELFIL-W15212                                  
091900*        ADD +1 TO SEG-IX                                                 
092000*      END-PERFORM                                                        
092100*    END-IF                                                               
092200*    .                                                                    
092300*    EJECT                                                                
092400*CA-LAGRA-FOTNOTRADER  SECTION.                                           
092500*    SKIP2                                                                
092600*    MOVE IN-IDBENNR TO SPAR-IDBENNR                                      
092700*                       W-IDFOTNR                                         
092800*                       W15215-IDLEXNR                                    
092900*    MOVE IN-IDPTYP  TO W15215-IDPTYP                                     
093000*    MOVE IN-DIFAELT TO W15215-DIFAELT                                    
093100*    MOVE IN-IDSPRAK TO W15215-IDSPRAK                                    
093200*                                                                         
093300*    MOVE +1 TO SEG-IX                                                    
093400*    PERFORM UNTIL END-OF-W15210 OR SEG-IX > +3                           
093500*    OR NOT ( IN-KDLEX = 'F' AND IN-IDBENNR = SPAR-IDBENNR )              
093600*                                                                         
093700*        MOVE IN-PFOT-W152PFOT TO SPAR-PFOT-W152PFOT (SEG-IX)             
093800*        PERFORM S11-LAES-W15210                                          
093900*        ADD +1 TO SEG-IX                                                 
094000*                                                                         
094100*    END-PERFORM                                                          
094200*    IF SEG-IX > +3 AND NOT END-OF-W15210                                 
094300*      IF IN-IDBENNR = SPAR-IDBENNR                                       
094400*        --- DET FANNS MER ÄN TRE IN-RADER.                               
094500****        DISPLAY SPAR-IDBENNR ' FOT1= ' SPAR-PFOT-W152PFOT(1)          
094600****        DISPLAY SPAR-IDBENNR ' FOT2= ' SPAR-PFOT-W152PFOT(2)          
094700****        DISPLAY SPAR-IDBENNR ' FOT3= ' SPAR-PFOT-W152PFOT(3)          
094800*                                                                         
094900*        SET INDATA-FEL TO TRUE                                           
095000*        --- SKRIV FELPOSTER OCH LÄS FÖRBI DESSA                          
095100*        PERFORM UNTIL IN-IDBENNR NOT = SPAR-IDBENNR                      
095200*                OR END-OF-W15210                                         
095300*          MOVE ' > 3 ÖVERS.RADER  DATABAS UPPDATERAS MED 3'              
095400*                                TO W15212-FEL-FELTEXT                    
095500*          MOVE IN-PFOT-W152PFOT TO W15212-FEL-DATADEL                    
095600*          PERFORM S12-SKRIV-FELFIL-W15212                                
095700*                                                                         
095800*          --- SKICKA POST TILL SPS                                       
095900*          MOVE IN-PFOT-BEFOTNOT TO W15215-TEXT                           
096000*          PERFORM S15-SKRIV-SPSFIL-W15215                                
096100*                                                                         
096200*          PERFORM S11-LAES-W15210                                        
096300*        END-PERFORM                                                      
096400*      END-IF                                                             
096500*    END-IF                                                               
096600*                                                                         
096700*    MOVE W-IDSKYLT    TO W-IDSKYLT-FOT                                   
096800*    .                                                                    
096900*    EJECT                                                                
097000*D-UPPDATERA-TILLAGGSTEXT  SECTION.                                       
097100*    SKIP2                                                                
097200*    MOVE IN-IDBENNR TO W-IDTTEXNR                                        
097300*                       SPAR-IDBENNR                                      
097400*    MOVE IN-KDLEX  TO W15214-KDLEX  SPAR-KDLEX                           
097500*                                                                         
097600*    SET INDATA-OK  TO TRUE                                               
097700*    PERFORM IMS-GHU-WDN401                                               
097800*    IF SEGMENT-FINNS                                                     
097900*      IF WDN4-TTEXT-FLOVERSATT = JA                                      
098000*        --- ADDITIONAL TEXT SHALL BE TRANSLATED TO ALL LANGUAGES         
098100*        PERFORM IMS-GHNP-WDN411                                          
098200*        IF SEGMENT-FINNS                                                 
098300*          IF IN-PTIL-BETTEXT NOT = SPACE                                 
098400*            MOVE IN-PTIL-BETTEXT TO WDN4-TEXT-BETTEXT                    
098500*          END-IF                                                         
098600*          PERFORM IMS-REPL-WDN411                                        
098700*        ELSE                                                             
098800*          MOVE W-IDSKYLT       TO WDN4-TEXT-IDSKYLT                      
098900*          MOVE IN-PTIL-BETTEXT TO WDN4-TEXT-BETTEXT                      
099000*          PERFORM IMS-ISRT-WDN411                                        
099100*        END-IF                                                           
099200*        --- SLÄCK T-TEXT-NR PÅ ÖVERSÄTTNINGSBEVAKNINGEN                  
099300*        MOVE     'T'      TO WS-ACTION-KDLEX                             
099400*        PERFORM S02-DELETE-ACTION-FILE                                   
099500*        PERFORM S03-UPPDATERA-WDN5                                       
099600*                                                                         
099700*        IF INDATA-OK AND ( W-IDSKYLT = 'GB ' )                           
099800*          --- SKRIV EN MEMOPOST TILL REDAKTÖRERNA                        
099900*          MOVE IN-KDLEX      TO W15214-KDLEX                             
100000*          MOVE SPAR-IDBENNR  TO W15214-IDBENNR                           
100100*          MOVE W-IDSKYLT     TO W15214-IDSKYLT                           
100200*          STRING IN-PTIL-BETTEXT DELIMITED BY '  '                       
100300*       '                        '  DELIMITED BY SIZE                     
100400*             '(SE 1506 och 1521)'  DELIMITED BY SIZE                     
100500*                                   INTO W15214-TEXT                      
100600*          PERFORM S14-SKRIV-MEMOFIL-W15214                               
100700*        END-IF                                                           
100800*      ELSE                                                               
100900*        --- ADDITIONAL TEXT OCCURS ONLY ON LANG-CODE 'S  '               
101000*        --- AND IS VALID FOR ALL OTHER LANG'S AS WELL                    
101100*        --- släck text-numret på översättningsbevakningen                
101200*        --- ifall den trots allt skulle finnas där                       
101300*        MOVE 'T' TO WS-ACTION-KDLEX                                      
101400*        PERFORM S02-DELETE-ACTION-FILE                                   
101500*        PERFORM S03-UPPDATERA-WDN5                                       
101600*      END-IF                                                             
101700*    ELSE                                                                 
101800*      --- SEGMENT SHOULD BE FOUND                                        
101900*      MOVE 'IDTTEXNR FANNS EJ PÅ BASEN (WDN4).   '                       
102000*                      TO W15212-FEL-FELTEXT                              
102100*      MOVE IN-AREA-0  TO W15212-FEL-DATADEL                              
102200*      PERFORM S12-SKRIV-FELFIL-W15212                                    
102300*                                                                         
102400*    END-IF                                                               
102500*    .                                                                    
102600*    EJECT                                                                
102700 E-UPPDATERA-BENAMNING     SECTION.                                       
102800     SKIP2                                                                
102900*                              KDBENSTAT 0 OCH 1 ÄR NAMNLEX/TECHLA        
103000*                              KDBENSTAT 2       ÄR RS-UNIK               
103100*                              KDBENSTAT 3       ÄR RENSAD                
103200     MOVE IN-IDBENNR TO W-IDBENNR                                         
103300                        SPAR-IDBENNR                                      
103400     PERFORM IMS-GHU-WDD301                                               
103500     IF SEGMENT-FINNS                                                     
103600       SET WDD3-UNTOUCHED TO TRUE                                         
103700                                                                          
103800       PERFORM IMS-GHNP-WDD311-UNIK                                       
103900                                                                          
104000       IF IN-PBEN-BEARTEXT NOT = SPACE                                    
104100*        IF  SEGMENT-FINNS                                                
104200*        AND WDD3-BEN-KDBENSTAT < +2                                      
104300*        AND WDD3-TEXT-FLOVERSATT = JA                                    
104400*          --- DO NOT DESTROY A NAMNLEX DESCRIPTION                       
104500*          MOVE 'NAMNLEX-BEN.  FÅR EJ UPPDATERAS VID FLOVERSATT=J'        
104600*                          TO W15212-FEL-FELTEXT                          
104700*          MOVE IN-AREA-0 TO W15212-FEL-DATADEL                           
104800*          PERFORM S12-SKRIV-FELFIL-W15212                                
104900*        ELSE                                                             
105000           MOVE IN-PBEN-BEARTEXT TO WDD3-TEXT-BEARTEXT                    
105100                                                                          
105200           IF W-IDSKYLT = 'D  ' OR 'DK ' OR 'E  ' OR 'F  '                
105300                       OR 'GB ' OR 'I  ' OR 'NL ' OR 'P  '                
105400                       OR 'S  ' OR 'SF ' OR 'USA' OR 'PL '                
105500             INSPECT WDD3-TEXT-BEARTEXT                                   
105600             CONVERTING WS-GEMEN         TO WS-VERSAL                     
105700             INSPECT WDD3-TEXT-BEARTEXT                                   
105800             CONVERTING WS-GEMEN-SPECIAL TO WS-VERSAL-SPECIAL             
105900           END-IF                                                         
106000                                                                          
106100           MOVE DAGENS-DATUM  TO WDD3-TEXT-TIUPPDAT                       
106200*          IF SEGMENT-SAKNAS                                              
106300*            IF WDD3-BEN-KDBENSTAT < +2                                   
106400*              MOVE NEJ TO WDD3-TEXT-FLOVERSATT                           
106500*            ELSE                                                         
106600*              MOVE JA TO WDD3-TEXT-FLOVERSATT                            
106700*            END-IF                                                       
106800*          ELSE                                                           
106900*            IF WDD3-BEN-KDBENSTAT > +1                                   
107000*              MOVE JA TO WDD3-TEXT-FLOVERSATT                            
107100*            END-IF                                                       
107200*          END-IF                                                         
107300                                                                          
107400           MOVE JA TO WDD3-TEXT-FLOVERSATT                                
107500           IF SEGMENT-FINNS                                               
107600             PERFORM IMS-REPL-WDD311                                      
107700           ELSE                                                           
107800             MOVE W-IDSKYLT TO WDD3-TEXT-IDSKYLT                          
107900             PERFORM IMS-ISRT-WDD311                                      
108000           END-IF                                                         
108100           SET WDD3-UPDATED TO TRUE                                       
108200                                                                          
108300           IF INDATA-OK AND ( W-IDSKYLT = 'GB ' )                         
108400*            --- SKRIV EN MEMOPOST TILL REDAKTÖRERNA                      
108500             MOVE IN-KDLEX        TO W15214-KDLEX                         
108600             MOVE SPAR-IDBENNR    TO W15214-IDBENNR                       
108700             MOVE W-IDSKYLT       TO W15214-IDSKYLT                       
108800             MOVE IN-PBEN-BEARTEXT TO W15214-TEXT                         
108900             STRING IN-PBEN-BEARTEXT DELIMITED BY '  '                    
109000                   '                   ' DELIMITED BY SIZE                
109100               '(SE 1131,1122 och 1526)' DELIMITED BY SIZE                
109200                                    INTO W15214-TEXT                      
109300             PERFORM S14-SKRIV-MEMOFIL-W15214                             
109400                                                                          
109500             PERFORM EB-UPPDAT-GB-I-TOMMA-EJ-OVERS                        
109600           END-IF                                                         
109700*        END-IF                                                           
109800       END-IF                                                             
109900       IF WDD3-UPDATED                                                    
110000         PERFORM EA-KOLLA-NEVIS-ARTIKLAR                                  
110100         IF WS-NEVIS-ART = JA                                             
110200           PERFORM IMS-GHU-WDD301                                         
110300           MOVE JA TO WDD3-BEN-FLAENDR                                    
110400           PERFORM IMS-REPL-WDD301                                        
110500         END-IF                                                           
110600       END-IF                                                             
110700     ELSE                                                                 
110800*      --- SEGMENT SHOULD BE FOUND                                        
110900       MOVE 'IDBENNR  FANNS EJ PÅ BASEN (WDD3).   '                       
111000                       TO W15212-FEL-FELTEXT                              
111100       MOVE IN-AREA-0  TO W15212-FEL-DATADEL                              
111200       PERFORM S12-SKRIV-FELFIL-W15212                                    
111300     END-IF                                                               
111400     .                                                                    
111500     EJECT                                                                
111600 EA-KOLLA-NEVIS-ARTIKLAR  SECTION.                                        
111700     SKIP2                                                                
111800     PERFORM IMS-GU-WDD301                                                
111900     PERFORM IMS-GNP-WDD312                                               
112000     MOVE NEJ TO WS-NEVIS-ART                                             
112100     PERFORM UNTIL SEGMENT-SAKNAS OR WS-NEVIS-ART = JA                    
112200       MOVE WDD3-ART-IDARTNR TO W-IDARTNR                                 
112300       PERFORM IMS-GU-WDK601                                              
112400       IF SEGMENT-FINNS                                                   
112500         MOVE WDK6-ART-KDPRODSL TO TEST-KDPRODSL                          
112600         IF KDPRODSL-VOLVO-ALL                                            
112700           MOVE JA TO WS-NEVIS-ART                                        
112800         END-IF                                                           
112900       END-IF                                                             
113000       PERFORM IMS-GNP-WDD312                                             
113100     END-PERFORM                                                          
113200     .                                                                    
113300     EJECT                                                                
113400 EB-UPPDAT-GB-I-TOMMA-EJ-OVERS  SECTION.                                  
113500     SKIP2                                                                
113600     IF WDD3-BEN-KDBENSTAT = +2                                           
113700*      --- Det är en partsunik benämning                                  
113800*      --- Fyll i levererad engelsk benämning i övriga väster-            
113900*      --- ländska språk som antingen är SPACE eller ej översatta         
114000       PERFORM IMS-GU-WDD301                                              
114100       PERFORM IMS-GHNP-WDD311                                            
114200       PERFORM UNTIL SEGMENT-SAKNAS                                       
114300         IF WDD3-TEXT-FLOVERSATT = NEJ                                    
114400         OR WDD3-TEXT-BEARTEXT   = SPACE                                  
114500                                                                          
114600           IF WDD3-TEXT-IDSKYLT = ' BG'                                   
114700             MOVE SPACE        TO REV-TETEXT                              
114800             MOVE IN-PBEN-BEARTEXT TO REV-TETEXT                          
114900             CALL WREVERSE  USING REV-TETEXT                              
115000                                                                          
115100             MOVE REV-TETEXT   TO WDD3-TEXT-BEARTEXT                      
115200             MOVE DAGENS-DATUM TO WDD3-TEXT-TIUPPDAT                      
115300             MOVE JA           TO WDD3-TEXT-FLOVERSATT                    
115400             PERFORM IMS-REPL-WDD311                                      
115500           ELSE                                                           
115600             MOVE WDD3-TEXT-IDSKYLT TO WS-WDD311-LANG                     
115700             IF WDD311-WESTERN-LANG                                       
115800               MOVE IN-PBEN-BEARTEXT TO WDD3-TEXT-BEARTEXT                
115900               PERFORM IMS-REPL-WDD311                                    
116000             END-IF                                                       
116100           END-IF                                                         
116200         END-IF                                                           
116300         PERFORM IMS-GHNP-WDD311                                          
116400       END-PERFORM                                                        
116500     END-IF                                                               
116600     .                                                                    
116700     EJECT                                                                
116800 X-KONV-SPRAK-IDSKYLT      SECTION.                                       
116900     SKIP2                                                                
117000     MOVE IN-IDSPRAK TO WS-IDSPRAK                                        
117100     MOVE SPACE      TO W-IDSKYLT                                         
117200                                                                          
117300*    --- SÖK I SPRÅKTABELLEN                                              
117400     MOVE +1 TO SPR-IX                                                    
117500     PERFORM UNTIL SPR-IX > MAX-ANTAL-SPRAK                               
117600                OR TAB-IDSPRAK (SPR-IX) = WS-IDSPRAK                      
117700        ADD +1 TO SPR-IX                                                  
117800     END-PERFORM                                                          
117900                                                                          
118000     IF SPR-IX NOT > MAX-ANTAL-SPRAK                                      
118100        MOVE TAB-IDSKYLT (SPR-IX) TO W-IDSKYLT                            
118200     ELSE                                                                 
118300        STRING 'FELAKTIGT SPRÅK: ' WS-IDSPRAK ' I IN-FILEN'               
118400        DELIMITED BY SIZE INTO FELTEXT-STR                                
118500        PERFORM S99-ABEND                                                 
118600     END-IF                                                               
118700     .                                                                    
118800     EJECT                                                                
118900                                                                          
119000 Z-FINIT SECTION.                                                         
119100                                                                          
119200     CLOSE  W15210                                                        
119300            W15212                                                        
119400            W15214                                                        
119500            W15215                                                        
119600                                                                          
119700     MOVE 'S' TO POSTSUM-OPKOD                                            
119800     CALL POSTSUM USING POSTSUM-PARM                                      
119900     .                                                                    
120000     EJECT                                                                
120100 S02-DELETE-ACTION-FILE SECTION.                                          
120200     SKIP2                                                                
120300     MOVE SPAR-IDBENNR TO WS-ACTION-IDLEXNR                               
120400     PERFORM IMS-GET-WDR201                                               
120500     IF SEGMENT-FINNS                                                     
120600       MOVE WS-ACTION-IDLEXNR TO W-IDLEXNR                                
120700       MOVE WS-ACTION-KDLEX   TO W-KDLEX                                  
120800       PERFORM IMS-GET-WDGX1206                                           
120900       IF SEGMENT-FINNS                                                   
121000         MOVE JA TO TA-BORT-TRANS                                         
121100         MOVE +1 TO SPRAAK-IX                                             
121200         PERFORM UNTIL SPRAAK-IX >= MAX-SPRAAK-PLUS-1                     
121300           IF WDR2-1206-IDSKYLT(SPRAAK-IX) = W-IDSKYLT                    
121400             MOVE SPACE TO WDR2-1206-IDSKYLT(SPRAAK-IX)                   
121500             MOVE ZERO TO WDR2-1206-TIUPPDAT(SPRAAK-IX)                   
121600           ELSE                                                           
121700             IF WDR2-1206-IDSKYLT(SPRAAK-IX) NOT = SPACE                  
121800               MOVE NEJ TO TA-BORT-TRANS                                  
121900             END-IF                                                       
122000           END-IF                                                         
122100           ADD +1 TO SPRAAK-IX                                            
122200         END-PERFORM                                                      
122300         IF TA-BORT-TRANS = NEJ                                           
122400           PERFORM IMS-REPL-WDGX1206                                      
122500         ELSE                                                             
122600           IF W-IDSKYLT = 'GB '                                           
122700*            --- NÄR ENGELSKAN UPPDATERAS MÅSTE TRANSEN VARA KVAR         
122800*            --- FÖR ATT ÖVRIGA SPRÅK SKALL KUNNA BESTÄLLAS.              
122900*            --- 1206-KDBEH skall därför vara 'X' tills kat.red.          
123000*            --- har kontrollerat texten, då den ligger till              
123100*            --- grund för övriga 14 språk.                               
123200             IF ( WS-ACTION-KDLEX = 'F'                                   
123300                 AND WDN3-FORD-FLOVERSATT = 'N' )                         
123400             OR ( WS-ACTION-KDLEX = 'T'                                   
123500                 AND WDN4-TTEXT-FLOVERSATT = 'N' )                        
123600*              --- tar bort bevakningen då texten inte längre             
123700*              --- skall översättas.                                      
123800               PERFORM IMS-DLET-WDGX1206                                  
123900             ELSE                                                         
124000               MOVE 'X' TO WDR2-1206-KDBEH                                
124100               PERFORM IMS-REPL-WDGX1206                                  
124200             END-IF                                                       
124300           ELSE                                                           
124400             PERFORM IMS-DLET-WDGX1206                                    
124500           END-IF                                                         
124600         END-IF                                                           
124700       END-IF                                                             
124800     END-IF                                                               
124900     .                                                                    
125000     EJECT                                                                
125100 S03-UPPDATERA-WDN5       SECTION.                                        
125200     SKIP2                                                                
125300*    -- Markera att avsnittet där TEXTEN finns, är uppdaterat             
125400*    -- Markera att raden     där TEXTEN finns, är ändrad                 
125500     EVALUATE SPAR-KDLEX                                                  
125600                                                                          
125700       WHEN  'R'                                                          
125800         MOVE SPAR-IDBENNR TO W-IDRUBNR-E                                 
125900         PERFORM S03A-UPPDATERA-WDN5-RUB                                  
126000                                                                          
126100       WHEN  'F'                                                          
126200         MOVE SPAR-IDBENNR TO W-IDFOTNR-F                                 
126300         PERFORM S03B-UPPDATERA-WDN5-FOT                                  
126400                                                                          
126500       WHEN  'T'                                                          
126600         MOVE SPAR-IDBENNR TO W-IDTTEXNR-D                                
126700         PERFORM S03C-UPPDATERA-WDN5-TIL                                  
126800                                                                          
126900     END-EVALUATE                                                         
127000     .                                                                    
127100     EJECT                                                                
127200 S03A-UPPDATERA-WDN5-RUB  SECTION.                                        
127300     SKIP2                                                                
127400*    --- Läs sekundärindex av wdn525                                      
127500     PERFORM IMS-GN-WDN5E1                                                
127600     IF SEGMENT-FINNS                                                     
127700       PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                       
127800*        --- WDN5E läses med GN och samma nyckel (RUB) varje gång         
127900         MOVE WDN5E-AVSE-IDCATNR     TO W-IDCATNR-N5                      
128000         MOVE WDN5E-AVSE-IDCATGRP    TO W-IDCATGRP-N5                     
128100         MOVE WDN5E-AVSE-IDCATAVS    TO W-IDCATAVS-N5                     
128200         MOVE WDN5E-AVSE-IDCATRAD    TO W-IDCATRAD-N5                     
128300         MOVE WDN5E-AVSE-KDCATPUB-FOM TO W-KDCATPUB-N5                    
128400         PERFORM IMS-GHU-WDN501                                           
128500*        -- segment måste finnas, SEKUNDÄRINDEX                           
128600         MOVE JA TO WDN5-AVS-FLAVSUST                                     
128700         PERFORM IMS-REPL-WDN5                                            
128800         PERFORM IMS-GHNP-WDN512                                          
128900*        -- segment måste finnas, SEKUNDÄRINDEX                           
129000         MOVE 'Ä'            TO WDN5-RAD-KDRADST                          
129100         MOVE DAGENS-DATUM   TO WDN5-RAD-TIUPPDAT                         
129200         MOVE IDPGM          TO WDN5-RAD-IDUSER                           
129300         PERFORM IMS-REPL-WDN5                                            
129400*        --- Läs nästa sekundärindex av wdn525                            
129500         PERFORM IMS-GN-WDN5E1                                            
129600       END-PERFORM                                                        
129700     ELSE                                                                 
129800       CONTINUE                                                           
129900*      -- Rubriken har tagits bort, FINNS EJ MER I KATALOG                
130000     END-IF                                                               
130100     .                                                                    
130200     EJECT                                                                
130300 S03B-UPPDATERA-WDN5-FOT  SECTION.                                        
130400     SKIP2                                                                
130500*    --- Läs sekundärindex av WDN526                                      
130600     PERFORM IMS-GN-WDN5F1                                                
130700     IF SEGMENT-FINNS                                                     
130800       PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                       
130900*        --- WDN5F läses med GN och samma nyckel (FOT) varje gång         
131000         MOVE WDN5F-AVSF-IDCATNR     TO W-IDCATNR-N5                      
131100         MOVE WDN5F-AVSF-IDCATGRP    TO W-IDCATGRP-N5                     
131200         MOVE WDN5F-AVSF-IDCATAVS    TO W-IDCATAVS-N5                     
131300         MOVE WDN5F-AVSF-IDCATRAD    TO W-IDCATRAD-N5                     
131400         MOVE WDN5F-AVSF-KDCATPUB-FOM TO W-KDCATPUB-N5                    
131500         PERFORM IMS-GHU-WDN501                                           
131600*        -- segment måste finnas, sekundärindex                           
131700         MOVE JA TO WDN5-AVS-FLAVSUST                                     
131800         PERFORM IMS-REPL-WDN5                                            
131900         PERFORM IMS-GHNP-WDN512                                          
132000*        -- segment måste finnas, sekundärindex                           
132100         MOVE 'Ä'            TO WDN5-RAD-KDRADST                          
132200         MOVE DAGENS-DATUM   TO WDN5-RAD-TIUPPDAT                         
132300         MOVE IDPGM          TO WDN5-RAD-IDUSER                           
132400         PERFORM IMS-REPL-WDN5                                            
132500*        --- Läs nästa sekundärindex av wdn526                            
132600         PERFORM IMS-GN-WDN5F1                                            
132700       END-PERFORM                                                        
132800     ELSE                                                                 
132900       CONTINUE                                                           
133000*      -- Fotnoten har tagits bort, FINNS EJ MER I KATALOG                
133100     END-IF                                                               
133200     .                                                                    
133300     EJECT                                                                
133400 S03C-UPPDATERA-WDN5-TIL  SECTION.                                        
133500     SKIP2                                                                
133600*    --- Läs sekundärindex av wdn521                                      
133700     PERFORM IMS-GN-WDN5D1                                                
133800     IF SEGMENT-FINNS                                                     
133900       PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                       
134000*        --- WDN5D läses med GN och samma nyckel (TIL) varje gång         
134100         MOVE WDN5D-AVSD-IDCATNR     TO W-IDCATNR-N5                      
134200         MOVE WDN5D-AVSD-IDCATGRP    TO W-IDCATGRP-N5                     
134300         MOVE WDN5D-AVSD-IDCATAVS    TO W-IDCATAVS-N5                     
134400         MOVE WDN5D-AVSD-IDCATRAD    TO W-IDCATRAD-N5                     
134500         MOVE WDN5D-AVSD-KDCATPUB-FOM TO W-KDCATPUB-N5                    
134600         PERFORM IMS-GHU-WDN501                                           
134700*        -- segment måste finnas, SEKUNDÄRINDEX                           
134800         MOVE JA TO WDN5-AVS-FLAVSUST                                     
134900         PERFORM IMS-REPL-WDN5                                            
135000         PERFORM IMS-GHNP-WDN512                                          
135100*        -- segment måste finnas, SEKUNDÄRINDEX                           
135200         MOVE 'Ä'            TO WDN5-RAD-KDRADST                          
135300         MOVE DAGENS-DATUM   TO WDN5-RAD-TIUPPDAT                         
135400         MOVE IDPGM          TO WDN5-RAD-IDUSER                           
135500         PERFORM IMS-REPL-WDN5                                            
135600*        --- Läs nästa sekundärindex av wdn521                            
135700         PERFORM IMS-GN-WDN5D1                                            
135800       END-PERFORM                                                        
135900     ELSE                                                                 
136000       CONTINUE                                                           
136100*      -- Tilläggstexten har tagits bort, FINNS EJ MER I KATALOG          
136200     END-IF                                                               
136300     .                                                                    
136400     EJECT                                                                
136500 S11-LAES-W15210  SECTION.                                                
136600     SKIP2                                                                
136700     READ W15210 INTO IN-AREA                                             
136800     AT END                                                               
136900        MOVE HIGH-VALUE TO IN-KDLEX                                       
137000        SET END-OF-W15210 TO TRUE                                         
137100                                                                          
137200     NOT AT END                                                           
137300        MOVE IN-KDLEX TO POSTSUM-TRANSTYP                                 
137400        MOVE 'W15210' TO POSTSUM-FDNAMN                                   
137500        MOVE 'W15212D1' TO POSTSUM-DDNAMN2                                
137600        CALL POSTSUM USING POSTSUM-PARM                                   
137700                                                                          
137800     END-READ                                                             
137900     .                                                                    
138000     EJECT                                                                
138100 S12-SKRIV-FELFIL-W15212  SECTION.                                        
138200     SKIP2                                                                
138300     WRITE  FEL-POST FROM W15212-AREA                                     
138400                                                                          
138500      MOVE IN-KDLEX  TO POSTSUM-TRANSTYP                                  
138600      MOVE 'W15212' TO POSTSUM-FDNAMN                                     
138700      MOVE 'W15212D2' TO POSTSUM-DDNAMN2                                  
138800      CALL POSTSUM USING POSTSUM-PARM                                     
138900                                                                          
139000     .                                                                    
139100     EJECT                                                                
139200 S14-SKRIV-MEMOFIL-W15214  SECTION.                                       
139300     SKIP2                                                                
139400     WRITE  W15214-MEMO-POST FROM W15214-AREA                             
139500                                                                          
139600      MOVE 'MEMO'    TO POSTSUM-TRANSTYP                                  
139700      MOVE 'W15214' TO POSTSUM-FDNAMN                                     
139800      MOVE 'W15212D3' TO POSTSUM-DDNAMN2                                  
139900      CALL POSTSUM USING POSTSUM-PARM                                     
140000                                                                          
140100     .                                                                    
140200     EJECT                                                                
140300 S15-SKRIV-SPSFIL-W15215  SECTION.                                        
140400     SKIP2                                                                
140500     WRITE  W15215-POST FROM W15215-AREA                                  
140600                                                                          
140700      MOVE 'SPS '    TO POSTSUM-TRANSTYP                                  
140800      MOVE 'W15215' TO POSTSUM-FDNAMN                                     
140900      MOVE 'W15212D4' TO POSTSUM-DDNAMN2                                  
141000      CALL POSTSUM USING POSTSUM-PARM                                     
141100                                                                          
141200     .                                                                    
141300     EJECT                                                                
141400 S99-ABEND   SECTION.                                                     
141500     SKIP2                                                                
141600     DISPLAY FELTEXT                                                      
141700     CALL FELLOG                                                          
141800     .                                                                    
141900     EJECT                                                                
142000 X-TAG-CHECKPOINT   SECTION.                                              
142100                                                                          
142200* --- AT CHECKPOINT YOU LOSE GN-POSITION IN DATABASE                      
142300* --- SAVE DATABASE KEYS IF NECESSARY                                     
142400     PERFORM IMS-CHECKPOINT                                               
142500     MOVE ZERO TO CHKP-ANT                                                
142600* --- REREAD DATABASE IF NEEDED                                           
142700     .                                                                    
142800     EJECT                                                                
142900* --- IMS SEKTIONER ---                                                   
143000     SKIP3                                                                
143100     EJECT                                                                
143200 IMS-GHU-WDN201   SECTION.                                                
143300                                                                          
143400     STRING 'WDN201  (IDRUBNR  =' W-IDRUBNR-X ')'                         
143500          DELIMITED BY SIZE INTO SSA1                                     
143600     MOVE '  GE' TO GODK-STATUSKODER                                      
143700     CALL CBLTDLI USING GHU WDN2-PCB DLI-IO-WDN201 SSA1                   
143800     MOVE WDN2-STATUS-CODE TO STATUS-WS                                   
143900     PERFORM IMS-STATUSKONTROLL                                           
144000     .                                                                    
144100     SKIP3                                                                
144200 IMS-GHNP-KVAL-WDN211    SECTION.                                         
144300                                                                          
144400     STRING 'WDN211  (WDN211KY =' W-WDN211KY-X ')'                        
144500          DELIMITED BY SIZE INTO SSA1                                     
144600     MOVE '  GE' TO GODK-STATUSKODER                                      
144700     CALL CBLTDLI USING GHNP WDN2-PCB DLI-IO-WDN211 SSA1                  
144800     MOVE WDN2-STATUS-CODE TO STATUS-WS                                   
144900     PERFORM IMS-STATUSKONTROLL                                           
145000     .                                                                    
145100     SKIP3                                                                
145200 IMS-ISRT-WDN211    SECTION.                                              
145300                                                                          
145400     STRING 'WDN201  (IDRUBNR  =' W-IDRUBNR-X ')'                         
145500          DELIMITED BY SIZE INTO SSA1                                     
145600     MOVE 'WDN211 ' TO SSA2                                               
145700     MOVE '  ' TO GODK-STATUSKODER                                        
145800     CALL CBLTDLI USING ISRT WDN2-PCB DLI-IO-WDN211 SSA1 SSA2             
145900     MOVE WDN2-STATUS-CODE TO STATUS-WS                                   
146000     PERFORM IMS-STATUSKONTROLL                                           
146100     ADD +1 TO CHKP-ANT                                                   
146200     .                                                                    
146300     SKIP3                                                                
146400 IMS-REPL-WDN211    SECTION.                                              
146500                                                                          
146600     MOVE '  ' TO GODK-STATUSKODER                                        
146700     CALL CBLTDLI USING REPL WDN2-PCB DLI-IO-WDN211                       
146800     MOVE WDN2-STATUS-CODE TO STATUS-WS                                   
146900     PERFORM IMS-STATUSKONTROLL                                           
147000     ADD +1 TO CHKP-ANT                                                   
147100     .                                                                    
147200     EJECT                                                                
147300 IMS-DLET-WDN211    SECTION.                                              
147400                                                                          
147500     MOVE '  ' TO GODK-STATUSKODER                                        
147600     CALL CBLTDLI USING DLET WDN2-PCB DLI-IO-WDN211                       
147700     MOVE WDN2-STATUS-CODE TO STATUS-WS                                   
147800     PERFORM IMS-STATUSKONTROLL                                           
147900     ADD +1 TO CHKP-ANT                                                   
148000     .                                                                    
148100     EJECT                                                                
148200 IMS-GHU-WDN301  SECTION.                                                 
148300                                                                          
148400     STRING 'WDN301  (IDFOTNR  =' W-IDFOTNR-X ')'                         
148500          DELIMITED BY SIZE INTO SSA1                                     
148600     MOVE '  GE' TO GODK-STATUSKODER                                      
148700     CALL CBLTDLI USING GHU WDN3-PCB DLI-IO-WDN301 SSA1                   
148800     MOVE WDN3-STATUS-CODE TO STATUS-WS                                   
148900     PERFORM IMS-STATUSKONTROLL                                           
149000     .                                                                    
149100     EJECT                                                                
149200 IMS-GNP-WDN311    SECTION.                                               
149300                                                                          
149400     MOVE   'WDN311  '        TO SSA1                                     
149500     MOVE '  GE' TO GODK-STATUSKODER                                      
149600     CALL CBLTDLI USING GNP WDN3-PCB DLI-IO-WDN311 SSA1                   
149700     MOVE WDN3-STATUS-CODE TO STATUS-WS                                   
149800     PERFORM IMS-STATUSKONTROLL                                           
149900     .                                                                    
150000     SKIP3                                                                
150100 IMS-GHU-WDN311   SECTION.                                                
150200                                                                          
150300     STRING 'WDN301  (IDFOTNR  =' W-IDFOTNR-X ')'                         
150400          DELIMITED BY SIZE INTO SSA1                                     
150500     STRING 'WDN311  (KDFORDON =' W-KDFORDON-X ')'                        
150600          DELIMITED BY SIZE INTO SSA2                                     
150700     MOVE '  ' TO GODK-STATUSKODER                                        
150800     CALL CBLTDLI USING GHU WDN3-PCB DLI-IO-WDN311 SSA1 SSA2              
150900     MOVE WDN3-STATUS-CODE TO STATUS-WS                                   
151000     PERFORM IMS-STATUSKONTROLL                                           
151100     .                                                                    
151200     EJECT                                                                
151300 IMS-GHNP-KVAL-WDN321    SECTION.                                         
151400                                                                          
151500     STRING 'WDN321  (WDN321KY =' W-WDN321KY-X ')'                        
151600          DELIMITED BY SIZE INTO SSA1                                     
151700     MOVE '  GE' TO GODK-STATUSKODER                                      
151800     CALL CBLTDLI USING GHNP WDN3-PCB DLI-IO-WDN321 SSA1                  
151900     MOVE WDN3-STATUS-CODE TO STATUS-WS                                   
152000     PERFORM IMS-STATUSKONTROLL                                           
152100     .                                                                    
152200     SKIP3                                                                
152300 IMS-ISRT-WDN321    SECTION.                                              
152400                                                                          
152500     STRING 'WDN311  (KDFORDON =' W-KDFORDON-X ')'                        
152600          DELIMITED BY SIZE INTO SSA1                                     
152700     MOVE 'WDN321 ' TO SSA2                                               
152800     MOVE '  ' TO GODK-STATUSKODER                                        
152900     CALL CBLTDLI USING ISRT WDN3-PCB DLI-IO-WDN321                       
153000                                      SSA1 SSA2                           
153100     MOVE WDN3-STATUS-CODE TO STATUS-WS                                   
153200     PERFORM IMS-STATUSKONTROLL                                           
153300     ADD +1 TO CHKP-ANT                                                   
153400     .                                                                    
153500     EJECT                                                                
153600 IMS-REPL-WDN321    SECTION.                                              
153700                                                                          
153800     MOVE '  ' TO GODK-STATUSKODER                                        
153900     CALL CBLTDLI USING REPL WDN3-PCB DLI-IO-WDN321                       
154000     MOVE WDN3-STATUS-CODE TO STATUS-WS                                   
154100     PERFORM IMS-STATUSKONTROLL                                           
154200     ADD +1 TO CHKP-ANT                                                   
154300     .                                                                    
154400     SKIP3                                                                
154500 IMS-DLET-WDN321    SECTION.                                              
154600                                                                          
154700     MOVE '  ' TO GODK-STATUSKODER                                        
154800     CALL CBLTDLI USING DLET WDN3-PCB DLI-IO-WDN321                       
154900     MOVE WDN3-STATUS-CODE TO STATUS-WS                                   
155000     PERFORM IMS-STATUSKONTROLL                                           
155100     ADD +1 TO CHKP-ANT                                                   
155200     .                                                                    
155300     EJECT                                                                
155400 IMS-GHU-WDN401     SECTION.                                              
155500                                                                          
155600     STRING 'WDN401  (IDTTEXNR =' W-IDTTEXNR-X ')'                        
155700          DELIMITED BY SIZE INTO SSA1                                     
155800     MOVE '  GE' TO GODK-STATUSKODER                                      
155900     CALL CBLTDLI USING GHU WDN4-PCB DLI-IO-WDN401 SSA1                   
156000     MOVE WDN4-STATUS-CODE TO STATUS-WS                                   
156100     PERFORM IMS-STATUSKONTROLL                                           
156200     .                                                                    
156300     SKIP3                                                                
156400 IMS-GHNP-WDN411    SECTION.                                              
156500                                                                          
156600     STRING 'WDN411  (IDSKYLT  =' W-IDSKYLT-X ')'                         
156700          DELIMITED BY SIZE INTO SSA1                                     
156800     MOVE '  GE' TO GODK-STATUSKODER                                      
156900     CALL CBLTDLI USING GHNP WDN4-PCB DLI-IO-WDN411 SSA1                  
157000     MOVE WDN4-STATUS-CODE TO STATUS-WS                                   
157100     PERFORM IMS-STATUSKONTROLL                                           
157200     .                                                                    
157300     SKIP3                                                                
157400 IMS-ISRT-WDN411    SECTION.                                              
157500                                                                          
157600     STRING 'WDN401  (IDTTEXNR =' W-IDTTEXNR-X ')'                        
157700          DELIMITED BY SIZE INTO SSA1                                     
157800     MOVE 'WDN411 ' TO SSA2                                               
157900     MOVE '  ' TO GODK-STATUSKODER                                        
158000     CALL CBLTDLI USING ISRT WDN4-PCB DLI-IO-WDN411 SSA1 SSA2             
158100     MOVE WDN4-STATUS-CODE TO STATUS-WS                                   
158200     PERFORM IMS-STATUSKONTROLL                                           
158300     ADD +1 TO CHKP-ANT                                                   
158400     .                                                                    
158500     SKIP3                                                                
158600 IMS-REPL-WDN411    SECTION.                                              
158700                                                                          
158800     MOVE '  ' TO GODK-STATUSKODER                                        
158900     CALL CBLTDLI USING REPL WDN4-PCB DLI-IO-WDN411                       
159000     MOVE WDN4-STATUS-CODE TO STATUS-WS                                   
159100     PERFORM IMS-STATUSKONTROLL                                           
159200     ADD +1 TO CHKP-ANT                                                   
159300     .                                                                    
159400     EJECT                                                                
159500 IMS-GHU-WDD301   SECTION.                                                
159600                                                                          
159700     STRING 'WDD301  (IDBENNR  =' W-IDBENNR-X ')'                         
159800          DELIMITED BY SIZE INTO SSA1                                     
159900     MOVE '  GE' TO GODK-STATUSKODER                                      
160000     CALL CBLTDLI USING GHU WDD3-PCB DLI-IO-WDD301 SSA1                   
160100     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
160200     PERFORM IMS-STATUSKONTROLL                                           
160300     .                                                                    
160400     SKIP3                                                                
160500 IMS-GU-WDD301   SECTION.                                                 
160600                                                                          
160700     STRING 'WDD301  (IDBENNR  =' W-IDBENNR-X ')'                         
160800          DELIMITED BY SIZE INTO SSA1                                     
160900     MOVE '  GE' TO GODK-STATUSKODER                                      
161000     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD301 SSA1                    
161100     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
161200     PERFORM IMS-STATUSKONTROLL                                           
161300     .                                                                    
161400     SKIP3                                                                
161500 IMS-GHNP-WDD311-UNIK    SECTION.                                         
161600                                                                          
161700     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
161800          DELIMITED BY SIZE INTO SSA1                                     
161900     MOVE '  GE' TO GODK-STATUSKODER                                      
162000     CALL CBLTDLI USING GHNP WDD3-PCB DLI-IO-WDD311 SSA1                  
162100     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
162200     PERFORM IMS-STATUSKONTROLL                                           
162300     .                                                                    
162400     SKIP3                                                                
162500 IMS-GHNP-WDD311         SECTION.                                         
162600                                                                          
162700     MOVE  'WDD311  '         TO SSA1                                     
162800     MOVE '  GE' TO GODK-STATUSKODER                                      
162900     CALL CBLTDLI USING GHNP WDD3-PCB DLI-IO-WDD311 SSA1                  
163000     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
163100     PERFORM IMS-STATUSKONTROLL                                           
163200     .                                                                    
163300     SKIP3                                                                
163400 IMS-GNP-WDD312         SECTION.                                          
163500                                                                          
163600     MOVE 'WDD312  '          TO SSA1                                     
163700     MOVE '  GE' TO GODK-STATUSKODER                                      
163800     CALL CBLTDLI USING GNP WDD3-PCB DLI-IO-WDD312 SSA1                   
163900     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
164000     PERFORM IMS-STATUSKONTROLL                                           
164100     .                                                                    
164200     SKIP3                                                                
164300 IMS-ISRT-WDD311    SECTION.                                              
164400                                                                          
164500     STRING 'WDD301  (IDBENNR  =' W-IDBENNR-X ')'                         
164600          DELIMITED BY SIZE INTO SSA1                                     
164700     MOVE 'WDD311 ' TO SSA2                                               
164800     MOVE '  ' TO GODK-STATUSKODER                                        
164900     CALL CBLTDLI USING ISRT WDD3-PCB DLI-IO-WDD311 SSA1 SSA2             
165000     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
165100     PERFORM IMS-STATUSKONTROLL                                           
165200     ADD +1 TO CHKP-ANT                                                   
165300     .                                                                    
165400     SKIP3                                                                
165500 IMS-REPL-WDD301    SECTION.                                              
165600                                                                          
165700     MOVE '  ' TO GODK-STATUSKODER                                        
165800     CALL CBLTDLI USING REPL WDD3-PCB DLI-IO-WDD301                       
165900     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
166000     PERFORM IMS-STATUSKONTROLL                                           
166100     ADD +1 TO CHKP-ANT                                                   
166200     .                                                                    
166300     EJECT                                                                
166400 IMS-REPL-WDD311    SECTION.                                              
166500                                                                          
166600     MOVE '  ' TO GODK-STATUSKODER                                        
166700     CALL CBLTDLI USING REPL WDD3-PCB DLI-IO-WDD311                       
166800     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
166900     PERFORM IMS-STATUSKONTROLL                                           
167000     ADD +1 TO CHKP-ANT                                                   
167100     .                                                                    
167200     EJECT                                                                
167300 IMS-GET-WDR201 SECTION.                                                  
167400                                                                          
167500     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-X ')'                         
167600          DELIMITED BY SIZE INTO SSA1                                     
167700     MOVE '  GE' TO GODK-STATUSKODER                                      
167800     CALL CBLTDLI USING GU WDR2-PCB DLI-IO-WDR201 SSA1                    
167900     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
168000     PERFORM IMS-STATUSKONTROLL                                           
168100     .                                                                    
168200     EJECT                                                                
168300 IMS-GET-WDGX1206 SECTION.                                                
168400                                                                          
168500     STRING 'WDGX1206(KDLEX    =' W-KDLEX-X                               
168600                OCH  'IDLEXNR  =' W-IDLEXNR-X ')'                         
168700          DELIMITED BY SIZE INTO SSA1                                     
168800     MOVE '  GE' TO GODK-STATUSKODER                                      
168900     CALL CBLTDLI USING GHNP WDR2-PCB DLI-IO-WDGX1206 SSA1                
169000     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
169100     PERFORM IMS-STATUSKONTROLL                                           
169200     .                                                                    
169300     SKIP3                                                                
169400 IMS-REPL-WDGX1206 SECTION.                                               
169500                                                                          
169600     MOVE '  ' TO GODK-STATUSKODER                                        
169700     CALL CBLTDLI USING REPL WDR2-PCB DLI-IO-WDGX1206                     
169800     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
169900     PERFORM IMS-STATUSKONTROLL                                           
170000     ADD +1 TO CHKP-ANT                                                   
170100     .                                                                    
170200     EJECT                                                                
170300 IMS-DLET-WDGX1206 SECTION.                                               
170400                                                                          
170500     MOVE '  ' TO GODK-STATUSKODER                                        
170600     CALL CBLTDLI USING DLET WDR2-PCB DLI-IO-WDGX1206                     
170700     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
170800     PERFORM IMS-STATUSKONTROLL                                           
170900     ADD +1 TO CHKP-ANT                                                   
171000     .                                                                    
171100     EJECT                                                                
171200 IMS-GHU-WDN501 SECTION.                                                  
171300     STRING 'WDN501  (WDN501KY =' W-WDN501KY-X ')'                        
171400            DELIMITED BY SIZE INTO SSA1                                   
171500     MOVE '  ' TO GODK-STATUSKODER                                        
171600     CALL CBLTDLI USING GHU WDN5-PCB DLI-IO-WDN5 SSA1                     
171700     MOVE WDN5-STATUS-CODE TO STATUS-WS                                   
171800     PERFORM IMS-STATUSKONTROLL                                           
171900     .                                                                    
172000     SKIP3                                                                
172100 IMS-GHNP-WDN512 SECTION.                                                 
172200     STRING 'WDN512  (WDN512KY =' W-WDN512KY-X ')'                        
172300            DELIMITED BY SIZE INTO SSA1                                   
172400     MOVE '  ' TO GODK-STATUSKODER                                        
172500     CALL CBLTDLI USING GHNP WDN5-PCB DLI-IO-WDN5   SSA1                  
172600     MOVE WDN5-STATUS-CODE TO STATUS-WS                                   
172700     PERFORM IMS-STATUSKONTROLL                                           
172800     .                                                                    
172900     SKIP3                                                                
173000 IMS-REPL-WDN5 SECTION.                                                   
173100     MOVE '  ' TO GODK-STATUSKODER                                        
173200     CALL CBLTDLI USING REPL WDN5-PCB DLI-IO-WDN5                         
173300     MOVE WDN5-STATUS-CODE TO STATUS-WS                                   
173400     PERFORM IMS-STATUSKONTROLL                                           
173500     ADD +1 TO CHKP-ANT                                                   
173600     .                                                                    
173700     SKIP3                                                                
173800 IMS-GN-WDN5D1 SECTION.                                                   
173900     STRING 'WDN5D1  (WDN5D1KY>=' W-IDTTEXNR-D-X                          
174000                                  21-LOW-VALUE                            
174100                 OCH 'WDN5D1KY<=' W-IDTTEXNR-D-X                          
174200                                  21-HIGH-VALUE  ')'                      
174300            DELIMITED BY SIZE INTO SSA1                                   
174400     MOVE '  GBGE' TO GODK-STATUSKODER                                    
174500     CALL CBLTDLI USING GN WDN5D-PCB DLI-IO-WDN5D1 SSA1                   
174600     MOVE WDN5D-STATUS-CODE TO STATUS-WS                                  
174700     PERFORM IMS-STATUSKONTROLL                                           
174800     .                                                                    
174900     EJECT                                                                
175000 IMS-GN-WDN5E1 SECTION.                                                   
175100     STRING 'WDN5E1  (WDN5E1KY>=' W-IDRUBNR-E-X                           
175200                                  21-LOW-VALUE                            
175300                 OCH 'WDN5E1KY<=' W-IDRUBNR-E-X                           
175400                                  21-HIGH-VALUE  ')'                      
175500            DELIMITED BY SIZE INTO SSA1                                   
175600     MOVE '  GBGE' TO GODK-STATUSKODER                                    
175700     CALL CBLTDLI USING GN WDN5E-PCB DLI-IO-WDN5E1 SSA1                   
175800     MOVE WDN5E-STATUS-CODE TO STATUS-WS                                  
175900     PERFORM IMS-STATUSKONTROLL                                           
176000     .                                                                    
176100     EJECT                                                                
176200 IMS-GN-WDN5F1 SECTION.                                                   
176300     SKIP2                                                                
176400     STRING 'WDN5F1  (WDN5F1KY>=' W-IDFOTNR-F-X                           
176500                                  21-LOW-VALUE                            
176600                 OCH 'WDN5F1KY<=' W-IDFOTNR-F-X                           
176700                                  21-HIGH-VALUE ')'                       
176800            DELIMITED BY SIZE INTO SSA1                                   
176900     MOVE '  GBGE' TO GODK-STATUSKODER                                    
177000     CALL CBLTDLI USING GN WDN5F-PCB DLI-IO-WDN5F1 SSA1                   
177100     MOVE WDN5F-STATUS-CODE TO STATUS-WS                                  
177200     PERFORM IMS-STATUSKONTROLL                                           
177300     .                                                                    
177400     SKIP3                                                                
177500 IMS-GU-WDK601 SECTION.                                                   
177600                                                                          
177700     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
177800            DELIMITED BY SIZE INTO SSA1                                   
177900     MOVE '  ' TO GODK-STATUSKODER                                        
178000     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
178100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
178200     PERFORM IMS-STATUSKONTROLL                                           
178300     .                                                                    
178400     SKIP3                                                                
178500 IMS-RESTART SECTION.                                                     
178600     SKIP2                                                                
178700     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
178800     MOVE '  ' TO GODK-STATUSKODER                                        
178900     CALL CBLTDLI USING XRST MSG-PCB                                      
179000                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
179100                        CHKP-AREA-LENGTH CHKP-AREA                        
179200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
179300     PERFORM IMS-STATUSKONTROLL                                           
179400     .                                                                    
179500     EJECT                                                                
179600 IMS-CHECKPOINT SECTION.                                                  
179700     SKIP2                                                                
179800     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
179900     MOVE '  XD' TO GODK-STATUSKODER                                      
180000     CALL CBLTDLI USING CHKP MSG-PCB                                      
180100                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
180200                        CHKP-AREA-LENGTH CHKP-AREA                        
180300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
180400     PERFORM IMS-STATUSKONTROLL                                           
180500                                                                          
180600     IF IMS-EJ-OK                                                         
180700       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
180800       DISPLAY FELTEXT                                                    
180900       CALL FELLOG                                                        
181000     END-IF                                                               
181100     .                                                                    
181200     EJECT                                                                
181300 IMS-STATUSKONTROLL SECTION.                                              
181400     SKIP2                                                                
181500     SET STATUS-IX TO 1                                                   
181600     SEARCH GODK-STATUS                                                   
181700       AT END                                                             
181800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
181900           DELIMITED BY SIZE INTO FELTEXT-STR                             
182000         DISPLAY FELTEXT                                                  
182100         CALL FELLOG                                                      
182200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
182300         CONTINUE                                                         
182400     END-SEARCH                                                           
182500     .                                                                    
