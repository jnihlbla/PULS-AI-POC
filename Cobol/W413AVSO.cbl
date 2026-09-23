000101*COMPOPT STDSUB=YES                                                       
000201 ID DIVISION.                                                             
000301     SKIP2                                                                
000401 PROGRAM-ID.     W413AVSO.                                                
000501 AUTHOR.         LASSE CALAIS.                                            
000601 DATE-WRITTEN.   JUNI -90.                                                
000701                                                                          
000801     REMARKS.                                                             
000901*                                                                         
001001*    DETTA ÄR EN SUBMODUL SOM ANROPAS VID ORDERAVSLUT.                    
001101*    ARBETSTABELLEN UPPDATERAS MED PRC FÖR RESP LO SAMT ORDER-            
001201*    DELAR SKAPAS FÖR ALLA PRC.                                           
001301*    ORDERDELAR SKAPAS ENDAST OM ARBETSTABELLEN (Q212) HAR                
001401*    TITRPAVT ELLER TIRFS >0 ELLER OM DETTA ÄR SATT I LÄNKAREAN.          
001501*    OM IDDC ÄR >0 I LÄNKAREAN, GÖRS ENDAST ORDERAVSLUT FÖR               
001601*    DETTA C-LAGER.                                                       
001701*                                                                         
001801*    REGISTER :    MLORQI (WDQ2)    ORDERHUVUD.                           
001901*                  WLGMTB (WDB3)    KUNDREGISTRET DC-SEGM.                
002001*                  WLORQA (WDQ3)    ORDERDELAR.                           
002101*                  WLXXKA (HTR4432) LAST/ADM TID.                         
002201*                  WL4437 (HTR4437) ARBETSTID-KALENDER                    
002301*                  WLXXKE (HTR4441) HLO-TABELL.                           
002401*                  WLXXKF (HTR4443) P-KLASS TABELL.                       
002501*                  WLXXKG (HTR4445) PRC-STYR TABELL.                      
002601*                  WLXXKH (HTR4447) PRC TABELL.                           
002701*                  WLXXKI (HTR4451) PTID TABELL.                          
002801*                  WLXXKP (HTR4525) PRODUKTIONSNR.                        
002901*                         (WDB2)    KUNDREGISTER GODSMOTTAGARE            
003001*                         (WDB6)    DC-REGISTER.                          
003101*                                                                         
003201*    LÄNKAREA :    W413AVSO                                               
003301*                                                                         
003401*    SUBPGM   :    W411TIME    W411ORDN                                   
003501*    LÄNKAREA :    W411TIME    W411ORDN                                   
003601*                                                                         
003701* HÖSTEN 2004 GÖRAN KJELLSON                                              
003801* ETRACKER 887753                                                         
003901*                                                                         
004001*  SEPT 2005 LINDA NILSSON                                                
004101*  ETRACKER 1334295                                                       
004201*  STORY 2375089 / ADD IDSYSTEM VOUI, ECOM                                
004301*                                                                         
004401                                                                          
004501     SKIP3                                                                
004601 ENVIRONMENT DIVISION.                                                    
004701     EJECT                                                                
004801 DATA DIVISION.                                                           
004901 WORKING-STORAGE SECTION.                                                 
005001*    -COPY WY2000WB                                                       
005101                                                                          
005201*    -COPY WY2000WC                                                       
005301     SKIP3                                                                
005401 01  FELTEXT                     PIC X(80)     VALUE SPACE.               
005501 01  FILLER                      PIC X(16)     VALUE 'KONSTANTER'.        
005601 01  IDPGM                       PIC X(08)      VALUE 'W413AVSO'.         
005900 01  CURR-IMS-SECTION            PIC X(16)      VALUE SPACE.              
005901 01  JA                          PIC X          VALUE 'J'.                
005902 01  NEJ                         PIC X          VALUE 'N'.                
005903 01  OKAY                        PIC X          VALUE '0'.                
006001 01  EJ-OKAY                     PIC X          VALUE '1'.                
006101 01  KILO                        PIC X(2)       VALUE 'KG'.               
006201 01  VOLYM                       PIC X(2)       VALUE 'M3'.               
006301 01  RADER                       PIC X(2)       VALUE 'RA'.               
006401 01  STYCK                       PIC X(2)       VALUE 'ST'.               
006501 01  MEDELSTYCK                  PIC X(2)       VALUE 'MS'.               
006601 01  EN-TIMMA                    PIC S9(4)V9(2) VALUE 0.6 COMP-3.         
006701     EJECT                                                                
006801*                                                                         
006901 01  FILLER                      PIC X(16)      VALUE 'WS-VÄRDEN'.        
007001 01  WS-BEVARREF                 PIC X(10).                               
007101 01  WS-FLODELUT                 PIC X.                                   
007201 01  WS-WDB6-IDFTG               PIC 9(2)       VALUE ZERO.               
007301 01  WS-FLORDSPE                 PIC X.                                   
007401 01  WS-IDSYSTEM                 PIC X(4).                                
007501 01  WS-FLOVRLEV                 PIC X.                                   
007601 01  WS-IDDISTR                  PIC S9(5)      COMP-3.                   
007701 01  WS-IDGMTOMR                 PIC 9(4).                                
007801 01  WS-IDHLOTAB                 PIC 9(2).                                
007901 01  WS-IDKUNDNR                 PIC S9(7)      COMP-3.                   
008001 01  WS-IDLEVNR                  PIC  X(5).                               
008101 01  WS-IDORDER                  PIC S9(7)      COMP-3.                   
008201 01  WS-IDPLKLST                 PIC S9(3)      VALUE +0 COMP-3.          
008301 01  WS-IDPKLTAB                 PIC X(2).                                
008401 01  WS-IDPRC                    PIC X(4).                                
008501 01  WS-IDPRCTAB                 PIC 9(2).                                
008601 01  WS-IDPRODNR                 PIC S9(7)      VALUE +0 COMP-3.          
008701 01  WS-IDTRP                    PIC X(5).                                
008801 01  WS-KDTRPKAT                 PIC X(1).                                
008901 01  WS-IDTRPLOS                 PIC X(3).                                
009001 01  WS-IDTRPVAR                 PIC X(2).                                
009101 01  WS-KDFRAKT                  PIC S9(3)      COMP-3.                   
009201 01  WS-KDORDKL                  PIC S9         COMP-3.                   
009301 01  WS-KDFDKRAV                 PIC S9(3).                               
009401 01  WS-KDPRODKL                 PIC X          VALUE SPACE.              
009501 01  WS-KVADMEL                  PIC S9(3)V9(2) COMP-3.                   
009601 01  WS-KVADMFL                  PIC S9(3)V9(2) COMP-3.                   
009701 01  WS-KVANTART                 PIC S9(5)      COMP-3.                   
009801 01  WS-KVLASTTI                 PIC S9(3)V9(2) COMP-3.                   
009901 01  WS-KVRADER                  PIC S9(5)      COMP-3.                   
010001 01  WS-KVSEMBRA                 PIC S9(3)      COMP-3.                   
010101 01  WS-RFS                      PIC 9(10).                               
010201 01  WS-SUORDV                   PIC S9(9)V9(2) COMP-3.                   
010301 01  WS-SUORDV-LOC               PIC S9(9)V9(2) COMP-3.                   
010401 01  WS-SUORDV-LOCPREL           PIC S9(9)V9(2) COMP-3.                   
010501 01  WS-KDVALISO                 PIC X(3) VALUE SPACE.                    
010601 01  WS-SUHANTII                 PIC S9(5)V9(2) VALUE ZERO COMP-3.        
010701 01  WS-TILST-O                  PIC S9(11)     COMP-3.                   
010801 01  WS-TILOKDAT                 PIC 9(6) VALUE ZERO.                     
010901 01  WS-TILOKTID                 PIC 9(4) VALUE ZERO.                     
011001 01  WS-TIREGDAT                 PIC S9(7)      COMP-3.                   
011101 01  WS-TIREGTID                 PIC S9(7)      COMP-3.                   
011201 01  WS-TIRFS-X.                                                          
011301     03 WS-TIRFS                 PIC 9(10).                               
011401 01  WS-TIRFS-X2 REDEFINES WS-TIRFS-X.                                    
011501     03 WS-TIRFS-DATUM           PIC 9(06).                               
011601     03 WS-TIRFS-KLOCKA          PIC 9(04).                               
011701 01  WS-DARFS                    PIC 9(12).                               
011801 01  FILLER REDEFINES WS-DARFS.                                           
011901     03 WS-DARFS-DATUM.                                                   
012001        05 WS-DARFS-SEKEL        PIC 9(2).                                
012101        05 WS-DARFS-AAMMDD       PIC 9(6).                                
012201     03 WS-DARFS-KLOCKA          PIC 9(4).                                
012301                                                                          
012401 01  WS-HELP-TIAAAAMMDD          PIC 9(8).                                
012501 01  WS-HELP-TIHHMM              PIC 9(4).                                
012601                                                                          
012701 01  WS-TIMESTAMP.                                                        
012801     03  WS-YEAR                 PIC X(4)    VALUE SPACE.                 
012901     03  FILLER                  PIC X       VALUE '-'.                   
013001     03  WS-MONTH                PIC X(2)    VALUE SPACE.                 
013101     03  FILLER                  PIC X       VALUE '-'.                   
013201     03  WS-DAY                  PIC X(2)    VALUE SPACE.                 
013301     03  FILLER                  PIC X       VALUE '-'.                   
013401     03  WS-HOUR                 PIC X(2)    VALUE SPACE.                 
013501     03  FILLER                  PIC X       VALUE '.'.                   
013601     03  WS-MINUTE               PIC X(2)    VALUE SPACE.                 
013701     03  FILLER                  PIC X       VALUE '.'.                   
013801     03  WS-SECOND               PIC X(2)    VALUE SPACE.                 
013901     03  FILLER                  PIC X       VALUE '.'.                   
014001     03  WS-DECIMAL              PIC X(2)    VALUE SPACE.                 
014101     03  FILLER                  PIC X(4)    VALUE '0000'.                
014201                                                                          
014301 01  WS-CURR-DATE                PIC 9(8)    VALUE ZERO.                  
014401 01  FILLER REDEFINES WS-CURR-DATE.                                       
014501     03  WS-CURR-SEK             PIC X(2).                                
014601     03  WS-CURR-TIAAMMDD        PIC 9(6).                                
014701     03  FILLER REDEFINES WS-CURR-TIAAMMDD.                               
014801         05  WS-CURR-YEAR        PIC X(2).                                
014901         05  WS-CURR-MONTH       PIC X(2).                                
015001         05  WS-CURR-DAY         PIC X(2).                                
015101                                                                          
015201 01  WS-CURR-TIHHMM              PIC 9(4)    VALUE ZERO.                  
015301 01  FILLER REDEFINES WS-CURR-TIHHMM.                                     
015401     03  WS-CURR-HOUR            PIC X(2).                                
015501     03  WS-CURR-MINUTE          PIC X(2).                                
015601                                                                          
015701 01  WS-VKORDNTO                 PIC S9(6)V9(5) COMP-3.                   
015801 01  WS-VLORDNTO                 PIC S9(4)V9(5) COMP-3.                   
015901 01  DC-INIT                     PIC S9(2)      VALUE +01.                
016001 01  DC-MAX                      PIC S9(2)      VALUE +14.                
016101     EJECT                                                                
016201*                                                                         
016301 01  FILLER                      PIC X(16)      VALUE 'DEFAULTID'.        
016401 01  DEFAULT-IDHLOTAB-1          PIC 9(2)       VALUE 1.                  
016501 01  DEFAULT-IDPKLTAB-01         PIC X(2)       VALUE '01'.               
016601 01  DEFAULT-IDPRCTAB-1          PIC 9(2)       VALUE 1.                  
016701 01  DEFAULT-IDPTIDTAB-1         PIC 9(2)       VALUE 1.                  
016801 01  SPECORD-IDPRC-9997          PIC X(4)       VALUE '9997'.             
016901 01  DIRLEV-IDPRC-9998           PIC X(4)       VALUE '9998'.             
017001 01  DEFAULT-IDPRC-9999          PIC X(4)       VALUE '9999'.             
017101 01  W480-IDPRC-999M             PIC X(4)       VALUE '999M'.             
017201 01  UNDANTAG-VID-VAL-AV-PRC     PIC X.                                   
017301 01  IDUSER-NOLL                 PIC X(8)       VALUE '00000000'.         
017401*                                                                         
017501 01  FILLER                      PIC X(16)      VALUE 'JFR-FÄLT'.         
017601 01  X-KVPTSORT-JFR              PIC S9(4)V9    VALUE +0 COMP-3.          
017701 01  Y-KVPTSORT-JFR              PIC S9(4)V9    VALUE +0 COMP-3.          
017801     EJECT                                                                
017901*                                                                         
018001 01  FILLER                      PIC X(16)      VALUE 'SWITCHAR'.         
018101 01  SW-TRAEFF                   PIC X.                                   
018201 01  SW-X-SORT-FUNNEN            PIC X.                                   
018301 01  SW-Y-SORT-FUNNEN            PIC X.                                   
018401 01  SW-PTID                     PIC X.                                   
018801 01  SW-RFS-EL-TRP               PIC X.                                   
018901     88 RFS-EL-TRP-AENDRING                     VALUE 'J'.                
019001 01  SW-IDDC-TRAEFF              PIC X.                                   
019101     88 IDDC-TRAEFF                             VALUE 'J'.                
019201 01  SW-UPPDAT-WDE601            PIC X.                                   
019301     88 EJ-UPPDAT-WDE601                        VALUE 'N'.                
019401     88 UPPDAT-WDE601                           VALUE 'J'.                
019501 01  K-TRAN-KDSVAR-0-OK          PIC  X(1)      VALUE '0'.                
019601     EJECT                                                                
019701*                                                                         
019801 01  FILLER                      PIC X(16)      VALUE 'INDEX'.            
019901 01  LO-IX                       PIC S9(9)      VALUE +0 COMP-3.          
020001 01  PRC-IX                      PIC S9(9)      VALUE +0 COMP-3.          
020101 01  LAGO-IX                     PIC S9(9)      VALUE +0 COMP-3.          
020201 01  W-4452-Y-IX                 PIC S9(9)      VALUE +0 COMP-3.          
020301 01  W-4452-X-IX                 PIC S9(9)      VALUE +0 COMP-3.          
020401     EJECT                                                                
020501*                                                                         
020601 01  FILLER                      PIC X(16)      VALUE 'TIDER'.            
020701 01  WS-TITRPAVT.                                                         
020801     03  TRP-DATRPAVD            PIC 9(8)       VALUE ZERO.               
020901     03  TRP-TIHHMMSS.                                                    
021001       05  TRP-TIHHMM            PIC 9(4)       VALUE ZERO.               
021101       05  TRP-TISS              PIC 9(2)       VALUE ZERO.               
021201     SKIP2                                                                
021301 01  TIDER.                                                               
021401     03  WS-TIAAMMDD             PIC 9(6)       VALUE ZERO.               
021501     03  WS-TIHHMMSS             PIC 9(6)       VALUE ZERO.               
021601 01  WS-TIHHMM-SS    REDEFINES TIDER.                                     
021701     03  TID-UTAN-SEK.                                                    
021801       05  TIAAMMDD              PIC 9(6).                                
021901       05  TIHHMM                PIC 9(4).                                
022001     03  SEKUNDER                PIC 9(2).                                
022101*                                                                         
022201 01  WS-TITRPAVT-HELP            PIC 9(8)       VALUE ZERO.               
022301*                                                                         
022401 01  RFS-JFR.                                                             
022501     03  RFS-DATE                PIC 9(6)       VALUE ZERO.               
022601     03  RFS-TIME                PIC 9(4)       VALUE ZERO.               
022701*                                                                         
022801 01  LST-JFR                     PIC 9(10)      VALUE ZERO.               
022901*                                                                         
023001 01  TEMP-SUPMIN                 PIC S9(7)      COMP-3.                   
023101 01  TEMP-SUPTIM                 PIC S9(5)      COMP-3.                   
023201 01  TEMP-SUPTID                 PIC S9(3)V9(2) COMP-3.                   
023301*                                                                         
023401 01  SUP-TID                     PIC S9(4)V9(2) COMP-3.                   
023501 01  SUP-TIM                     PIC S9(4)      COMP-3.                   
023601*                                                                         
023701 01  WAIT-TID                    PIC S9(4)V9(2) COMP-3.                   
023801 01  WAIT-TIM                    PIC S9(4)      COMP-3.                   
023901     EJECT                                                                
024001 01  TEST-IDDISTR                PIC S9(5)   COMP-3 VALUE ZERO.           
024101*    --- SCRAP DISTRICTS                                                  
024201*01  FILLER   -COPY WWDIST18    -RED TEST-IDDISTR.                        
024301*    --- REFILL DISTRICTS                                                 
024401*01  FILLER   -COPY WWDIST35    -RED TEST-IDDISTR.                        
024501     EJECT                                                                
024601*    --- VALID IDDC CODES                                                 
024701*01  -COPY WWDC99                                                         
024801*01  -COPY WWDCKONS                                                       
024901*    --- VALID COMPANY                                                    
025001*01  -COPY WWIDFTG                                                        
025101     SKIP3                                                                
025201*                                                                         
025301 01  GENERELLA-SUBPROGRAM.                                                
025401     03  CBLTDLI                 PIC X(8)       VALUE 'CBLTDLI '.         
025501     03  FELLOG                  PIC X(8)       VALUE 'FELLOG  '.         
025601     03  W411TIME                PIC X(8)       VALUE 'W411TIME'.         
025701     03  W411ORDN                PIC X(8)       VALUE 'W411ORDN'.         
025801     03  W411TRAN                PIC X(8)       VALUE 'W411TRAN'.         
025901     03  W005INIT                PIC X(8)       VALUE 'W005INIT'.         
026001     EJECT                                                                
026101                                                                          
026201*    --- PARAMETRAR TILL GENERELLA SUBPROGRAM                             
026301 01 FILLER                       PIC X(8) VALUE 'W005INIT'.               
026401*   -COPY WMSGINIT                                                        
026501     EJECT                                                                
026601*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
026701*                                                                         
026801 01  FILLER                      PIC X(16)      VALUE 'IMS-WS'.           
026901     SKIP2                                                                
027001*    --- STATUS-KOD FRÅN IMS                                              
027101 01  STATUS-WS                   PIC XX.                                  
027201     88  SEGMENT-FINNS                          VALUE '  '.               
027301     88  BASEN-SLUT                             VALUE 'GB'.               
027401     88  SEGMENT-SAKNAS                         VALUE 'GE'.               
027501     88  SEGMENT-FINNS-REDAN                    VALUE 'II'.               
027601     SKIP2                                                                
027701 01  GODK-STATUSKODER.                                                    
027801     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
027901     SKIP2                                                                
028001 01  SSA1                        PIC X(300).                              
028101 01  SSA2                        PIC X(128).                              
028201 01  SSA3                        PIC X(64).                               
028301     EJECT                                                                
028401                                                                          
028501*    --- IMS FUNKTIONSKODER                                               
028601*01  -COPY W0003                                                          
028701     EJECT                                                                
028801*                                                                         
028901 01  FILLER                    PIC X(16) VALUE 'NYCKLAR-TILL-DLI'.        
029001 01  NYCKLAR-TILL-DLI.                                                    
029101                                                                          
029201     03 W-WDB301KY-X.                                                     
029301         05 W-B301-IDDC             PIC X(2).                             
029401         05 W-B301-IDDISTR          PIC S9(5)   COMP-3.                   
029501         05 W-B301-IDKUNDNR         PIC S9(7)   COMP-3.                   
029601*                                                                         
029701     03 W-WDB301KY-DEFAULT-X.                                             
029801         05 W-B301-IDDC-DEFAULT     PIC X(2).                             
029901         05 W-B301-IDDISTR-DEFAULT  PIC S9(5)   COMP-3.                   
030001         05 W-B301-IDKUNDNR-DEFAULT PIC S9(7)                             
030101                                    VALUE 9999999 COMP-3.                 
030201     03  W-IDDISTR-X.                                                     
030301         05  W-IDDISTR           PIC  S9(5)    COMP-3.                    
030710                                                                          
030810     03  W-IDGMTOMR-X.                                                    
030910         05  W-IDGMTOMR          PIC  9(4).                               
031010                                                                          
031110     03  W-IDHLO-X.                                                       
031210         05  W-IDHLO             PIC  9(2).                               
031310                                                                          
031410     03  W-IDKUNDNR-X.                                                    
031510         05  W-IDKUNDNR          PIC  S9(7)    COMP-3.                    
031610                                                                          
031710     03  W-WDQ211KY-X.                                                    
031810         05  W-IDDC-Q211         PIC  X(2)     VALUE SPACE.               
031910         05  W-IDLEVNR-Q211      PIC  X(5).                               
032010                                                                          
032110     03  W-WDQ211KY-MIN-X.                                                
032210         05  W-IDDC-Q211-MIN     PIC  X(2)     VALUE SPACE.               
032310         05  W-IDLEVNR-Q211-MIN  PIC  X(5).                               
032410                                                                          
032510     03  W-WDQ211KY-MAX-X.                                                
032610         05  W-IDDC-Q211-MAX     PIC  X(2)     VALUE SPACE.               
032710         05  W-IDLEVNR-Q211-MAX  PIC  X(5).                               
032810                                                                          
032910     03  W-IDLEVNR-X.                                                     
033010         05  W-IDLEVNR           PIC  X(5).                               
033110                                                                          
033210     03  W-IDORDER-X.                                                     
033310         05  W-IDORDER           PIC  S9(7)    COMP-3.                    
033410                                                                          
033510     03  W-IDPRC-X.                                                       
033610         05  W-IDPRC             PIC  X(4).                               
033710                                                                          
033810     03  W-IDPRODNR-X.                                                    
033910         05  W-IDPRODNR          PIC  S9(7)    COMP-3.                    
034010                                                                          
034110     03  W-IDTRP-X.                                                       
034210         05  W-IDTRP             PIC  X(5).                               
034310                                                                          
034410     03  W-IDTRP-BLANK-X.                                                 
034510         05  W-IDTRP-BLANK       PIC  X(5)     VALUE SPACE.               
034610                                                                          
034710     EJECT                                                                
034810     03  W-IDDC-X.                                                        
034910         05  W-IDDC              PIC  X(2).                               
035010                                                                          
035110     03  W-KDFRAKT-X.                                                     
035210         05  W-KDFRAKT           PIC  S9(3)    COMP-3.                    
035310                                                                          
035410     03  W-KDODELST-X.                                                    
035510         05  W-KDODELST          PIC  X.                                  
035610                                                                          
035710     03  W-KDORDKL-X.                                                     
035810         05  W-KDORDKL           PIC  S9       COMP-3.                    
035910                                                                          
036010     03  W-KDSEGKEY-X.                                                    
036110         05  W-KDSEGKEY          PIC  X        VALUE '1'.                 
036210                                                                          
036310     03  W-KDPRODKL-X.                                                    
036410         05  W-KDPRODKL          PIC  X.                                  
036510                                                                          
036610     03  W-KVRADER-X.                                                     
036710         05  W-KVRADER           PIC S9(5)     COMP-3.                    
036810                                                                          
036910     03  W-VKORDNTO-X.                                                    
037010         05  W-VKORDNTO          PIC S9(6)V9   COMP-3.                    
037110                                                                          
037210     03  W-VLORDNTO-X.                                                    
037310         05  W-VLORDNTO          PIC S9(4)V9(3) COMP-3.                   
037410                                                                          
037510     EJECT                                                                
037610                                                                          
037710     03  W-WDQ301KY-FOM.                                                  
037810         05  W-Q3-IDORDERF       PIC  S9(7)    COMP-3.                    
037910         05  W-Q3-IDDCF          PIC  X(2).                               
038010         05  W-Q3-IDPRODNRF      PIC  S9(7)    COMP-3.                    
038110         05  FILLER              PIC  X(2)     VALUE LOW-VALUE.           
038210                                                                          
038310     03  W-WDQ301KY-TOM.                                                  
038410         05  W-Q3-IDORDERT       PIC  S9(7)    COMP-3.                    
038510         05  W-Q3-IDDCT          PIC  X(2).                               
038610         05  W-Q3-IDPRODNRT      PIC  S9(7)    COMP-3.                    
038710         05  FILLER              PIC  X(2)     VALUE HIGH-VALUE.          
038810                                                                          
038910     03  W-WDQ301KY-MIN.                                                  
039010         05  W-MIN-IDORDER       PIC  S9(7)    COMP-3.                    
039110         05  W-MIN-IDDC          PIC  X(2).                               
039210         05  FILLER              PIC  X(6)     VALUE LOW-VALUE.           
039310                                                                          
039410     03  W-WDQ301KY-MAX.                                                  
039510         05  W-MAX-IDORDER       PIC  S9(7)    COMP-3.                    
039610         05  W-MAX-IDDC          PIC  X(2).                               
039710         05  FILLER              PIC  X(6)     VALUE HIGH-VALUE.          
039810                                                                          
039910     EJECT                                                                
040010     03  W-WDGXKEY-4431-X.                                                
040110         05  FILLER              PIC  X(4)   VALUE '4431'.                
040210         05  W-4431-IDDC         PIC  X(2).                               
040310         05  FILLER              PIC  X(24)  VALUE LOW-VALUE.             
040410                                                                          
040510     03  W-WDGXKEY-4432-X.                                                
040610         05  W-4432-IDTRPLOS     PIC  X(3).                               
040710         05  W-4432-IDTRPVAR     PIC  X(2).                               
040810                                                                          
040910     03  W-WDGXKEY-4441-X.                                                
041010         05  FILLER              PIC  X(4)   VALUE '4441'.                
041110         05  FILLER              PIC  X(26)  VALUE LOW-VALUE.             
041210                                                                          
041310     03  W-WDGXKEY-4442-X.                                                
041410         05  W-4442-IDDC         PIC  X(2).                               
041510         05  W-4442-IDHLOTAB     PIC  9(2).                               
041610         05  FILLER              PIC  X(6)   VALUE LOW-VALUE.             
041710                                                                          
041810     03  W-WDGXKEY-4443-X.                                                
041910         05  FILLER              PIC  X(4)   VALUE '4443'.                
042010         05  W-4443-IDDC         PIC  X(2).                               
042110         05  W-4443-IDPKLTAB     PIC  X(2).                               
042210         05  FILLER              PIC  X(22)  VALUE LOW-VALUE.             
042310                                                                          
042410     EJECT                                                                
042510     03  W-WDGXKEY-4445-X.                                                
042610         05  FILLER              PIC  X(4)   VALUE '4445'.                
042710         05  W-4445-IDDC         PIC  X(2).                               
042810         05  W-4445-IDPRCTAB     PIC  9(2).                               
042910         05  FILLER              PIC  X(22)  VALUE LOW-VALUE.             
043010                                                                          
043110     03  W-WDGXKEY-4447-X.                                                
043210         05  FILLER              PIC  X(4)   VALUE '4447'.                
043310         05  W-4447-IDDC         PIC  X(2).                               
043410         05  FILLER              PIC  X(24)  VALUE LOW-VALUE.             
043510                                                                          
043610     03  W-WDGXKEY-4448-X.                                                
043710         05  W-4448-IDPRC        PIC  X(4).                               
043810         05  FILLER              PIC  X(1)   VALUE LOW-VALUE.             
043910                                                                          
044010     03  W-WDGXKEY-4451-X.                                                
044110         05  FILLER              PIC  X(4)   VALUE '4451'.                
044210         05  W-4451-IDDC         PIC  X(2).                               
044310         05  W-4451-IDPTIDTAB    PIC  9(2).                               
044410         05  FILLER              PIC  X(22)  VALUE LOW-VALUE.             
044510                                                                          
044610     03  W-WDGXKEY-4525-X.                                                
044710         05  FILLER              PIC  X(4)   VALUE '4525'.                
044810         05  FILLER              PIC  X(26)  VALUE LOW-VALUE.             
044910                                                                          
045010     03  W-WDGXKEY-4526-X.                                                
045110         05  FILLER              PIC  X(1)   VALUE '1'.                   
045210         05  FILLER              PIC  X(4)   VALUE LOW-VALUE.             
045310                                                                          
045410     03  W-IDGMT-X.                                                       
045510         05  W-IDDISTR-WDB2      PIC S9(5) VALUE ZERO COMP-3.             
045610         05  W-IDKUNDNR-WDB2     PIC S9(7) VALUE ZERO COMP-3.             
045710                                                                          
045810     03  W-IDDC-B6-X.                                                     
045910         05 W-IDDC-B6                  PIC X(2).                          
046010                                                                          
046110     03  W-IDFTG-X.                                                       
046210         05 W-IDFTG                    PIC 9(2).                          
046310                                                                          
046410     03  W-FLMAINDC-X.                                                    
046510         05 W-FLMAINDC                 PIC X(1).                          
046610                                                                          
046710     EJECT                                                                
046810 01  FILLER                      PIC  X(16)  VALUE 'DCTABELL'.            
046910 01  IDDC-TABELL.                                                         
047010     03  IDDC-TAB OCCURS 14 INDEXED BY DC-IX                              
047110                                 PIC  X(2).                               
047210*                                                                         
047310 01  FILLER                      PIC  X(16)  VALUE 'LOTABELL'.            
047410 01  INTERNTAB.                                                           
047510     03  INT-KVSEMBRA            PIC  S9(3)      COMP-3.                  
047610     03  INT-LO                                  OCCURS 99.               
047710       05  INT-KVANTART          PIC  S9(5)      COMP-3.                  
047810       05  INT-KVRADER           PIC  S9(5)      COMP-3.                  
047910       05  INT-SUORDV            PIC  S9(9)V9(2) COMP-3.                  
048010       05  INT-SUORDV-LOC        PIC  S9(9)V9(2) COMP-3.                  
048110       05  INT-SUORDV-LOCPREL    PIC  S9(9)V9(2) COMP-3.                  
048210       05  INT-KDVALISO          PIC  X(3).                               
048310       05  INT-VKORDNTO          PIC  S9(6)V9(5) COMP-3.                  
048410       05  INT-VLORDNTO          PIC  S9(4)V9(5) COMP-3.                  
048510       05  INT-IDPRC             PIC  X(4).                               
048610       05  INT-IDPTIDTAB         PIC  9(2).                               
048710     EJECT                                                                
048810                                                                          
048910*                                                                         
049010 01  FILLER                      PIC  X(16)  VALUE                        
049110                                      'INTERNAL TABLE'.                   
050010 01  INTERNTAB2.                                                          
051801     03 WS-LOR-ADLAGOMR-GRP     OCCURS 99.                                
051901        05 WS-LOR-IDPRC.                                                  
052001           07 WS-LOR-IDPRCBAS   PIC X(3).                                 
053001           07 WS-LOR-IDPRCVAR   PIC X.                                    
053101        05 WS-LOR-KVANTART      PIC S9(5)           COMP-3.               
053201        05 WS-LOR-KVRADER       PIC S9(5)           COMP-3.               
053301        05 WS-LOR-SUORDV        PIC S9(9)V9(2)      COMP-3.               
053401        05 WS-LOR-VKORDNTO      PIC S9(6)V9(1)      COMP-3.               
053501        05 WS-LOR-VLORDNTO      PIC S9(4)V9(3)      COMP-3.               
053601        05 WS-LOR-DEAL-PR-SUM.                                            
053701           07 WS-LOR-SUORDV-LOC PIC S9(9)V9(2)      COMP-3.               
053801           07 WS-LOR-SUORDV-LOCPREL                                       
053901                             PIC S9(9)V9(2)      COMP-3.                  
054001           07 WS-LOR-KDVALISO   PIC X(3).                                 
054101*                                                                         
054200 01  FILLER                      PIC  X(16)  VALUE 'PRCTABELL'.           
054300 01  PRCTAB.                                                              
054400     03  PRC-KVSEMBRA-PT         PIC  S9(3)      COMP-3.                  
054500     03  PRC-KVSEMBRA            PIC  S9(3)      COMP-3.                  
054600     03  PRC                                     OCCURS 99.               
054700       05  PRC-IDPRC             PIC  X(4).                               
054800       05  PRC-IDPTIDTAB         PIC  9(2).                               
054900       05  PRC-KVVTID            PIC  S9(3)V9(2) COMP-3.                  
055000       05  PRC-TILST-OD          PIC  S9(11)     COMP-3.                  
055100       05  PRC-KVANTART-PT       PIC  S9(5)      COMP-3.                  
055200       05  PRC-KVRADER-PT        PIC  S9(5)      COMP-3.                  
055300       05  PRC-VKORDNTO-PT       PIC  S9(6)V9(5) COMP-3.                  
055400       05  PRC-VLORDNTO-PT       PIC  S9(4)V9(5) COMP-3.                  
055500       05  PRC-SUORDV-PT         PIC  S9(9)V9(2) COMP-3.                  
055600       05  PRC-SUORDV-PT-LOC     PIC  S9(9)V9(2) COMP-3.                  
055700       05  PRC-SUORDV-PT-LOCPREL PIC  S9(9)V9(2) COMP-3.                  
055800       05  PRC-SUPTID            PIC  S9(3)V9(2) COMP-3.                  
055900       05  PRC-KVPTID            PIC  S9(2)V9(1) COMP-3.                  
056000       05  PRC-KVANTART          PIC  S9(5)      COMP-3.                  
056100       05  PRC-KVRADER           PIC  S9(5)      COMP-3.                  
056200       05  PRC-VKORDNTO          PIC  S9(6)V9(5) COMP-3.                  
056300       05  PRC-VLORDNTO          PIC  S9(4)V9(5) COMP-3.                  
056400       05  PRC-SUORDV            PIC  S9(9)V9(2) COMP-3.                  
056500       05  PRC-SUORDV-LOC        PIC  S9(9)V9(2) COMP-3.                  
056600       05  PRC-SUORDV-LOCPREL    PIC  S9(9)V9(2) COMP-3.                  
056700       05  PRC-KDVALISO          PIC  X(3)     VALUE SPACE.               
056800     EJECT                                                                
056900*                                                                         
057000                                                                          
057100 01  FILLER                      PIC X(16)    VALUE 'WS-WDQ211  '.        
057200 01  DIRLEV.                                                              
057300*    03  WDQ211 -COPY WDQ211      -PRE WS-.                               
057400*                                                                         
057500     EJECT                                                                
057600                                                                          
057700 01  FILLER                      PIC X(16)    VALUE 'WS-WDQ212  '.        
057800 01  IDDC.                                                                
057900*    03  WDQ212 -COPY WDQ212      -PRE WS-.                               
058000*                                                                         
058100     EJECT                                                                
058200                                                                          
058300 01  FILLER                      PIC X(16)    VALUE 'WS-XXKE11  '.        
058400 01  HLOTAB.                                                              
058500*    03  WLXXKE11 -COPY WDGX4442    -PRE WS-.                             
058600*                                                                         
058700     EJECT                                                                
058800                                                                          
058900*                                                                         
059000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
059100 01  DLI-IO-AREA.                                                         
059200     03  IO-AREA                 PIC X(4000) VALUE SPACE.                 
059300     EJECT                                                                
059400                                                                          
059500*    03           -COPY WDE601      -RED IO-AREA.                         
059600     EJECT                                                                
059700                                                                          
059800*    03  WLORQA01 -COPY WDQ301      -RED IO-AREA.                         
059900     EJECT                                                                
060000                                                                          
061000*    03  WLXXKA11 -COPY WDGX4432    -RED IO-AREA.                         
061100     EJECT                                                                
061200                                                                          
061300*    03  WLXXKE11 -COPY WDGX4442    -RED IO-AREA.                         
061400     EJECT                                                                
061500                                                                          
061600*    03  WLXXKF11 -COPY WDGX4444    -RED IO-AREA.                         
061700     EJECT                                                                
061800                                                                          
061900*    03  WLXXKG11 -COPY WDGX4446    -RED IO-AREA.                         
062000     EJECT                                                                
062100                                                                          
062200*    03  WLXXKH11 -COPY WDGX4448    -RED IO-AREA.                         
062300     EJECT                                                                
062400                                                                          
062500*    03  WLXXKI11 -COPY WDGX4452    -RED IO-AREA.                         
062600     EJECT                                                                
062700 01  FILLER                      PIC X(16)  VALUE 'WDB2-AREA'.            
062800 01  DLI-IO-AREA-WDB201.                                                  
062900*    03  -COPY WDB201                                                     
063000     EJECT                                                                
063100 01  FILLER                      PIC X(16)  VALUE 'WDB3-AREA'.            
063200*01  -COPY WDB301                                                         
063300     EJECT                                                                
063400 01  FILLER                      PIC X(16)  VALUE 'WDB6-AREA'.            
063500 01  DLI-IO-AREA-WDB601.                                                  
063600*    03  -COPY WDB601                                                     
063700     EJECT                                                                
063807 01  FILLER                      PIC X(16)  VALUE 'WDQ201 AREA'.          
063907 01  DLI-IO-AREA-WDQ201.                                                  
064007*    03  -COPY WDQ201                                                     
064107     EJECT                                                                
064207 01  FILLER                      PIC X(16)  VALUE 'WDQ211 AREA'.          
064307 01  DLI-IO-AREA-WDQ211.                                                  
064407*    03  -COPY WDQ211                                                     
064507     EJECT                                                                
064607 01  FILLER                      PIC X(16)  VALUE 'WDQ212 AREA'.          
064707 01  DLI-IO-AREA-WDQ212.                                                  
064807*    03  -COPY WDQ212                                                     
064907     EJECT                                                                
065007 01  FILLER                      PIC X(16)  VALUE 'WDQ221 AREA'.          
065107 01  DLI-IO-AREA-WDQ221.                                                  
065207*    03  -COPY WDQ221                                                     
065307     EJECT                                                                
065407 01  FILLER                      PIC X(16)   VALUE 'TIME-AREA  '.         
065507*    01 -COPY W411TIME                                                    
065607     EJECT                                                                
065707                                                                          
065807 01  FILLER                      PIC X(16)   VALUE 'ORDN-AREA  '.         
065907*    01 -COPY W411ORDN                                                    
066007     EJECT                                                                
066107                                                                          
066207 01  FILLER                      PIC X(16)   VALUE 'TRAN-AREA  '.         
066307*    01 -COPY W411TRAN                                                    
066407     EJECT                                                                
066507                                                                          
066607 LINKAGE SECTION.                                                         
066707*                                                                         
066807 01  FILLER                      PIC X(16)   VALUE 'AVSO-AREA  '.         
066907*   -COPY W413AVSO                                                        
067007     EJECT                                                                
067107*01  -COPY W0008      -PRE WDE6-                                          
067207     05  FILLER                  PIC X.                                   
067307     EJECT                                                                
067407*01  -COPY W0008      -PRE ORQA-                                          
067507     05  FILLER                  PIC X.                                   
067607     EJECT                                                                
067707*01  -COPY W0008      -PRE WDQ2-                                          
067807     05  FILLER                  PIC X.                                   
067907     EJECT                                                                
068007*01  -COPY W0008      -PRE GMTB-                                          
068107     05  FILLER                  PIC X.                                   
068207     EJECT                                                                
068307*01  -COPY W0008      -PRE XXKA-                                          
068407     05  FILLER                  PIC X.                                   
068507     EJECT                                                                
068607*01  -COPY W0008      -PRE 4437-                                          
068707     05  FILLER                  PIC X.                                   
068807     EJECT                                                                
068907*01  -COPY W0008      -PRE XXKE-                                          
069007     05  FILLER                  PIC X.                                   
069107     EJECT                                                                
069207*01  -COPY W0008      -PRE XXKF-                                          
069307     05  FILLER                  PIC X.                                   
069407     EJECT                                                                
069507*01  -COPY W0008      -PRE XXKG-                                          
069607     05  FILLER                  PIC X.                                   
069707     EJECT                                                                
069807*01  -COPY W0008      -PRE XXKH-                                          
069907     05  FILLER                  PIC X.                                   
070007     EJECT                                                                
070107*01  -COPY W0008      -PRE XXKI-                                          
070207     05  FILLER                  PIC X.                                   
070307     EJECT                                                                
070407*01  -COPY W0008      -PRE XXKP-                                          
070507     05  FILLER                  PIC X.                                   
070607     EJECT                                                                
070707*01  -COPY W0008      -PRE WDB2-                                          
070807     05  FILLER                  PIC X.                                   
070907     EJECT                                                                
071007*01  -COPY W0008      -PRE WDB6-                                          
071107     05  FILLER                  PIC X.                                   
071207     EJECT                                                                
071307*01  -COPY W0008      -PRE WDP7-                                          
071407     05  FILLER                  PIC X.                                   
071507     EJECT                                                                
071607*01  -COPY W0008      -PRE TRAN-XXKB-                                     
071707     05  FILLER                  PIC X.                                   
071807     EJECT                                                                
071907 01  ORDN-ORQL-PCB               PIC X.                                   
072007 01  ORDN-PROC-PCB               PIC X.                                   
072107 01  ORDN-ORQI-PCB               PIC X.                                   
072207 01  ORDN-WDQ3-PCB               PIC X.                                   
072307     EJECT                                                                
072407 PROCEDURE DIVISION  USING AVSO-W413AVSO WDE6-PCB ORQA-PCB                
072507                           WDQ2-PCB GMTB-PCB XXKA-PCB                     
072607                           4437-PCB XXKE-PCB XXKF-PCB XXKG-PCB            
072707                           XXKH-PCB XXKI-PCB XXKP-PCB                     
072807                           WDB2-PCB WDB6-PCB                              
072907                           WDP7-PCB TRAN-XXKB-PCB                         
073007                           ORDN-ORQL-PCB                                  
073107                           ORDN-PROC-PCB                                  
073207                           ORDN-ORQI-PCB                                  
073307                           ORDN-WDQ3-PCB.                                 
073407     PERFORM A-INIT                                                       
073507     MOVE WS-IDORDER            TO W-IDORDER                              
073607     PERFORM IMS-GU-WDQ201                                                
073707*    FÖR ATT SLÄPPA IN DDGS-ORDER                                         
073807     MOVE SPACE                 TO W-IDDC-Q211                            
073907     MOVE WS-IDLEVNR            TO W-IDLEVNR-Q211                         
074007     PERFORM IMS-GNP-WDQ211-FIRST                                         
074107     IF OHUV-KDTPOTYP            = ZERO                                   
074207     OR OHUV-KDTPOTYP = +2 AND SEGMENT-FINNS                              
074307        MOVE OHUV-KDORDKL        TO WS-KDORDKL                            
074407*       MOVE OHUV-TIREGDAT-STO   TO WS-TIREGDAT                           
074507*       MOVE OHUV-TIREGTID-STO   TO WS-TIREGTID                           
074607        MOVE OHUV-TIREGDAT       TO WS-TIREGDAT                           
074707        MOVE OHUV-TIREGTID       TO WS-TIREGTID                           
074807        MOVE OHUV-FLORDSPE       TO WS-FLORDSPE                           
074907        MOVE OHUV-IDSYSTEM       TO WS-IDSYSTEM                           
075007        MOVE OHUV-FLOVRLEV       TO WS-FLOVRLEV                           
075107        MOVE OHUV-BEVARREF       TO WS-BEVARREF                           
075207                                                                          
075307        IF AVSO-IDDC = SPACE                                              
075407*?         MOVE SPACE           TO W-IDDC-Q211                            
075507*?         MOVE WS-IDLEVNR      TO W-IDLEVNR-Q211                         
075607*?         PERFORM IMS-GNP-WDQ211-FIRST                                   
075707           PERFORM UNTIL SEGMENT-SAKNAS                                   
075807             MOVE DIRL-WDQ211     TO WS-DIRL-WDQ211                       
075907             PERFORM B-SKAPA-DIRL-ORDERDEL                                
076007             PERFORM IMS-GNP-WDQ211-FIRST                                 
076107           END-PERFORM                                                    
076207           PERFORM IMS-GNP-WDQ212                                         
076307           PERFORM UNTIL SEGMENT-SAKNAS                                   
076407             IF ((ARB-DATRPAVD > +0 AND ARB-TIHHMM > +0)   OR             
076507                  ARB-TIRFS > +0)                          OR             
076607                ((AVSO-TIAAMMDD > +0 AND AVSO-TIHHMM > +0) OR             
076707                  AVSO-TIRFS > +0)                                        
076807               MOVE ARB-IDDC       TO WS-IDDC                             
076914                                      W-IDDC                              
077014               PERFORM IMS-GNP-WDQ221                                     
077114               IF SEGMENT-FINNS                                           
077214                 MOVE ARB-WDQ212   TO WS-ARB-WDQ212                       
077314                 PERFORM S03-POPULATE-WS-LOR                              
077414                 MOVE ARB-KDFRAKT  TO WS-KDFRAKT                          
077514                 IF WS-ARB-FLODELUT = JA                                  
077614*FIX                                                                      
077714                   IF AVSO-IDTRANS = 'FIXA'                               
077814                      PERFORM S01-NOLLA-PRC-TAB                           
077914                      PERFORM C-ORDERDELAR                                
078014                      PERFORM D-RENSA-ODEL-UTAN-RADER                     
078114                      PERFORM E-KOMPLETTERA-ARBTAB                        
078214                    ELSE                                                  
078314                    PERFORM I-FORENKLAT-ORDERAVSLUT                       
078414                    END-IF                                                
078514*FIX-END                                                                  
078614                 ELSE                                                     
078714                    PERFORM S01-NOLLA-PRC-TAB                             
078814                    PERFORM C-ORDERDELAR                                  
078914                    PERFORM D-RENSA-ODEL-UTAN-RADER                       
079014                    PERFORM E-KOMPLETTERA-ARBTAB                          
079114                 END-IF                                                   
079214*          MOVE 'SM 1' TO FELTEXT                                         
079314*         CALL FELLOG                                                     
079414               ELSE                                                       
079514                 PERFORM H-RENSA-ALLA-ORDERDELAR                          
079614               END-IF                                                     
079714             END-IF                                                       
079814             PERFORM IMS-GNP-WDQ212                                       
079914           END-PERFORM                                                    
080014        ELSE                                                              
080114*          ENDAST ORDERAVSLUT FÖR AVSO-IDDC (IDDC I MIDDEN).              
080214           MOVE AVSO-IDDC        TO W-IDDC                                
080314                                    W-IDDC-Q211-MAX                       
080414           MOVE SPACE            TO W-IDDC-Q211-MIN                       
080514           MOVE WS-IDLEVNR       TO W-IDLEVNR-Q211-MIN                    
080614           PERFORM IMS-GNP-WDQ211-DC-FIRST                                
080714           PERFORM UNTIL SEGMENT-SAKNAS                                   
080814             MOVE DIRL-WDQ211    TO WS-DIRL-WDQ211                        
080914             PERFORM B-SKAPA-DIRL-ORDERDEL                                
081014             PERFORM IMS-GNP-WDQ211-DC-FIRST                              
081114           END-PERFORM                                                    
081214           PERFORM IMS-GU-WDQ212                                          
081314           MOVE ARB-IDDC         TO WS-IDDC                               
081414           PERFORM IMS-GNP-WDQ221                                         
081514           IF SEGMENT-FINNS                                               
081614             MOVE ARB-WDQ212     TO WS-ARB-WDQ212                         
081714             PERFORM S03-POPULATE-WS-LOR                                  
081814             MOVE ARB-KDFRAKT    TO WS-KDFRAKT                            
081914             IF WS-ARB-FLODELUT = JA                                      
082014**** FIX BÖRJAN                                                           
082114*               IF AVSO-IDORDER = 554531                                  
082214*                    PERFORM S01-NOLLA-PRC-TAB                            
082314*                    PERFORM C-ORDERDELAR                                 
082414*                    PERFORM D-RENSA-ODEL-UTAN-RADER                      
082514*                    PERFORM E-KOMPLETTERA-ARBTAB                         
082614*                    MOVE OKAY TO AVSO-KDSVAR                             
082714*                                                                         
082814*               ELSE                                                      
082914**** FIX SLUT                                                             
083014                     PERFORM I-FORENKLAT-ORDERAVSLUT                      
083114*               END-IF                                                    
083214             ELSE                                                         
083314                PERFORM S01-NOLLA-PRC-TAB                                 
083414                PERFORM C-ORDERDELAR                                      
083514                PERFORM D-RENSA-ODEL-UTAN-RADER                           
083614                PERFORM E-KOMPLETTERA-ARBTAB                              
083714                MOVE OKAY           TO AVSO-KDSVAR                        
083814             END-IF                                                       
083914*          MOVE 'SM 2' TO FELTEXT                                         
084014*       CALL FELLOG                                                       
084114           ELSE                                                           
084214             MOVE EJ-OKAY        TO AVSO-KDSVAR                           
084314             IF RFS-EL-TRP-AENDRING                                       
084414               PERFORM G-AENDRA-RFS-TRP                                   
084514             END-IF                                                       
084614             PERFORM H-RENSA-ALLA-ORDERDELAR                              
084714           END-IF                                                         
084814        END-IF                                                            
084914     END-IF                                                               
085014     GOBACK                                                               
085114     .                                                                    
085214     EJECT                                                                
085314                                                                          
085414 A-INIT                        SECTION.                                   
085514                                                                          
085614     MOVE FUNCTION CURRENT-DATE (1:4)  TO WS-YEAR                         
085714     MOVE FUNCTION CURRENT-DATE (5:2)  TO WS-MONTH                        
085814     MOVE FUNCTION CURRENT-DATE (7:2)  TO WS-DAY                          
085914     MOVE FUNCTION CURRENT-DATE (9:2)  TO WS-HOUR                         
086014     MOVE FUNCTION CURRENT-DATE (11:2) TO WS-MINUTE                       
086114     MOVE FUNCTION CURRENT-DATE (13:2) TO WS-SECOND                       
086214     MOVE FUNCTION CURRENT-DATE (15:6) TO WS-DECIMAL                      
086314                                                                          
086414     MOVE WS-YEAR (3:2) TO WS-CURR-YEAR                                   
086514     MOVE WS-MONTH      TO WS-CURR-MONTH                                  
086614     MOVE WS-DAY        TO WS-CURR-DAY                                    
086714     MOVE WS-HOUR       TO WS-CURR-HOUR                                   
086814     MOVE WS-MINUTE     TO WS-CURR-MINUTE                                 
086914                                                                          
087014     MOVE AVSO-IDDISTR          TO WS-IDDISTR                             
087114                                   TEST-IDDISTR                           
087214     MOVE AVSO-IDKUNDNR         TO WS-IDKUNDNR                            
087314     MOVE AVSO-IDDC             TO WS-IDDC                                
087414     MOVE SPACE                 TO WS-IDLEVNR                             
087514     MOVE ZERO                  TO SEKUNDER                               
087614     MOVE NEJ                   TO WS-FLODELUT                            
087714     MOVE AVSO-IDORDER          TO WS-IDORDER                             
087814     IF AVSO-TIRFS               > +0                                     
087914        MOVE JA                 TO SW-RFS-EL-TRP                          
088014     END-IF                                                               
088114     IF AVSO-TIAAMMDD            > +0 AND                                 
088214        AVSO-TIHHMM              > +0                                     
088314        MOVE JA                 TO SW-RFS-EL-TRP                          
088414     END-IF                                                               
088514                                                                          
088614     MOVE NEJ                   TO SW-IDDC-TRAEFF                         
088714                                                                          
088814     SET DC-IX                  TO +01                                    
088914     PERFORM UNTIL DC-IX        > DC-MAX                                  
089014         MOVE SPACE             TO IDDC-TAB (DC-IX)                       
089114         SET DC-IX UP BY +1                                               
089214     END-PERFORM                                                          
089314                                                                          
089414     MOVE ZERO                  TO WS-HELP-TIAAAAMMDD                     
089514                                   WS-HELP-TIHHMM                         
089614     .                                                                    
089714     EJECT                                                                
089814                                                                          
089914 B-SKAPA-DIRL-ORDERDEL         SECTION.                                   
090014*                                                                         
090114*                                                                         
090214                                                                          
090314     MOVE +01                 TO DC-INIT                                  
090414     SET DC-IX                TO DC-INIT                                  
090514     MOVE WS-DIRL-IDDC        TO WS-IDDC                                  
090614                                 W-IDDC-Q211                              
090714                                 W-IDDC-Q211-MIN                          
090814                                                                          
090914     MOVE WS-IDDISTR          TO W-B301-IDDISTR                           
091014                                 W-B301-IDDISTR-DEFAULT                   
091114     MOVE WS-IDKUNDNR         TO W-B301-IDKUNDNR                          
091214                                                                          
091314     IF GOOD-DDC                                                          
091414       MOVE WC-CDC-SE         TO IDDC-TAB (DC-IX)                         
091514                                 W-IDDC                                   
091614                                 W-B301-IDDC                              
091714                                 W-B301-IDDC-DEFAULT                      
091814     ELSE                                                                 
091914       MOVE WS-DIRL-IDDC      TO IDDC-TAB (DC-IX)                         
092014                                 W-IDDC                                   
092114                                 W-B301-IDDC                              
092214                                 W-B301-IDDC-DEFAULT                      
092314     END-IF                                                               
092414                                                                          
092514     PERFORM IMS-GU-GMTB01-WDB301                                         
092614                                                                          
092714     MOVE WS-DIRL-IDLEVNR     TO WS-IDLEVNR                               
092814                                 W-IDLEVNR-Q211                           
092914                                 W-IDLEVNR-Q211-MIN                       
093014     PERFORM IMS-GHNP-WDQ212                                              
093114*                                                                         
093214     IF ((ARB-DATRPAVD > +0 AND ARB-TIHHMM > +0)   OR                     
093314          ARB-TIRFS > +0)                          OR                     
093414        ((AVSO-TIAAMMDD > +0 AND AVSO-TIHHMM > +0) OR                     
093514          AVSO-TIRFS > +0)                                                
093614       MOVE ARB-WDQ212          TO WS-ARB-WDQ212                          
093615       PERFORM IMS-GNP-WDQ221                                             
093616       IF SEGMENT-FINNS                                                   
093714          PERFORM S03-POPULATE-WS-LOR                                     
093715       END-IF                                                             
093814       MOVE DIRLEV-IDPRC-9998   TO WS-IDPRC                               
093914       MOVE WS-ARB-IDTRPLOS     TO WS-IDTRPLOS                            
094014       MOVE WS-ARB-IDTRPVAR     TO WS-IDTRPVAR                            
094114       PERFORM S04-RAEKNA-RFS                                             
094214       PERFORM IMS-GHNP-WDQ212                                            
094314       MOVE WS-TIRFS            TO ARB-TIRFS                              
094414       ADD +1                   TO ARB-IDPLKLST-SISTA                     
094514       MOVE ARB-IDPLKLST-SISTA  TO WS-IDPLKLST                            
094614       IF WS-HELP-TIAAAAMMDD(3:6) > ZERO                                  
094714          MOVE WS-HELP-TIAAAAMMDD  TO ARB-DATRPAVD                        
094814          MOVE WS-HELP-TIHHMM      TO ARB-TIHHMM                          
094914       END-IF                                                             
095014       PERFORM IMS-REPL-WDQ212                                            
095107       MOVE WS-ARB-KDFRAKT      TO WS-KDFRAKT                             
095207                                                                          
095307       MOVE WS-ARB-KDFDKRAV     TO WS-KDFDKRAV                            
095407*                                                                         
095507       MOVE WS-IDORDER          TO W-MIN-IDORDER                          
095607                                   W-MAX-IDORDER                          
095707                                                                          
095807       IF GOOD-DDC                                                        
095907         MOVE WS-DIRL-IDDC      TO W-MIN-IDDC                             
096007                                   W-MAX-IDDC                             
096107       ELSE                                                               
096207         MOVE WS-ARB-IDDC       TO W-MIN-IDDC                             
096307                                   W-MAX-IDDC                             
096407       END-IF                                                             
096507                                                                          
096607       MOVE WS-IDLEVNR          TO W-IDLEVNR                              
096707       PERFORM IMS-GHU-ORQA-LEVNR                                         
096807*                                                                         
096907       IF SEGMENT-SAKNAS                                                  
097007         PERFORM S02-TAG-UT-IDPRODNR                                      
097107         PERFORM BA-INIT-DIRLEV                                           
097207         PERFORM IMS-ISRT-ORQA                                            
097307         MOVE JA                TO WS-FLODELUT                            
097407       END-IF                                                             
097507     ELSE                                                                 
097607       MOVE WS-IDLEVNR          TO W-IDLEVNR                              
097707     END-IF                                                               
097807                                                                          
097907     .                                                                    
098007     EJECT                                                                
098107                                                                          
098207 BA-INIT-DIRLEV                SECTION.                                   
098307                                                                          
098407     MOVE ZERO                TO ODEL-KVART                               
098507                                 ODEL-KVPACKRAD-OD                        
098607                                 ODEL-KVPTID                              
098707                                 ODEL-SUHANTTI                            
098807                                 ODEL-SUPTID                              
098907                                 ODEL-TILST-OD                            
099007                                 ODEL-TIPACKN                             
099107                                 ODEL-TIPACTID                            
099207                                 ODEL-DAUTSKR                             
099307                                 ODEL-TIUTSTID                            
099407                                 ODEL-DALSTORD                            
099507     MOVE SPACE               TO ODEL-IDBORD                              
099607                                 ODEL-KDPRODKL                            
099707                                 ODEL-KDVALISO                            
099807                                 ODEL-IDDC-EXP                            
099907     MOVE WS-ARB-IDTRP        TO ODEL-IDTRP                               
100007     MOVE IDUSER-NOLL         TO ODEL-IDUSER                              
100107     MOVE WS-IDORDER          TO ODEL-IDORDER                             
100207     IF GOOD-DDC                                                          
100307       MOVE WS-DIRL-IDDC      TO ODEL-IDDC                                
100407     ELSE                                                                 
100507       MOVE WS-ARB-IDDC       TO ODEL-IDDC                                
100607     END-IF                                                               
100707     MOVE WS-IDPRODNR         TO ODEL-IDPRODNR                            
100807     MOVE WS-IDPLKLST         TO ODEL-IDPLKLST                            
100907     MOVE AVSO-IDGMTREF       TO ODEL-IDGMTREF                            
101007     MOVE WS-DIRL-IDLEVNR     TO ODEL-IDLEVNR                             
101107     MOVE DIRLEV-IDPRC-9998   TO ODEL-IDPRC                               
101207     MOVE WS-KDFDKRAV         TO ODEL-KDFDKRAV                            
101307     MOVE 'R'                 TO ODEL-KDODELSTA                           
101407     MOVE WS-DIRL-KVRADER     TO ODEL-KVRADER                             
101507     MOVE WS-DIRL-SUORDV-LOC  TO ODEL-SUORDV-LOC                          
101607     MOVE WS-DIRL-SUORDV-LOCPREL TO ODEL-SUORDV-LOCPREL                   
101707     MOVE WS-DIRL-KDVALISO    TO ODEL-KDVALISO                            
101807     MOVE WS-DIRL-SUORDV      TO ODEL-SUORDV                              
101907     IF WS-HELP-TIAAAAMMDD(3:6) > ZERO                                    
102007        MOVE WS-HELP-TIAAAAMMDD TO ODEL-DATRPAVD                          
102107        MOVE WS-HELP-TIHHMM     TO ODEL-TIHHMM                            
102207     ELSE                                                                 
102307        MOVE WS-ARB-DATRPAVD    TO ODEL-DATRPAVD                          
102407        MOVE WS-ARB-TIHHMM      TO ODEL-TIHHMM                            
102507     END-IF                                                               
102607     MOVE WS-TIREGDAT         TO ODEL-TIREGDAT                            
102707     MOVE WS-TIREGTID         TO ODEL-TIREGTID                            
102807     MOVE WS-DARFS            TO ODEL-DARFS                               
102907     MOVE WS-DARFS-DATUM      TO ODEL-DARFSDAT                            
103007     MOVE WS-DIRL-VKORDNTO    TO ODEL-VKORDNTO                            
103107     MOVE WS-DIRL-VLORDNTO    TO ODEL-VLORDNTO                            
103207     MOVE SPACE               TO ODEL-IDPRCPLK                            
103307     MOVE ZERO                TO ODEL-IDLOTNR-PLK                         
103407                                 ODEL-IDLOPNR-ORD                         
103507     .                                                                    
103607     EJECT                                                                
103707                                                                          
103807 C-ORDERDELAR                  SECTION.                                   
103907                                                                          
104007     MOVE WS-ARB-IDPLKLST-SISTA TO WS-IDPLKLST                            
104107     MOVE WS-ARB-IDTRP          TO WS-IDTRP                               
104207     MOVE WS-ARB-KDTRPKAT       TO WS-KDTRPKAT                            
104307     MOVE WS-ARB-IDTRPLOS       TO WS-IDTRPLOS                            
104407     MOVE WS-ARB-IDTRPVAR       TO WS-IDTRPVAR                            
104507     MOVE WS-ARB-IDDC           TO WS-IDDC                                
104607     MOVE WS-ARB-KDFRAKT        TO WS-KDFRAKT                             
104707     MOVE WS-ARB-DATRPAVD       TO TRP-DATRPAVD                           
104807     MOVE WS-ARB-TIHHMM         TO TRP-TIHHMM                             
104911     PERFORM CA-LAES-IN-LORTAB                                            
105007                                                                          
105107     MOVE WS-ARB-KDFDKRAV     TO WS-KDFDKRAV                              
105207*                                                                         
105307     MOVE WS-IDDISTR            TO W-B301-IDDISTR                         
105407                                   W-B301-IDDISTR-DEFAULT                 
105507     MOVE WS-IDKUNDNR           TO W-B301-IDKUNDNR                        
105607     MOVE WS-IDDC               TO W-B301-IDDC                            
105707                                   W-B301-IDDC-DEFAULT                    
105807                                                                          
105907     PERFORM IMS-GU-GMTB01-WDB301                                         
106007                                                                          
106107     MOVE DC-IDGMTOMR           TO WS-IDGMTOMR                            
106207                                                                          
106307     IF DC-IDPKLTAB = '00'                                                
106407       MOVE DEFAULT-IDPKLTAB-01 TO WS-IDPKLTAB                            
106507     ELSE                                                                 
106607       MOVE DC-IDPKLTAB         TO WS-IDPKLTAB                            
106707     END-IF                                                               
106807     IF DC-IDPRCTAB > 0                                                   
106907       MOVE DC-IDPRCTAB         TO WS-IDPRCTAB                            
107007     ELSE                                                                 
107107       MOVE DEFAULT-IDPRCTAB-1  TO WS-IDPRCTAB                            
107207     END-IF                                                               
107307                                                                          
107407*                                                                         
107507     MOVE WS-IDDC               TO W-4443-IDDC                            
107607     MOVE WS-IDPKLTAB           TO W-4443-IDPKLTAB                        
107707     MOVE WS-KDORDKL            TO W-KDORDKL                              
107807     MOVE WS-KVRADER            TO W-KVRADER                              
107907     MOVE WS-VKORDNTO           TO W-VKORDNTO                             
108007     MOVE WS-VLORDNTO           TO W-VLORDNTO                             
108107     PERFORM IMS-GU-XXKF11                                                
108207*                                                                         
108307     IF SEGMENT-SAKNAS                                                    
108407       MOVE DEFAULT-IDPKLTAB-01 TO W-4443-IDPKLTAB                        
108507       PERFORM IMS-GU-XXKF11                                              
108607     END-IF                                                               
108707*                                                                         
108807     MOVE 4444-IDHLOTAB         TO WS-IDHLOTAB                            
108907     MOVE 4444-KDPRODKL         TO WS-KDPRODKL                            
109007     PERFORM CB-SAETT-PRC-INTTAB                                          
109107     PERFORM CC-RAEKNA-PTID                                               
109207     PERFORM CD-SKAPA-ORDERDELAR                                          
109307     .                                                                    
109407     EJECT                                                                
109507                                                                          
109611 CA-LAES-IN-LORTAB             SECTION.                                   
109707                                                                          
109807     MOVE WS-ARB-KVSEMBRA        TO INT-KVSEMBRA                          
109907     MOVE +1                     TO LO-IX                                 
110007     PERFORM UNTIL LO-IX > 99                                             
110107       MOVE WS-LOR-IDPRC    (LO-IX) TO INT-IDPRC    (LO-IX)               
110207       MOVE WS-LOR-KVANTART (LO-IX) TO INT-KVANTART (LO-IX)               
110307       MOVE WS-LOR-KVRADER  (LO-IX) TO INT-KVRADER  (LO-IX)               
110407       MOVE WS-LOR-SUORDV-LOC(LO-IX) TO INT-SUORDV-LOC (LO-IX)            
110507       MOVE WS-LOR-SUORDV-LOCPREL(LO-IX)                                  
110607                                 TO INT-SUORDV-LOCPREL (LO-IX)            
110707       MOVE WS-LOR-KDVALISO(LO-IX)   TO INT-KDVALISO (LO-IX)              
110807       MOVE WS-LOR-SUORDV(LO-IX)    TO INT-SUORDV   (LO-IX)               
110907       MOVE WS-LOR-VKORDNTO (LO-IX) TO INT-VKORDNTO (LO-IX)               
111007       MOVE WS-LOR-VLORDNTO (LO-IX) TO INT-VLORDNTO (LO-IX)               
111107       ADD  WS-LOR-KVRADER  (LO-IX) TO WS-KVRADER                         
111207       COMPUTE WS-VKORDNTO ROUNDED   =                                    
111307            WS-LOR-VKORDNTO (LO-IX)  + WS-VKORDNTO                        
111407       COMPUTE WS-VLORDNTO ROUNDED   =                                    
111507            WS-LOR-VLORDNTO (LO-IX)  + WS-VLORDNTO                        
111607       ADD +1                    TO LO-IX                                 
111707     END-PERFORM                                                          
111807                                                                          
111907     .                                                                    
112007     EJECT                                                                
112107                                                                          
112207 CB-SAETT-PRC-INTTAB           SECTION.                                   
112307                                                                          
112407     MOVE WS-IDDC               TO W-4442-IDDC                            
112507     MOVE WS-IDHLOTAB           TO W-4442-IDHLOTAB                        
112607     PERFORM IMS-GU-XXKE11                                                
112707*                                                                         
112807     IF SEGMENT-SAKNAS                                                    
112907       MOVE DEFAULT-IDHLOTAB-1  TO W-4442-IDHLOTAB                        
113007       PERFORM IMS-GU-XXKE11                                              
113107     END-IF                                                               
113207*                                                                         
113307     MOVE 4442-WDGX4442         TO WS-4442-WDGX4442                       
113407     MOVE WS-IDDC               TO W-4445-IDDC                            
113507     MOVE WS-IDPRCTAB           TO W-4445-IDPRCTAB                        
113607     MOVE WS-IDGMTOMR           TO W-IDGMTOMR                             
113707     MOVE WS-IDTRP              TO W-IDTRP                                
113807     MOVE WS-KDFRAKT            TO W-KDFRAKT                              
113907     MOVE WS-KDPRODKL           TO W-KDPRODKL                             
114007     PERFORM IMS-GU-XXKG01                                                
114107*                                                                         
114207     IF SEGMENT-SAKNAS                                                    
114307       MOVE DEFAULT-IDPRCTAB-1  TO W-4445-IDPRCTAB                        
114407       PERFORM IMS-GU-XXKG01                                              
114507     END-IF                                                               
114607*                                                                         
114707     PERFORM CBA-EV-UNDANTAG-VID-VAL-AV-PRC                               
114807                                                                          
114907     IF UNDANTAG-VID-VAL-AV-PRC = NEJ                                     
115007        MOVE WS-4442-IDHLO (LO-IX) TO W-IDHLO                             
115107        MOVE +1                 TO LO-IX                                  
115207        PERFORM IMS-GNP-XXKG11                                            
115307        PERFORM UNTIL LO-IX > 99                                          
115407          IF WS-4442-IDHLO (LO-IX) NOT = W-IDHLO                          
115507             MOVE WS-4442-IDHLO (LO-IX) TO W-IDHLO                        
115607             PERFORM IMS-GNP-XXKG11                                       
115707          END-IF                                                          
115807          MOVE 4446-IDPTIDTAB   TO INT-IDPTIDTAB(LO-IX)                   
115907          MOVE 4446-IDPRC       TO INT-IDPRC    (LO-IX)                   
116011                                   WS-LOR-IDPRC (LO-IX)                   
116107          ADD +1                TO LO-IX                                  
116207        END-PERFORM                                                       
116307     END-IF                                                               
116407                                                                          
116507     .                                                                    
116607     EJECT                                                                
116707 CBA-EV-UNDANTAG-VID-VAL-AV-PRC          SECTION.                         
116807                                                                          
116907     MOVE NEJ TO UNDANTAG-VID-VAL-AV-PRC                                  
117007     MOVE +1                    TO LO-IX                                  
117107                                                                          
117207     IF (WS-FLORDSPE              = JA  AND                               
117307         WS-IDSYSTEM NOT = 'W216')      OR                                
117407        WS-FLOVRLEV              = JA                                     
117507        PERFORM UNTIL LO-IX > 99                                          
117607          MOVE SPECORD-IDPRC-9997 TO INT-IDPRC    (LO-IX)                 
117707                                     WS-LOR-IDPRC (LO-IX)                 
117807          ADD +1                  TO LO-IX                                
117907        END-PERFORM                                                       
118007                                                                          
118107        MOVE JA TO UNDANTAG-VID-VAL-AV-PRC                                
118207     END-IF                                                               
118307                                                                          
118407     IF WS-BEVARREF              = 'W480      '                           
118507        PERFORM UNTIL LO-IX > 99                                          
118607          MOVE W480-IDPRC-999M    TO INT-IDPRC    (LO-IX)                 
118707                                     WS-LOR-IDPRC (LO-IX)                 
118807          ADD +1                  TO LO-IX                                
118907        END-PERFORM                                                       
119007                                                                          
119107        MOVE JA TO UNDANTAG-VID-VAL-AV-PRC                                
119207     END-IF                                                               
119307     .                                                                    
119407     EJECT                                                                
119507 CC-RAEKNA-PTID                SECTION.                                   
119607                                                                          
119707     MOVE +1                          TO PRC-IX                           
119807     MOVE WS-IDDC                     TO W-4447-IDDC                      
119907     PERFORM IMS-GU-XXKH01                                                
120007     PERFORM UNTIL PRC-IX > 99                                            
120107       MOVE +1                        TO LO-IX                            
120207       PERFORM UNTIL (LO-IX > 99) OR (INT-IDPRC (LO-IX) > ZERO)           
120307         ADD +1                       TO LO-IX                            
120407       END-PERFORM                                                        
120507       IF LO-IX < 100                                                     
120607        MOVE INT-IDPRC (LO-IX)        TO PRC-IDPRC       (PRC-IX)         
120707                                         W-4448-IDPRC                     
120807        MOVE INT-IDPTIDTAB (LO-IX)    TO PRC-IDPTIDTAB   (PRC-IX)         
120907        PERFORM IMS-GNP-XXKH11-F                                          
121007*                                                                         
121107        MOVE 4448-KVVTID              TO PRC-KVVTID      (PRC-IX)         
121207        ADD INT-KVSEMBRA              TO PRC-KVSEMBRA-PT                  
121307        PERFORM UNTIL LO-IX > 99                                          
121407         IF INT-IDPRC (LO-IX)         =  PRC-IDPRC       (PRC-IX)         
121507          PERFORM CCA-PTID-I-LO                                           
121607          MOVE '0000'                 TO INT-IDPRC       (LO-IX)          
121707          IF SW-PTID = JA                                                 
121807            PERFORM CCB-MED-PTID                                          
121907          ELSE                                                            
122007            PERFORM CCC-UTAN-PTID                                         
122107          END-IF                                                          
122207         END-IF                                                           
122307         ADD +1                       TO LO-IX                            
122407        END-PERFORM                                                       
122507       ELSE                                                               
122607        MOVE +99                      TO PRC-IX                           
122707       END-IF                                                             
122807       ADD +1                         TO PRC-IX                           
122907     END-PERFORM                                                          
123007     PERFORM CCF-PTIDSBERAEKNING                                          
123107     .                                                                    
123207     EJECT                                                                
123307                                                                          
123407 CCA-PTID-I-LO                 SECTION.                                   
123507                                                                          
123607     MOVE +1                      TO LAGO-IX                              
123707     MOVE JA                      TO SW-PTID                              
123807     PERFORM UNTIL LAGO-IX > 10                                           
123907       IF 4448-ADLAGOMR (LAGO-IX)  > ZERO                                 
124007         IF 4448-ADLAGOMR(LAGO-IX) = LO-IX                                
124107           MOVE JA                TO SW-PTID                              
124207           MOVE +10               TO LAGO-IX                              
124307         ELSE                                                             
124407           MOVE NEJ               TO SW-PTID                              
124507         END-IF                                                           
124607       END-IF                                                             
124707       ADD +1                     TO LAGO-IX                              
124807     END-PERFORM                                                          
124907     .                                                                    
125007     EJECT                                                                
125107                                                                          
125207 CCB-MED-PTID                  SECTION.                                   
125307                                                                          
125407     ADD INT-KVANTART   (LO-IX) TO PRC-KVANTART-PT (PRC-IX)               
125507     ADD INT-KVRADER    (LO-IX) TO PRC-KVRADER-PT  (PRC-IX)               
125607     COMPUTE PRC-VKORDNTO-PT (PRC-IX) ROUNDED =                           
125707      INT-VKORDNTO   (LO-IX)  + PRC-VKORDNTO-PT    (PRC-IX)               
125807     COMPUTE PRC-VLORDNTO-PT (PRC-IX) ROUNDED =                           
125907      INT-VLORDNTO   (LO-IX)  + PRC-VLORDNTO-PT    (PRC-IX)               
126007     COMPUTE PRC-SUORDV-PT-LOC (PRC-IX) ROUNDED =                         
126107      INT-SUORDV-LOC (LO-IX) + PRC-SUORDV-PT-LOC   (PRC-IX)               
126207     COMPUTE PRC-SUORDV-PT-LOCPREL (PRC-IX) ROUNDED =                     
126307      INT-SUORDV-LOCPREL (LO-IX) + PRC-SUORDV-PT-LOCPREL (PRC-IX)         
126407     COMPUTE PRC-SUORDV-PT (PRC-IX) ROUNDED =                             
126507      INT-SUORDV   (LO-IX)  + PRC-SUORDV-PT        (PRC-IX)               
126607     IF PRC-KDVALISO(PRC-IX) = SPACE                                      
126707       MOVE INT-KDVALISO(LO-IX)   TO PRC-KDVALISO(PRC-IX)                 
126807     END-IF                                                               
126907     .                                                                    
127007     EJECT                                                                
127107                                                                          
127207 CCC-UTAN-PTID                 SECTION.                                   
127307                                                                          
127407     ADD INT-KVANTART   (LO-IX) TO PRC-KVANTART    (PRC-IX)               
127507     ADD INT-KVRADER    (LO-IX) TO PRC-KVRADER     (PRC-IX)               
127607     COMPUTE PRC-VKORDNTO (PRC-IX) ROUNDED =                              
127707         INT-VKORDNTO   (LO-IX)  + PRC-VKORDNTO    (PRC-IX)               
127807     COMPUTE PRC-VLORDNTO (PRC-IX) ROUNDED =                              
127907         INT-VLORDNTO   (LO-IX)  + PRC-VLORDNTO    (PRC-IX)               
128007     COMPUTE PRC-SUORDV-LOC (PRC-IX) ROUNDED =                            
128107         INT-SUORDV-LOC (LO-IX) + PRC-SUORDV-LOC   (PRC-IX)               
128207     COMPUTE PRC-SUORDV-LOCPREL (PRC-IX) ROUNDED =                        
128307         INT-SUORDV-LOCPREL (LO-IX) + PRC-SUORDV-LOCPREL (PRC-IX)         
128407     COMPUTE PRC-SUORDV (PRC-IX) ROUNDED =                                
128507         INT-SUORDV (LO-IX) + PRC-SUORDV           (PRC-IX)               
128607     IF PRC-KDVALISO(PRC-IX) = SPACE                                      
128707       MOVE INT-KDVALISO(LO-IX)   TO PRC-KDVALISO(PRC-IX)                 
128807     END-IF                                                               
128907     .                                                                    
129007     EJECT                                                                
129107                                                                          
129207 CCF-PTIDSBERAEKNING           SECTION.                                   
129307                                                                          
129407     MOVE +1                          TO PRC-IX                           
129507     MOVE WS-IDDC                     TO W-4451-IDDC                      
129607     PERFORM UNTIL (PRC-IX > 99) OR (PRC-IDPRC (PRC-IX) = ZERO)           
129707       IF PRC-KVRADER-PT (PRC-IX)      > ZERO                             
129807         MOVE PRC-IDPTIDTAB (PRC-IX)  TO W-4451-IDPTIDTAB                 
129907         PERFORM IMS-GU-XXKI11                                            
130007         IF SEGMENT-SAKNAS                                                
130107           MOVE DEFAULT-IDPTIDTAB-1   TO W-4451-IDPTIDTAB                 
130207           PERFORM IMS-GU-XXKI11                                          
130307         END-IF                                                           
130407         PERFORM CCFA-PTID-PER-RAD                                        
130507       END-IF                                                             
130607       IF PRC-KVRADER-PT (PRC-IX) > 0                                     
130707          OR                                                              
130807          PRC-KVRADER    (PRC-IX) > 0                                     
130907          PERFORM CCFB-BERAEKNA-RFS-LST                                   
131007       END-IF                                                             
131107       ADD +1                         TO PRC-IX                           
131207     END-PERFORM                                                          
131307     .                                                                    
131407     EJECT                                                                
131507                                                                          
131607 CCFA-PTID-PER-RAD             SECTION.                                   
131707                                                                          
131807     IF 4452-KDSORT-Y                  = KILO                             
131907       COMPUTE Y-KVPTSORT-JFR  =                                          
132007        PRC-VKORDNTO-PT (PRC-IX) / PRC-KVRADER-PT (PRC-IX)                
132107     ELSE                                                                 
132207       IF 4452-KDSORT-Y                = VOLYM                            
132307         COMPUTE Y-KVPTSORT-JFR  =                                        
132407          PRC-VLORDNTO-PT (PRC-IX) / PRC-KVRADER-PT (PRC-IX)              
132507       ELSE                                                               
132607         IF 4452-KDSORT-Y              = RADER                            
132707           MOVE PRC-KVRADER-PT(PRC-IX)     TO Y-KVPTSORT-JFR              
132807         ELSE                                                             
132907           IF 4452-KDSORT-Y            = STYCK                            
133007             MOVE PRC-KVANTART-PT(PRC-IX)  TO Y-KVPTSORT-JFR              
133107           ELSE                                                           
133207             IF 4452-KDSORT-Y          = MEDELSTYCK                       
133307               COMPUTE Y-KVPTSORT-JFR  =                                  
133407                PRC-KVANTART-PT (PRC-IX) / PRC-KVRADER-PT (PRC-IX)        
133507             END-IF                                                       
133607           END-IF                                                         
133707         END-IF                                                           
133807       END-IF                                                             
133907     END-IF                                                               
134007*                                                                         
134107     IF 4452-KDSORT-X                  = KILO                             
134207       COMPUTE X-KVPTSORT-JFR  =                                          
134307        PRC-VKORDNTO-PT (PRC-IX) / PRC-KVRADER-PT (PRC-IX)                
134407     ELSE                                                                 
134507       IF 4452-KDSORT-X                = VOLYM                            
134607         COMPUTE X-KVPTSORT-JFR  =                                        
134707          PRC-VLORDNTO-PT (PRC-IX) / PRC-KVRADER-PT (PRC-IX)              
134807       ELSE                                                               
134907         IF 4452-KDSORT-X              = RADER                            
135007           MOVE PRC-KVRADER-PT(PRC-IX)     TO X-KVPTSORT-JFR              
135107         ELSE                                                             
135207           IF 4452-KDSORT-X            = STYCK                            
135307             MOVE PRC-KVANTART-PT(PRC-IX)  TO X-KVPTSORT-JFR              
135407           ELSE                                                           
135507             IF 4452-KDSORT-X          = MEDELSTYCK                       
135607               COMPUTE X-KVPTSORT-JFR  =                                  
135707               PRC-KVANTART-PT (PRC-IX) / PRC-KVRADER-PT (PRC-IX)         
135807             END-IF                                                       
135907           END-IF                                                         
136007         END-IF                                                           
136107       END-IF                                                             
136207     END-IF                                                               
136307*                                                                         
136407     MOVE +1                          TO W-4452-Y-IX                      
136507     MOVE NEJ                         TO SW-Y-SORT-FUNNEN                 
136607     PERFORM UNTIL (W-4452-Y-IX > 9) OR (SW-Y-SORT-FUNNEN = JA)           
136707       IF 4452-KVPTSORT-Y (W-4452-Y-IX) >=  Y-KVPTSORT-JFR                
136807         MOVE JA                      TO SW-Y-SORT-FUNNEN                 
136907       ELSE                                                               
137007         ADD +1                       TO W-4452-Y-IX                      
137107       END-IF                                                             
137207     END-PERFORM                                                          
137307*                                                                         
137407     MOVE +1                          TO W-4452-X-IX                      
137507     MOVE NEJ                         TO SW-X-SORT-FUNNEN                 
137607     PERFORM UNTIL (W-4452-X-IX > 9) OR (SW-X-SORT-FUNNEN = JA)           
137707       IF 4452-KVPTSORT-X (W-4452-X-IX) >=  X-KVPTSORT-JFR                
137807         MOVE JA                      TO SW-X-SORT-FUNNEN                 
137907       ELSE                                                               
138007         ADD +1                       TO  W-4452-X-IX                     
138107       END-IF                                                             
138207     END-PERFORM                                                          
138307                                                                          
138407*                                                                         
138507     MOVE 4452-KVPTID (W-4452-Y-IX, W-4452-X-IX) TO                       
138607                         PRC-KVPTID (PRC-IX)                              
138707                                                                          
138807     COMPUTE TEMP-SUPMIN ROUNDED            =                             
138907      (PRC-KVPTID (PRC-IX) * PRC-KVRADER-PT (PRC-IX))                     
139007                                                                          
139107     DIVIDE  TEMP-SUPMIN BY 60  GIVING    TEMP-SUPTIM                     
139207     COMPUTE TEMP-SUPMIN = TEMP-SUPMIN - (TEMP-SUPTIM * 60)               
139307                                                                          
139407     DIVIDE  TEMP-SUPMIN BY 100 GIVING    TEMP-SUPTID                     
139507     ADD     TEMP-SUPTIM               TO TEMP-SUPTID                     
139607                                                                          
139707     MOVE    TEMP-SUPTID               TO PRC-SUPTID (PRC-IX)             
139807     .                                                                    
139907     EJECT                                                                
140007                                                                          
140107 CCFB-BERAEKNA-RFS-LST         SECTION.                                   
140207                                                                          
140307     MOVE PRC-IDPRC (PRC-IX)    TO WS-IDPRC                               
140407     PERFORM S04-RAEKNA-RFS                                               
140507     MOVE +011                  TO TIME-KDCALL                            
140607     MOVE PRC-IDPRC (PRC-IX)    TO TIME-IDPRC                             
140707     MOVE WS-IDDC               TO TIME-IDDC                              
140807     MOVE ZERO                  TO TIME-STARTDAT                          
140907     MOVE WS-TIAAMMDD           TO TIME-STOP-TIAAMMDD                     
141007     MOVE WS-TIHHMMSS           TO TIME-STOP-TIHHMMSS                     
141107*                                                                         
141207     MOVE PRC-SUPTID (PRC-IX)   TO SUP-TID                                
141307     MOVE PRC-KVVTID (PRC-IX)   TO WAIT-TID                               
141407*                                                                         
141507     MOVE    SUP-TID            TO SUP-TIM                                
141607     COMPUTE SUP-TID             = SUP-TID - SUP-TIM                      
141707     MOVE    WAIT-TID           TO WAIT-TIM                               
141807     COMPUTE WAIT-TID            = WAIT-TID - WAIT-TIM                    
141907*                                                                         
142007     ADD  WAIT-TIM              TO SUP-TIM                                
142107     ADD  WAIT-TID              TO SUP-TID                                
142207     IF SUP-TID                  > 0.59                                   
142307       ADD +1                   TO SUP-TIM                                
142407       COMPUTE SUP-TID           = SUP-TID - EN-TIMMA                     
142507     END-IF                                                               
142607     MOVE SUP-TIM               TO TIME-TIARB                             
142707     ADD  SUP-TID               TO TIME-TIARB                             
142807     IF TIME-TIARB              =  ZERO                                   
142907         MOVE TIME-STOP-TIAAMMDD TO TIME-START-TIAAMMDD                   
143007         MOVE TIME-STOP-TIHHMMSS TO TIME-START-TIHHMMSS                   
143107      ELSE                                                                
143207         CALL W411TIME USING TIME-W411TIME 4437-PCB                       
143307     END-IF                                                               
143407*                                                                         
143507     MOVE TIME-START-TIAAMMDD   TO WS-TIAAMMDD                            
143607     MOVE TIME-START-TIHHMMSS   TO WS-TIHHMMSS                            
143707*                                                                         
143807     MOVE TID-UTAN-SEK          TO LST-JFR                                
143907     MOVE LST-JFR               TO PRC-TILST-OD (PRC-IX)                  
144007     IF PRC-TILST-OD (PRC-IX)    > ZERO                                   
144107        MOVE PRC-TILST-OD (PRC-IX) TO TMP1-YYMMDDHHMM                     
144207        MOVE WS-TILST-O            TO TMP2-YYMMDDHHMM                     
144307        PERFORM WY2000PB                                                  
144407        IF TMP1-YYMMDDHHMM          < TMP2-YYMMDDHHMM                     
144507          MOVE PRC-TILST-OD(PRC-IX) TO WS-TILST-O                         
144607        END-IF                                                            
144707     END-IF                                                               
144807                                                                          
144907     .                                                                    
145007     EJECT                                                                
145107                                                                          
145207 CD-SKAPA-ORDERDELAR           SECTION.                                   
145307                                                                          
145407     MOVE WS-IDORDER          TO W-MIN-IDORDER                            
145507                                 W-MAX-IDORDER                            
145607     MOVE WS-IDDC             TO W-MIN-IDDC                               
145707                                 W-MAX-IDDC                               
145807     IF WS-IDPLKLST              = ZERO                                   
145907       PERFORM S02-TAG-UT-IDPRODNR                                        
146007     ELSE                                                                 
146107       MOVE SPACE               TO W-IDLEVNR                              
146207       PERFORM IMS-GHU-ORQA-LEVNR                                         
146307*                                                                         
146407       IF SEGMENT-FINNS                                                   
146507          MOVE ODEL-IDPRODNR    TO WS-IDPRODNR                            
146607       ELSE                                                               
146707          PERFORM S02-TAG-UT-IDPRODNR                                     
146807       END-IF                                                             
146907     END-IF                                                               
147007*                                                                         
147107     MOVE +1                    TO PRC-IX                                 
147207     MOVE ZERO                  TO WS-KVANTART                            
147307                                   WS-KVRADER                             
147407                                   WS-VKORDNTO                            
147507                                   WS-VLORDNTO                            
147607                                   WS-KVSEMBRA                            
147707                                   WS-SUORDV                              
147807                                   WS-SUORDV-LOC                          
147907                                   WS-SUORDV-LOCPREL                      
148007     MOVE SPACE                 TO WS-KDVALISO                            
148107     MOVE 'R'                   TO W-KDODELST                             
148207     COMPUTE WS-KVSEMBRA       =                                          
148307         PRC-KVSEMBRA          + PRC-KVSEMBRA-PT                          
148407     PERFORM UNTIL (PRC-IX > 99) OR (PRC-IDPRC (PRC-IX) = '0000')         
148507       PERFORM CDA-SUMMERA                                                
148607       IF WS-KVRADER             > ZERO                                   
148707         MOVE PRC-IDPRC (PRC-IX) TO W-IDPRC                               
148807         PERFORM IMS-GHU-ORQA-PRC                                         
148907         IF SEGMENT-FINNS                                                 
149007           IF WS-KVANTART        = ODEL-KVART     AND                     
149107              WS-KVRADER         = ODEL-KVRADER   AND                     
149207              WS-VKORDNTO        = ODEL-VKORDNTO  AND                     
149307              WS-VLORDNTO        = ODEL-VLORDNTO  AND                     
149407              WS-SUHANTII        = ODEL-SUHANTTI  AND                     
149507              WS-SUORDV          = ODEL-SUORDV    AND                     
149607              WS-SUORDV-LOC      = ODEL-SUORDV-LOC AND                    
149707              WS-SUORDV-LOCPREL  = ODEL-SUORDV-LOCPREL AND                
149807              WS-TILST-O         = ODEL-DALSTORD  AND                     
149907              WS-DARFS           = ODEL-DARFS     AND                     
150007              WS-KDFDKRAV        = ODEL-KDFDKRAV  AND                     
150107              WS-KDPRODKL        = ODEL-KDPRODKL  AND                     
150207              WS-IDTRP           = ODEL-IDTRP     AND                     
150307              TRP-DATRPAVD       = ODEL-DATRPAVD  AND                     
150407              TRP-TIHHMM         = ODEL-TIHHMM    AND                     
150507              PRC-TILST-OD(PRC-IX) = ODEL-TILST-OD AND                    
150607              PRC-SUPTID (PRC-IX) = ODEL-SUPTID   AND                     
150707              PRC-KVPTID (PRC-IX) = ODEL-KVPTID                           
150807              CONTINUE                                                    
150907           ELSE                                                           
151007              MOVE WS-KVANTART       TO ODEL-KVART                        
151107              MOVE WS-KVRADER        TO ODEL-KVRADER                      
151207              MOVE WS-VKORDNTO       TO ODEL-VKORDNTO                     
151307              MOVE WS-VLORDNTO       TO ODEL-VLORDNTO                     
151407              MOVE WS-SUORDV-LOC     TO ODEL-SUORDV-LOC                   
151507              MOVE WS-SUORDV-LOCPREL TO ODEL-SUORDV-LOCPREL               
151607              MOVE WS-KDVALISO       TO ODEL-KDVALISO                     
151707              MOVE WS-SUORDV         TO ODEL-SUORDV                       
151807              MOVE WS-TILST-O        TO ODEL-DALSTORD                     
151907              IF WS-TILST-O NOT = ZERO                                    
152007                IF WS-TILST-O < 5000000000                                
152107                  MOVE 20            TO ODEL-DALSTORD (1:2)               
152207                ELSE                                                      
152307                  IF WS-TILST-O < 9999999999                              
152407                    MOVE 19          TO ODEL-DALSTORD (1:2)               
152507                  ELSE                                                    
152607                    MOVE 999999999999  TO ODEL-DALSTORD                   
152707                  END-IF                                                  
152807                END-IF                                                    
152907              END-IF                                                      
153107              MOVE WS-DARFS      TO ODEL-DARFS                            
153207              MOVE WS-DARFS-DATUM TO ODEL-DARFSDAT                        
153307              MOVE WS-KDPRODKL   TO ODEL-KDPRODKL                         
153407              MOVE WS-KDFDKRAV   TO ODEL-KDFDKRAV                         
153507              MOVE WS-IDTRP      TO ODEL-IDTRP                            
153607              IF WS-HELP-TIAAAAMMDD(3:6) > ZERO                           
153707                 MOVE WS-HELP-TIAAAAMMDD TO ODEL-DATRPAVD                 
153807                 MOVE WS-HELP-TIHHMM     TO ODEL-TIHHMM                   
153907              ELSE                                                        
154007                 MOVE TRP-DATRPAVD       TO ODEL-DATRPAVD                 
154107                 MOVE TRP-TIHHMM         TO ODEL-TIHHMM                   
154207              END-IF                                                      
154307              MOVE PRC-TILST-OD(PRC-IX) TO ODEL-TILST-OD                  
154407              MOVE PRC-SUPTID (PRC-IX) TO ODEL-SUPTID                     
154507              MOVE PRC-KVPTID (PRC-IX) TO ODEL-KVPTID                     
154607              PERFORM IMS-REPL-ORQA                                       
154707           END-IF                                                         
154807         ELSE                                                             
154907           PERFORM CDB-INIT-ORDERDEL                                      
155007           PERFORM IMS-ISRT-ORQA                                          
155107         END-IF                                                           
155207       END-IF                                                             
155307       ADD +1                    TO PRC-IX                                
155407     END-PERFORM                                                          
155507     .                                                                    
155607     EJECT                                                                
155707                                                                          
155807 CDA-SUMMERA                   SECTION.                                   
155907                                                                          
156007     COMPUTE WS-KVANTART         =                                        
156107         PRC-KVANTART (PRC-IX) + PRC-KVANTART-PT (PRC-IX)                 
156207     COMPUTE WS-KVRADER          =                                        
156307         PRC-KVRADER  (PRC-IX) + PRC-KVRADER-PT  (PRC-IX)                 
156407     COMPUTE WS-VKORDNTO ROUNDED =                                        
156507         PRC-VKORDNTO (PRC-IX) + PRC-VKORDNTO-PT (PRC-IX)                 
156607     COMPUTE WS-VLORDNTO ROUNDED =                                        
156707         PRC-VLORDNTO (PRC-IX) + PRC-VLORDNTO-PT (PRC-IX)                 
156807     COMPUTE WS-SUORDV-LOC ROUNDED =                                      
156907         PRC-SUORDV-LOC (PRC-IX) + PRC-SUORDV-PT-LOC   (PRC-IX)           
157007     COMPUTE WS-SUORDV-LOCPREL ROUNDED =                                  
157107         PRC-SUORDV-LOCPREL (PRC-IX) +                                    
157207                                 PRC-SUORDV-PT-LOCPREL (PRC-IX)           
157307     COMPUTE WS-SUORDV ROUNDED =                                          
157407         PRC-SUORDV   (PRC-IX) + PRC-SUORDV-PT   (PRC-IX)                 
157507                                                                          
157607     IF WS-KDVALISO = SPACE                                               
157707       MOVE PRC-KDVALISO(PRC-IX)  TO WS-KDVALISO                          
157807     END-IF                                                               
157907     .                                                                    
158007     EJECT                                                                
158107                                                                          
158207 CDB-INIT-ORDERDEL             SECTION.                                   
158307                                                                          
158407     MOVE ZERO                TO ODEL-KVPACKRAD-OD                        
158507                                 ODEL-TIPACKN                             
158607                                 ODEL-TIPACTID                            
158707                                 ODEL-DAUTSKR                             
158807                                 ODEL-TIUTSTID                            
158907                                 ODEL-SUHANTTI                            
159007     MOVE SPACE               TO ODEL-IDBORD                              
159107                                 ODEL-IDLEVNR                             
159207                                 ODEL-IDUSER                              
159307     MOVE AVSO-IDGMTREF       TO ODEL-IDGMTREF                            
159407     MOVE WS-IDORDER          TO ODEL-IDORDER                             
159507     ADD +1                   TO WS-IDPLKLST                              
159607     MOVE WS-IDPLKLST         TO ODEL-IDPLKLST                            
159707     MOVE WS-IDPRODNR         TO ODEL-IDPRODNR                            
159807     MOVE PRC-IDPRC (PRC-IX)  TO ODEL-IDPRC                               
159907     MOVE WS-IDTRP            TO ODEL-IDTRP                               
160007     MOVE WS-IDDC             TO ODEL-IDDC                                
160107     MOVE WS-KDFDKRAV         TO ODEL-KDFDKRAV                            
160207     MOVE 'R'                 TO ODEL-KDODELSTA                           
160307     MOVE WS-KDPRODKL         TO ODEL-KDPRODKL                            
160407     MOVE WS-KVANTART         TO ODEL-KVART                               
160507     MOVE PRC-KVPTID (PRC-IX) TO ODEL-KVPTID                              
160607     MOVE WS-KVRADER          TO ODEL-KVRADER                             
160707     MOVE WS-SUORDV-LOC       TO ODEL-SUORDV-LOC                          
160807     MOVE WS-SUORDV-LOCPREL   TO ODEL-SUORDV-LOCPREL                      
160907     MOVE WS-KDVALISO         TO ODEL-KDVALISO                            
161007     MOVE WS-SUORDV           TO ODEL-SUORDV                              
161107     MOVE PRC-SUPTID (PRC-IX) TO ODEL-SUPTID                              
161207     MOVE WS-TILST-O          TO ODEL-DALSTORD                            
161307     IF WS-TILST-O NOT = ZERO                                             
161407       IF WS-TILST-O < 5000000000                                         
161507         MOVE 20              TO ODEL-DALSTORD (1:2)                      
161607       ELSE                                                               
161707         IF WS-TILST-O < 9999999999                                       
161807           MOVE 19            TO ODEL-DALSTORD (1:2)                      
161907         ELSE                                                             
162007           MOVE 999999999999  TO ODEL-DALSTORD                            
162107         END-IF                                                           
162207       END-IF                                                             
162307     END-IF                                                               
162407     MOVE PRC-TILST-OD (PRC-IX) TO ODEL-TILST-OD                          
162507     MOVE WS-DARFS            TO ODEL-DARFS                               
162607     MOVE WS-DARFS-DATUM      TO ODEL-DARFSDAT                            
162707     IF WS-HELP-TIAAAAMMDD(3:6) > ZERO                                    
162807        MOVE WS-HELP-TIAAAAMMDD TO ODEL-DATRPAVD                          
162907        MOVE WS-HELP-TIHHMM     TO ODEL-TIHHMM                            
163007     ELSE                                                                 
163107        MOVE TRP-DATRPAVD       TO ODEL-DATRPAVD                          
163207        MOVE TRP-TIHHMM         TO ODEL-TIHHMM                            
163307     END-IF                                                               
163407     MOVE WS-TIREGDAT         TO ODEL-TIREGDAT                            
163507     MOVE WS-TIREGTID         TO ODEL-TIREGTID                            
163607     MOVE WS-VKORDNTO         TO ODEL-VKORDNTO                            
163707     MOVE WS-VLORDNTO         TO ODEL-VLORDNTO                            
163807     MOVE SPACE               TO ODEL-IDPRCPLK                            
163907     MOVE ZERO                TO ODEL-IDLOTNR-PLK                         
164007                                 ODEL-IDLOPNR-ORD                         
164107     PERFORM CDAA-GET-ODEL-IDDC-EXP                                       
164207     .                                                                    
164307     EJECT                                                                
164407                                                                          
164507 CDAA-GET-ODEL-IDDC-EXP        SECTION.                                   
164607                                                                          
164707     MOVE SPACE             TO ODEL-IDDC-EXP                              
164807     MOVE ODEL-IDDC         TO W-IDDC-B6                                  
164907     PERFORM IMS-GU-WDB601                                                
165007     MOVE DCS-IDFTG         TO WS-WDB6-IDFTG                              
165107                                                                          
165207     MOVE AVSO-IDDISTR      TO W-IDDISTR-WDB2                             
165307     MOVE AVSO-IDKUNDNR     TO W-IDKUNDNR-WDB2                            
165407     PERFORM IMS-GU-WDB201                                                
165507                                                                          
165607*    *CANADA HAS IDFTG=54 ON WDB6 AND USA HAS IDFTG=53 ON WDB6.           
165707*    *DISTRICT/CUSTOMER FOR US AND CA HAS IDFTG=57 ON WDB2.               
165807*    *MEANS THAT IT WILL BE BOUNCE ORDER -> NOT CORRECT!!                 
165907*    *EXCEPTION IS REFILL DISTRICTS FOR US OR IMPORTER FOR US             
166007*    *THAT SHOULD BE BOUNCE.                                              
166107     IF WS-WDB6-IDFTG = WC-IDFTG-US OR WC-IDFTG-CA                        
166207        IF WS-WDB6-IDFTG = WC-IDFTG-CA                                    
166307           MOVE WC-IDFTG-PV TO WS-WDB6-IDFTG                              
166407        ELSE                                                              
166507          IF DIST35-NONVCC-REFILL   OR GMT-KDKUNDKAT = '04'               
166607             CONTINUE                                                     
166707          ELSE                                                            
166807             MOVE WC-IDFTG-PV TO WS-WDB6-IDFTG                            
166907          END-IF                                                          
167007        END-IF                                                            
167107     END-IF                                                               
167207                                                                          
167307     IF WS-WDB6-IDFTG NOT = GMT-IDFTG AND                                 
167407        NOT (DIST35-CN-CDC-RETURNS OR                                     
167507             DIST35-IN-CDC-RETURNS OR                                     
167607             DIST35-KR-CDC-RETURNS OR                                     
167707             DIST35-AE-CDC-RETURNS OR                                     
167807             DIST35-MY-CDC-RETURNS OR                                     
167907             DIST35-TR-CDC-RETURNS OR                                     
167908             DIST35-MX-CDC-RETURNS OR                                     
167909             DIST35-BR-CDC-RETURNS OR                                     
168007             DIST35-TH-CDC-RETURNS OR                                     
168107             DIST35-TW-CDC-RETURNS OR                                     
168108             DIST35-ZA-CDC-RETURNS OR                                     
168207*            DIST35-JP-NDC-RETURNS OR                                     
168307             DIST18-SCRAP-NDC-QUAL OR                                     
168407             WS-IDSYSTEM = 'SOFT')                                        
168507        MOVE GMT-IDFTG      TO W-IDFTG                                    
168607        MOVE JA             TO W-FLMAINDC                                 
168707        PERFORM IMS-GU-WDB601-EXP                                         
168807        MOVE DCS-IDDC       TO ODEL-IDDC-EXP                              
168907     END-IF                                                               
169007     .                                                                    
169107     EJECT                                                                
169207                                                                          
169307 D-RENSA-ODEL-UTAN-RADER       SECTION.                                   
169407                                                                          
169507     MOVE WS-IDORDER           TO W-Q3-IDORDERF                           
169607                                  W-Q3-IDORDERT                           
169707     MOVE WS-IDDC              TO W-Q3-IDDCF                              
169807                                  W-Q3-IDDCT                              
169907     MOVE WS-IDPRODNR          TO W-Q3-IDPRODNRF                          
170007                                  W-Q3-IDPRODNRT                          
170107     MOVE 'R'                  TO W-KDODELST                              
170207     PERFORM IMS-GHU-ORQA                                                 
170307     PERFORM UNTIL SEGMENT-SAKNAS                                         
170407       MOVE +1                 TO PRC-IX                                  
170507       MOVE NEJ                TO SW-TRAEFF                               
170607       PERFORM UNTIL (PRC-IX > 99) OR (SW-TRAEFF = JA) OR                 
170707                     (PRC-IDPRC (PRC-IX) = '0000')                        
170807         IF PRC-IDPRC      (PRC-IX)  = ODEL-IDPRC                         
170907            AND                                                           
171007           (PRC-KVRADER    (PRC-IX)  > 0                                  
171107            OR                                                            
171207            PRC-KVRADER-PT (PRC-IX)  > 0)                                 
171307            MOVE JA             TO SW-TRAEFF                              
171407         END-IF                                                           
171507         ADD +1                TO PRC-IX                                  
171607       END-PERFORM                                                        
171707       IF SW-TRAEFF             = NEJ                                     
171807         PERFORM IMS-DLET-ORQA                                            
171907       END-IF                                                             
172007       PERFORM IMS-GHN-ORQA                                               
172107     END-PERFORM                                                          
172207     .                                                                    
172307     EJECT                                                                
172407                                                                          
172507 E-KOMPLETTERA-ARBTAB           SECTION.                                  
172607                                                                          
172707     MOVE WS-IDORDER           TO W-IDORDER                               
172807     PERFORM IMS-GU-WDQ201                                                
172907     MOVE WS-IDDC              TO W-IDDC                                  
173007     PERFORM IMS-GHNP-WDQ212                                              
173107*                                                                         
173207     IF WS-FLODELUT            = JA                                       
173307         PERFORM S05-SOEK-IDDC-TAB                                        
173407         IF IDDC-TRAEFF                                                   
173507             MOVE JA           TO ARB-FLODELUT                            
173607         END-IF                                                           
173707     END-IF                                                               
173807*                                                                         
173907     MOVE WS-IDPLKLST          TO ARB-IDPLKLST-SISTA                      
174007     MOVE WS-TIRFS             TO ARB-TIRFS                               
174107     MOVE TRP-DATRPAVD         TO ARB-DATRPAVD                            
174207     MOVE TRP-TIHHMM           TO ARB-TIHHMM                              
174307*                                                                         
174407     IF WS-HELP-TIAAAAMMDD(3:6) > ZERO                                    
174507        MOVE WS-HELP-TIAAAAMMDD  TO ARB-DATRPAVD                          
174607        MOVE WS-HELP-TIHHMM      TO ARB-TIHHMM                            
174707     END-IF                                                               
174807*                                                                         
174907     PERFORM IMS-REPL-WDQ212                                              
175007*                                                                         
175008     PERFORM IMS-GHNP-WDQ221                                              
175009     PERFORM UNTIL SEGMENT-SAKNAS                                         
175010       MOVE WS-LOR-IDPRC (LOR-ADLAGOMR) TO LOR-IDPRC                      
175020       PERFORM IMS-REPL-WDQ221                                            
175030       PERFORM IMS-GHNP-WDQ221                                            
175040     END-PERFORM                                                          
176507     .                                                                    
176607     EJECT                                                                
177007                                                                          
177100 G-AENDRA-RFS-TRP              SECTION.                                   
177200                                                                          
177300     PERFORM IMS-GU-WDQ201                                                
177400     PERFORM IMS-GHNP-WDQ212                                              
177500     IF AVSO-TIRFS              > +0                                      
177600        MOVE AVSO-TIRFS        TO ARB-TIRFS                               
177700     END-IF                                                               
177800     IF AVSO-TIAAMMDD           > +0 AND                                  
177900        AVSO-TIHHMM             > +0                                      
178000        MOVE AVSO-TIAAMMDD     TO ARB-DATRPAVD                            
178100        IF AVSO-TIAAMMDD NOT = ZERO                                       
178200          IF AVSO-TIAAMMDD < 500000                                       
178300            MOVE 20            TO ARB-DATRPAVD (1:2)                      
178400          ELSE                                                            
178500            IF AVSO-TIAAMMDD < 999999                                     
178600              MOVE 19          TO ARB-DATRPAVD (1:2)                      
178700            ELSE                                                          
178800              MOVE 99999999    TO ARB-DATRPAVD                            
178900            END-IF                                                        
179000          END-IF                                                          
179100        END-IF                                                            
179200        MOVE AVSO-TIHHMM       TO ARB-TIHHMM                              
179300     END-IF                                                               
179400     IF WS-FLODELUT             = JA                                      
179500       MOVE JA                 TO ARB-FLODELUT                            
179600     END-IF                                                               
179700     IF WS-HELP-TIAAAAMMDD(3:6) > ZERO                                    
179800        MOVE WS-HELP-TIAAAAMMDD  TO ARB-DATRPAVD                          
179900        MOVE WS-HELP-TIHHMM      TO ARB-TIHHMM                            
180000     END-IF                                                               
180105     PERFORM IMS-REPL-WDQ212                                              
180200     .                                                                    
180300     EJECT                                                                
180400                                                                          
180500 H-RENSA-ALLA-ORDERDELAR       SECTION.                                   
180600                                                                          
180700     PERFORM IMS-GU-WDQ201                                                
180800     MOVE WS-IDDC              TO W-IDDC                                  
180900     PERFORM IMS-GHNP-WDQ212                                              
181012                                                                          
182500     IF WS-FLODELUT            = JA                                       
182600         PERFORM S05-SOEK-IDDC-TAB                                        
182700         IF IDDC-TRAEFF                                                   
182800             MOVE JA           TO ARB-FLODELUT                            
182900         END-IF                                                           
183000     END-IF                                                               
183100     IF ARB-FLODELUT            = NEJ                                     
183200       MOVE ZERO               TO ARB-IDPLKLST-SISTA                      
183300                                  ARB-KVSEMBRA                            
183400*                                 ARB-TIRFS                               
183500     END-IF                                                               
183600     IF WS-HELP-TIAAAAMMDD(3:6) > ZERO                                    
183700        MOVE WS-HELP-TIAAAAMMDD  TO ARB-DATRPAVD                          
183800        MOVE WS-HELP-TIHHMM      TO ARB-TIHHMM                            
183900     END-IF                                                               
184005     PERFORM IMS-REPL-WDQ212                                              
184100*                                                                         
184205* CLEAN UP ALL WDQ221 RECORDS                                             
184305     PERFORM IMS-GHNP-WDQ221                                              
184405     PERFORM UNTIL SEGMENT-SAKNAS                                         
184505        PERFORM IMS-DLET-WDQ221                                           
184605        PERFORM IMS-GHNP-WDQ221                                           
184705     END-PERFORM                                                          
186105*                                                                         
186200     MOVE LOW-VALUE            TO W-WDQ301KY-FOM                          
186300     MOVE HIGH-VALUE           TO W-WDQ301KY-TOM                          
186400     MOVE WS-IDORDER           TO W-Q3-IDORDERF                           
186500                                  W-Q3-IDORDERT                           
186600     MOVE ARB-IDDC             TO W-Q3-IDDCF                              
186700                                  W-Q3-IDDCT                              
186800     MOVE 'R'                  TO W-KDODELST                              
186900     MOVE SPACE                TO W-IDLEVNR                               
187000     PERFORM IMS-GHU-ORQA-ANN                                             
187100     IF SEGMENT-FINNS                                                     
187200       MOVE ODEL-IDPRODNR      TO W-Q3-IDPRODNRF                          
187300                                  W-Q3-IDPRODNRT                          
187400                                  W-IDPRODNR                              
187500       PERFORM IMS-DLET-ORQA                                              
187600       PERFORM IMS-GHN-ORQA                                               
187700       PERFORM UNTIL SEGMENT-SAKNAS                                       
187800         PERFORM IMS-DLET-ORQA                                            
187900         PERFORM IMS-GHN-ORQA                                             
188000       END-PERFORM                                                        
188100       PERFORM HA-KOLLA-STATUS-WDE601                                     
188200     END-IF                                                               
188300     .                                                                    
188400     EJECT                                                                
188500                                                                          
188600 HA-KOLLA-STATUS-WDE601        SECTION.                                   
188700                                                                          
188800     MOVE JA TO SW-UPPDAT-WDE601                                          
188900                                                                          
189000     PERFORM IMS-GU-ORQA-FOM-TOM                                          
189100                                                                          
189200     IF SEGMENT-FINNS                                                     
189300        IF ODEL-KDODELSTA NOT = 'P'                                       
189400           MOVE NEJ TO SW-UPPDAT-WDE601                                   
189500        END-IF                                                            
189600     ELSE                                                                 
189700        MOVE NEJ TO SW-UPPDAT-WDE601                                      
189800     END-IF                                                               
189900                                                                          
190000     PERFORM UNTIL SEGMENT-SAKNAS   OR                                    
190100                   BASEN-SLUT       OR                                    
190200                   EJ-UPPDAT-WDE601                                       
190300        PERFORM IMS-GN-ORQA-FOM-TOM                                       
190400        IF SEGMENT-FINNS                                                  
190500           IF ODEL-KDODELSTA NOT = 'P'                                    
190600              MOVE NEJ TO SW-UPPDAT-WDE601                                
190700           ELSE                                                           
190800              PERFORM IMS-GN-ORQA-FOM-TOM                                 
190900           END-IF                                                         
191000        END-IF                                                            
191100     END-PERFORM                                                          
191200                                                                          
191300     IF UPPDAT-WDE601                                                     
191400        PERFORM HAA-UPPDAT-WDE601                                         
191500     END-IF                                                               
191600     .                                                                    
191700     EJECT                                                                
191800                                                                          
191900 HAA-UPPDAT-WDE601             SECTION.                                   
192000                                                                          
192100     PERFORM IMS-GHU-WDE601                                               
192200                                                                          
192300     IF SEGMENT-FINNS AND VORD-KDORDSTA < 3                               
192400        IF VORD-KVKOLPAC = 0                                              
192500           ACCEPT VORD-TIPACKN-SK FROM DATE                               
192600        END-IF                                                            
192700        MOVE 3 TO VORD-KDORDSTA                                           
192800        IF VORD-KVKOLLI = VORD-KVKOLLI-FL                                 
192900           MOVE 4 TO VORD-KDORDSTA                                        
193000        END-IF                                                            
193100        IF VORD-KVKOLLI = VORD-KVKOLLI-FAKT   AND                         
193200           VORD-KVKOLLI = VORD-KVKOLLI-LAST                               
193300           MOVE 5 TO VORD-KDORDSTA                                        
193400        END-IF                                                            
193500        PERFORM IMS-REPL-WDE601                                           
193600     END-IF                                                               
193700     .                                                                    
193800     EJECT                                                                
193900                                                                          
194000 I-FORENKLAT-ORDERAVSLUT       SECTION.                                   
194100                                                                          
194200     MOVE WS-IDORDER  TO W-MIN-IDORDER                                    
194300                         W-MAX-IDORDER                                    
194400     MOVE WS-IDDC     TO W-MIN-IDDC                                       
194500                         W-MAX-IDDC                                       
194600     MOVE 'R'         TO W-KDODELST                                       
194700     MOVE SPACE       TO W-IDLEVNR                                        
194800                                                                          
194900     PERFORM IMS-GHU-ORQA-MIN-MAX                                         
195000                                                                          
195100     PERFORM UNTIL SEGMENT-SAKNAS                                         
195200        PERFORM IA-KOLLA-MOT-ARBTABELL                                    
195300        PERFORM IMS-GHN-ORQA-MIN-MAX                                      
195400     END-PERFORM                                                          
195500     .                                                                    
195600     EJECT                                                                
195700                                                                          
195800 IA-KOLLA-MOT-ARBTABELL        SECTION.                                   
195900                                                                          
196000     MOVE 0  TO INT-KVANTART (1)                                          
196100                INT-KVRADER  (1)                                          
196200                INT-SUORDV   (1)                                          
196300                INT-SUORDV-LOC   (1)                                      
196400                INT-SUORDV-LOCPREL   (1)                                  
196500                INT-VKORDNTO (1)                                          
196600                INT-VLORDNTO (1)                                          
196700                                                                          
196800     MOVE 1 TO LO-IX                                                      
196900     PERFORM UNTIL LO-IX > 99                                             
197001        IF WS-LOR-IDPRC (LO-IX) = ODEL-IDPRC                              
197101           ADD WS-LOR-KVANTART (LO-IX)  TO INT-KVANTART (1)               
197201           ADD WS-LOR-KVRADER  (LO-IX)  TO INT-KVRADER  (1)               
197301           ADD WS-LOR-SUORDV-LOC (LO-IX)                                  
197400                                        TO INT-SUORDV-LOC     (1)         
197501           ADD WS-LOR-SUORDV-LOCPREL (LO-IX)                              
197600                                        TO INT-SUORDV-LOCPREL (1)         
197701           ADD WS-LOR-SUORDV (LO-IX)    TO INT-SUORDV   (1)               
197801           ADD WS-LOR-VKORDNTO (LO-IX)  TO INT-VKORDNTO (1)               
197901           ADD WS-LOR-VLORDNTO (LO-IX)  TO INT-VLORDNTO (1)               
198000*          IF INT-KDVALISO(1) = SPACE                                     
198101            MOVE WS-LOR-KDVALISO(LO-IX)  TO INT-KDVALISO (1)              
198200*          END-IF                                                         
198300        END-IF                                                            
198400        ADD 1 TO LO-IX                                                    
198500     END-PERFORM                                                          
198600                                                                          
198700     IF INT-KVANTART (1) = ODEL-KVART       AND                           
198800        INT-KVRADER  (1) = ODEL-KVRADER     AND                           
198900        INT-SUORDV-LOC     (1) = ODEL-SUORDV-LOC     AND                  
199000        INT-SUORDV-LOCPREL (1) = ODEL-SUORDV-LOCPREL AND                  
199100        INT-SUORDV   (1) = ODEL-SUORDV      AND                           
199200        INT-VKORDNTO (1) = ODEL-VKORDNTO    AND                           
199300        INT-VLORDNTO (1) = ODEL-VLORDNTO                                  
199400        CONTINUE                                                          
199500     ELSE                                                                 
199600        IF INT-KVRADER (1) = 0                                            
199700           PERFORM IMS-DLET-ORQA                                          
199800        ELSE                                                              
199900           MOVE INT-KVANTART (1) TO ODEL-KVART                            
200000           MOVE INT-KVRADER  (1) TO ODEL-KVRADER                          
200100           MOVE INT-SUORDV   (1) TO ODEL-SUORDV                           
200200           MOVE INT-SUORDV-LOC  (1) TO ODEL-SUORDV-LOC                    
200300           MOVE INT-SUORDV-LOCPREL  (1) TO ODEL-SUORDV-LOCPREL            
200400           MOVE INT-KDVALISO (1) TO ODEL-KDVALISO                         
200500                                                                          
200600           COMPUTE ODEL-VKORDNTO ROUNDED = INT-VKORDNTO (1)               
200700           COMPUTE ODEL-VLORDNTO ROUNDED = INT-VLORDNTO (1)               
200800           PERFORM IMS-REPL-ORQA                                          
200900        END-IF                                                            
201000     END-IF                                                               
201100     .                                                                    
201200     EJECT                                                                
201300                                                                          
201400 S01-NOLLA-PRC-TAB             SECTION.                                   
201500                                                                          
201600     MOVE +1                 TO PRC-IX                                    
201700     MOVE ZERO               TO PRC-KVSEMBRA-PT                           
201800                                PRC-KVSEMBRA                              
201900     PERFORM UNTIL PRC-IX     > 99                                        
202000       MOVE ZERO             TO PRC-IDPTIDTAB         (PRC-IX)            
202100                                PRC-TILST-OD          (PRC-IX)            
202200                                PRC-KVVTID            (PRC-IX)            
202300                                PRC-KVANTART-PT       (PRC-IX)            
202400                                PRC-KVRADER-PT        (PRC-IX)            
202500                                PRC-VKORDNTO-PT       (PRC-IX)            
202600                                PRC-VLORDNTO-PT       (PRC-IX)            
202700                                PRC-SUORDV-PT         (PRC-IX)            
202800                                PRC-SUORDV-PT-LOC     (PRC-IX)            
202900                                PRC-SUORDV-PT-LOCPREL (PRC-IX)            
203000                                PRC-SUPTID            (PRC-IX)            
203100                                PRC-KVPTID            (PRC-IX)            
203200                                PRC-KVANTART          (PRC-IX)            
203300                                PRC-KVRADER           (PRC-IX)            
203400                                PRC-VKORDNTO          (PRC-IX)            
203500                                PRC-VLORDNTO          (PRC-IX)            
203600                                PRC-SUORDV            (PRC-IX)            
203700                                PRC-SUORDV-LOC        (PRC-IX)            
203800                                PRC-SUORDV-LOCPREL    (PRC-IX)            
203900       MOVE SPACE            TO PRC-KDVALISO          (PRC-IX)            
204000       MOVE '0000'           TO PRC-IDPRC             (PRC-IX)            
204100       ADD +1                TO PRC-IX                                    
204200     END-PERFORM                                                          
204300     MOVE +99999999999       TO WS-TILST-O                                
204400     MOVE ZERO               TO WS-VKORDNTO                               
204500                                WS-VLORDNTO                               
204600                                WS-KVRADER                                
204700     .                                                                    
204800     EJECT                                                                
204900                                                                          
205000 S02-TAG-UT-IDPRODNR           SECTION.                                   
205100                                                                          
205200     PERFORM S02A-CALL-W411ORDN                                           
205300     .                                                                    
205400     SKIP2                                                                
205500                                                                          
205601 S03-POPULATE-WS-LOR           SECTION.                                   
205602                                                                          
205903     MOVE 1     TO LO-IX                                                  
207503     PERFORM UNTIL LO-IX > 99                                             
207603       MOVE '0000'             TO WS-LOR-IDPRC    (LO-IX)                 
207704       MOVE ZERO               TO WS-LOR-KVANTART (LO-IX)                 
207804                                  WS-LOR-KVRADER  (LO-IX)                 
207904                                  WS-LOR-SUORDV   (LO-IX)                 
208004                                  WS-LOR-SUORDV-LOC  (LO-IX)              
208104                                  WS-LOR-SUORDV-LOCPREL  (LO-IX)          
208204                                  WS-LOR-VKORDNTO (LO-IX)                 
208304                                  WS-LOR-VLORDNTO (LO-IX)                 
208404       MOVE SPACE              TO WS-LOR-KDVALISO (LO-IX)                 
208503       ADD +1                  TO LO-IX                                   
208603     END-PERFORM                                                          
208604                                                                          
208605     PERFORM UNTIL SEGMENT-SAKNAS                                         
208606        MOVE LOR-ADLAGOMR        TO LO-IX                                 
208607        MOVE LOR-IDPRC           TO WS-LOR-IDPRC          (LO-IX)         
208608        MOVE LOR-KVANTART        TO WS-LOR-KVANTART       (LO-IX)         
208609        MOVE LOR-KVRADER         TO WS-LOR-KVRADER        (LO-IX)         
208610        MOVE LOR-SUORDV          TO WS-LOR-SUORDV         (LO-IX)         
208611        MOVE LOR-VKORDNTO        TO WS-LOR-VKORDNTO       (LO-IX)         
208612        MOVE LOR-VLORDNTO        TO WS-LOR-VLORDNTO       (LO-IX)         
208613        MOVE LOR-SUORDV-LOC      TO WS-LOR-SUORDV-LOC     (LO-IX)         
208614        MOVE LOR-SUORDV-LOCPREL  TO WS-LOR-SUORDV-LOCPREL (LO-IX)         
208615        MOVE LOR-KDVALISO        TO WS-LOR-KDVALISO       (LO-IX)         
208616        PERFORM  IMS-GNP-WDQ221                                           
208617     END-PERFORM                                                          
208701     .                                                                    
208801     SKIP2                                                                
208901                                                                          
209001 S02A-CALL-W411ORDN            SECTION.                                   
209101                                                                          
209201     MOVE 'WOPS'              TO ORDN-IDSYSTEM                            
209301     CALL W411ORDN USING ORDN-W411ORDN XXKP-PCB                           
209401                                  ORDN-ORQL-PCB                           
209501                                  ORDN-PROC-PCB                           
209601                                  ORDN-ORQI-PCB                           
209701                                  ORDN-WDQ3-PCB                           
209801     MOVE ORDN-IDPRODNR-UT    TO WS-IDPRODNR                              
209901     .                                                                    
210001                                                                          
210101 S04-RAEKNA-RFS                SECTION.                                   
210201*                                                                         
210301     IF AVSO-TIAAMMDD > ZERO                                              
210401        MOVE AVSO-TIAAMMDD      TO TIAAMMDD                               
210501                                   TRP-DATRPAVD                           
210601        IF AVSO-TIAAMMDD NOT = ZERO                                       
210701          IF AVSO-TIAAMMDD < 500000                                       
210801            MOVE 20             TO TRP-DATRPAVD (1:2)                     
210901          ELSE                                                            
211001            IF AVSO-TIAAMMDD < 999999                                     
211101              MOVE 19           TO TRP-DATRPAVD (1:2)                     
211201            ELSE                                                          
211301              MOVE 99999999     TO TRP-DATRPAVD                           
211401            END-IF                                                        
211501          END-IF                                                          
211601        END-IF                                                            
211701        MOVE AVSO-TIHHMM        TO TIHHMM                                 
211801                                   TRP-TIHHMM                             
211901     ELSE                                                                 
212001        MOVE WS-ARB-DATRPAVD (3:6) TO TIAAMMDD                            
212101        MOVE WS-ARB-TIHHMM      TO TIHHMM                                 
212201*  FÖR ATT VARA SÄKER BERÄKNAS EN NY TRANSPORTAVGÅNGSTID                  
212301*  (OM DET INTE GÄLLER TILLÄGG AV RADER IDTRANS = 4203 ELLER 4255)        
212401*  FÖR ATT SÄKERSTÄLLA ATT VI FÅR RÄTT RFS NEDAN.                         
212501        IF (AVSO-IDTRANS = '4203' OR '4204' OR '4255' OR '4258')          
212601        OR ((WS-IDSYSTEM = 'LDC ' OR 'TACD' OR 'LDCB' OR 'LDCD'           
212701        OR 'LYNB' OR 'LYND' OR 'ECOB' OR 'ECOD'                           
212801        OR 'VOUB' OR 'VOUD' OR 'TADB' OR 'TADD'                           
212901        OR 'ACCB' OR 'ACCD' OR 'APAB' OR 'APAD'OR 'APBB' OR 'APBD'        
213001        OR 'APCB' OR 'APCD' OR 'APDB' OR 'APDD'OR 'APEB' OR 'APED'        
213101        OR 'APFB' OR 'APFD' OR 'APGB' OR 'APGD'OR 'APHB' OR 'APHD'        
213201        OR 'APIB' OR 'APID' OR 'APJB' OR 'APJD')                          
213301        AND (WS-KDORDKL = 3 OR 2))                                        
213401           CONTINUE                                                       
213501        ELSE                                                              
213601           PERFORM S04B-RAEKNA-NY-TRP-AVG-TID                             
213701           IF TRAN-KDSVAR = K-TRAN-KDSVAR-0-OK                            
213801              IF NOT TRAN-TIAAMMDD = TIAAMMDD OR                          
213901                 NOT TRAN-TIHHMM   = TIHHMM                               
214001                                                                          
214101* OM TRANSPORTAVGÅNGSTID BERÄKNATS TIDIGARE                               
214201* SKALL EVENTUELLT NY TID UPPDATERAS PÅ WDQ2 OCH WDQ3                     
214301                 COMPUTE WS-HELP-TIAAAAMMDD =                             
214401                         TRAN-TIAAMMDD + 20000000                         
214501                 MOVE TRAN-TIHHMM  TO WS-HELP-TIHHMM                      
214601                                                                          
214701                 IF WS-ARB-DATRPAVD > ZERO AND                            
214801                    WS-ARB-TIHHMM   > ZERO AND                            
214901                                                                          
215001                   (NOT WS-HELP-TIAAAAMMDD = WS-ARB-DATRPAVD OR           
215101                    NOT TRAN-TIHHMM   = WS-ARB-TIHHMM)                    
215201                                                                          
215301                    MOVE TRAN-TIAAMMDD      TO TIAAMMDD                   
215401                    MOVE TRAN-TIHHMM        TO TIHHMM                     
215501                 ELSE                                                     
215601                    MOVE ZERO  TO WS-HELP-TIAAAAMMDD                      
215701                                  WS-HELP-TIHHMM                          
215801                 END-IF                                                   
215901                                                                          
216001              END-IF                                                      
216101           END-IF                                                         
216201        END-IF                                                            
216301     END-IF                                                               
216401     MOVE ZERO                  TO SEKUNDER                               
216501     IF AVSO-TIRFS > ZERO                                                 
216601        MOVE AVSO-TIRFS         TO WS-RFS                                 
216701     ELSE                                                                 
216801        MOVE WS-ARB-TIRFS       TO WS-RFS                                 
216901     END-IF                                                               
217001     DIVIDE WS-RFS BY 10000 GIVING RFS-DATE                               
217101                         REMAINDER RFS-TIME                               
217201     IF WS-RFS                   = ZERO                                   
217301       PERFORM S04A-BERAEKNA-RFS                                          
217401       MOVE TID-UTAN-SEK        TO WS-TIRFS                               
217501     ELSE                                                                 
217601       IF WS-RFS                 > ZERO                                   
217701         IF TID-UTAN-SEK         = ZERO                                   
217801           MOVE WS-RFS          TO WS-TIRFS                               
217901           MOVE WS-TIRFS        TO TID-UTAN-SEK                           
218001                                   LST-JFR                                
218101         ELSE                                                             
218201           PERFORM S04A-BERAEKNA-RFS                                      
218301           MOVE TIAAMMDD           TO TMP1-C-YYMMDD                       
218401           MOVE TIHHMM             TO TMP1-C-HHMM                         
218501           MOVE RFS-DATE           TO TMP2-C-YYMMDD                       
218601           MOVE RFS-TIME           TO TMP2-C-HHMM                         
218602           IF   RFS-TIME > 0                                              
218701             PERFORM WY2000PC                                             
218801             IF TMP1-YYMMDD-HHMM > TMP2-YYMMDD-HHMM                       
218901               MOVE WS-RFS  TO WS-TIRFS                                   
219001               MOVE WS-TIRFS TO TID-UTAN-SEK                              
219101             ELSE                                                         
219201               MOVE WS-RFS      TO WS-TIRFS                               
219301               MOVE WS-TIRFS    TO TID-UTAN-SEK                           
219401             END-IF                                                       
219402           ELSE                                                           
219403               MOVE TID-UTAN-SEK TO WS-TIRFS                              
219501           END-IF                                                         
219502         END-IF                                                           
219601       END-IF                                                             
219701     END-IF                                                               
219801     MOVE WS-TIRFS          TO WS-DARFS                                   
219901     IF WS-TIRFS NOT = ZERO                                               
220001       IF WS-TIRFS < 5000000000                                           
220101         MOVE 20            TO WS-DARFS (1:2)                             
220201       ELSE                                                               
220301         IF WS-TIRFS < 9999999999                                         
220401           MOVE 19          TO WS-DARFS (1:2)                             
220501         ELSE                                                             
220601           MOVE 999999999999 TO WS-DARFS                                  
220701         END-IF                                                           
220801       END-IF                                                             
220901     END-IF                                                               
221001     .                                                                    
221101     EJECT                                                                
221201                                                                          
221301 S04A-BERAEKNA-RFS             SECTION.                                   
221401                                                                          
221501*                                                                         
221601     IF GOOD-DDC                                                          
221701       MOVE WC-CDC-SE           TO W-4431-IDDC                            
221801     ELSE                                                                 
221901       MOVE WS-IDDC             TO W-4431-IDDC                            
222001     END-IF                                                               
222101     MOVE WS-IDTRPLOS           TO W-4432-IDTRPLOS                        
222201     MOVE WS-IDTRPVAR           TO W-4432-IDTRPVAR                        
222301     PERFORM IMS-GU-XXKA11                                                
222401     IF SEGMENT-FINNS                                                     
222501        MOVE 4432-KVLASTTI         TO WS-KVLASTTI                         
222601        MOVE 4432-KVADMFL          TO WS-KVADMFL                          
222701        MOVE 4432-KVADMEL          TO WS-KVADMEL                          
222801     ELSE                                                                 
222901        MOVE +0                    TO WS-KVLASTTI                         
223001                                      WS-KVADMFL                          
223101                                      WS-KVADMEL                          
223201     END-IF                                                               
223301*                                                                         
223401     MOVE +012                  TO TIME-KDCALL                            
223501     MOVE WS-IDPRC              TO TIME-IDPRC                             
223601     IF GOOD-DDC                                                          
223701       MOVE WC-CDC-SE           TO TIME-IDDC                              
223801     ELSE                                                                 
223901       MOVE WS-IDDC             TO TIME-IDDC                              
224001     END-IF                                                               
224101     MOVE ZERO                  TO TIME-STARTDAT                          
224201     MOVE WS-TIAAMMDD           TO TIME-STOP-TIAAMMDD                     
224301     MOVE WS-TIHHMMSS           TO TIME-STOP-TIHHMMSS                     
224401     MOVE WS-KVADMEL            TO TIME-TIARB                             
224501*                                                                         
224601     IF TIME-TIARB              =  ZERO                                   
224701         MOVE TIME-STOP-TIAAMMDD TO TIME-START-TIAAMMDD                   
224801         MOVE TIME-STOP-TIHHMMSS TO TIME-START-TIHHMMSS                   
224901      ELSE                                                                
225001         CALL W411TIME USING TIME-W411TIME 4437-PCB                       
225101     END-IF                                                               
225201*                                                                         
225301     MOVE TIME-START-TIAAMMDD   TO WS-TIAAMMDD                            
225401     MOVE TIME-START-TIHHMMSS   TO WS-TIHHMMSS                            
225501*                                                                         
225601     MOVE +013                  TO TIME-KDCALL                            
225701     MOVE ZERO                  TO TIME-STARTDAT                          
225801     MOVE WS-TIAAMMDD           TO TIME-STOP-TIAAMMDD                     
225901     MOVE WS-TIHHMMSS           TO TIME-STOP-TIHHMMSS                     
226001     MOVE WS-KVLASTTI           TO TIME-TIARB                             
226101*                                                                         
226201     IF TIME-TIARB              =  ZERO                                   
226301         MOVE TIME-STOP-TIAAMMDD TO TIME-START-TIAAMMDD                   
226401         MOVE TIME-STOP-TIHHMMSS TO TIME-START-TIHHMMSS                   
226501      ELSE                                                                
226601         CALL W411TIME USING TIME-W411TIME 4437-PCB                       
226701     END-IF                                                               
226801*                                                                         
226901     MOVE TIME-START-TIAAMMDD   TO WS-TIAAMMDD                            
227001     MOVE TIME-START-TIHHMMSS   TO WS-TIHHMMSS                            
227101*                                                                         
227201     MOVE +012                  TO TIME-KDCALL                            
227301     MOVE ZERO                  TO TIME-STARTDAT                          
227401     MOVE WS-TIAAMMDD           TO TIME-STOP-TIAAMMDD                     
227501     MOVE WS-TIHHMMSS           TO TIME-STOP-TIHHMMSS                     
227601     MOVE WS-KVADMFL            TO TIME-TIARB                             
227701*                                                                         
227801     IF TIME-TIARB              =  ZERO                                   
227901         MOVE TIME-STOP-TIAAMMDD TO TIME-START-TIAAMMDD                   
228001         MOVE TIME-STOP-TIHHMMSS TO TIME-START-TIHHMMSS                   
228101      ELSE                                                                
228201         CALL W411TIME USING TIME-W411TIME 4437-PCB                       
228301     END-IF                                                               
228401*                                                                         
228501     MOVE TIME-START-TIAAMMDD   TO WS-TIAAMMDD                            
228601     MOVE TIME-START-TIHHMMSS   TO WS-TIHHMMSS                            
228701     .                                                                    
228801     EJECT                                                                
228901                                                                          
229001 S04B-RAEKNA-NY-TRP-AVG-TID  SECTION.                                     
229101                                                                          
229201     MOVE ALL '+'                 TO MSGI-WMSGINIT                        
229301     MOVE '013'                   TO MSGI-KDCALL                          
229401     MOVE 'WIDDC   '              TO MSGI-IDUSER                          
229501     MOVE WS-ARB-IDDC             TO MSGI-IDUSER(6:2)                     
229601     MOVE AVSO-IDTRANS            TO MSGI-IDTRANS                         
229701                                                                          
229801     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
229901                                                                          
230001     MOVE   MSGI-TILOKDAT         TO   WS-TILOKDAT                        
230101     MOVE   WS-TILOKDAT           TO   TRAN-TIREGDAT                      
230201     MOVE   MSGI-TILOKTID         TO   WS-TILOKTID                        
230301     MOVE   WS-TILOKTID           TO   TRAN-TIHHMM-REG                    
230401     MOVE   'IMS'                 TO   TRAN-IDSYSTEM                      
230501     MOVE   WS-IDTRP              TO   TRAN-IDTRP                         
230601     MOVE   WS-ARB-IDDC           TO   TRAN-IDDC                          
230701     MOVE   WS-KDORDKL            TO   TRAN-KDORDKL                       
230801     MOVE   WS-KDTRPKAT           TO   TRAN-KDTRPKAT                      
230901     MOVE   DC-KVLEDTIM-0         TO   TRAN-KVLEDTIM-0                    
231001     MOVE   DC-KVLEDTIM-1         TO   TRAN-KVLEDTIM-1                    
231101     MOVE   DC-KVLEDTIM-2         TO   TRAN-KVLEDTIM-2                    
231201     MOVE   DC-KVLEDTIM-3         TO   TRAN-KVLEDTIM-3                    
231301     MOVE   DC-KVLEDTIM-4         TO   TRAN-KVLEDTIM-4                    
231401     MOVE   WS-ARB-TIRFS          TO   TRAN-TIRFS                         
231501     MOVE   ZERO                  TO   TRAN-KDTPOTYP                      
231601                                                                          
231701     CALL   W411TRAN USING TRAN-W411TRAN                                  
231801                           TRAN-XXKB-PCB                                  
231901     .                                                                    
232001     EJECT                                                                
232101 S05-SOEK-IDDC-TAB          SECTION.                                      
232201                                                                          
232301     SET DC-IX                 TO 1                                       
232401     MOVE JA                   TO SW-IDDC-TRAEFF                          
232501     SEARCH IDDC-TAB AT END MOVE NEJ TO SW-IDDC-TRAEFF                    
232601       WHEN IDDC-TAB(DC-IX) = WS-IDDC CONTINUE                            
232701     END-SEARCH                                                           
232801     .                                                                    
232901     SKIP2                                                                
233001 IMS-GU-GMTB01-WDB301      SECTION.                                       
233002     MOVE 'IMS-GU-GMTB01-WDB301    ' TO CURR-IMS-SECTION                  
233003                                                                          
233101     STRING 'WDB301  (WDB301KY =' W-WDB301KY-X                            
233201                    '!WDB301KY =' W-WDB301KY-DEFAULT-X ')'                
233301            DELIMITED BY SIZE INTO SSA1                                   
233401     MOVE '    ' TO GODK-STATUSKODER                                      
233501     CALL CBLTDLI USING GU GMTB-PCB DC-WDB301 SSA1                        
233601     MOVE GMTB-STATUS-CODE TO STATUS-WS                                   
233701     PERFORM IMS-STATUSKONTROLL                                           
233801     SKIP3                                                                
233901     .                                                                    
234001 IMS-GHU-WDE601                SECTION.                                   
234101                                                                          
234102     MOVE 'IMS-GHU-WDE601          ' TO CURR-IMS-SECTION                  
234103                                                                          
234201     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
234301            DELIMITED BY SIZE INTO SSA1                                   
234401     MOVE 'GE  '              TO GODK-STATUSKODER                         
234501     CALL CBLTDLI USING GHU WDE6-PCB DLI-IO-AREA SSA1                     
234601     MOVE WDE6-STATUS-CODE      TO STATUS-WS                              
234701     PERFORM IMS-STATUSKONTROLL                                           
234801     .                                                                    
234901     EJECT                                                                
235001                                                                          
235101 IMS-REPL-WDE601                SECTION.                                  
235201                                                                          
235202     MOVE 'IMS-REPL-WDE601         ' TO CURR-IMS-SECTION                  
235203                                                                          
235301     MOVE '    '              TO GODK-STATUSKODER                         
235401     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-AREA                         
235501     MOVE WDE6-STATUS-CODE      TO STATUS-WS                              
235601     PERFORM IMS-STATUSKONTROLL                                           
235701     .                                                                    
235801     EJECT                                                                
235901                                                                          
236001 IMS-GHN-ORQA                  SECTION.                                   
236101                                                                          
236102     MOVE 'IMS-GHN-ORQA            ' TO CURR-IMS-SECTION                  
236103                                                                          
236201     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-FOM                          
236301                    '&WDQ301KY<=' W-WDQ301KY-TOM                          
236401                    '&KDODELST =' W-KDODELST-X ')'                        
236501            DELIMITED BY SIZE INTO SSA1                                   
236601     MOVE '  GE'              TO GODK-STATUSKODER                         
236701     CALL CBLTDLI USING GHN ORQA-PCB DLI-IO-AREA SSA1                     
236801     MOVE ORQA-STATUS-CODE      TO STATUS-WS                              
236901     PERFORM IMS-STATUSKONTROLL                                           
237001     .                                                                    
237101     SKIP2                                                                
237201                                                                          
237301 IMS-GHU-ORQA                  SECTION.                                   
237401                                                                          
237402     MOVE 'IMS-GHU-ORQA            ' TO CURR-IMS-SECTION                  
237403                                                                          
237501     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-FOM                          
237601                    '&WDQ301KY<=' W-WDQ301KY-TOM                          
237701                    '&KDODELST =' W-KDODELST-X ')'                        
237801            DELIMITED BY SIZE INTO SSA1                                   
237901     MOVE '  GE'              TO GODK-STATUSKODER                         
238001     CALL CBLTDLI USING GHU ORQA-PCB DLI-IO-AREA SSA1                     
238101     MOVE ORQA-STATUS-CODE      TO STATUS-WS                              
238201     PERFORM IMS-STATUSKONTROLL                                           
238301     .                                                                    
238401     SKIP2                                                                
238501                                                                          
238601 IMS-GHN-ORQA-MIN-MAX          SECTION.                                   
238701                                                                          
238702     MOVE 'IMS-GHN-ORQA-MIN-MAX    ' TO CURR-IMS-SECTION                  
238703                                                                          
238801     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN                          
238901                    '&WDQ301KY<=' W-WDQ301KY-MAX                          
239001                    '&KDODELST =' W-KDODELST-X                            
239101                    '&IDLEVNR  =' W-IDLEVNR-X ')'                         
239201            DELIMITED BY SIZE INTO SSA1                                   
239301     MOVE '  GE'              TO GODK-STATUSKODER                         
239401     CALL CBLTDLI USING GHN ORQA-PCB DLI-IO-AREA SSA1                     
239501     MOVE ORQA-STATUS-CODE      TO STATUS-WS                              
239601     PERFORM IMS-STATUSKONTROLL                                           
239701     .                                                                    
239801     SKIP2                                                                
239901                                                                          
240001 IMS-GHU-ORQA-MIN-MAX          SECTION.                                   
240101                                                                          
240102     MOVE 'IMS-GHU-ORQA-MIN-MAX    ' TO CURR-IMS-SECTION                  
240103                                                                          
240201     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN                          
240301                    '&WDQ301KY<=' W-WDQ301KY-MAX                          
240401                    '&KDODELST =' W-KDODELST-X                            
240501                    '&IDLEVNR  =' W-IDLEVNR-X ')'                         
240601            DELIMITED BY SIZE INTO SSA1                                   
240701     MOVE '  GE'              TO GODK-STATUSKODER                         
240801     CALL CBLTDLI USING GHU ORQA-PCB DLI-IO-AREA SSA1                     
240901     MOVE ORQA-STATUS-CODE      TO STATUS-WS                              
241001     PERFORM IMS-STATUSKONTROLL                                           
241101     .                                                                    
241201     SKIP2                                                                
241301                                                                          
241401 IMS-GU-ORQA-FOM-TOM          SECTION.                                    
241501                                                                          
241502     MOVE 'IMS-GU-ORQA-FOM-TOM     ' TO CURR-IMS-SECTION                  
241503                                                                          
241601     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-FOM                          
241701                    '&WDQ301KY<=' W-WDQ301KY-TOM                          
241801                    '&IDLEVNR  =' W-IDLEVNR-X ')'                         
241901            DELIMITED BY SIZE INTO SSA1                                   
242001     MOVE '  GE'              TO GODK-STATUSKODER                         
242101     CALL CBLTDLI USING GU ORQA-PCB DLI-IO-AREA SSA1                      
242201     MOVE ORQA-STATUS-CODE      TO STATUS-WS                              
242301     PERFORM IMS-STATUSKONTROLL                                           
242401     .                                                                    
242501     SKIP2                                                                
242601                                                                          
242701 IMS-GN-ORQA-FOM-TOM          SECTION.                                    
242801                                                                          
242802     MOVE 'IMS-GN-ORQA-FOM-TOM     ' TO CURR-IMS-SECTION                  
242803                                                                          
242901     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-FOM                          
243001                    '&WDQ301KY<=' W-WDQ301KY-TOM                          
243101                    '&IDLEVNR  =' W-IDLEVNR-X ')'                         
243201            DELIMITED BY SIZE INTO SSA1                                   
243301     MOVE '  GEGB'            TO GODK-STATUSKODER                         
243401     CALL CBLTDLI USING GN ORQA-PCB DLI-IO-AREA SSA1                      
243501     MOVE ORQA-STATUS-CODE      TO STATUS-WS                              
243601     PERFORM IMS-STATUSKONTROLL                                           
243701     .                                                                    
243801     SKIP2                                                                
243901                                                                          
244001 IMS-GHU-ORQA-ANN              SECTION.                                   
244101                                                                          
244102     MOVE 'IMS-GHU-ORQA-ANN        ' TO CURR-IMS-SECTION                  
244103                                                                          
244201     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-FOM                          
244301                    '&WDQ301KY<=' W-WDQ301KY-TOM                          
244401                    '&KDODELST =' W-KDODELST-X                            
244501                    '&IDLEVNR  =' W-IDLEVNR-X ')'                         
244601            DELIMITED BY SIZE INTO SSA1                                   
244701     MOVE '  GE'              TO GODK-STATUSKODER                         
244801     CALL CBLTDLI USING GHU ORQA-PCB DLI-IO-AREA SSA1                     
244901     MOVE ORQA-STATUS-CODE      TO STATUS-WS                              
245001     PERFORM IMS-STATUSKONTROLL                                           
245101     .                                                                    
245201     SKIP2                                                                
245301                                                                          
245401 IMS-GHU-ORQA-LEVNR            SECTION.                                   
245501                                                                          
245502     MOVE 'IMS-GHU-ORQA-LEVNR      ' TO CURR-IMS-SECTION                  
245503                                                                          
245601     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN                          
245701                    '&WDQ301KY<=' W-WDQ301KY-MAX                          
245801                    '&IDLEVNR  =' W-IDLEVNR-X ')'                         
245901            DELIMITED BY SIZE INTO SSA1                                   
246001     MOVE '  GE'              TO GODK-STATUSKODER                         
246101     CALL CBLTDLI USING GHU ORQA-PCB DLI-IO-AREA SSA1                     
246201     MOVE ORQA-STATUS-CODE      TO STATUS-WS                              
246301     PERFORM IMS-STATUSKONTROLL                                           
246401     .                                                                    
246501     SKIP2                                                                
246601                                                                          
246701 IMS-GHU-ORQA-PRC              SECTION.                                   
246801                                                                          
246802     MOVE 'IMS-GHU-ORQA-PRC        ' TO CURR-IMS-SECTION                  
246803                                                                          
246901     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN                          
247001                    '&WDQ301KY<=' W-WDQ301KY-MAX                          
247101                    '&IDPRC    =' W-IDPRC-X                               
247201                    '&KDODELST =' W-KDODELST-X ')'                        
247301            DELIMITED BY SIZE INTO SSA1                                   
247401     MOVE '  GE'              TO GODK-STATUSKODER                         
247501     CALL CBLTDLI USING GHU ORQA-PCB DLI-IO-AREA SSA1                     
247601     MOVE ORQA-STATUS-CODE      TO STATUS-WS                              
247701     PERFORM IMS-STATUSKONTROLL                                           
247801     .                                                                    
247901     EJECT                                                                
248001                                                                          
248101 IMS-REPL-ORQA                 SECTION.                                   
248202     MOVE 'IMS-REPL-ORQA           ' TO CURR-IMS-SECTION                  
248203                                                                          
248301     MOVE   '    '            TO GODK-STATUSKODER                         
248401     CALL CBLTDLI USING REPL ORQA-PCB DLI-IO-AREA                         
248501     MOVE ORQA-STATUS-CODE    TO STATUS-WS                                
248601     PERFORM IMS-STATUSKONTROLL                                           
248701     .                                                                    
248801     SKIP2                                                                
248901                                                                          
249001 IMS-ISRT-ORQA                 SECTION.                                   
249102     MOVE 'IMS-ISRT-ORQA           ' TO CURR-IMS-SECTION                  
249103                                                                          
249201     MOVE   'WLORQA01 '       TO SSA1                                     
249301     MOVE   '    '            TO GODK-STATUSKODER                         
249401     CALL CBLTDLI USING ISRT ORQA-PCB DLI-IO-AREA SSA1                    
249501     MOVE ORQA-STATUS-CODE    TO STATUS-WS                                
249601     PERFORM IMS-STATUSKONTROLL                                           
249701     .                                                                    
249801     SKIP2                                                                
249901                                                                          
250001 IMS-DLET-ORQA                 SECTION.                                   
250103     MOVE 'IMS-DLET-ORQA           ' TO CURR-IMS-SECTION                  
250104                                                                          
250201     MOVE   '    '            TO GODK-STATUSKODER                         
250301     CALL CBLTDLI USING DLET ORQA-PCB DLI-IO-AREA                         
250401     MOVE ORQA-STATUS-CODE    TO STATUS-WS                                
250501     PERFORM IMS-STATUSKONTROLL                                           
250601     .                                                                    
250701     EJECT                                                                
250801                                                                          
250901 IMS-GU-WDQ201                 SECTION.                                   
251001                                                                          
251002     MOVE 'IMS-GU-WDQ201           ' TO CURR-IMS-SECTION                  
251003                                                                          
251101     STRING 'WDQ201  (IDORDER  =' W-IDORDER-X ')'                         
251201            DELIMITED BY SIZE INTO SSA1                                   
251301     MOVE '    '              TO GODK-STATUSKODER                         
251407     CALL CBLTDLI USING GU  WDQ2-PCB DLI-IO-AREA-WDQ201 SSA1              
251501     MOVE WDQ2-STATUS-CODE      TO STATUS-WS                              
251601     PERFORM IMS-STATUSKONTROLL                                           
251701     .                                                                    
251801     SKIP2                                                                
251901                                                                          
252001 IMS-GNP-WDQ211-FIRST          SECTION.                                   
252101                                                                          
252102     MOVE 'IMS-GNP-WDQ211-FIRST    ' TO CURR-IMS-SECTION                  
252103                                                                          
252201     STRING 'WDQ211  *F(WDQ211KY >' W-WDQ211KY-X ')'                      
252301            DELIMITED BY SIZE INTO SSA1                                   
252401     MOVE '  GE'               TO GODK-STATUSKODER                        
252507     CALL CBLTDLI USING GNP WDQ2-PCB DLI-IO-AREA-WDQ211 SSA1              
252601     MOVE WDQ2-STATUS-CODE     TO STATUS-WS                               
252701     PERFORM IMS-STATUSKONTROLL                                           
252801     .                                                                    
252901     SKIP2                                                                
253001                                                                          
253101 IMS-GNP-WDQ211-DC-FIRST       SECTION.                                   
253201                                                                          
253202     MOVE 'IMS-GNP-WDQ211-DC-FIRST ' TO CURR-IMS-SECTION                  
253203                                                                          
253301     STRING 'WDQ211  *F(WDQ211KY >' W-WDQ211KY-MIN-X                      
253401                      '&WDQ211KY<=' W-WDQ211KY-MAX-X ')'                  
253501            DELIMITED BY SIZE INTO SSA1                                   
253601     MOVE '  GE'               TO GODK-STATUSKODER                        
253707     CALL CBLTDLI USING GNP WDQ2-PCB DLI-IO-AREA-WDQ211 SSA1              
253801     MOVE WDQ2-STATUS-CODE     TO STATUS-WS                               
253901     PERFORM IMS-STATUSKONTROLL                                           
254001     .                                                                    
254101     SKIP2                                                                
254201                                                                          
254301 IMS-GNP-WDQ212                SECTION.                                   
254401                                                                          
254402     MOVE 'IMS-GNP-WDQ212          ' TO CURR-IMS-SECTION                  
254403                                                                          
254501     MOVE 'WDQ212   ' TO SSA1                                             
254601     MOVE '  GE'               TO GODK-STATUSKODER                        
254707     CALL CBLTDLI USING GNP WDQ2-PCB DLI-IO-AREA-WDQ212 SSA1              
254801     MOVE WDQ2-STATUS-CODE     TO STATUS-WS                               
254901     PERFORM IMS-STATUSKONTROLL                                           
255001     .                                                                    
255101     EJECT                                                                
255201                                                                          
255301 IMS-GU-WDQ212               SECTION.                                     
255401                                                                          
255402     MOVE 'IMS-GU-WDQ212           ' TO CURR-IMS-SECTION                  
255403                                                                          
255501     STRING 'WDQ212  (IDDC     =' W-IDDC-X ')'                            
255601            DELIMITED BY SIZE INTO SSA1                                   
255701     MOVE '    '              TO GODK-STATUSKODER                         
255807     CALL CBLTDLI USING GU   WDQ2-PCB DLI-IO-AREA-WDQ212 SSA1             
255901     MOVE WDQ2-STATUS-CODE      TO STATUS-WS                              
256001     PERFORM IMS-STATUSKONTROLL                                           
256101     .                                                                    
256201     SKIP2                                                                
256301                                                                          
256401 IMS-GHNP-WDQ212               SECTION.                                   
256501                                                                          
256502     MOVE 'IMS-GHNP-WDQ212         ' TO CURR-IMS-SECTION                  
256503                                                                          
256601     STRING 'WDQ212  *F(IDDC     =' W-IDDC-X ')'                          
256701            DELIMITED BY SIZE INTO SSA1                                   
256801     MOVE '    '              TO GODK-STATUSKODER                         
256907     CALL CBLTDLI USING GHNP WDQ2-PCB DLI-IO-AREA-WDQ212 SSA1             
257001     MOVE WDQ2-STATUS-CODE      TO STATUS-WS                              
257101     PERFORM IMS-STATUSKONTROLL                                           
257201     .                                                                    
257301     SKIP2                                                                
257401                                                                          
257505 IMS-REPL-WDQ212               SECTION.                                   
257601                                                                          
257602     MOVE 'IMS-REPL-WDQ212         ' TO CURR-IMS-SECTION                  
257603                                                                          
257701     MOVE   '    '            TO GODK-STATUSKODER                         
257807     CALL CBLTDLI USING REPL WDQ2-PCB DLI-IO-AREA-WDQ212                  
257901     MOVE WDQ2-STATUS-CODE    TO STATUS-WS                                
258001     PERFORM IMS-STATUSKONTROLL                                           
258101     .                                                                    
258201     EJECT                                                                
258301                                                                          
260709 IMS-GNP-WDQ221                SECTION.                                   
260710                                                                          
260720     MOVE 'IMS-GNP-WDQ221          ' TO CURR-IMS-SECTION                  
260721                                                                          
260722     STRING 'WDQ212  (IDDC     =' W-IDDC-X ')'                            
260730            DELIMITED BY SIZE INTO SSA1                                   
260740     MOVE   'WDQ221'            TO SSA2                                   
260750     MOVE '  GE'                TO GODK-STATUSKODER                       
260760     CALL CBLTDLI USING GNP  WDQ2-PCB DLI-IO-AREA-WDQ221 SSA1 SSA2        
260770     MOVE WDQ2-STATUS-CODE      TO STATUS-WS                              
260780     PERFORM IMS-STATUSKONTROLL                                           
260802     .                                                                    
260803     SKIP2                                                                
260804                                                                          
260810 IMS-GHNP-WDQ221               SECTION.                                   
260910                                                                          
260920     MOVE 'IMS-GHNP-WDQ221         ' TO CURR-IMS-SECTION                  
260930                                                                          
261010     STRING 'WDQ212  (IDDC     =' W-IDDC-X ')'                            
261110            DELIMITED BY SIZE INTO SSA1                                   
261210     MOVE   'WDQ221'            TO SSA2                                   
261310     MOVE '  GE'                TO GODK-STATUSKODER                       
261410     CALL CBLTDLI USING GHNP WDQ2-PCB DLI-IO-AREA-WDQ221 SSA1 SSA2        
261510     MOVE WDQ2-STATUS-CODE      TO STATUS-WS                              
261610     PERFORM IMS-STATUSKONTROLL                                           
261710     .                                                                    
261810     SKIP2                                                                
261910                                                                          
263320 IMS-REPL-WDQ221               SECTION.                                   
263330                                                                          
263340     MOVE 'IMS-REPL-WDQ221         ' TO CURR-IMS-SECTION                  
263341                                                                          
263342     MOVE   '    '            TO GODK-STATUSKODER                         
263350     CALL CBLTDLI USING REPL WDQ2-PCB DLI-IO-AREA-WDQ221                  
263360     MOVE WDQ2-STATUS-CODE    TO STATUS-WS                                
263370     PERFORM IMS-STATUSKONTROLL                                           
263380     .                                                                    
263390     EJECT                                                                
263400                                                                          
263414 IMS-DLET-WDQ221               SECTION.                                   
263512                                                                          
263513     MOVE 'IMS-DLET-WDQ221         ' TO CURR-IMS-SECTION                  
263514                                                                          
263612     MOVE   '    '            TO GODK-STATUSKODER                         
263712     CALL CBLTDLI USING DLET WDQ2-PCB DLI-IO-AREA-WDQ221                  
263812     MOVE ORQA-STATUS-CODE    TO STATUS-WS                                
263912     PERFORM IMS-STATUSKONTROLL                                           
264012     .                                                                    
264112     EJECT                                                                
264212                                                                          
264310 IMS-GU-XXKA11                 SECTION.                                   
264410                                                                          
264420     MOVE 'IMS-GU-XXKA11           ' TO CURR-IMS-SECTION                  
264430                                                                          
264510     STRING 'WLXXKA01(WDGXKEY  =' W-WDGXKEY-4431-X ')'                    
264610            DELIMITED BY SIZE INTO SSA1                                   
264710     STRING 'WLXXKA11(WDGXKEY  =' W-WDGXKEY-4432-X ')'                    
264810            DELIMITED BY SIZE INTO SSA2                                   
264910     MOVE '  GE'              TO GODK-STATUSKODER                         
265010     CALL CBLTDLI USING GU XXKA-PCB DLI-IO-AREA SSA1 SSA2                 
265110     MOVE XXKA-STATUS-CODE      TO STATUS-WS                              
265210     PERFORM IMS-STATUSKONTROLL                                           
265310     .                                                                    
265410     EJECT                                                                
265510                                                                          
265610 IMS-GU-XXKE11                 SECTION.                                   
265710                                                                          
265720     MOVE 'IMS-GU-XXKE11           ' TO CURR-IMS-SECTION                  
265730                                                                          
265810     STRING 'WLXXKE01(WDGXKEY  =' W-WDGXKEY-4441-X ')'                    
265910            DELIMITED BY SIZE INTO SSA1                                   
266010     STRING 'WLXXKE11(WDGXKEY  =' W-WDGXKEY-4442-X ')'                    
266110            DELIMITED BY SIZE INTO SSA2                                   
266210     MOVE '  GE'              TO GODK-STATUSKODER                         
266310     CALL CBLTDLI USING GU XXKE-PCB DLI-IO-AREA SSA1 SSA2                 
266410     MOVE XXKE-STATUS-CODE      TO STATUS-WS                              
266510     PERFORM IMS-STATUSKONTROLL                                           
266610     .                                                                    
266710     EJECT                                                                
266810                                                                          
266910 IMS-GU-XXKF11                 SECTION.                                   
267010                                                                          
267020     MOVE 'IMS-GU-XXKF11           ' TO CURR-IMS-SECTION                  
267030                                                                          
267110     STRING 'WLXXKF01(WDGXKEY  =' W-WDGXKEY-4443-X ')'                    
267210            DELIMITED BY SIZE INTO SSA1                                   
267310     STRING 'WLXXKF11(KDORDKL  =' W-KDORDKL-X                             
267410                    '&KVRADER  >' W-KVRADER-X                             
267510                    '&VKORDNTO >' W-VKORDNTO-X                            
267610                    '&VLORDNTO >' W-VLORDNTO-X ')'                        
267710            DELIMITED BY SIZE INTO SSA2                                   
267810     MOVE '  GE'              TO GODK-STATUSKODER                         
267910     CALL CBLTDLI USING GU XXKF-PCB DLI-IO-AREA SSA1 SSA2                 
268010     MOVE XXKF-STATUS-CODE      TO STATUS-WS                              
268110     PERFORM IMS-STATUSKONTROLL                                           
268210     .                                                                    
268310     EJECT                                                                
268410                                                                          
268510 IMS-GU-XXKG01                 SECTION.                                   
268610                                                                          
268620     MOVE 'IMS-GU-XXKG01           ' TO CURR-IMS-SECTION                  
268630                                                                          
268710     STRING 'WLXXKG01(WDGXKEY  =' W-WDGXKEY-4445-X ')'                    
268810            DELIMITED BY SIZE INTO SSA1                                   
268910     MOVE '  GE'              TO GODK-STATUSKODER                         
269010     CALL CBLTDLI USING GU  XXKG-PCB DLI-IO-AREA SSA1                     
269110     MOVE XXKG-STATUS-CODE      TO STATUS-WS                              
269210     PERFORM IMS-STATUSKONTROLL                                           
269310     .                                                                    
269410     SKIP2                                                                
269510                                                                          
269610 IMS-GNP-XXKG11                SECTION.                                   
269710                                                                          
269720     MOVE 'IMS-GNP-XXKG11          ' TO CURR-IMS-SECTION                  
269730                                                                          
269810     STRING 'WLXXKG11*F(IDGMTOMF<=' W-IDGMTOMR-X                          
269910                      '&IDGMTOMT>=' W-IDGMTOMR-X                          
270010                      '&IDHLOFOM<=' W-IDHLO-X                             
270110                      '&IDHLOTOM>=' W-IDHLO-X                             
270210                      '&IDTRP    =' W-IDTRP-X                             
270310                      '&KDFRAKTF<=' W-KDFRAKT-X                           
270410                      '&KDFRAKTT>=' W-KDFRAKT-X                           
270510                      '&KDPRODKF<=' W-KDPRODKL-X                          
270610                      '&KDPRODKT>=' W-KDPRODKL-X                          
270710                      '!IDGMTOMF<=' W-IDGMTOMR-X                          
270810                      '&IDGMTOMT>=' W-IDGMTOMR-X                          
270910                      '&IDHLOFOM<=' W-IDHLO-X                             
271010                      '&IDHLOTOM>=' W-IDHLO-X                             
271110                      '&IDTRP    =' W-IDTRP-BLANK-X                       
271210                      '&KDFRAKTF<=' W-KDFRAKT-X                           
271310                      '&KDFRAKTT>=' W-KDFRAKT-X                           
271410                      '&KDPRODKF<=' W-KDPRODKL-X                          
271510                      '&KDPRODKT>=' W-KDPRODKL-X ')'                      
271610            DELIMITED BY SIZE INTO SSA1                                   
271710     MOVE '    '              TO GODK-STATUSKODER                         
271810     CALL CBLTDLI USING GNP XXKG-PCB DLI-IO-AREA SSA1                     
271910     MOVE XXKG-STATUS-CODE      TO STATUS-WS                              
272010     PERFORM IMS-STATUSKONTROLL                                           
272110     .                                                                    
272210     EJECT                                                                
272310                                                                          
272410 IMS-GU-XXKH01                 SECTION.                                   
272510                                                                          
272520     MOVE 'IMS-GU-XXKH01           ' TO CURR-IMS-SECTION                  
272530                                                                          
272610     STRING 'WLXXKH01(WDGXKEY  =' W-WDGXKEY-4447-X ')'                    
272710            DELIMITED BY SIZE INTO SSA1                                   
272810     MOVE '    '              TO GODK-STATUSKODER                         
272910     CALL CBLTDLI USING GU XXKH-PCB DLI-IO-AREA SSA1                      
273010     MOVE XXKH-STATUS-CODE      TO STATUS-WS                              
273110     PERFORM IMS-STATUSKONTROLL                                           
273210     .                                                                    
273310                                                                          
273410 IMS-GNP-XXKH11-F               SECTION.                                  
273510                                                                          
273520     MOVE 'IMS-GNP-XXKH11-F        ' TO CURR-IMS-SECTION                  
273530                                                                          
273610     STRING 'WLXXKH11*F(WDGXKEY  =' W-WDGXKEY-4448-X ')'                  
273710            DELIMITED BY SIZE INTO SSA1                                   
273810     MOVE '    '              TO GODK-STATUSKODER                         
273910     CALL CBLTDLI USING GNP XXKH-PCB DLI-IO-AREA SSA1                     
274010     MOVE XXKH-STATUS-CODE      TO STATUS-WS                              
274110     PERFORM IMS-STATUSKONTROLL                                           
274210     .                                                                    
274310     EJECT                                                                
274410                                                                          
274510 IMS-GU-XXKI11                 SECTION.                                   
274610                                                                          
274620     MOVE 'IMS-GU-XXKI11           ' TO CURR-IMS-SECTION                  
274630                                                                          
274710     STRING 'WLXXKI01(WDGXKEY  =' W-WDGXKEY-4451-X ')'                    
274810            DELIMITED BY SIZE INTO SSA1                                   
274910     STRING 'WLXXKI11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
275010            DELIMITED BY SIZE INTO SSA2                                   
275110     MOVE '  GE'              TO GODK-STATUSKODER                         
275210     CALL CBLTDLI USING GU XXKI-PCB DLI-IO-AREA SSA1 SSA2                 
275310     MOVE XXKI-STATUS-CODE      TO STATUS-WS                              
275410     PERFORM IMS-STATUSKONTROLL                                           
275510     .                                                                    
275610     EJECT                                                                
275710                                                                          
275810 IMS-GU-WDB201 SECTION.                                                   
275910                                                                          
275920     MOVE 'IMS-GU-WDB201           ' TO CURR-IMS-SECTION                  
275930                                                                          
276010     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
276110            DELIMITED BY SIZE INTO SSA1                                   
276210     MOVE '    '               TO GODK-STATUSKODER                        
276310     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-AREA-WDB201 SSA1               
276410     MOVE WDB2-STATUS-CODE     TO STATUS-WS                               
276510     PERFORM IMS-STATUSKONTROLL                                           
276610     .                                                                    
276710     EJECT                                                                
276810                                                                          
276910 IMS-GU-WDB601    SECTION.                                                
276920                                                                          
276930     MOVE 'IMS-GU-WDB601           ' TO CURR-IMS-SECTION                  
277010                                                                          
277110     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
277210          DELIMITED BY SIZE INTO SSA1                                     
277310     MOVE '    ' TO GODK-STATUSKODER                                      
277410     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-WDB601 SSA1               
277510     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
277610     PERFORM IMS-STATUSKONTROLL                                           
277710     .                                                                    
277810     EJECT                                                                
277910                                                                          
278010 IMS-GU-WDB601-EXP SECTION.                                               
278110                                                                          
278120     MOVE 'IMS-GU-WDB601-EXP       ' TO CURR-IMS-SECTION                  
278130                                                                          
278210     STRING 'WDB601  (IDFTG    =' W-IDFTG-X                               
278310                    '&FLMAINDC =' W-FLMAINDC ')'                          
278410          DELIMITED BY SIZE INTO SSA1                                     
278510     MOVE '    ' TO GODK-STATUSKODER                                      
278610     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-WDB601 SSA1               
278710     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
278810     PERFORM IMS-STATUSKONTROLL                                           
278910     .                                                                    
279010     EJECT                                                                
279110                                                                          
279210 IMS-STATUSKONTROLL            SECTION.                                   
279310                                                                          
279410     SET STATUS-IX             TO 1                                       
279510     SEARCH GODK-STATUS AT END CALL FELLOG                                
279610       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
279710     END-SEARCH                                                           
279810     .                                                                    
279910*    -COPY WY2000PB                                                       
280010                                                                          
290001*    -COPY WY2000PC                                                       
