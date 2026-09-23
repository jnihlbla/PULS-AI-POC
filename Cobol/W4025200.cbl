000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W4025200.                                                
000500 AUTHOR.         KERSTIN JOHANSSON  GUIDE DATAKONSULT AB                  
000600 DATE-WRITTEN.   NOV -90.                                                 
000700                                                                          
000800     REMARKS.                                                             
000900*                                                                         
001000*    FUNKTION.                                                            
001100*        PROGRAMMET ÄR EN BAKGRUNDSTRANS FÖR KONTROLL OCH                 
001200*        UPPDATERING AV ORDERRADER I ORDERKÖN.                            
001300*        INPUTTRANSAKTIONEN ÄR SKAPAD FRÅN EN BATCH SEKVENSFIL            
001400*        OCH UPPDATERAD PÅ KOMMUNIKATIONS DB.EN GENERELL MPP -            
001500*        DISPATCHERN HÄMTAR TRANSAKTIONEN PÅ KOMMUNIKATIONS DB            
001600*        OCH STARTAR DENNA BAKGRUNDSTRANS.                                
001700*                                                                         
001800*        ANGIVNA VÄRDEN KONTROLLERAS OCH EJ ANGIVNA VÄRDEN HÄMTAS         
001900*        FRÅN ARTIKELREGISTRET.                                           
002000*        FÖR BIPACKNING AV TÄCKTA RO/TPO STARTAS W40299                   
002100*        DÅ SISTA ORDERRADEN UPPDATERATS STARTAS ORDERAVSLUT.             
002200*        VCBC-ÖVERFÖRING AV ORDERRADER STARTAS VID ORDERAVSLUT.           
002300*        ETT FEL/KLAR MEDDELANDE SKICKAS TILL DISPATCHERN DÅ              
002400*        TRANSAKTIONEN BEHANDLAD.                                         
002500*                                                                         
002600*              I DE FALL ORAD-ADLAGOMR BLIR = +0                          
002700*              SÄTTER VI ORAD-ADLAGOMR = +1,                              
002800*                                                                         
002900*    LAGEROMRÅDE 0 ÄNDRAS ALLTID TILL 1. DETTA PGA AV ATT                 
003000*    LAGEROMRÅDESTABELLEN I WDQ212 ÄR 1 TILL 99. DET FINNS INTE           
003100*    NÅGOT LAGEROMRÅDE NOLL... MEN EFTERSOM MAN MÅSTE LAGRA DEN           
003200*    DATA SOM HÖR TILL DE ARTIKLAR SOM HAR LAGEROMRÅDE NOLL LÄGGS         
003300*    DETTA I LAGEROMRÅDE 1.                                               
003400*                                                                         
003500*        PROGRAMMET ÄR ETT UPPDATERINGS-MPP                               
003600*        PROGRAMMET LÄSER      WLORQM (WDQ1)  ORDERBEKRÄFTELSER           
003700*        PROGRAMMET UPPDATERAR WLORQM (WDQ1)  ORDERBEKR.BAS               
003800*        PROGRAMMET LÄSER      WLORQI (WDQ2)  ORDERHUVUD                  
003900*        PROGRAMMET LÄSER      WLORQA (WDQ3)  ORDERDELAR                  
004000*        PROGRAMMET UPPDATERAR WLORQF (WDQ4)  ORDERRADER                  
004100*        PROGRAMMET LÄSER              WDK6   ARTIKELREGISTER             
004200*        PROGRAMMET LÄSER      WLARTM (WDK9)  ARTIKELREGISTER             
004300*        PROGRAMMET LÄSER      WLXXKM (WDR1)  RANSFAKTOR.TAB              
004400*        PROGRAMMET LÄSER      WLLEVF (WDF2)  STYRREG DIRLEV              
004500*        PROGRAMMET LÄSER      WLLEVG (WDF2)  STYRREG DIRLEV ASEQ         
004600*        PROGRAMMET LÄSER      WLLEVA (WDF1)  LEVERANTÖRSREG              
004700*        PROGRAMMET LÄSER      WLERSA (WDD7)  ERSÄTTNINGSREG.             
004800*        PROGRAMMET LÄSER              WDM2   KAMPANJREGISTER             
004900*        PROGRAMMET LÄSER      WLZZAC (WDG6)  TRANSAKTIONSBAS             
005000*        PROGRAMMET LÄSER      WLORDP (WDA5)  RESTORDERREG.               
005100*        PROGRAMMET LÄSER      WLXXBU (WDR5)  LARMKÖ                      
005200*        PROGRAMMET LÄSER      WL4437 (WDR1)  ARBETSTIDKALENDER           
005300*        PROGRAMMET LÄSER      WLXXKO (WDR1)  CLEARING ARTIKEL            
005400*        PROGRAMMET LÄSER             (WDB6)  DC REGISTER                 
005500*        PROGRAMMET LÄSER             (WDB2)  KUNDREGISTER                
005600*        PROGRAMMET UPPDATERAR WDA5           RESTORDER                   
005700*        PROGRAMMET UPPDATERAR WDR5           DDGS-RADER(4251/2)          
005800*                                                                         
005900*    INDATA.                                                              
006000*        TRANSAKTION: W4T252X                                             
006100*        MID:         W4I25201                                            
006200*                     WMSGKOM                                             
006300*    UTDATA.                                                              
006400*        MOD:         WMSGMOD    FEL/KLAR MED TILL DISPATCHER             
006500*                     W40291     VCBV ÖVERFÖRING                          
006600*                                                                         
006700* CHANGE LOG:  E'TRACKER                 2006-12-06                       
006800*              E'TRACKER 5444132  DATED  2007-08-27                       
006900*              E'TRACKER 6451537  DATED  2008-03-06 RÄTTA OKS NDC         
007000*              E'TRACKER 2218613  DATED  2008-03-11 KAMPANJORDER          
007100*              E'TRACKER 6828898  DATED  2008-06-15 STOP SEN ORDE         
007200*              E'TRACKER 6921638  DATED  2008-06-15 RFS FEL-LDC           
007300*              E'TRACKER 7194690  DATED  2008-08-15 DDGS-RADER            
007400*              E'TRACKER 7450328  DATED  2008 HÖST  VOHF                  
007500*              E'TRACKER 8081720  DATED  2009-04-09  ORDER STEERIN        
007600*              E'TRACKER 10254592 2015 HÖST  DECOMISSION VOHF             
007700*              E'TRACKER 10228562 CHNG FR WLXXKR/XXKT/XXKS TO WDM2        
007800*              STORY 2375089 / ADD IDSYSTEM VOUI, ECOM                    
007900*              STORY 2373879 / REPLACE DIST35- 88 LEVELS OF IN,KR,        
008000*               AE,CN CDC RETURNS TO DIST35-CDC-RETURNS-NON-VCC           
008100     EJECT                                                                
008200 ENVIRONMENT DIVISION.                                                    
008300                                                                          
008400 DATA DIVISION.                                                           
008500 WORKING-STORAGE SECTION.                                                 
008600*    -- CHECKED BY WY2000                                                 
008700 77  IDPGM                       PIC X(08)   VALUE 'W4025200'.            
008800 77  WS-PGM-POSITION             PIC X(24)   VALUE SPACE.                 
008900 77  WS-IDDC-SEEK                PIC X(2)    VALUE SPACE.                 
009000                                                                          
009100*01  -COPY WWDCKONS                                                       
009200                                                                          
009300*01  -COPY WWPRODSL                                                       
009400                                                                          
009500*    DC FÖR CLEARING AV VERKSTADSORDER                                    
009600*01  -COPY WWDC01                                                         
009700                                                                          
009800 77  FELTEXT                     PIC X(64)   VALUE SPACE.                 
009900 77  KDRC-DISPLAY                PIC Z(5)    VALUE ZERO.                  
010000 77  YES                         PIC X(1)    VALUE 'Y'.                   
010100 77  JA                          PIC X(1)    VALUE 'J'.                   
010200 77  NEJ                         PIC X(1)    VALUE 'N'.                   
010300 77  SPEC-FORBI                  PIC X(1)    VALUE 'S'.                   
010400 77  RKOD-ABEND                  PIC S9(3)   COMP SYNC VALUE +33.         
010500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)  VALUE +1000 COMP SYNC.        
010600 77  IX-DCCLEAR-MAX              PIC S9(3)   COMP SYNC VALUE +99.         
010700 77  WS-INDEX                    PIC S9(9)   COMP SYNC VALUE ZERO.        
010800 77  WS-INDEX-MID                PIC S9(9)   COMP SYNC VALUE ZERO.        
010900 77  WS-INDEX-MID-MAX            PIC S9(9)   COMP SYNC VALUE +5.          
011000 77  WS-INDEX-TILLK              PIC S9(9)   COMP SYNC VALUE ZERO.        
011100 77  WS-INDEX-TILLK-MAX          PIC S9(9)   COMP SYNC VALUE +20.         
011200 77  WS-INDEX-WOPS               PIC S9(9)   COMP SYNC VALUE ZERO.        
011300 77  WS-INDEX-WOPS-MAX           PIC S9(9)   COMP SYNC VALUE +100.        
011400 77  WS-RESLATT                  PIC S9(3)   VALUE +0  COMP-3.            
011500 77  WS-IDPRQUES                 PIC S9(7)   VALUE +0.                    
011600 77  WS-IDANSK                   PIC 9(3)    VALUE  0.                    
011700 77  SPAR-ORAD-KVSLATT           PIC S9(7)   VALUE +0  COMP-3.            
011800 77  W-KDORDBEK                  PIC S9(2)   VALUE +0.                    
011900 77  W-KDTPOTYP                  PIC S9      VALUE +0.                    
012000 77  KDRC-DISP                   PIC 9(4)    VALUE ZERO.                  
012100 77  WS-ADLAGOMR                 PIC S9(3)   VALUE ZERO COMP-3.           
012200 77  WS-ADGANG                   PIC S9(3)   VALUE ZERO COMP-3.           
012300 77  WS-ADPLATS                  PIC S9(5)   VALUE ZERO COMP-3.           
012400 77  WS-CLDC-IX                  PIC S9(5)   VALUE +1   COMP-3.           
012500 77  WS-IDDISTR-NUM4             PIC 9(4).                                
012600 77  WS-IDKUNDNR-NUM6            PIC 9(6).                                
012700 77  WS-KDVALISO-NA              PIC X(3)    VALUE 'N/A'.                 
012800                                                                          
012900 77  TILK-IX                     PIC S9(3)  VALUE ZERO COMP-3.            
013000 77  TILK-IX-MAX                 PIC S9(3)  VALUE +20  COMP-3.            
013100 77  RFS-IX                      PIC S9(4)   COMP SYNC VALUE ZERO.        
013200 77  MAX-RFS-IX                  PIC S9(4)   COMP SYNC VALUE +4.          
013300 77  WS-SAVE-INDEX               PIC S9(9)   COMP SYNC VALUE ZERO.        
013400 77  IDDC-IX                     PIC S9(3)   VALUE ZERO COMP-3.           
013500 77  W-TILLK-DC                  PIC X(2)    VALUE SPACE.                 
013600                                                                          
013700 77  WS-HFAK-REF-X10             PIC X(10)   VALUE SPACE.                 
013800 77  HFAK-TAB-IX                 PIC S9(9)   VALUE +0   COMP SYNC.        
013900 77  BEVARREF-I-HFAK-TAB-SW      PIC X       VALUE 'N'.                   
014000     88  BEVARREF-I-HFAK-TAB                 VALUE 'J'.                   
014100                                                                          
014200 01  WS-TIHHMMSS                 PIC 9(6)   VALUE ZERO.                   
014300 01  FILLER REDEFINES WS-TIHHMMSS.                                        
014400     03 WS-TIHHMM                PIC 9(4).                                
014500     03 FILLER                   PIC 9(2).                                
014600     EJECT                                                                
014700                                                                          
014800 77  ALLT-SW                     PIC X       VALUE 'J'.                   
014900     88  ALLT-OK                             VALUE 'J'.                   
015000                                                                          
015100 77  TILLK-SW                    PIC X       VALUE 'N'.                   
015200     88  TILLKOMMANDE-RAD                    VALUE 'J'.                   
015300     88  EJ-TILLKOMMANDE-RAD                 VALUE 'N'.                   
015400                                                                          
015500 77  KOLLA-ERS-SW                PIC X       VALUE 'N'.                   
015600     88  KOLLA-ERS                           VALUE 'J'.                   
015700                                                                          
015800 77  OBKR-SW                     PIC X       VALUE 'N'.                   
015900     88  SKRIV-OBKR                          VALUE 'J'.                   
016000     88  OBKR-SKRIVEN                        VALUE 'S'.                   
016100                                                                          
016200 77  SW-BYT-ARTIKEL              PIC X       VALUE 'N'.                   
016300     88  BYT-ARTIKEL                         VALUE 'J'.                   
016400     88  BYT-EJ-ARTIKEL                      VALUE 'N'.                   
016500                                                                          
016600 77  SW-DDGS-TPO-OBKR71          PIC X       VALUE 'N'.                   
016700     88  DDGS-TPO-OBKR71                     VALUE 'J'.                   
016800     88  EJ-DDGS-TPO-OBKR71                  VALUE 'N'.                   
016900                                                                          
017000 77  LDC-ARTIKELBYTE-SW          PIC X       VALUE 'N'.                   
017100     88  LDC-INGET-BYTE                      VALUE 'N'.                   
017200     88  LDC-ARTIKELBYTE                     VALUE 'J'.                   
017300     88  LDC-ARTIKEL-BYTT                    VALUE 'B'.                   
017400     88  LDC-ARTIKEL-TILLBAKA                VALUE 'T'.                   
017500                                                                          
017600 77  BAL-DC-FND-SW               PIC X       VALUE 'N'.                   
017700     88  BAL-DC-FND                          VALUE 'J'.                   
017800                                                                          
017900 77  TILLK-BAL-DC-FND-SW         PIC X       VALUE 'N'.                   
018000     88  TILLK-BAL-DC-FND                    VALUE 'J'.                   
018100                                                                          
018200 77  CDC-MOVE-SW                 PIC X       VALUE 'N'.                   
018300     88  CDC-MOVE                            VALUE 'J'.                   
018400                                                                          
018500 77  PREPLANED-SW                PIC X       VALUE 'N'.                   
018600     88  PREPLANED                           VALUE 'J'.                   
018700                                                                          
018800                                                                          
018900 77  KDERS-CHAIN-SW              PIC X       VALUE 'N'.                   
019000     88  KDERS-CHAIN                         VALUE 'J'.                   
019100                                                                          
019200 77  HOLIDAY-SW                PIC X         VALUE 'N'.                   
019300     88  ITS-HOLIDAY                         VALUE 'J'.                   
019400                                                                          
019500*01  -COPY WDQ401   -PRE HELP-                                            
019600*01  -COPY WDQ101   -PRE HELP-                                            
019700*01  -COPY W411CDCA -PRE HELP-                                            
019800*01  -COPY W411DLEV -PRE HELP-                                            
019900*01  -COPY W411KAMP -PRE HELP-                                            
020000*01  -COPY W411KERS -PRE HELP-                                            
020100*01  -COPY W411KVAN -PRE HELP-                                            
020200*01  -COPY W411ORFK -PRE HELP-                                            
020300*01  -COPY W411NDCA -PRE HELP-                                            
020400*01  -COPY W411XDCA -PRE HELP-                                            
020500*01  -COPY W411SDCA -PRE HELP-                                            
020600*01  -COPY W411SPAR -PRE HELP-                                            
020700*01  -COPY W411STOR -PRE HELP-                                            
020800*01  -COPY W411TPO1 -PRE HELP-                                            
020900*01  -COPY W411TPO2 -PRE HELP-                                            
021000*01  -COPY W411RELS -PRE HELP-                                            
021100*01  -COPY W411TILK -PRE HELP-                                            
021200*01  -COPY W411AREG -PRE HELP-                                            
021300                                                                          
021400 77  W-REPSW                     PIC X       VALUE 'J'.                   
021500                                                                          
021600 77  WS-TRANSFER                 PIC X       VALUE 'N'.                   
021700                                                                          
021800 01  WS-ALFA-1.                                                           
021900     03  WS-NUM-1                PIC 9(1).                                
022000 01  WS-ALFA-6.                                                           
022100     03  WS-NUM-6                PIC 9(6).                                
022200                                                                          
022300 01  WS-IDARTNR.                                                          
022400     03  WS-IDARTNR-1-9          PIC X(9).                                
022500     03  WS-IDARTNR-10           PIC X(1) VALUE '-'.                      
022600     03  WS-IDARTNR-11           PIC X(1).                                
022700                                                                          
022800 01  WS-TITIREGD-9KOMPL          PIC 9(8).                                
022900 01  FILLER REDEFINES WS-TITIREGD-9KOMPL.                                 
023000     03  WS-SEKEL-9KOMPL         PIC 9(2).                                
023100     03  WS-AAMMDD-9KOMPL        PIC 9(6).                                
023200                                                                          
023300 01  DAGENS-DATUM                PIC 9(6)  VALUE ZERO.                    
023400 01  FILLER                REDEFINES DAGENS-DATUM.                        
023500     03  DAGENS-AA         PIC  9(2).                                     
023600     03  DAGENS-MM         PIC  9(2).                                     
023700     03  DAGENS-DD         PIC  9(2).                                     
023800                                                                          
023900 01  WS-TIRFS                    PIC 9(10).                               
024000 01  FILLER REDEFINES WS-TIRFS.                                           
024100     03  WS-TIRFS-DAT            PIC 9(6).                                
024200     03  WS-TIRFS-TID            PIC 9(4).                                
024300 01  W-WORK-VAR.                                                          
024400     03 W-FLREFILL-MAIN          PIC X       VALUE SPACE.                 
024500     03 W-KDPRODSL-MAIN          PIC S9(3)   COMP-3 VALUE 0.              
024600     03 W-KDSORT-MAIN            PIC X(2)    VALUE SPACE.                 
024700     03 W-KVQPACK-1-MAIN         PIC S9(5)   COMP-3 VALUE 0.              
024800     03 W-REDIRLEV-MAIN          PIC S9V9(2) COMP-3 VALUE 0.              
024900     03 W-FLREFILL-REPL          PIC X       VALUE SPACE.                 
025000     03 W-KDPRODSL-REPL          PIC S9(3)   COMP-3 VALUE 0.              
025100     03 W-KDSORT-REPL            PIC X(2)    VALUE SPACE.                 
025200     03 W-KVQPACK-1-REPL         PIC S9(5)   COMP-3 VALUE 0.              
025300     03 W-REDIRLEV-REPL          PIC S9V9(2) COMP-3 VALUE 0.              
025400     03 W-IDARTNR-SDCA           PIC S9(9)   COMP-3 VALUE 0.              
025500                                                                          
025600     03 W-GMT-IDDC-CLEAR-GRP.                                             
025700*                                 GRUPP AV IDDC-CLEAR                     
025800        05 W-GMT-IDDC-CLEAR   OCCURS 99 TIMES                             
025900                                 PIC X(2)    VALUE SPACE.                 
026000                                                                          
026100 01 DB2-LASNING.                                                          
026200     03 FILLER                   PIC X(16)   VALUE                        
026300                                             'WS-DB2-SEKTION'.            
026400     03 WS-DB2-SEKTION           PIC X(24)   VALUE SPACE.                 
026500                                                                          
026600     EJECT                                                                
026700 01 NYCKLAR-TP4TRAN.                                                      
026800     03 W-TP4TRAN-IDDISTR        PIC S9(5)   COMP-3 VALUE ZERO.           
026900                                                                          
027000 01  TEST-IDDISTR                PIC  9(5)   COMP-3.                      
027100*01  FILLER   -COPY WWDIST07    -RED TEST-IDDISTR.                        
027200     EJECT                                                                
027300*01  FILLER   -COPY WWDIST11    -RED TEST-IDDISTR.                        
027400     EJECT                                                                
027500*01  FILLER   -COPY WWDIST18    -RED TEST-IDDISTR.                        
027600     EJECT                                                                
027700*01  FILLER   -COPY WWDIST23    -RED TEST-IDDISTR.                        
027800     EJECT                                                                
027900*01  FILLER   -COPY WWDIST34    -RED TEST-IDDISTR.                        
028000     EJECT                                                                
028100*01  FILLER   -COPY WWDIST35    -RED TEST-IDDISTR.                        
028200     EJECT                                                                
028300*01  FILLER   -COPY WWDIST79    -RED TEST-IDDISTR.                        
028400*    ----DISTR-DEALER-PRICE----                                           
028500     EJECT                                                                
028600                                                                          
028700 01  TEST-ARTIKEL                PIC 9(9)    COMP-3.                      
028800*01 FILLER  -COPY WWART04       -RED  TEST-ARTIKEL.                       
028900     EJECT                                                                
029000 01  FILLER                   PIC X(16) VALUE 'REFILLTAB-DC '.            
029100*    -COPY WWDIST57                                                       
029200     EJECT                                                                
029300 01  FILLER                   PIC X(16) VALUE 'TILLKOMMANDE-TAB'.         
029400*    -COPY W411TILK                                                       
029500                                                                          
029600*   -COPY W413WHFA                                                        
029700                                                                          
029800*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
029900 01  GENERELLA-SUBPROGRAM.                                                
030000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
030100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
030200     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
030300     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
030400     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
030500     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
030600*                                                                         
030700*                                                                         
030800*    --- PARAMETRAR TILL GENERELLA SUBPROGRAM                             
030900 01 FILLER                       PIC X(8) VALUE 'W005INIT'.               
031000*   -COPY WMSGINIT                                                        
031100     EJECT                                                                
031200 01  FILLER                  PIC X(16)   VALUE 'WORKAREA   '.             
031300*   -COPY WORKAREA                                                        
031400     EJECT                                                                
031500*                                                                         
031600 01  GEMENSAMMA-SUBPROGRAM.                                               
031700     03  W335PRIS                PIC X(8)    VALUE 'W335PRIS'.            
031800*        PRISTILLÄMPNING                                                  
031900     03  W335PRNO                PIC X(8)    VALUE 'W335PRNO'.            
032000*        HÄMTA PRISFRÅGENR                                                
032100     03  W335PRQU                PIC X(8)    VALUE 'W335PRQU'.            
032200*        DEALER PRISFRÅGABEHANDLING                                       
032300     03  W411AREG                PIC X(8)    VALUE 'W411AREG'.            
032400*        LÄSNING ARTIKELREGISTER                                          
032500     03  W411ARTM                PIC X(8)    VALUE 'W411ARTM'.            
032600*        NYUPPLÄGG AV TOM ROT PÅ WDK9                                     
032700     03  W411DLEV                PIC X(8)    VALUE 'W411DLEV'.            
032800*        KONTROLL DIREKTLEVERANS                                          
032900     03  W411DNOT                PIC X(8)    VALUE 'W411DNOT'.            
033000*        DATA TILL DEL NOTE NDC                                           
033100     03  W411KAMP                PIC X(8)    VALUE 'W411KAMP'.            
033200*        KONTROLL TPO4 - KAMPANJ                                          
033300     03  W411KERS                PIC X(8)    VALUE 'W411KERS'.            
033400*        KONTROLL ERSÄTTNINGAR                                            
033500     03  W411KVAN                PIC X(8)    VALUE 'W411KVAN'.            
033600*        KONTROLL KVANTANPASSNING                                         
033700     03  W411LAST                PIC X(8)    VALUE 'W411LAST'.            
033800*        KONTROLL ENHETSLAST                                              
033900     03  W411ORFK                PIC X(8)    VALUE 'W411ORFK'.            
034000*        FORMELLA KONTROLLER AV INDATA                                    
034100     03  W411CDCA                PIC X(8)    VALUE 'W411CDCA'.            
034200*        KONTROLL PRELIMINÄRAVBOKNING-CDC                                 
034300     03  W411RANS                PIC X(8)    VALUE 'W411RANS'.            
034400*        BERÄKNA RANSONERING                                              
034500     03  W411NDCA                PIC X(8)    VALUE 'W411NDCA'.            
034600*        KONTROLL PRELIMINÄRAVBOKNING-NDC                                 
034700     03  W411XDCA                PIC X(8)    VALUE 'W411XDCA'.            
034800*        KONTROLL PRELIMINÄRAVBOKNING-XDC                                 
034900     03  W411SDCA                PIC X(8)    VALUE 'W411SDCA'.            
035000*        KONTROLL PRELIMINÄRAVBOKNING-SDC                                 
035100     03  W411SPAR                PIC X(8)    VALUE 'W411SPAR'.            
035200*        KONTROLL SPÄRRAR                                                 
035300     03  W411STOR                PIC X(8)    VALUE 'W411STOR'.            
035400*        KONTROLL STORA UTTAG                                             
035500     03  W411TPO1                PIC X(8)    VALUE 'W411TPO1'.            
035600*        KONTROLL TPO1                                                    
035700     03  W411TPO2                PIC X(8)    VALUE 'W411TPO2'.            
035800*        KONTROLL TPO2                                                    
035900     03  W411RELS                PIC X(8)    VALUE 'W411RELS'.            
036000*        KONTROLL RELS                                                    
036100     03  W411TPO3                PIC X(8)    VALUE 'W411TPO3'.            
036200*        UPPDATERING TPO3                                                 
036300     03  W411TPO5                PIC X(8)    VALUE 'W411TPO5'.            
036400*        KONTROLL PROFORMOR                                               
036500     03  W411TPO6                PIC X(8)    VALUE 'W411TPO6'.            
036600*        UPPDATERING TPO6                                                 
036700     03  W411CLDC                PIC X(8)    VALUE 'W411CLDC'.            
036800*        WDB601-SEGMENT FÖR CLARING-DC                                    
036900     03  W413AVSR                PIC X(8)    VALUE 'W413AVSR'.            
037000*        WOPS RADBEHANDLING                                               
037100     03  W413ADRS                PIC X(8)    VALUE 'W413ADRS'.            
037200*        OMVANDLING AV LAGOMR + PLATS                                     
037300     EJECT                                                                
037400                                                                          
037500 01  MESSAGE-CODES.                                                       
037600     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
037700     03  ERR-ORDER-SAKNAS        PIC X(3)    VALUE '701'.                 
037800     03  ERR-ORDER-AVSLUTAD      PIC X(3)    VALUE '718'.                 
037900     03  OK-BEHANDLAD            PIC X(3)    VALUE '101'.                 
038000     EJECT                                                                
038100*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
038200 01 FILLER                       PIC X(8) VALUE 'W335PRIS'.               
038300*   -COPY W335PRIS                                                        
038400     EJECT                                                                
038500 01 FILLER                       PIC X(8) VALUE 'W335PRNO'.               
038600*   -COPY W335PRNO                                                        
038700     EJECT                                                                
038800 01 FILLER                       PIC X(8) VALUE 'W335PRQU'.               
038900*   -COPY W335PRQU                                                        
039000     EJECT                                                                
039100 01 FILLER                       PIC X(8) VALUE 'W411AREG'.               
039200*   -COPY W411AREG                                                        
039300     EJECT                                                                
039400 01 FILLER                       PIC X(8) VALUE 'W411ARTM'.               
039500*   -COPY W411ARTM                                                        
039600     EJECT                                                                
039700 01 FILLER                       PIC X(8) VALUE 'W411DLEV'.               
039800*   -COPY W411DLEV                                                        
039900     EJECT                                                                
040000 01 FILLER                       PIC X(8) VALUE 'W411DNOT'.               
040100*   -COPY W411DNOT                                                        
040200     EJECT                                                                
040300 01 FILLER                       PIC X(8) VALUE 'W411KAMP'.               
040400*   -COPY W411KAMP                                                        
040500     EJECT                                                                
040600 01 FILLER                       PIC X(8) VALUE 'W411KERS'.               
040700*   -COPY W411KERS                                                        
040800     EJECT                                                                
040900 01 FILLER                       PIC X(8) VALUE 'W411KVAN'.               
041000*   -COPY W411KVAN                                                        
041100     EJECT                                                                
041200 01 FILLER                       PIC X(8) VALUE 'W411LAST'.               
041300*   -COPY W411LAST                                                        
041400     EJECT                                                                
041500 01 FILLER                       PIC X(8) VALUE 'W411ORFK'.               
041600*   -COPY W411ORFK                                                        
041700     EJECT                                                                
041800 01 FILLER                       PIC X(8) VALUE 'W411CDCA'.               
041900*   -COPY W411CDCA                                                        
042000     EJECT                                                                
042100 01 FILLER                       PIC X(8) VALUE 'W411RANS'.               
042200*   -COPY W411RANS                                                        
042300     EJECT                                                                
042400 01 FILLER                       PIC X(8) VALUE 'W411NDCA'.               
042500*   -COPY W411NDCA                                                        
042600     EJECT                                                                
042700 01 FILLER                       PIC X(8) VALUE 'W411XDCA'.               
042800*   -COPY W411XDCA                                                        
042900     EJECT                                                                
043000 01 FILLER                       PIC X(8) VALUE 'W411XDK7'.               
043100*   -COPY W411XDK7 -PRE NDCA-                                             
043200     EJECT                                                                
043300 01 FILLER                       PIC X(8) VALUE 'W411SDCA'.               
043400*   -COPY W411SDCA                                                        
043500     EJECT                                                                
043600 01 FILLER                       PIC X(8) VALUE 'W411SPAR'.               
043700*   -COPY W411SPAR                                                        
043800     EJECT                                                                
043900 01 FILLER                       PIC X(8) VALUE 'W411STOR'.               
044000*   -COPY W411STOR                                                        
044100     EJECT                                                                
044200 01 FILLER                       PIC X(8) VALUE 'W411TPO1'.               
044300*   -COPY W411TPO1                                                        
044400     EJECT                                                                
044500 01 FILLER                       PIC X(8) VALUE 'W411TPO2'.               
044600*   -COPY W411TPO2                                                        
044700     EJECT                                                                
044800 01 FILLER                       PIC X(8) VALUE 'W411RELS'.               
044900*   -COPY W411RELS                                                        
045000     EJECT                                                                
045100 01 FILLER                       PIC X(8) VALUE 'W411TPO3'.               
045200*   -COPY W411TPO3                                                        
045300     EJECT                                                                
045400 01 FILLER                       PIC X(8) VALUE 'W411TPO5'.               
045500*   -COPY W411TPO5                                                        
045600     EJECT                                                                
045700 01 FILLER                       PIC X(8) VALUE 'W411TPO6'.               
045800*   -COPY W411TPO6                                                        
045900     EJECT                                                                
046000 01 FILLER                       PIC X(8) VALUE 'W411CLDC'.               
046100*   -COPY W411CLDC                                                        
046200     EJECT                                                                
046300 01 FILLER                       PIC X(8) VALUE 'W413AVSR'.               
046400*   -COPY W413AVSR                                                        
046500     SKIP2                                                                
046600 01 FILLER                       PIC X(8) VALUE 'W413ADRS'.               
046700*   -COPY W413ADRS                                                        
046800     EJECT                                                                
046900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
047000     SKIP3                                                                
047100 01  MID-AREA.                                                            
047200*03  MID -COPY W4I25201                                                   
047300     EJECT                                                                
047400 01  SPAR-MID-AREA.                                                       
047500*03  MID -COPY W4I25201   -PRE SPAR-                                      
047600     EJECT                                                                
047700 01  FILLER                      PIC X(16)  VALUE 'MSG-IO-AREA'.          
047800     SKIP3                                                                
047900*01  -COPY WMSGAREA                                                       
048000     EJECT                                                                
048100*    05  -COPY W4I29801  -PRE 4298-  -RED MSG-MID-OUT                     
048200     EJECT                                                                
048300*    05  -COPY W4I29901  -PRE 4299-  -RED MSG-MID-OUT                     
048400     EJECT                                                                
048500*    05  -COPY W4I25701  -PRE 4257-  -RED MSG-MID-OUT                     
048600     EJECT                                                                
048700******************************************************************        
048800*    MID-AREA FÖR W2T191                                         *        
048900******************************************************************        
049000*01  -COPY  W2I19101  -PRE 2191-                                          
049100     EJECT                                                                
049200 01  FILLER                      PIC X(16)  VALUE 'KOM-IO-AREA'.          
049300     SKIP3                                                                
049400 01  KOM-IO-AREA.                                                         
049500*03  -COPY WMSGKOM                                                        
049600     EJECT                                                                
049700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
049800*                                                                         
049900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
050000     SKIP3                                                                
050100 01  NYCKLAR-TILL-DLI.                                                    
050200                                                                          
050300     03  W-IDGMTREF-X.                                                    
050400         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
050500         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
050600         05  W-IDKUNDRF          PIC X(10)   VALUE SPACE.                 
050700                                                                          
050800     03  W-IDORDER-X.                                                     
050900         05  W-IDORDER           PIC S9(7)   VALUE ZERO COMP-3.           
051000                                                                          
051100     03  W-IDARTNR-X.                                                     
051200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
051300                                                                          
051400     03  W-IDARTNR-T-X.                                                   
051500         05  W-IDARTNR-T         PIC S9(9)   VALUE ZERO COMP-3.           
051600                                                                          
051700     03  W-IDDC-X.                                                        
051800         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
051900                                                                          
052000     03  W-IDDC-T-X.                                                      
052100         05  W-IDDC-T            PIC X(2)    VALUE SPACE.                 
052200                                                                          
052300     03  W-WDGXKEY-4541-X.                                                
052400         05  W-IDHTYP            PIC X(04)   VALUE '4541'.                
052500         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
052600                                                                          
052700     03  W-WDQ101KY-MIN-X.                                                
052800         05  W-IDORDER-Q1-MIN    PIC S9(7)   COMP-3.                      
052900         05  W-IDARTNR-Q1-MIN    PIC S9(9)   COMP-3.                      
053000         05  W-IDLOPNR-Q1-MIN    PIC S9(3)   COMP-3.                      
053100         05  W-IDSEKVNR-Q1-MIN   PIC S9(3)   COMP-3.                      
053200         05  FILLER              PIC X(04)   VALUE LOW-VALUE.             
053300                                                                          
053400     03  W-WDQ101KY-MAX-X.                                                
053500         05  W-IDORDER-Q1-MAX    PIC S9(7)   COMP-3.                      
053600         05  W-IDARTNR-Q1-MAX    PIC S9(9)   COMP-3.                      
053700         05  W-IDLOPNR-Q1-MAX    PIC S9(3)   COMP-3.                      
053800         05  W-IDSEKVNR-Q1-MAX   PIC S9(3)   COMP-3.                      
053900         05  FILLER              PIC X(04)   VALUE HIGH-VALUE.            
054000                                                                          
054100     03  W-IDGMT-X.                                                       
054200         05  W-IDDISTR-WDB2      PIC S9(5) VALUE ZERO COMP-3.             
054300         05  W-IDKUNDNR-WDB2     PIC S9(7) VALUE ZERO COMP-3.             
054400*                                                                         
054500     03  W-WDB101KY-X.                                                    
054600         05  W-WDB1-IDPARTNR     PIC X(9)    VALUE SPACE.                 
054700         05  W-WDB1-IDFTG        PIC 9(2)    VALUE ZERO.                  
054800                                                                          
054900     03  W-IDDC-B6-X.                                                     
055000         05 W-IDDC-B6                  PIC X(2).                          
055100                                                                          
055200     03  W-4511-IDHTYP-X.                                                 
055300         05  W-4511-IDHTYP       PIC  X(04) VALUE '4511'.                 
055400         05  W-LOW-VALUE         PIC  X(26) VALUE LOW-VALUE.              
055500                                                                          
055600     03  W-4512-KDTPOTYP-X.                                               
055700         05  W-4512-KDTPOTYP     PIC  S9(1) COMP-3.                       
055800                                                                          
055900     03  W-4512-KDORDKL-X.                                                
056000         05  W-4512-KDORDKL      PIC  S9(1) COMP-3.                       
056100                                                                          
056200     03  W-4512-IDDISTR-FOM-X.                                            
056300         05  W-4512-IDDISTR-FOM  PIC  S9(5) COMP-3.                       
056400                                                                          
056500     03  W-4512-IDDISTR-TOM-X.                                            
056600         05  W-4512-IDDISTR-TOM  PIC  S9(5) COMP-3.                       
056700*                                                                         
056800     03  W-WDB301KY-X.                                                    
056900         05  W-IDDC-WDB3          PIC X(2)    VALUE SPACE.                
057000         05  W-IDDISTR-WDB3       PIC S9(5)   COMP-3 VALUE ZERO.          
057100         05  W-IDKUNDNR-WDB3      PIC S9(7)   COMP-3 VALUE ZERO.          
057200*                                                                         
057300     03  W-WDB301KY-DEF-X.                                                
057400         05  W-IDDC-WDB3-DEF      PIC X(2)    VALUE SPACE.                
057500         05  W-IDDISTR-WDB3-DEF   PIC S9(5)   COMP-3 VALUE ZERO.          
057600         05  W-IDKUNDNR-WDB3-DEF  PIC S9(7) VALUE +9999999 COMP-3.        
057700     EJECT                                                                
057800*    --- STATUS-KOD FRÅN IMS                                              
057900 01  STATUS-WS                   PIC XX.                                  
058000     88  SEGMENT-FINNS                       VALUE '  '.                  
058100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
058200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
058300     88  BASEN-SLUT                          VALUE 'GB'.                  
058400     SKIP2                                                                
058500 01  GODK-STATUSKODER.                                                    
058600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
058700                                                                          
058800 01  SSA1                        PIC X(160).                              
058900 01  SSA2                        PIC X(96).                               
059000 01  SSA3                        PIC X(64).                               
059100     EJECT                                                                
059200*                            DB2 FUNKTIONSKODER                           
059300 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
059400       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
059500                                                                          
059600 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
059700 01  DB2-WS.                                                              
059800     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
059900         88  CURSOR-OK                       VALUE 000.                   
060000         88  RADER-FINNS                     VALUE 000.                   
060100         88  RADER-SAKNAS                    VALUE 100.                   
060200         88  ATKOMST-FEL                     VALUE 904.                   
060300     03  GODK-SQLCODEKODER.                                               
060400         05  GODK-SQLCODE OCCURS 5                                        
060500             INDEXED BY SQLCODE-IX PIC 9(3).                              
060600 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
060700     EJECT                                                                
060800*    --- IMS FUNKTIONSKODER                                               
060900*01  -COPY W0003                                                          
061000     EJECT                                                                
061100*    ---  DLI INPUT-OUTPUT AREA                                           
061200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
061300     EJECT                                                                
061400 01  FILLER                      PIC X(16)   VALUE 'WDQ101-AREA'.         
061500 01  DLI-IO-AREA-OBKR.                                                    
061600     03  WLORQM01.                                                        
061700*        05  -COPY WDQ101                                                 
061800     EJECT                                                                
061900 01  FILLER                      PIC X(16)   VALUE 'WDQ201-AREA'.         
062000 01  DLI-IO-AREA-OHUV.                                                    
062100     03  WLORQI01.                                                        
062200*        05  -COPY WDQ201                                                 
062300     EJECT                                                                
062400 01  FILLER                      PIC X(16)   VALUE 'WDQ201-EMPTY'.        
062500 01  DLI-IO-AREA-OHUV-EMPTY.                                              
062600     03  WLORQI01.                                                        
062700*        05  -COPY WDQ201 -PRE EMPTY-                                     
062800     EJECT                                                                
062900 01  FILLER                      PIC X(16)   VALUE 'WDQ212-AREA'.         
063000 01  DLI-IO-AREA-ARB.                                                     
063100     03  WLORQI12.                                                        
063200*        05  -COPY WDQ212                                                 
063300     EJECT                                                                
063400 01  FILLER                      PIC X(16)   VALUE 'WDQ401-AREA'.         
063500 01  DLI-IO-AREA-ORAD.                                                    
063600     03  WLORQF01.                                                        
063700*        05  -COPY WDQ401                                                 
063800     EJECT                                                                
063900 01  FILLER                      PIC X(16)   VALUE 'WDK901-AREA'.         
064000 01  DLI-IO-AREA-ART.                                                     
064100     03  WLARTM01.                                                        
064200*        05  -COPY WDK901                                                 
064300     EJECT                                                                
064400 01  FILLER                      PIC X(16)   VALUE '454111-AREA'.         
064500 01  DLI-IO-AREA-4541.                                                    
064600     03  WL454111.                                                        
064700*        05  -COPY WDGX4542                                               
064800     EJECT                                                                
064900 01  FILLER                      PIC X(16)   VALUE 'WDK711-AREA'.         
065000 01  DLI-IO-AREA-WDK7.                                                    
065100     03  WDK711.                                                          
065200*        05  -COPY WDK711                                                 
065300 01  FILLER                      PIC X(16)   VALUE 'WDK722-AREA'.         
065400 01  DLI-IO-WDK722.                                                       
065500     03  WDK722.                                                          
065600*        05  -COPY WDK722                                                 
065700     EJECT                                                                
065800 01  FILLER                      PIC X(16)   VALUE 'WDK611-AREA'.         
065900 01  DLI-IO-AREA-WDK611.                                                  
066000*    03  -COPY WDK611                                                     
066100     EJECT                                                                
066200 01  FILLER                      PIC X(16)   VALUE 'WDK611-T-AR'.         
066300 01  DLI-IO-AREA-WDK611-T.                                                
066400*    03  -COPY WDK611 -PRE TILK-                                          
066500     EJECT                                                                
066600 01  FILLER                      PIC X(16)   VALUE 'WDB201-AREA'.         
066700 01  DLI-IO-AREA-WDB201.                                                  
066800*    03  -COPY WDB201                                                     
066900     EJECT                                                                
067000 01  FILLER                      PIC X(16)   VALUE 'WDB101-AREA'.         
067100 01  DLI-IO-AREA-WDB101.                                                  
067200     03  WLBETC01.                                                        
067300         05  -COPY WDB101                                                 
067400                                                                          
067500 01  FILLER                      PIC X(16)   VALUE 'WDB601 AREA'.         
067600 01   DLI-IO-AREA-B601.                                                   
067700*     03  -COPY WDB601                                                    
067800                                                                          
067900 01  FILLER                   PIC X(16) VALUE 'DLI-IO-WDA5  '.            
068000 01  DLI-IO-WDA501.                                                       
068100*    03  WDA5 -COPY WDA501 -PRE WDA5-                                     
068200     EJECT                                                                
068300                                                                          
068400 01  FILLER                   PIC X(16)   VALUE 'WDB301 AREA'.            
068500 01   DLI-IO-AREA-WDB301.                                                 
068600*     03  -COPY WDB301                                                    
068700     EJECT                                                                
068800 01  FILLER                   PIC X(16)   VALUE '4251 AREA'.              
068900 01  DLI-IO-4251.                                                         
069000*    03  -COPY WDGX4251                                                   
069100     EJECT                                                                
069200 01  FILLER                   PIC X(16)   VALUE '4252 AREA'.              
069300 01  DLI-IO-4252.                                                         
069400*    03  -COPY WDGX4252                                                   
069500     EJECT                                                                
069600 01  FILLER                    PIC X(11)  VALUE 'IO-AREA-SUB'.            
069700 01  DLI-IO-AREA-SUB.                                                     
069800     03  WLXXJN11.                                                        
069900*        05  -COPY WDGX4512                                               
070000 01  FILLER                    PIC X(16)   VALUE 'SEND-CONTROL'.          
070100     SKIP3                                                                
070200 01  WS-IDCOM           PIC S9(9)  VALUE ZERO  COMP-3.                    
070300 01  -COPY WZ01SEND                                                       
070400     EJECT                                                                
070500 01  FILLER                    PIC X(16)   VALUE 'SEND-AREA'.             
070600     SKIP3                                                                
070700 01  SEND-AREA.                                                           
070800*    03  -COPY WZ01REQU  -PRE 3039-                                       
070900*    03  -COPY W30391I1  -PRE 3039-                                       
071000     EJECT                                                                
071100 01  FILLER                      PIC X(16)  VALUE 'TP4TRAN-AREA'.         
071200                                                                          
071300*01  -COPY TP4TRAN -PRE TP4TRAN-                                          
071400     EJECT                                                                
071500     EXEC SQL INCLUDE TP4TRAN END-EXEC.                                   
071600     EJECT                                                                
071700 01  DLI-IO-AREA2.                                                        
071800     03  IO-AREA2                PIC X(300)  VALUE SPACE.                 
071900     SKIP3                                                                
072000*    03  WLFILA01     -COPY WDR601           -RED IO-AREA2.               
072100*    07  W414203A     -COPY W414203A         -RED FIL-WDR601-DATA.        
072200                                                                          
072300 01  FILLER               PIC X(16)   VALUE 'WDR601 AREA'.                
072400 01   DLI-IO-AREA-R601.                                                   
072500*     03  -COPY WDR601 -PRE R6-                                           
072600*     05  -COPY W414XDCA      -RED R6-FIL-WDR601-DATA                     
072700     EJECT                                                                
072800 LINKAGE SECTION.                                                         
072900                                                                          
073000*01  -COPY W0009   -PRE MSG-                                              
073100                                                                          
073200*01  -COPY W0009   -PRE DISP-                                             
073300     EJECT                                                                
073400*01  -COPY W0009   -PRE 4298-                                             
073500     EJECT                                                                
073600*01  -COPY W0009   -PRE 4299-                                             
073700     EJECT                                                                
073800*01  -COPY W0009   -PRE 2191-                                             
073900     EJECT                                                                
074000*01  -COPY W0009   -PRE PRQRY-                                            
074100     SKIP2                                                                
074200*01  -COPY W0009   -PRE DLEV-                                             
074300     SKIP2                                                                
074400*01  -COPY W0008   -PRE USEA-                                             
074500     05  FILLER                  PIC X.                                   
074600     SKIP2                                                                
074700*01  -COPY W0008   -PRE ORQF-                                             
074800     05  FILLER                  PIC X.                                   
074900     SKIP2                                                                
075000*01  -COPY W0008   -PRE ORQI-                                             
075100     05  FILLER                  PIC X.                                   
075200     EJECT                                                                
075300*01  -COPY W0008   -PRE ORQM-                                             
075400     05  FILLER                  PIC X.                                   
075500     SKIP2                                                                
075600*01  -COPY W0008   -PRE ARTM-                                             
075700     05  FILLER                  PIC X.                                   
075800     EJECT                                                                
075900*01  -COPY W0008   -PRE 4541-                                             
076000     05  FILLER                  PIC X.                                   
076100     EJECT                                                                
076200*01  -COPY W0008   -PRE WDK7-                                             
076300     05  FILLER                  PIC X.                                   
076400     EJECT                                                                
076500*01  -COPY W0008   -PRE WDK6-                                             
076600     05  FILLER                  PIC X.                                   
076700     EJECT                                                                
076800*01  -COPY W0008   -PRE WDB2-                                             
076900     05  FILLER                  PIC X.                                   
077000     EJECT                                                                
077100*01  -COPY W0008   -PRE WDB1-                                             
077200     05  FILLER                  PIC X.                                   
077300     EJECT                                                                
077400*01  -COPY W0008   -PRE WDB6-                                             
077500     05  FILLER                  PIC X.                                   
077600     EJECT                                                                
077700*01  -COPY W0008   -PRE WDA5-                                             
077800     05  FILLER                  PIC X.                                   
077900     EJECT                                                                
078000*01  -COPY W0008   -PRE XXJN-                                             
078100     05  FILLER                  PIC X.                                   
078200     EJECT                                                                
078300*01  -COPY W0008   -PRE FILA-                                             
078400     05  FILLER                  PIC X.                                   
078500     EJECT                                                                
078600*01  -COPY W0008   -PRE WDB3-                                             
078700     05  FILLER                  PIC X.                                   
078800     EJECT                                                                
078900*01  -COPY W0008   -PRE 4251-                                             
079000     05  FILLER                  PIC X.                                   
079100     EJECT                                                                
079200*01  -COPY W0008   -PRE WDR6-                                             
079300     05  FILLER                  PIC X.                                   
079400     EJECT                                                                
079500                                                                          
079600 01  PRIS-ARTC-PCB               PIC X.                                   
079700 01  PRIS-WDK7-PCB               PIC X.                                   
079800 01  PRIS-GMTA-PCB               PIC X.                                   
079900 01  PRIS-BETA-PCB               PIC X.                                   
080000 01  PRIS-GPRIA-PCB              PIC X.                                   
080100 01  PRIS-GPRIB-PCB              PIC X.                                   
080200 01  PRIS-COST-WDK6-PCB          PIC X.                                   
080300 01  PRIS-COST-WDK7-PCB          PIC X.                                   
080400 01  PRIS-COST-WDF1-PCB          PIC X.                                   
080500 01  PRIS-COST-9305-PCB          PIC X.                                   
080600 01  PRIS-COST-WDK72-PCB         PIC X.                                   
080700 01  PRIS-COST-WDB6-PCB          PIC X.                                   
080800                                                                          
080900 01  PRNO-3107-PCB               PIC X.                                   
081000 01  PRQU-WDC7-PCB               PIC X.                                   
081100 01  PRQU-WDG2-PCB               PIC X.                                   
081200 01  PRQU-SJKO-WDK6-PCB          PIC X.                                   
081300                                                                          
081400 01  AREG-WDK6-PCB               PIC X.                                   
081500 01  AREG-WDK7-PCB               PIC X.                                   
081600                                                                          
081700 01  ARTM-ARTM-PCB               PIC X.                                   
081800                                                                          
081900 01  DLEV-LEVF-PCB               PIC X.                                   
082000 01  DLEV-LEVG-PCB               PIC X.                                   
082100 01  DLEV-LEVA-PCB               PIC X.                                   
082200 01  DLEV-ARTS-PCB               PIC X.                                   
082300 01  DLEV-WDB6-PCB               PIC X.                                   
082400                                                                          
082500 01  SPAR-WDF8-PCB               PIC X.                                   
082600 01  SPAR-WDF8A-PCB              PIC X.                                   
082700 01  SPAR-WDK6-PCB               PIC X.                                   
082800                                                                          
082900 01  DNOT-ORQP-PCB               PIC X.                                   
083000 01  DNOT-ORQP2-PCB              PIC X.                                   
083100 01  DNOT-ORQP3-PCB              PIC X.                                   
083200 01  DNOT-4013-PCB               PIC X.                                   
083300 01  DNOT-BENA-PCB               PIC X.                                   
083400                                                                          
083500 01  KAMP-ORDP-PCB               PIC X.                                   
083600 01  KAMP-ZZAC-PCB               PIC X.                                   
083700 01  KAMP-WDM2-PCB               PIC X.                                   
083800                                                                          
083900 01  KERS-ARTC-PCB               PIC X.                                   
084000 01  KERS-ERSA-PCB               PIC X.                                   
084100                                                                          
084200 01  NDCA-USEA-PCB               PIC X.                                   
084300 01  NDCA-WDK7-PCB               PIC X.                                   
084400 01  NDCA-WDL6-PCB               PIC X.                                   
084500 01  NDCA-WDB6-PCB               PIC X.                                   
084600                                                                          
084700 01  SDCA-ARTS-PCB               PIC X.                                   
084800 01  SDCA-WDB6-PCB               PIC X.                                   
084900 01  SDCA-WDK9-PCB               PIC X.                                   
085000 01  SDCA-WDR6-PCB               PIC X.                                   
085100 01  SDCA-WDK6-PCB               PIC X.                                   
085200 01  SDCA-WDQ4B-PCB              PIC X.                                   
085300 01  SDCA-WDQ2-PCB               PIC X.                                   
085400 01  SDCA-WDQ4-PCB               PIC X.                                   
085500 01  SDCA-WDB6-2-PCB             PIC X.                                   
085600 01  SDCA-WDK6-2-PCB             PIC X.                                   
085700 01  SDCA-WDK7-2-PCB             PIC X.                                   
085800 01  SDCA-WDK7-3-PCB             PIC X.                                   
085900                                                                          
086000 01  CDCA-ARTM-PCB               PIC X.                                   
086100 01  CDCA-INLB-PCB               PIC X.                                   
086200 01  CDCA-WDB2-PCB               PIC X.                                   
086300 01  CDCA-WDC1-PCB               PIC X.                                   
086400                                                                          
086500 01  RANS-XXKM-PCB               PIC X.                                   
086600 01  RANS-ARTM-PCB               PIC X.                                   
086700 01  RANS-ARTS-PCB               PIC X.                                   
086800                                                                          
086900 01  TPO1-ORDP-PCB               PIC X.                                   
087000 01  TPO1-ARTM-PCB               PIC X.                                   
087100 01  TPO1-ZZAC-PCB               PIC X.                                   
087200                                                                          
087300 01  TPO2-ORDP-PCB               PIC X.                                   
087400 01  TPO2-XXBU-PCB               PIC X.                                   
087500 01  TPO2-XXBV-PCB               PIC X.                                   
087600 01  TPO2-ARTM-PCB               PIC X.                                   
087700 01  TPO2-FILA-PCB               PIC X.                                   
087800 01  TPO2-XXBX-PCB               PIC X.                                   
087900                                                                          
088000 01  RELS-ORDP-PCB               PIC X.                                   
088100 01  RELS-FILA-PCB               PIC X.                                   
088200 01  RELS-ARTM-PCB               PIC X.                                   
088300                                                                          
088400 01  TPO3-ORDP-PCB               PIC X.                                   
088500 01  TPO3-ZZAC-PCB               PIC X.                                   
088600                                                                          
088700 01  TPO5-ORDP-PCB               PIC X.                                   
088800 01  TPO5-ARTM-PCB               PIC X.                                   
088900                                                                          
089000 01  2109-PCB                    PIC X.                                   
089100 01  TPO6-ORDP-PCB               PIC X.                                   
089200 01  TPO6-XXBU-PCB               PIC X.                                   
089300 01  TPO6-XXBV-PCB               PIC X.                                   
089400 01  TPO6-XXBX-PCB               PIC X.                                   
089500 01  TPO6-ARTS-PCB               PIC X.                                   
089600                                                                          
089700 01  TIME-4437-PCB               PIC X.                                   
089800                                                                          
089900 01  AVSR-LIST-PCB               PIC X.                                   
090000 01  AVSR-ORQI-PCB               PIC X.                                   
090100 01  AVSR-GMTB-PCB               PIC X.                                   
090200 01  AVSR-GMTC-PCB               PIC X.                                   
090300 01  AVSR-WDB2-PCB               PIC X.                                   
090400 01  AVSR-WDB6-PCB               PIC X.                                   
090500                                                                          
090600 01  TRAN-XXKB-PCB               PIC X.                                   
090700 01  KVAN-WDB2-PCB               PIC X.                                   
090800 01  KVAN-WDC1-PCB               PIC X.                                   
090900                                                                          
091000 01  XDCA-USEA-PCB               PIC X.                                   
091100 01  XDCA-WDB6-PCB               PIC X.                                   
091200 01  XDCA-WDK6-PCB               PIC X.                                   
091300 01  XDCA-WDK7-PCB               PIC X.                                   
091400 01  XDCA-WDK9-PCB               PIC X.                                   
091500 01  XDCA-WDL6-PCB               PIC X.                                   
091600 01  XDCA-WDQ4B-PCB              PIC X.                                   
091700 01  XDCA-WDQ2-PCB               PIC X.                                   
091800 01  XDCA-WDQ4-PCB               PIC X.                                   
091900 01  XDCA-WDR6-PCB               PIC X.                                   
092000 01  XDCA-WDB6-2-PCB             PIC X.                                   
092100 01  XDCA-WDK6-2-PCB             PIC X.                                   
092200 01  XDCA-WDK7-2-PCB             PIC X.                                   
092300 01  XDCA-WDK7-3-PCB             PIC X.                                   
092400     EJECT                                                                
092500 PROCEDURE DIVISION  USING MSG-PCB DISP-PCB 4298-PCB                      
092600      4299-PCB AVSR-LIST-PCB 2109-PCB 2191-PCB PRQRY-PCB                  
092700      DLEV-PCB USEA-PCB                                                   
092800      ORQF-PCB ORQI-PCB ORQM-PCB ARTM-PCB 4541-PCB WDK7-PCB               
092900      WDK6-PCB WDB2-PCB WDB1-PCB WDB6-PCB                                 
093000      WDA5-PCB XXJN-PCB FILA-PCB WDB3-PCB 4251-PCB WDR6-PCB               
093100      PRIS-ARTC-PCB PRIS-WDK7-PCB                                         
093200      PRIS-GMTA-PCB PRIS-BETA-PCB                                         
093300      PRIS-GPRIA-PCB PRIS-GPRIB-PCB                                       
093400      PRIS-COST-WDK6-PCB                                                  
093500      PRIS-COST-WDK7-PCB                                                  
093600      PRIS-COST-WDF1-PCB                                                  
093700      PRIS-COST-9305-PCB                                                  
093800      PRIS-COST-WDK72-PCB                                                 
093900      PRIS-COST-WDB6-PCB                                                  
094000      PRNO-3107-PCB                                                       
094100      PRQU-WDG2-PCB                                                       
094200      PRQU-WDC7-PCB                                                       
094300      PRQU-SJKO-WDK6-PCB                                                  
094400      AREG-WDK6-PCB                                                       
094500      AREG-WDK7-PCB                                                       
094600      ARTM-ARTM-PCB                                                       
094700      DLEV-LEVF-PCB                                                       
094800      DLEV-LEVG-PCB                                                       
094900      DLEV-LEVA-PCB                                                       
095000      DLEV-ARTS-PCB                                                       
095100      DLEV-WDB6-PCB                                                       
095200      SPAR-WDF8-PCB                                                       
095300      SPAR-WDF8A-PCB                                                      
095400      SPAR-WDK6-PCB                                                       
095500      DNOT-ORQP-PCB                                                       
095600      DNOT-ORQP2-PCB                                                      
095700      DNOT-ORQP3-PCB                                                      
095800      DNOT-4013-PCB                                                       
095900      DNOT-BENA-PCB                                                       
096000      KAMP-ORDP-PCB KAMP-ZZAC-PCB KAMP-WDM2-PCB                           
096100      KERS-ARTC-PCB KERS-ERSA-PCB                                         
096200      NDCA-USEA-PCB NDCA-WDK7-PCB NDCA-WDL6-PCB NDCA-WDB6-PCB             
096300      SDCA-ARTS-PCB SDCA-WDB6-PCB SDCA-WDK9-PCB SDCA-WDR6-PCB             
096400      SDCA-WDK6-PCB SDCA-WDQ4B-PCB SDCA-WDQ2-PCB SDCA-WDQ4-PCB            
096500      SDCA-WDB6-2-PCB SDCA-WDK6-2-PCB SDCA-WDK7-2-PCB                     
096600      SDCA-WDK7-3-PCB                                                     
096700      CDCA-ARTM-PCB CDCA-INLB-PCB CDCA-WDB2-PCB CDCA-WDC1-PCB             
096800      RANS-XXKM-PCB RANS-ARTM-PCB RANS-ARTS-PCB                           
096900      TPO1-ORDP-PCB TPO1-ARTM-PCB TPO1-ZZAC-PCB                           
097000      TPO2-ORDP-PCB TPO2-XXBU-PCB TPO2-XXBV-PCB                           
097100      TPO2-ARTM-PCB TPO2-FILA-PCB TPO2-XXBX-PCB                           
097200      RELS-ORDP-PCB RELS-FILA-PCB RELS-ARTM-PCB                           
097300      TPO3-ORDP-PCB TPO3-ZZAC-PCB                                         
097400      TPO5-ORDP-PCB TPO5-ARTM-PCB                                         
097500      TPO6-ORDP-PCB TPO6-XXBU-PCB TPO6-XXBV-PCB                           
097600      TPO6-XXBX-PCB TPO6-ARTS-PCB                                         
097700      TIME-4437-PCB                                                       
097800      AVSR-ORQI-PCB AVSR-GMTB-PCB AVSR-GMTC-PCB                           
097900      AVSR-WDB2-PCB AVSR-WDB6-PCB                                         
098000      TRAN-XXKB-PCB                                                       
098100      KVAN-WDB2-PCB                                                       
098200      KVAN-WDC1-PCB                                                       
098300      XDCA-USEA-PCB                                                       
098400        XDCA-WDB6-PCB XDCA-WDK6-PCB XDCA-WDK7-PCB                         
098500        XDCA-WDK9-PCB XDCA-WDL6-PCB XDCA-WDQ4B-PCB                        
098600        XDCA-WDQ2-PCB XDCA-WDQ4-PCB XDCA-WDR6-PCB                         
098700        XDCA-WDB6-2-PCB XDCA-WDK6-2-PCB XDCA-WDK7-2-PCB                   
098800        XDCA-WDK7-3-PCB.                                                  
098900                                                                          
099000     EJECT                                                                
099100                                                                          
099200     ENTRY 'DLITCBL' USING MSG-PCB DISP-PCB 4298-PCB                      
099300      4299-PCB AVSR-LIST-PCB 2109-PCB 2191-PCB PRQRY-PCB                  
099400      DLEV-PCB USEA-PCB                                                   
099500      ORQF-PCB ORQI-PCB ORQM-PCB ARTM-PCB 4541-PCB WDK7-PCB               
099600      WDK6-PCB WDB2-PCB WDB1-PCB WDB6-PCB                                 
099700      WDA5-PCB XXJN-PCB FILA-PCB WDB3-PCB 4251-PCB WDR6-PCB               
099800      PRIS-ARTC-PCB PRIS-WDK7-PCB                                         
099900      PRIS-GMTA-PCB PRIS-BETA-PCB                                         
100000      PRIS-GPRIA-PCB PRIS-GPRIB-PCB                                       
100100      PRIS-COST-WDK6-PCB                                                  
100200      PRIS-COST-WDK7-PCB                                                  
100300      PRIS-COST-WDF1-PCB                                                  
100400      PRIS-COST-9305-PCB                                                  
100500      PRIS-COST-WDK72-PCB                                                 
100600      PRIS-COST-WDB6-PCB                                                  
100700      PRNO-3107-PCB                                                       
100800      PRQU-WDG2-PCB                                                       
100900      PRQU-WDC7-PCB                                                       
101000      PRQU-SJKO-WDK6-PCB                                                  
101100      AREG-WDK6-PCB                                                       
101200      AREG-WDK7-PCB                                                       
101300      ARTM-ARTM-PCB                                                       
101400      DLEV-LEVF-PCB                                                       
101500      DLEV-LEVG-PCB                                                       
101600      DLEV-LEVA-PCB                                                       
101700      DLEV-ARTS-PCB                                                       
101800      DLEV-WDB6-PCB                                                       
101900      SPAR-WDF8-PCB                                                       
102000      SPAR-WDF8A-PCB                                                      
102100      SPAR-WDK6-PCB                                                       
102200      DNOT-ORQP-PCB                                                       
102300      DNOT-ORQP2-PCB                                                      
102400      DNOT-ORQP3-PCB                                                      
102500      DNOT-4013-PCB                                                       
102600      DNOT-BENA-PCB                                                       
102700      KAMP-ORDP-PCB KAMP-ZZAC-PCB KAMP-WDM2-PCB                           
102800      KERS-ARTC-PCB KERS-ERSA-PCB                                         
102900      NDCA-USEA-PCB NDCA-WDK7-PCB NDCA-WDL6-PCB NDCA-WDB6-PCB             
103000      SDCA-ARTS-PCB SDCA-WDB6-PCB SDCA-WDK9-PCB SDCA-WDR6-PCB             
103100      SDCA-WDK6-PCB SDCA-WDQ4B-PCB SDCA-WDQ2-PCB SDCA-WDQ4-PCB            
103200      SDCA-WDB6-2-PCB SDCA-WDK6-2-PCB SDCA-WDK7-2-PCB                     
103300      SDCA-WDK7-3-PCB                                                     
103400      CDCA-ARTM-PCB CDCA-INLB-PCB CDCA-WDB2-PCB CDCA-WDC1-PCB             
103500      RANS-XXKM-PCB RANS-ARTM-PCB RANS-ARTS-PCB                           
103600      TPO1-ORDP-PCB TPO1-ARTM-PCB TPO1-ZZAC-PCB                           
103700      TPO2-ORDP-PCB TPO2-XXBU-PCB TPO2-XXBV-PCB                           
103800      TPO2-ARTM-PCB TPO2-FILA-PCB TPO2-XXBX-PCB                           
103900      RELS-ORDP-PCB RELS-FILA-PCB RELS-ARTM-PCB                           
104000      TPO3-ORDP-PCB TPO3-ZZAC-PCB                                         
104100      TPO5-ORDP-PCB TPO5-ARTM-PCB                                         
104200      TPO6-ORDP-PCB TPO6-XXBU-PCB TPO6-XXBV-PCB                           
104300      TPO6-XXBX-PCB TPO6-ARTS-PCB                                         
104400      TIME-4437-PCB                                                       
104500      AVSR-ORQI-PCB AVSR-GMTB-PCB AVSR-GMTC-PCB                           
104600      AVSR-WDB2-PCB AVSR-WDB6-PCB                                         
104700      TRAN-XXKB-PCB                                                       
104800      KVAN-WDB2-PCB                                                       
104900      KVAN-WDC1-PCB                                                       
105000      XDCA-USEA-PCB                                                       
105100        XDCA-WDB6-PCB XDCA-WDK6-PCB XDCA-WDK7-PCB                         
105200        XDCA-WDK9-PCB XDCA-WDL6-PCB XDCA-WDQ4B-PCB                        
105300        XDCA-WDQ2-PCB XDCA-WDQ4-PCB XDCA-WDR6-PCB                         
105400        XDCA-WDB6-2-PCB XDCA-WDK6-2-PCB XDCA-WDK7-2-PCB                   
105500        XDCA-WDK7-3-PCB.                                                  
105600     EJECT                                                                
105700                                                                          
105800     PERFORM IMS-GET-MSG                                                  
105900                                                                          
106000     IF SEGMENT-FINNS                                                     
106100        PERFORM IMS-GN-MSG                                                
106200     END-IF                                                               
106300                                                                          
106400     IF SEGMENT-FINNS                                                     
106500        PERFORM A-INIT                                                    
106600                                                                          
106700        PERFORM B-KONTROLL-ATT-ORDER-FINNS                                
106800                                                                          
106900        IF ALLT-OK                                                        
107000           PERFORM C-FORMELL-KONTROLL                                     
107100                                                                          
107200           IF ALLT-OK                                                     
107300              PERFORM E-BEHANDLA-RADER                                    
107400                                                                          
107500              IF MID-FLSLUT = 'J'                                         
107600                 IF DIST79-DEALER-PRICE AND WS-IDPRQUES NOT = +0          
107700                    PERFORM G-SKICKA-PRISFRAGA                            
107800                 END-IF                                                   
107900                 PERFORM F-BIPACKNING-ORDERAVSLUT                         
108000                 IF MID-IDKUNDRF-RO NOT = SPACE                           
108100                   PERFORM S30-STARTA-MPP-NY                              
108200                 END-IF                                                   
108300              ELSE                                                        
108400                 IF DIST79-DEALER-PRICE AND WS-IDPRQUES NOT = +0          
108500                    PERFORM G-SKICKA-PRISFRAGA                            
108600                 END-IF                                                   
108700              END-IF                                                      
108800           END-IF                                                         
108900        END-IF                                                            
109000                                                                          
109100        PERFORM Z-FINIT                                                   
109200     END-IF                                                               
109300     MOVE +0 TO RETURN-CODE                                               
109400     GOBACK                                                               
109500     .                                                                    
109600     EJECT                                                                
109700 A-INIT SECTION.                                                          
109800                                                                          
109900     MOVE JA                   TO ALLT-SW                                 
110000                                                                          
110100     MOVE MSG-INDATA-MINUS-1-TRANSKOD                                     
110200                               TO MID-AREA                                
110300     MOVE SPACE                TO MSG-KOM-IDMFSMED                        
110400                                                                          
110500     ACCEPT DAGENS-DATUM FROM DATE                                        
110600                                                                          
110700     PERFORM AA-NOLLA-WOPS-TABELL                                         
110800                                                                          
110900     MOVE SPACE                TO 2191-MID-W2I19101                       
111000     MOVE NEJ                  TO BEVARREF-I-HFAK-TAB-SW                  
111100     .                                                                    
111200     EJECT                                                                
111300                                                                          
111400 AA-NOLLA-WOPS-TABELL SECTION.                                            
111500                                                                          
111600     MOVE +1                   TO WS-INDEX-WOPS                           
111700     PERFORM UNTIL WS-INDEX-WOPS > WS-INDEX-WOPS-MAX                      
111800        MOVE +0            TO AVSR-ADLAGOMR(WS-INDEX-WOPS)                
111900        MOVE SPACE         TO AVSR-IDLEVNR(WS-INDEX-WOPS)                 
112000        MOVE SPACE         TO AVSR-IDDC(WS-INDEX-WOPS)                    
112100        MOVE +0            TO AVSR-KDSPEEMB(WS-INDEX-WOPS)                
112200        MOVE +0            TO AVSR-KVANNANT(WS-INDEX-WOPS)                
112300        MOVE +0            TO AVSR-KVBEART-Q(WS-INDEX-WOPS)               
112400        MOVE +0            TO AVSR-PRARTNTO(WS-INDEX-WOPS)                
112500        MOVE +0            TO AVSR-PRAVCOST(WS-INDEX-WOPS)                
112600        INITIALIZE         AVSR-DEAL-PR-LINE(WS-INDEX-WOPS)               
112700        MOVE +0            TO AVSR-VKART(WS-INDEX-WOPS)                   
112800        MOVE +0            TO AVSR-VLARTNTO(WS-INDEX-WOPS)                
112900        MOVE SPACE         TO AVSR-KDORDSTA(WS-INDEX-WOPS)                
113000        MOVE +0            TO AVSR-KDVIA   (WS-INDEX-WOPS)                
113100        MOVE +0            TO AVSR-KVDAGAR-DIFF(WS-INDEX-WOPS)            
113200        MOVE +0            TO AVSR-TISKEPPN-DDC(WS-INDEX-WOPS)            
113300        MOVE +0            TO AVSR-KDVSOP(WS-INDEX-WOPS)                  
113400                              AVSR-KDFARLIG(WS-INDEX-WOPS)                
113500        ADD +1             TO WS-INDEX-WOPS                               
113600     END-PERFORM                                                          
113700                                                                          
113800     MOVE +1                   TO WS-INDEX-WOPS                           
113900     .                                                                    
114000     EJECT                                                                
114100 B-KONTROLL-ATT-ORDER-FINNS SECTION.                                      
114200     MOVE 'STA B-KONTROLL'        TO   WS-PGM-POSITION                    
114300     MOVE MID-AREA                TO SPAR-MID-AREA                        
114400     IF MID-IDDISTR NUMERIC AND MID-IDDISTR > ZERO                        
114500        MOVE MID-IDDISTR          TO W-IDDISTR                            
114600                                     TEST-IDDISTR                         
114700     ELSE                                                                 
114800        MOVE NEJ                  TO ALLT-SW                              
114900     END-IF                                                               
115000     IF MID-IDKUNDNR = SPACE                                              
115100        MOVE ZERO                 TO W-IDKUNDNR                           
115200     ELSE                                                                 
115300        IF MID-IDKUNDNR NUMERIC                                           
115400           MOVE MID-IDKUNDNR      TO W-IDKUNDNR                           
115500        ELSE                                                              
115600           MOVE NEJ               TO ALLT-SW                              
115700        END-IF                                                            
115800     END-IF                                                               
115900                                                                          
116000     IF MID-IDORDNR NUMERIC AND MID-IDORDNR > ZERO                        
116100        MOVE MID-IDORDNR          TO W-IDKUNDRF                           
116200     ELSE                                                                 
116300        MOVE NEJ                  TO ALLT-SW                              
116400     END-IF                                                               
116500                                                                          
116600     IF NOT ALLT-OK                                                       
116700        MOVE ERR-WRONG-KEY        TO MSG-KOM-IDMFSMED                     
116800        MOVE '4'                  TO MSG-KOM-KDSVAR                       
116900     ELSE                                                                 
117000        PERFORM IMS-01-GHU-ORQI-WDQ201                                    
117100        IF SEGMENT-FINNS                                                  
117200           PERFORM BA-HAMTA-KUND                                          
117300           PERFORM BB-HAMTA-WDB6-INFO                                     
117400           IF OHUV-FLKLAR = JA                                            
117500              MOVE ERR-ORDER-AVSLUTAD TO MSG-KOM-IDMFSMED                 
117600              MOVE '4'                TO MSG-KOM-KDSVAR                   
117700              MOVE NEJ                TO ALLT-SW                          
117800           ELSE                                                           
117900              PERFORM IMS-03-GNP-ORQI-WDQ212                              
118000           END-IF                                                         
118100           PERFORM BC-FIXA-LOKAL-TID                                      
118200*          PERFORM BC-FIXA-IDSYSTEM                                       
118300        ELSE                                                              
118400           MOVE ERR-ORDER-SAKNAS     TO MSG-KOM-IDMFSMED                  
118500           MOVE '4'                  TO MSG-KOM-KDSVAR                    
118600           MOVE NEJ                  TO ALLT-SW                           
118700        END-IF                                                            
118800     END-IF                                                               
118900                                                                          
119000     IF ALLT-OK                                                           
119100        IF MID-IDKUNDRF-RO NOT = SPACE                                    
119200           MOVE MID-IDKUNDRF-RO TO W-IDKUNDRF                             
119300           PERFORM IMS-02-GU-ORQI-WDQ201-EMPTY                            
119400           MOVE MID-IDORDNR     TO W-IDKUNDRF                             
119500        END-IF                                                            
119600     END-IF                                                               
119700     .                                                                    
119800     EJECT                                                                
119900                                                                          
120000 BA-HAMTA-KUND      SECTION.                                              
120100                                                                          
120200     MOVE W-IDDISTR            TO W-IDDISTR-WDB2                          
120300     MOVE W-IDKUNDNR           TO W-IDKUNDNR-WDB2                         
120400     PERFORM IMS-GU-WDB201                                                
120500                                                                          
120600     IF OHUV-KDORDKL > 1                                                  
120700                                                                          
120800        MOVE +1 TO WS-INDEX                                               
120900        PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                           
121000           MOVE GMT-IDDC-BULK(WS-INDEX) TO                                
121100                                  W-GMT-IDDC-CLEAR  (WS-INDEX)            
121200           ADD +1 TO WS-INDEX                                             
121300        END-PERFORM                                                       
121400                                                                          
121500     ELSE                                                                 
121600       IF OHUV-KDORDKL = 1                                                
121700                                                                          
121800          MOVE +1 TO WS-INDEX                                             
121900          PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                         
122000             MOVE GMT-IDDC-DAY(WS-INDEX) TO                               
122100                                  W-GMT-IDDC-CLEAR  (WS-INDEX)            
122200             ADD +1 TO WS-INDEX                                           
122300          END-PERFORM                                                     
122400                                                                          
122500       ELSE                                                               
122600         IF OHUV-KDORDKL = 0                                              
122700                                                                          
122800            MOVE +1 TO WS-INDEX                                           
122900            PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                       
123000               MOVE GMT-IDDC-VOR(WS-INDEX) TO                             
123100                                  W-GMT-IDDC-CLEAR  (WS-INDEX)            
123200               ADD +1 TO WS-INDEX                                         
123300            END-PERFORM                                                   
123400                                                                          
123500         END-IF                                                           
123600       END-IF                                                             
123700     END-IF                                                               
123800     .                                                                    
123900     EJECT                                                                
124000 BB-HAMTA-WDB6-INFO SECTION.                                              
124100                                                                          
124200     MOVE SPACE                TO CLDC-W411CLDC                           
124300     MOVE W-GMT-IDDC-CLEAR-GRP TO CLDC-IDDC-CLEAR-GRP                     
124400                                                                          
124500     CALL W411CLDC USING CLDC-W411CLDC WDB6-PCB                           
124600     .                                                                    
124700     EJECT                                                                
124800 BC-FIXA-LOKAL-TID SECTION.                                               
124900                                                                          
125000     MOVE ALL '+'              TO MSGI-WMSGINIT                           
125100     MOVE '013'                TO MSGI-KDCALL                             
125200     MOVE 'WIDDC   '           TO MSGI-IDUSER                             
125300     IF OHUV-IDDC-TVS = SPACE                                             
125400       MOVE OHUV-IDDC-PRIM     TO MSGI-IDUSER(6:2)                        
125500     ELSE                                                                 
125600       MOVE OHUV-IDDC-TVS      TO MSGI-IDUSER(6:2)                        
125700     END-IF                                                               
125800                                                                          
125900     MOVE '4252'               TO MSGI-IDTRANS                            
126000     MOVE MSG-LTERM-NAME       TO MSGI-IDLTERM-USER                       
126100                                                                          
126200     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
126300     .                                                                    
126400     EJECT                                                                
126500                                                                          
126600*BC-FIXA-IDSYSTEM   SECTION.                                              
126700                                                                          
126800*    JUST A FIX FOR PREPLANNED ORDER UPLOADED FROM WEB                    
126900*    SAME FIX IN W4025100                                                 
127000                                                                          
127100*    IF OHUV-IDSYSTEM = 'LDC ' AND                                        
127200*       OHUV-TIREPDAT > ZERO   AND                                        
127300*        MID-IDSYSTEM = 'XCEL'                                            
127400*       MOVE 'LDC ' TO MID-IDSYSTEM                                       
127500*    END-IF                                                               
127600*    .                                                                    
127700     EJECT                                                                
127800 C-FORMELL-KONTROLL SECTION.                                              
127900                                                                          
128000     MOVE 'STA C-FORMELL '                TO   WS-PGM-POSITION            
128100     MOVE MID-IDSYSTEM               TO ORFK-IDSYSTEM                     
128200     IF OHUV-IDDC-TVS = SPACE                                             
128300       MOVE OHUV-IDDC-PRIM           TO ORFK-IDDC                         
128400     ELSE                                                                 
128500       MOVE OHUV-IDDC-TVS            TO ORFK-IDDC                         
128600     END-IF                                                               
128700     MOVE OHUV-IDDISTR               TO ORFK-IDDISTR                      
128800     MOVE OHUV-IDKUNDNR              TO ORFK-IDKUNDNR                     
128900     MOVE SPACE                      TO ORFK-IDUSER                       
129000     MOVE OHUV-KDFAKTYP              TO ORFK-KDFAKTYP                     
129100     MOVE OHUV-KDORDKL               TO ORFK-KDORDKL                      
129200     MOVE OHUV-KDTPOTYP              TO ORFK-KDTPOTYP                     
129300     MOVE OHUV-FLFORBI               TO ORFK-FLFORBI                      
129400     MOVE OHUV-FLORDSPE              TO ORFK-FLORDSPE                     
129500     MOVE OHUV-FLOVRLEV              TO ORFK-FLOVRLEV                     
129600     MOVE OHUV-FLEMBORD              TO ORFK-FLEMBORD                     
129700     MOVE OHUV-IDFTG                 TO ORFK-IDFTG                        
129800                                                                          
129900     PERFORM CB-CHECK-PRE-PLANNED                                         
130000                                                                          
130100     MOVE +1                   TO WS-INDEX-MID                            
130200     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX                        
130300                                                                          
130400        IF MID-IDARTNR (WS-INDEX-MID) = SPACE                             
130500           MOVE ALL '+'        TO ORFK-IDARTNR-IN(WS-INDEX-MID)           
130600                                  MID-IDARTNR (WS-INDEX-MID)              
130700        ELSE                                                              
130800                                                                          
130900           MOVE MID-IDARTNR(WS-INDEX-MID)                                 
131000                               TO WS-IDARTNR-1-9                          
131100           MOVE MID-REKSIFFR(WS-INDEX-MID)                                
131200                               TO WS-IDARTNR-11                           
131300           MOVE WS-IDARTNR     TO ORFK-IDARTNR-IN(WS-INDEX-MID)           
131400                                                                          
131500           IF MID-FLRESTN(WS-INDEX-MID) = SPACE                           
131600              MOVE ALL '+'     TO MID-FLRESTN(WS-INDEX-MID)               
131700              MOVE OHUV-FLRESTN TO ORFK-FLRESTN(WS-INDEX-MID)             
131800           ELSE                                                           
131900              IF MID-FLRESTN(WS-INDEX-MID) = 'Y'                          
132000                 MOVE JA       TO MID-FLRESTN(WS-INDEX-MID)               
132100              END-IF                                                      
132200              MOVE MID-FLRESTN(WS-INDEX-MID)                              
132300                               TO ORFK-FLRESTN(WS-INDEX-MID)              
132400           END-IF                                                         
132500                                                                          
132600           IF MID-IDKONTO(WS-INDEX-MID) = SPACE                           
132700              MOVE ALL '+'     TO MID-IDKONTO(WS-INDEX-MID)               
132800              MOVE ZERO        TO ORFK-IDKONTO(WS-INDEX-MID)              
132900           ELSE                                                           
133000              MOVE ZERO        TO MID-IDKONTO(WS-INDEX-MID)               
133100                                  ORFK-IDKONTO(WS-INDEX-MID)              
133200           END-IF                                                         
133300                                                                          
133400           IF MID-IDKST(WS-INDEX-MID) = SPACE                             
133500              MOVE ALL '+'     TO MID-IDKST(WS-INDEX-MID)                 
133600              MOVE SPACE       TO ORFK-IDKST(WS-INDEX-MID)                
133700           ELSE                                                           
133800              MOVE SPACE       TO MID-IDKST(WS-INDEX-MID)                 
133900                                  ORFK-IDKST(WS-INDEX-MID)                
134000           END-IF                                                         
134100                                                                          
134200           IF MID-KDVRINFO(WS-INDEX-MID) = SPACE                          
134300              MOVE ALL '+'     TO MID-KDVRINFO(WS-INDEX-MID)              
134400              MOVE OHUV-KDVRINFO TO ORFK-KDVRINFO(WS-INDEX-MID)           
134500           ELSE                                                           
134600              MOVE MID-KDVRINFO(WS-INDEX-MID)                             
134700                               TO ORFK-KDVRINFO(WS-INDEX-MID)             
134800           END-IF                                                         
134900                                                                          
135000           IF MID-KDKVBRYT(WS-INDEX-MID) = SPACE                          
135100              MOVE ALL '+'     TO MID-KDKVBRYT(WS-INDEX-MID)              
135200           END-IF                                                         
135300           IF MID-KDKVBRYT(WS-INDEX-MID) = '5'                            
135400              MOVE ALL '+'     TO MID-KDKVBRYT(WS-INDEX-MID)              
135500              MOVE JA          TO MID-FLINVEST(WS-INDEX-MID)              
135600           END-IF                                                         
135700           MOVE MID-KDKVBRYT(WS-INDEX-MID)                                
135800                               TO ORFK-KDKVBRYT(WS-INDEX-MID)             
135900                                                                          
136000           IF MID-FLINVEST(WS-INDEX-MID) = SPACE                          
136100              MOVE ALL '+'     TO MID-FLINVEST(WS-INDEX-MID)              
136200              MOVE NEJ         TO ORFK-FLINVEST(WS-INDEX-MID)             
136300           ELSE                                                           
136400              MOVE MID-FLINVEST(WS-INDEX-MID)                             
136500                               TO ORFK-FLINVEST(WS-INDEX-MID)             
136600           END-IF                                                         
136700                                                                          
136800           IF MID-KVBEART (WS-INDEX-MID) = SPACE                          
136900              MOVE ALL '+'     TO MID-KVBEART (WS-INDEX-MID)              
137000           END-IF                                                         
137100           MOVE MID-KVBEART(WS-INDEX-MID)                                 
137200                               TO ORFK-KVBEART(WS-INDEX-MID)              
137300                                                                          
137400           IF MID-PRARTNTO(WS-INDEX-MID) = SPACE                          
137500              MOVE ALL '+'     TO MID-PRARTNTO(WS-INDEX-MID)              
137600           END-IF                                                         
137700           MOVE MID-PRARTNTO(WS-INDEX-MID)                                
137800                               TO ORFK-PRARTNTO(WS-INDEX-MID)             
137900                                                                          
138000           IF MID-TITPO (WS-INDEX-MID) = SPACE OR ZERO                    
138100              MOVE ALL '+'     TO MID-TITPO (WS-INDEX-MID)                
138200           END-IF                                                         
138300           IF MID-IDSYSTEM = 'OREL'                                       
138400              IF MID-TITPO (WS-INDEX-MID) NOT = ALL '+'                   
138500                 MOVE 5        TO ORFK-KDTPOTYP                           
138600              END-IF                                                      
138700           END-IF                                                         
138800           MOVE MID-TITPO(WS-INDEX-MID)                                   
138900                               TO ORFK-TITPO-RAD(WS-INDEX-MID)            
139000                                                                          
139100           IF MID-FLSLATT (WS-INDEX-MID) = SPACE                          
139200              MOVE JA          TO MID-FLSLATT (WS-INDEX-MID)              
139300           ELSE                                                           
139400              IF MID-FLSLATT (WS-INDEX-MID) = YES                         
139500                 MOVE JA       TO MID-FLSLATT (WS-INDEX-MID)              
139600              END-IF                                                      
139700           END-IF                                                         
139800           MOVE MID-FLSLATT(WS-INDEX-MID)                                 
139900                               TO ORFK-FLSLATT (WS-INDEX-MID)             
140000                                                                          
140100           MOVE +0        TO ORFK-PRARTNTO-LOCPREL(WS-INDEX-MID)          
140200                                                                          
140300           IF MID-PRARTNTO-LOC(WS-INDEX-MID) = SPACE                      
140400              MOVE ALL '+' TO MID-PRARTNTO-LOC(WS-INDEX-MID)              
140500           END-IF                                                         
140600           MOVE MID-PRARTNTO-LOC(WS-INDEX-MID)                            
140700                           TO ORFK-PRARTNTO-LOC(WS-INDEX-MID)             
140800                                                                          
140900           IF MID-PRARTBTO-LOC(WS-INDEX-MID) = SPACE                      
141000              MOVE ALL '+' TO MID-PRARTBTO-LOC(WS-INDEX-MID)              
141100           END-IF                                                         
141200           MOVE MID-PRARTBTO-LOC(WS-INDEX-MID)                            
141300                           TO ORFK-PRARTBTO-LOC(WS-INDEX-MID)             
141400                                                                          
141500*          IF MID-RERAB(WS-INDEX-MID) = SPACE                             
141600*             MOVE ALL '+'     TO MID-RERAB(WS-INDEX-MID)                 
141700*          END-IF                                                         
141800*          MOVE MID-RERAB(WS-INDEX-MID)                                   
141900*                              TO ORFK-RERAB(WS-INDEX-MID)                
142000                                                                          
142100        END-IF                                                            
142200                                                                          
142300        ADD +1                 TO WS-INDEX-MID                            
142400     END-PERFORM                                                          
142500                                                                          
142600     MOVE ALL '+'              TO ORFK-IDARTNR-IN(6)                      
142700                                  ORFK-IDARTNR-IN(7)                      
142800                                  ORFK-IDARTNR-IN(8)                      
142900                                  ORFK-IDARTNR-IN(9)                      
143000                                  ORFK-IDARTNR-IN(+10)                    
143100                                  ORFK-IDARTNR-IN(+11)                    
143200                                  ORFK-IDARTNR-IN(+12)                    
143300                                  ORFK-IDARTNR-IN(+13)                    
143400                                  ORFK-IDARTNR-IN(+14)                    
143500                                                                          
143600     CALL W411ORFK USING ORFK-W411ORFK                                    
143700                         AREG-WDK6-PCB                                    
143800                         AREG-WDK7-PCB                                    
143900                                                                          
144000     MOVE +1                   TO WS-INDEX-MID                            
144100     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX                        
144200        PERFORM CA-KOLLA-FEL-FK                                           
144300        ADD +1                 TO WS-INDEX-MID                            
144400     END-PERFORM                                                          
144500     .                                                                    
144600     EJECT                                                                
144700                                                                          
144800 CA-KOLLA-FEL-FK SECTION.                                                 
144900                                                                          
145000     MOVE 'STA CA-KOLLA  '                TO   WS-PGM-POSITION            
145100     IF ORFK-FLINVEST-OK(WS-INDEX-MID) = NEJ                              
145200        MOVE 58                  TO ORFK-KDORDBEK(WS-INDEX-MID)           
145300     END-IF                                                               
145400                                                                          
145500     IF ORFK-FLRESTN-OK(WS-INDEX-MID) = NEJ                               
145600        MOVE 58                  TO ORFK-KDORDBEK(WS-INDEX-MID)           
145700     END-IF                                                               
145800                                                                          
145900     IF ORFK-IDARTNR-OK(WS-INDEX-MID) = NEJ                               
146000        IF ORFK-IDARTNR-IN(WS-INDEX-MID) NOT NUMERIC                      
146100           MOVE ZERO             TO ORFK-IDARTNR(WS-INDEX-MID)            
146200        END-IF                                                            
146300        IF ORFK-KDORDBEK (WS-INDEX-MID) = ZERO                            
146400           MOVE 58               TO ORFK-KDORDBEK(WS-INDEX-MID)           
146500        END-IF                                                            
146600     END-IF                                                               
146700                                                                          
146800     IF ORFK-IDKONTO-OK(WS-INDEX-MID) = NEJ                               
146900       MOVE ZERO                 TO ORFK-IDKONTO(WS-INDEX-MID)            
147000     END-IF                                                               
147100                                                                          
147200     IF ORFK-IDKST-OK(WS-INDEX-MID) = NEJ                                 
147300       MOVE SPACE               TO ORFK-IDKST(WS-INDEX-MID)               
147400     END-IF                                                               
147500                                                                          
147600     IF ORFK-KDKVBRYT-OK(WS-INDEX-MID) = NEJ                              
147700        IF ORFK-KDKVBRYT(WS-INDEX-MID) NOT NUMERIC                        
147800           MOVE ZERO             TO ORFK-KDKVBRYT(WS-INDEX-MID)           
147900        END-IF                                                            
148000        MOVE 58                  TO ORFK-KDORDBEK(WS-INDEX-MID)           
148100     END-IF                                                               
148200                                                                          
148300     IF ORFK-KDVRINFO-OK(WS-INDEX-MID) = NEJ                              
148400        IF ORFK-KDVRINFO(WS-INDEX-MID) NOT NUMERIC                        
148500           MOVE ZERO             TO ORFK-KDVRINFO(WS-INDEX-MID)           
148600        END-IF                                                            
148700        MOVE 58                  TO ORFK-KDORDBEK(WS-INDEX-MID)           
148800     END-IF                                                               
148900                                                                          
149000     IF ORFK-KVBEART-OK(WS-INDEX-MID) = NEJ                               
149100        IF ORFK-KVBEART(WS-INDEX-MID) NOT NUMERIC                         
149200           MOVE ZERO             TO ORFK-KVBEART(WS-INDEX-MID)            
149300        END-IF                                                            
149400        MOVE 58                  TO ORFK-KDORDBEK(WS-INDEX-MID)           
149500     END-IF                                                               
149600                                                                          
149700     IF ORFK-PRARTNTO-OK(WS-INDEX-MID) = NEJ                              
149800        IF ORFK-PRARTNTO(WS-INDEX-MID) NOT NUMERIC                        
149900           MOVE ZERO             TO ORFK-PRARTNTO-UT(WS-INDEX-MID)        
150000        END-IF                                                            
150100        MOVE 58                  TO ORFK-KDORDBEK(WS-INDEX-MID)           
150200     END-IF                                                               
150300                                                                          
150400     IF ORFK-PRARTNTO-LOC-OK(WS-INDEX-MID) = NEJ                          
150500        IF ORFK-PRARTNTO-LOC(WS-INDEX-MID) NOT NUMERIC                    
150600           MOVE ZERO        TO ORFK-PRARTNTO-LOC-UT(WS-INDEX-MID)         
150700        END-IF                                                            
150800        MOVE 58                  TO ORFK-KDORDBEK(WS-INDEX-MID)           
150900     END-IF                                                               
151000                                                                          
151100     IF ORFK-PRARTNTO-LOCPREL-OK(WS-INDEX-MID) = NEJ                      
151200        IF ORFK-PRARTNTO-LOCPREL(WS-INDEX-MID) NOT NUMERIC                
151300           MOVE ZERO   TO ORFK-PRARTNTO-LOCPREL-UT(WS-INDEX-MID)          
151400        END-IF                                                            
151500        MOVE 58                  TO ORFK-KDORDBEK(WS-INDEX-MID)           
151600     END-IF                                                               
151700                                                                          
151800     IF ORFK-PRARTBTO-LOC-OK(WS-INDEX-MID) = NEJ                          
151900        IF ORFK-PRARTBTO-LOC(WS-INDEX-MID) NOT NUMERIC                    
152000           MOVE ZERO        TO ORFK-PRARTBTO-LOC-UT(WS-INDEX-MID)         
152100        END-IF                                                            
152200        MOVE 58                  TO ORFK-KDORDBEK(WS-INDEX-MID)           
152300     END-IF                                                               
152400                                                                          
152500*    IF ORFK-RERAB-OK(WS-INDEX-MID) = NEJ                                 
152600*       IF ORFK-RERAB(WS-INDEX-MID) NOT NUMERIC                           
152700*          MOVE ZERO        TO ORFK-RERAB-UT(WS-INDEX-MID)                
152800*       END-IF                                                            
152900*       MOVE 58                  TO ORFK-KDORDBEK(WS-INDEX-MID)           
153000*    END-IF                                                               
153100                                                                          
153200     IF ORFK-TITPO-OK(WS-INDEX-MID) = NEJ                                 
153300        IF ORFK-TITPO-RAD(WS-INDEX-MID) NOT NUMERIC                       
153400           MOVE ZERO             TO ORFK-TITPO-RAD(WS-INDEX-MID)          
153500        END-IF                                                            
153600        MOVE 58                  TO ORFK-KDORDBEK(WS-INDEX-MID)           
153700     END-IF                                                               
153800                                                                          
153900     IF MID-KDDSP (WS-INDEX-MID) = SPACE                                  
154000        MOVE ZERO              TO MID-KDDSP(WS-INDEX-MID)                 
154100     ELSE                                                                 
154200        IF MID-KDDSP (WS-INDEX-MID) = '1' OR '2'                          
154300           CONTINUE                                                       
154400        ELSE                                                              
154500           MOVE ZERO             TO MID-KDDSP(WS-INDEX-MID)               
154600           MOVE 58               TO ORFK-KDORDBEK(WS-INDEX-MID)           
154700        END-IF                                                            
154800     END-IF                                                               
154900                                                                          
155000     IF ORFK-FLSLATT-OK(WS-INDEX-MID) = NEJ                               
155100        MOVE 58                  TO ORFK-KDORDBEK(WS-INDEX-MID)           
155200     END-IF                                                               
155300                                                                          
155400     IF ORFK-KDORDBEK(WS-INDEX-MID) = 56                                  
155500       CONTINUE                                                           
155600     ELSE                                                                 
155700       IF ORFK-KDORDBEK(WS-INDEX-MID) > ZERO                              
155800          MOVE SPACE               TO ORFK-IDLEVNR(WS-INDEX-MID)          
155900          MOVE ZERO                TO ORFK-KDFARLIG(WS-INDEX-MID)         
156000                                      ORFK-KDPRODSL(WS-INDEX-MID)         
156100                                      ORFK-REKSIFFR(WS-INDEX-MID)         
156200                                      ORFK-VKART(WS-INDEX-MID)            
156300                                      ORFK-VLARTNTO(WS-INDEX-MID)         
156400                                      ORFK-KDERS(WS-INDEX-MID)            
156500                                      ORFK-TIDISPIN(WS-INDEX-MID)         
156600                                      ORFK-KVQPACK-0(WS-INDEX-MID)        
156700                                      ORFK-KVQPACK-1(WS-INDEX-MID)        
156800       END-IF                                                             
156900     END-IF                                                               
157000     .                                                                    
157100     EJECT                                                                
157200                                                                          
157300 CB-CHECK-PRE-PLANNED SECTION.                                            
157400                                                                          
157500     MOVE NEJ TO PREPLANED-SW                                             
157600                                                                          
157700     IF OHUV-TIREPDAT > ZERO                                              
157800         MOVE JA          TO PREPLANED-SW                                 
157900     END-IF                                                               
158000     .                                                                    
158100     EJECT                                                                
158200                                                                          
158300 E-BEHANDLA-RADER SECTION.                                                
158400                                                                          
158500     MOVE 'STA E-BEHANDLA'                TO   WS-PGM-POSITION            
158600     MOVE +1 TO WS-INDEX-MID                                              
158700     MOVE NEJ                     TO TILLK-SW                             
158800                                     OBKR-SW                              
158900                                     LDC-ARTIKELBYTE-SW                   
159000                                     CDC-MOVE-SW                          
159100     MOVE +0                      TO WS-IDPRQUES                          
159200                                                                          
159300     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX                        
159400        IF MID-IDARTNR(WS-INDEX-MID) NOT = ALL '+'                        
159500           PERFORM S02-RENSA-TILLK-TAB                                    
159600                                                                          
159700             MOVE ORFK-W411AREG-001(WS-INDEX-MID)                         
159800                                  TO AREG-W411AREG-001                    
159900           PERFORM EC-BEHANDLA-RAD                                        
160000           PERFORM S40-HAMTA-WDB6-INFO                                    
160100           IF DCS-NDC-NA                                                  
160200              PERFORM S22-DATA-TILL-DEL-NOTE                              
160300           END-IF                                                         
160400           MOVE JA                TO TILLK-SW                             
160500           MOVE NEJ               TO CDC-MOVE-SW                          
160600           MOVE +1                TO WS-INDEX-TILLK                       
160700           PERFORM UNTIL WS-INDEX-TILLK > WS-INDEX-TILLK-MAX OR           
160800              TILK-IDARTNR(WS-INDEX-TILLK) = ZERO                         
160900              IF TILK-FLTILLK-X(WS-INDEX-TILLK) = JA                      
161000                 PERFORM ED-LAES-TILLK-DATA                               
161100                 PERFORM EC-BEHANDLA-RAD                                  
161200                 IF DCS-NDC-NA                                            
161300                    PERFORM S22-DATA-TILL-DEL-NOTE                        
161400                 END-IF                                                   
161500              END-IF                                                      
161600              ADD +1              TO WS-INDEX-TILLK                       
161700           END-PERFORM                                                    
161800        END-IF                                                            
161900        MOVE NEJ                  TO TILLK-SW                             
162000                                     OBKR-SW                              
162100                                     LDC-ARTIKELBYTE-SW                   
162200                                     CDC-MOVE-SW                          
162300        ADD +1 TO WS-INDEX-MID                                            
162400     END-PERFORM                                                          
162500                                                                          
162600     IF DIST79-DEALER-PRICE AND WS-IDPRQUES NOT = +0                      
162700       MOVE WS-IDPRQUES             TO PRNO-IDPRQUES-IN                   
162800       MOVE +3                      TO PRNO-KDCALL                        
162900                                                                          
163000       CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                    
163100     END-IF                                                               
163200                                                                          
163300     IF AVSR-IDDC(1) NOT = SPACE                                          
163400        CALL W413AVSR USING AVSR-W413AVSR AVSR-LIST-PCB                   
163500          AVSR-ORQI-PCB AVSR-GMTB-PCB AVSR-GMTC-PCB                       
163600          AVSR-WDB2-PCB AVSR-WDB6-PCB                                     
163700          TRAN-XXKB-PCB                                                   
163800                                                                          
163900        PERFORM AA-NOLLA-WOPS-TABELL                                      
164000     END-IF                                                               
164100     MOVE JA                      TO ALLT-SW                              
164200     .                                                                    
164300     EJECT                                                                
164400 EC-BEHANDLA-RAD SECTION.                                                 
164500                                                                          
164600     MOVE 'STA EA-BEHANDLA'                TO   WS-PGM-POSITION           
164700     PERFORM S05-NOLLSTALL-OBKR-KODER                                     
164800     PERFORM ECB-BYGG-UPP-ORDERRAD                                        
164900     PERFORM ECC-LAS-NYA-ARTIKELREG                                       
165000     MOVE ORAD-KVBEART             TO ORAD-KVBEART-Q                      
165100     MOVE ORAD-IDDISTR             TO TEST-IDDISTR                        
165200* TO EXCLUDE DOING QTY ADAPATAION FOR DISCREPANCY, REFILL                 
165300     IF OHUV-FLOVRLEV = JA      OR                                        
165400        DIST35-REFILL           OR                                        
165500        DIST35-REFILL-INOM-NDC  OR                                        
165600        DIST35-NONVCC-REFILL    OR                                        
165700        DIST35-NONVCC-NONVCC-TRANSFER OR                                  
165800        DIST35-NONVCC-VCC-TRANSFER                                        
165900       CONTINUE                                                           
166000     ELSE                                                                 
166100       PERFORM ECF-KOMPLETTERA-KVANT-BRYTES                               
166200     END-IF                                                               
166300     PERFORM ECI-KOMPLETTERA-DIREKTLEVERANS                               
166400     IF  MID-IDKUNDRF-RO NOT = SPACE                                      
166500        AND DLEV-KDORDBEK-UT > ZERO                                       
166600        AND NOT TILLKOMMANDE-RAD                                          
166700*       TILLÄGG + DIRLEVRAD + EJ TILLKOMMANDE                             
166800        PERFORM S27-SLACK-OBKR                                            
166900        PERFORM S28-RAD-TILL-WDR5                                         
167000     ELSE                                                                 
167100                                                                          
167200        IF NOT TILLKOMMANDE-RAD                                           
167300           PERFORM ECK-KOMPLETTERA-ERSATTNING                             
167400        END-IF                                                            
167500                                                                          
167600        PERFORM ECL-KOMPLETTERA-SPARRAR                                   
167700        IF MID-IDSYSTEM(1:3) NOT = 'ECO'                                  
167800          PERFORM ECJ-KOMPLETTERA-PRIS                                    
167900        END-IF                                                            
168000        PERFORM ECM-KOMPLETTERA-TPO1                                      
168100        PERFORM ECN-KOMPLETTERA-TPO2                                      
168200        PERFORM ECO-KOMPLETTERA-TPO3                                      
168300        PERFORM ECP-KOMPLETTERA-KAMPANJER                                 
168400        PERFORM ECT-KOMPLETTERA-RELEASESPARR                              
168500        PERFORM ECX-KOMPLETTERA-PROFORMOR                                 
168600        IF (KOLLA-ERS AND PREPLANED-SW = NEJ)                             
168700        AND (DCS-CDC OR DCS-SDC)                                          
168800           PERFORM ECZ-CHECK-KDERS-IN-DC                                  
168900        END-IF                                                            
169000        PERFORM ECW-PREL-AVBOKNING-SDC1                                   
169100        PERFORM ECH-PREL-AVBOKNING-SDC2                                   
169200        PERFORM ECX-PREL-AVBOKNING-SDC3                                   
169300        PERFORM ECG-PREL-AVBOKNING-NDC                                    
169400                                                                          
169500        IF (DIST18-SKROT                                                  
169600        OR  DIST35-NL-CDC-RETUR-Q                                         
169700        OR  DIST35-ES-CDC-RETUR-Q                                         
169800        OR  DIST35-AT-CDC-RETUR-Q)                                        
169900                                                                          
170000         MOVE ORAD-IDARTNR TO W-IDARTNR                                   
170100         MOVE ORAD-IDDC  TO W-IDDC                                        
170200         PERFORM S40-HAMTA-WDB6-INFO                                      
170300         IF DCS-CDC                                                       
170400           PERFORM IMS-GHU-WDK611                                         
170500           IF (CLAG-KDLEVSP > 19                                          
170600           OR  CLAG-KVSPARR-KVAL > 0)                                     
170700             CONTINUE                                                     
170800           ELSE                                                           
170900             PERFORM ECT-KOMPLETTERA-RANSONERING                          
171000           END-IF                                                         
171100         ELSE                                                             
171200           PERFORM IMS-GHU-WDK711                                         
171300           IF (SLAG-KDLEVSP > 19                                          
171400           OR  SLAG-KVSPARR-KVAL > 0)                                     
171500             CONTINUE                                                     
171600           ELSE                                                           
171700             PERFORM ECT-KOMPLETTERA-RANSONERING                          
171800           END-IF                                                         
171900         END-IF                                                           
172000       ELSE                                                               
172100         PERFORM ECT-KOMPLETTERA-RANSONERING                              
172200       END-IF                                                             
172300       PERFORM ECQ-KOMPLETTERA-STORA-UTTAG                                
172400       PERFORM ECR-PREL-AVBOKNING-CDC                                     
172500                                                                          
172501*IF ORDER COMES FROM W412S5 THEN IT IS AUTORELEASED AND KDSTRAD IN        
172510*WDA5 IS 4 SO WE DONT NEED TO INSERT IN WDA5 HENCE SKIPPING IT            
172511*FROM 4578 , W4122300                                                     
172520                                                                          
172600       IF  MID-IDKUNDRF-RO NOT = SPACE AND                                
172620           MID-IDSYSTEM NOT = 'LDCB'   AND                                
172621           MID-IDSYSTEM NOT = 'LYNB'   AND                                
172622           MID-IDSYSTEM NOT = 'ECOB'   AND                                
172623           MID-IDSYSTEM NOT = 'VOUB'   AND                                
172624           MID-IDSYSTEM NOT = 'TADB'   AND                                
172625           MID-IDSYSTEM NOT = 'ACCB'   AND                                
172626           MID-IDSYSTEM NOT = 'APAB'   AND                                
172627           MID-IDSYSTEM NOT = 'APBB'   AND                                
172628           MID-IDSYSTEM NOT = 'APCB'   AND                                
172629           MID-IDSYSTEM NOT = 'APDB'   AND                                
172630           MID-IDSYSTEM NOT = 'APEB'   AND                                
172631           MID-IDSYSTEM NOT = 'APFB'   AND                                
172632           MID-IDSYSTEM NOT = 'APGB'   AND                                
172633           MID-IDSYSTEM NOT = 'APHB'   AND                                
172634           MID-IDSYSTEM NOT = 'APIB'   AND                                
172635           MID-IDSYSTEM NOT = 'APJB'                                      
172660                                                                          
172700         PERFORM S07-RO-TVINGANDE-TILLAGG                                 
172800         PERFORM S08-OBKR-TVINGANDE-TILLAGG                               
172900       END-IF                                                             
173000                                                                          
173100       IF NOT LDC-ARTIKELBYTE                                             
173200       IF SKRIV-OBKR                                                      
173300         PERFORM ECS-SKRIV-OBKR-OCH-VOR-RAD                               
173400         IF (OHUV-KDORDKL > +0 AND                                        
173500           (ORAD-KVPREAVB > +0 OR ORAD-KVPRERO > +0))                     
173600                    OR                                                    
173700           (OHUV-KDORDKL = +0 AND ORAD-KVPREAVB > +0)                     
173800                                                                          
173900           IF CDCA-KDORDBEK-UT = 92                                       
174000             COMPUTE ORAD-KVBEART = ORAD-KVBEART + ORAD-KVPRERO           
174100             MOVE +0             TO ORAD-KVPRERO                          
174200           END-IF                                                         
174300                                                                          
174400           PERFORM S09-KONTROLLERA-ENHETSLAST                             
174500           PERFORM S10-BERAKNA-WOPS-SKRIV-ORAD                            
174600         END-IF                                                           
174700       ELSE                                                               
174800         IF TPO1-FLKLAR = JA OR TPO2-FLKLAR = JA OR                       
174900            TPO3-FLKLAR = JA OR TPO5-FLKLAR = JA OR                       
175000            KAMP-FLKLAR = JA OR RELS-FLKLAR = JA                          
175100            CONTINUE                                                      
175200         ELSE                                                             
175300           PERFORM S09-KONTROLLERA-ENHETSLAST                             
175400           PERFORM S10-BERAKNA-WOPS-SKRIV-ORAD                            
175500         END-IF                                                           
175600       END-IF                                                             
175700       ELSE                                                               
175800       IF SKRIV-OBKR                                                      
175900         PERFORM ECS-SKRIV-OBKR-OCH-VOR-RAD                               
176000       END-IF                                                             
176100       END-IF                                                             
176200     END-IF                                                               
176300     .                                                                    
176400     EJECT                                                                
176500                                                                          
176600 ECB-BYGG-UPP-ORDERRAD SECTION.                                           
176700                                                                          
176800     MOVE 'STA ECB-BYGG   '                TO   WS-PGM-POSITION           
176900     MOVE OHUV-IDORDER         TO ORAD-IDORDER                            
177000     IF OHUV-IDDC-TVS = SPACE                                             
177100       MOVE OHUV-IDDC-PRIM     TO ORAD-IDDC                               
177200     ELSE                                                                 
177300       MOVE OHUV-IDDC-TVS      TO ORAD-IDDC                               
177400     END-IF                                                               
177500     IF  MID-ADLAGOMR-CD(WS-INDEX-MID) NOT = ALL '+'                      
177600     AND MID-ADLAGOMR-CD(WS-INDEX-MID) > ZERO                             
177700        MOVE MID-ADLAGOMR-CD(WS-INDEX-MID)                                
177800                               TO ORAD-ADLAGOMR                           
177900        MOVE MID-ADGANG-CD(WS-INDEX-MID)                                  
178000                               TO ORAD-ADGANG                             
178100        MOVE MID-ADPLATS-CD(WS-INDEX-MID)                                 
178200                               TO ORAD-ADPLATS                            
178300     ELSE                                                                 
178400        MOVE AREG-ADLAGOMR     TO ORAD-ADLAGOMR                           
178500        MOVE AREG-ADGANG       TO ORAD-ADGANG                             
178600        MOVE AREG-ADPLATS      TO ORAD-ADPLATS                            
178700     END-IF                                                               
178800     IF OHUV-FLFORBI = SPEC-FORBI                                         
178900        MOVE '07'              TO ORAD-ADLAGOMR                           
179000     END-IF                                                               
179100     IF TILLKOMMANDE-RAD                                                  
179200        MOVE AREG-IDARTNR      TO ORAD-IDARTNR                            
179300     ELSE                                                                 
179400        MOVE ORFK-IDARTNR(WS-INDEX-MID)                                   
179500                               TO ORAD-IDARTNR                            
179600     END-IF                                                               
179700     MOVE +1                   TO ORAD-IDLOPNR                            
179800                                                                          
179900     MOVE MID-BEVOLREF         TO ORAD-BEVOLREF                           
180000     IF MID-BERADREF(WS-INDEX-MID) = ALL '+'                              
180100        MOVE SPACE             TO ORAD-BERADREF                           
180200     ELSE                                                                 
180300        MOVE MID-BERADREF(WS-INDEX-MID)                                   
180400                               TO ORAD-BERADREF                           
180500     END-IF                                                               
180600                                                                          
180700* TO HANDLE BEVARREF FROM EXCEL - ORDER UPLOAD                            
180800* TO MAKE IT POSSIBLE TO STEER ORDERS TO PARTICULAR PRC                   
180900* THIS DONE USING W413WHFA                                                
181000     IF OHUV-IDSYSTEM = 'XCEL' OR 'SPX'                                   
181100       IF OHUV-BEVARREF = ALL '+' OR SPACE                                
181200         MOVE MID-BERADREF(WS-INDEX-MID) TO WS-HFAK-REF-X10               
181300       ELSE                                                               
181400         MOVE OHUV-BEVARREF      TO WS-HFAK-REF-X10                       
181500         PERFORM S01-KOLLA-I-HFAK-TAB                                     
181600       END-IF                                                             
181700                                                                          
181800       IF BEVARREF-I-HFAK-TAB                                             
181900         MOVE OHUV-BEVARREF      TO ORAD-BERADREF                         
182000       END-IF                                                             
182100                                                                          
182200       MOVE OHUV-BEVARREF        TO ORAD-BEVOLREF                         
182300     END-IF                                                               
182400                                                                          
182500*                                                                         
182600     MOVE MID-IDKLIENT         TO ORAD-IDKLIENT                           
182700     MOVE MID-IDARBREF         TO ORAD-IDARBREF                           
182800     MOVE MID-IDVIN            TO ORAD-IDVIN                              
182900     MOVE SPACE                TO ORAD-FLAKPLOC                           
183000     MOVE NEJ                  TO ORAD-FLSDCLEV                           
183100                                                                          
183200     IF MID-FLINVEST(WS-INDEX-MID) = ALL '+'                              
183300        MOVE NEJ               TO ORAD-FLINVEST                           
183400     ELSE                                                                 
183500        MOVE MID-FLINVEST(WS-INDEX-MID)                                   
183600                               TO ORAD-FLINVEST                           
183700     END-IF                                                               
183800     MOVE JA                   TO ORAD-FLOBTRAN                           
183900     IF TILLKOMMANDE-RAD                                                  
184000       IF DIST79-DEALER-PRICE                                             
184100         MOVE NEJ              TO ORAD-FLPRTILL                           
184200       ELSE                                                               
184300        IF TILK-PRARTNTO(WS-INDEX-TILLK) > 0                              
184400           MOVE TILK-FLPRTILL(WS-INDEX-TILLK)                             
184500                               TO ORAD-FLPRTILL                           
184600        ELSE                                                              
184700           MOVE NEJ            TO ORAD-FLPRTILL                           
184800        END-IF                                                            
184900       END-IF                                                             
185000     ELSE                                                                 
185100        MOVE NEJ               TO ORAD-FLPRTILL                           
185200     END-IF                                                               
185300     IF MID-FLRESTN(WS-INDEX-MID) = ALL '+'                               
185400        MOVE OHUV-FLRESTN      TO ORAD-FLRESTN                            
185500     ELSE                                                                 
185600        MOVE MID-FLRESTN(WS-INDEX-MID)                                    
185700                               TO ORAD-FLRESTN                            
185800     END-IF                                                               
185900     IF TILLKOMMANDE-RAD                                                  
186000        MOVE JA                TO ORAD-FLTILLK                            
186100     ELSE                                                                 
186200        MOVE NEJ               TO ORAD-FLTILLK                            
186300     END-IF                                                               
186400     MOVE WC-CDC-SE            TO ORAD-IDDC-RO                            
186500     MOVE OHUV-IDDISTR         TO ORAD-IDDISTR                            
186600     MOVE OHUV-IDKUNDNR        TO ORAD-IDKUNDNR                           
186700     MOVE OHUV-IDKAMPRF        TO ORAD-IDKAMPRF                           
186800     MOVE SPACE                TO ORAD-IDLEVNR                            
186900     MOVE +0                   TO ORAD-IDLOPNR-RO                         
187000     MOVE '0000000   '         TO ORAD-IDKUNDRF-RO                        
187100     MOVE W-IDKUNDRF           TO ORAD-IDKUNDRF                           
187200     MOVE ZERO                 TO ORAD-IDSPECEMB                          
187300     MOVE MID-IDSYSTEM         TO ORAD-IDSYSTEM                           
187400     MOVE AREG-KDARTURS        TO ORAD-KDARTURS                           
187500                                                                          
187600     MOVE +1                   TO ORAD-KDDSP                              
187700     MOVE +0                   TO ORAD-KDVRINFO                           
187800     IF MID-IDSYSTEM = 'VR  '                                             
187900        IF MID-KDVRINFO(WS-INDEX-MID) = ALL '+'                           
188000           MOVE OHUV-KDVRINFO  TO ORAD-KDVRINFO                           
188100        ELSE                                                              
188200           MOVE ORFK-KDVRINFO(WS-INDEX-MID)                               
188300                                  TO WS-ALFA-1                            
188400           MOVE WS-NUM-1       TO ORAD-KDVRINFO                           
188500        END-IF                                                            
188600        MOVE +1                TO ORAD-KDDSP                              
188700     END-IF                                                               
188800     IF OHUV-FLOVRLEV = JA                                                
188900        MOVE OHUV-KDVRINFO     TO ORAD-KDVRINFO                           
189000     END-IF                                                               
189100     IF MID-IDSYSTEM = 'VIPS' OR 'VDI '                                   
189200        MOVE MID-KDDSP (WS-INDEX-MID)                                     
189300                               TO WS-ALFA-1                               
189400        MOVE WS-NUM-1          TO ORAD-KDDSP                              
189500     END-IF                                                               
189600                                                                          
189700     IF MID-IDSYSTEM = 'VDI '                                             
189800        IF MID-IDBIL (WS-INDEX-MID) > SPACE                               
189900           IF AREG-KDSORT = 'SW'                                          
190000             MOVE ORAD-IDARTNR TO TEST-ARTIKEL                            
190100             MOVE ORAD-IDDISTR TO TEST-IDDISTR                            
190200             IF DIST07-NA-CUSTOMERS                                       
190300               MOVE SPACE      TO ORAD-IDBIL                              
190400             ELSE                                                         
190500               IF ART04-SOFTWARE                                          
190600                 MOVE SPACE    TO ORAD-IDBIL                              
190700               ELSE                                                       
190800                 MOVE MID-IDBIL (WS-INDEX-MID)                            
190900                                 TO ORAD-IDBIL                            
191000                 MOVE WC-CDC-SE TO ORAD-IDDC                              
191100               END-IF                                                     
191200             END-IF                                                       
191300           ELSE                                                           
191400             MOVE SPACE        TO ORAD-IDBIL                              
191500           END-IF                                                         
191600        ELSE                                                              
191700           MOVE SPACE          TO ORAD-IDBIL                              
191800        END-IF                                                            
191900     ELSE                                                                 
192000       IF MID-IDSYSTEM = 'SOFT'                                           
192100         MOVE MID-IDBIL (WS-INDEX-MID)                                    
192200                               TO ORAD-IDBIL                              
192300       ELSE                                                               
192400         MOVE SPACE            TO ORAD-IDBIL                              
192500       END-IF                                                             
192600     END-IF                                                               
192700                                                                          
192800     MOVE ORAD-IDDC            TO WS-IDDC-SEEK                            
192900                                                                          
193000     MOVE AREG-KDFARLIG        TO ORAD-KDFARLIG                           
193100     IF MID-KDKVBRYT(WS-INDEX-MID) = ALL '+'                              
193200        MOVE +0                TO ORAD-KDKVBRYT                           
193300     ELSE                                                                 
193400        MOVE ORFK-KDKVBRYT(WS-INDEX-MID)                                  
193500                               TO ORAD-KDKVBRYT                           
193600     END-IF                                                               
193700     MOVE OHUV-KDORDING        TO ORAD-KDORDING                           
193800     MOVE OHUV-KDORDKL         TO ORAD-KDORDKL                            
193900     MOVE AREG-KDPRODSL        TO ORAD-KDPRODSL                           
194000     IF ORAD-KDORDING = +3                                                
194100       MOVE SPACE              TO ORAD-KDOI                               
194200     ELSE                                                                 
194300       MOVE ORAD-IDDISTR       TO TEST-IDDISTR                            
194400       IF DIST35-REFILL                                                   
194500       OR DIST35-NONVCC-REFILL                                            
194600       OR DIST35-NONVCC-NONVCC-TRANSFER                                   
194700       OR DIST35-NONVCC-VCC-TRANSFER                                      
194800         IF ORFK-FLCDCBEH(WS-INDEX-MID) = 'N'                             
194900           MOVE SPACE          TO ORAD-KDOI                               
195000         ELSE                                                             
195100           MOVE 'RE'           TO ORAD-KDOI                               
195200         END-IF                                                           
195300       ELSE                                                               
195400         IF DIST35-REFILL-INOM-NDC                                        
195500           MOVE 'RE'           TO ORAD-KDOI                               
195600         ELSE                                                             
195700           MOVE ORAD-KDPRODSL  TO TEST-KDPRODSL                           
195800           IF KDPRODSL-VOLVO-EMB OR ORAD-KDORDING = +1                    
195900             MOVE 'CD'         TO ORAD-KDOI                               
196000           ELSE                                                           
196100             MOVE 'DT'         TO ORAD-KDOI                               
196200           END-IF                                                         
196300         END-IF                                                           
196400       END-IF                                                             
196500     END-IF                                                               
196600     MOVE SPACE                TO ORAD-CLEARGROUP                         
196700                                                                          
196800     IF TILLKOMMANDE-RAD                                                  
196900       IF DIST79-DEALER-PRICE                                             
197000           MOVE SPACE          TO ORAD-KDPRTYP                            
197100       ELSE                                                               
197200        IF TILK-PRARTNTO(WS-INDEX-TILLK) > +0                             
197300           MOVE TILK-KDPRTYP(WS-INDEX-TILLK)                              
197400                               TO ORAD-KDPRTYP                            
197500        ELSE                                                              
197600           MOVE SPACE          TO ORAD-KDPRTYP                            
197700        END-IF                                                            
197800       END-IF                                                             
197900     ELSE                                                                 
198000        MOVE SPACE             TO ORAD-KDPRTYP                            
198100     END-IF                                                               
198200     MOVE AREG-KDSPEEMB        TO ORAD-KDSPEEMB                           
198300     IF OHUV-KDTPOTYP = 1 OR 2 OR 3 OR 4                                  
198400        MOVE OHUV-KDTPOTYP     TO ORAD-KDTPOTYP                           
198500     ELSE                                                                 
198600        IF OHUV-KDTPOTYP = +0 AND                                         
198700              MID-TITPO (WS-INDEX-MID) NOT = ALL '+'                      
198800           IF OHUV-IDSYSTEM = 'OREL'                                      
198900              MOVE +5          TO ORAD-KDTPOTYP                           
199000           ELSE                                                           
199100              IF OHUV-IDKAMPRF > +0                                       
199200                 MOVE +4       TO ORAD-KDTPOTYP                           
199300              ELSE                                                        
199400                 MOVE +2       TO ORAD-KDTPOTYP                           
199500                 IF ORAD-KDORDING = +3                                    
199600                    CONTINUE                                              
199700                 ELSE                                                     
199800                    MOVE +2    TO ORAD-KDORDING                           
199900                 END-IF                                                   
200000              END-IF                                                      
200100           END-IF                                                         
200200        ELSE                                                              
200300           MOVE +0             TO ORAD-KDTPOTYP                           
200400        END-IF                                                            
200500     END-IF                                                               
200600     MOVE JA                   TO ORAD-FLORDING                           
200700                                                                          
200800     IF TILLKOMMANDE-RAD                                                  
200900        MOVE TILK-KVBEART(WS-INDEX-TILLK)                                 
201000                               TO ORAD-KVBEART                            
201100     ELSE                                                                 
201200        MOVE ORFK-KVBEART(WS-INDEX-MID) TO WS-ALFA-6                      
201300        MOVE WS-NUM-6          TO ORAD-KVBEART                            
201400     END-IF                                                               
201500     MOVE +0                   TO ORAD-KVBEART-Q                          
201600     MOVE +0                   TO ORAD-KVPREAVB                           
201700     MOVE +0                   TO ORAD-KVPRERO                            
201800     MOVE +0                   TO ORAD-KVOKS-PREL                         
201900                                                                          
202000     MOVE +0                   TO ORAD-IDPRQUES                           
202100                                                                          
202200     IF ORFK-PRARTBTO-LOC(WS-INDEX-MID) = ALL '+'                         
202300       MOVE +0                             TO  ORAD-PRARTBTO-LOC          
202400     ELSE                                                                 
202500      MOVE ORFK-PRARTBTO-LOC-UT(WS-INDEX-MID) TO ORAD-PRARTBTO-LOC        
202600     END-IF                                                               
202700                                                                          
202800     MOVE MID-KDVAT(WS-INDEX-MID)       TO ORAD-KDVAT                     
202900                                                                          
203000     MOVE MID-RERAB(WS-INDEX-MID)       TO ORAD-RERAB                     
203100     MOVE MID-KDRAB(WS-INDEX-MID)       TO ORAD-KDRAB                     
203200     MOVE MID-BEART-VIPS(WS-INDEX-MID)  TO ORAD-BEART-VIPS                
203300                                                                          
203400     IF TILLKOMMANDE-RAD                                                  
203500       IF DIST79-DEALER-PRICE                                             
203600           MOVE +0             TO ORAD-PRARTNTO                           
203700           INITIALIZE          ORAD-DEAL-PR-LINE                          
203800       ELSE                                                               
203900        IF TILK-PRARTNTO(WS-INDEX-TILLK) > +0                             
204000          IF MID-IDSYSTEM(1:3) = 'ECO'                                    
204100             MOVE TILK-PRARTNTO(WS-INDEX-TILLK)                           
204200                               TO ORAD-PRARTNTO-LOC                       
204300             MOVE ZERO         TO ORAD-PRARTNTO                           
204400          ELSE                                                            
204500                                                                          
204600             MOVE TILK-PRARTNTO(WS-INDEX-TILLK)                           
204700                               TO ORAD-PRARTNTO                           
204800             MOVE ZERO         TO ORAD-PRARTNTO-LOC                       
204900          END-IF                                                          
205000        ELSE                                                              
205100           MOVE +0             TO ORAD-PRARTNTO                           
205200           INITIALIZE          ORAD-DEAL-PR-LINE                          
205300        END-IF                                                            
205400       END-IF                                                             
205500     ELSE                                                                 
205600       IF MID-PRARTNTO-LOC(WS-INDEX-MID) = ALL '+'                        
205700          MOVE +0             TO ORAD-PRARTNTO-LOC                        
205800       ELSE                                                               
205900          MOVE ORFK-PRARTNTO-LOC-UT(WS-INDEX-MID)                         
206000                              TO ORAD-PRARTNTO-LOC                        
206100       END-IF                                                             
206200       MOVE +0                TO ORAD-PRARTNTO-LOCPREL                    
206300       IF MID-PRARTNTO(WS-INDEX-MID) = ALL '+'                            
206400          MOVE +0             TO ORAD-PRARTNTO                            
206500       ELSE                                                               
206600          MOVE ORFK-PRARTNTO-UT(WS-INDEX-MID)                             
206700                              TO ORAD-PRARTNTO                            
206800       END-IF                                                             
206900                                                                          
207000       MOVE +0                TO ORAD-PRBPRIS                             
207100     END-IF                                                               
207200                                                                          
207300     IF ORFK-KDORDBEK (WS-INDEX-MID) = 59                                 
207400        MOVE MID-REKSIFFR (WS-INDEX-MID)                                  
207500                               TO ORAD-REKSIFFR                           
207600     ELSE                                                                 
207700        MOVE AREG-REKSIFFR     TO ORAD-REKSIFFR                           
207800     END-IF                                                               
207900     MOVE +0                   TO ORAD-RERF-RAD                           
208000     MOVE +0                   TO ORAD-KVSLATT                            
208100     IF TILLKOMMANDE-RAD                                                  
208200       IF DIST79-DEALER-PRICE                                             
208300          MOVE +0              TO ORAD-TIPRIS                             
208400       ELSE                                                               
208500        IF TILK-PRARTNTO(WS-INDEX-TILLK) > +0                             
208600           MOVE TILK-TIPRIS(WS-INDEX-TILLK)                               
208700                               TO ORAD-TIPRIS                             
208800        ELSE                                                              
208900           MOVE +0             TO ORAD-TIPRIS                             
209000        END-IF                                                            
209100       END-IF                                                             
209200     ELSE                                                                 
209300        MOVE +0                TO ORAD-TIPRIS                             
209400     END-IF                                                               
209500     MOVE MSGI-TILOKDAT        TO ORAD-TIREGDAT                           
209600     MOVE MSGI-TILOKTID        TO WS-TIHHMM                               
209700     MOVE WS-TIHHMMSS          TO ORAD-TIREGTID                           
209800     MOVE +0                   TO ORAD-TIRODAT                            
209900     IF MID-TITPO(WS-INDEX-MID) = ALL '+'                                 
210000        MOVE OHUV-TITPO        TO ORAD-TITPO                              
210100     ELSE                                                                 
210200        MOVE MID-TITPO(WS-INDEX-MID) TO WS-ALFA-6                         
210300        MOVE WS-NUM-6          TO ORAD-TITPO                              
210400     END-IF                                                               
210500     MOVE AREG-VKART           TO ORAD-VKART                              
210600     MOVE AREG-VKART-NTO       TO ORAD-VKART-NTO                          
210700     MOVE AREG-VLARTNTO        TO ORAD-VLARTNTO                           
210800     MOVE MID-IDKUNDRF-WIP (WS-INDEX-MID)                                 
210900                               TO ORAD-IDKUNDRF-WIP                       
211000     MOVE +0                   TO ORAD-PRAVCOST                           
211100     MOVE MID-KDVALISO(WS-INDEX-MID)    TO ORAD-KDVALISO                  
211200*    *GLOBAL EXPORT. MAY NOT BE SPACE.                                    
211300     IF ORAD-KDVALISO = SPACE                                             
211400        MOVE WS-KDVALISO-NA             TO ORAD-KDVALISO                  
211500     END-IF                                                               
211600     .                                                                    
211700     EJECT                                                                
211800                                                                          
211900 ECC-LAS-NYA-ARTIKELREG SECTION.                                          
212000                                                                          
212100     MOVE 'STA ECC-LAS-ART'                TO   WS-PGM-POSITION           
212200     IF ALLT-OK                                                           
212300                                                                          
212400     MOVE ORAD-IDARTNR                TO W-IDARTNR                        
212500     PERFORM IMS-09-GU-WLARTM-WDK901                                      
212600                                                                          
212700     IF SEGMENT-SAKNAS                                                    
212800        MOVE W-IDARTNR         TO ARTM-IDARTNR-IN                         
212900        CALL W411ARTM USING ARTM-W411ARTM ARTM-ARTM-PCB                   
213000     END-IF                                                               
213100                                                                          
213200     END-IF                                                               
213300     .                                                                    
213400     EJECT                                                                
213500                                                                          
213600 ECF-KOMPLETTERA-KVANT-BRYTES SECTION.                                    
213700                                                                          
213800     MOVE 'STA ECF-KVANT  '                TO   WS-PGM-POSITION           
213900     IF ALLT-OK                                                           
214000                                                                          
214100     MOVE ORAD-KDKVBRYT        TO KVAN-KDKVBRYT-IN                        
214200     MOVE ORAD-IDSYSTEM        TO KVAN-IDSYSTEM-IN                        
214300     MOVE ORAD-KVBEART         TO KVAN-KVBEART-IN                         
214400* FOR VR ORDERS (IDYSTEM=VR AND KDORDURS=G VIA VIPS) INCLUDE              
214500* QUANTITY ADAPTATION TO NOT USE Q0 THEREFORE MOVE ZERO TO Q0             
214600     IF (MID-IDSYSTEM = 'VR  ' OR MID-KDORDURS = 'G')                     
214700       MOVE 0                  TO KVAN-KVQPACK-0-IN                       
214800     ELSE                                                                 
214900       MOVE AREG-KVQPACK-0     TO KVAN-KVQPACK-0-IN                       
215000     END-IF                                                               
215100     MOVE AREG-KVQPACK-1       TO KVAN-KVQPACK-1-IN                       
215200     MOVE AREG-KDPRODSL        TO KVAN-KDPRODSL-IN                        
215300     MOVE AREG-KDSORT          TO KVAN-KDSORT-IN                          
215400     MOVE AREG-IDFKNGRP        TO KVAN-IDFKNGRP-IN                        
215500     MOVE OHUV-KDORDKL         TO KVAN-KDORDKL-IN                         
215600     MOVE OHUV-FLFORBI         TO KVAN-FLFORBI-IN                         
215700     MOVE OHUV-IDKAMPRF        TO KVAN-IDKAMPRF-IN                        
215800                                                                          
215900     IF ORAD-IDBIL = SPACE OR ORAD-IDSYSTEM = 'SOFT'                      
216000       MOVE OHUV-FLORDSPE      TO KVAN-FLORDSPE-IN                        
216100     ELSE                                                                 
216200       MOVE JA                 TO KVAN-FLORDSPE-IN                        
216300     END-IF                                                               
216400                                                                          
216500     MOVE OHUV-FLOVRLEV        TO KVAN-FLOVRLEV-IN                        
216600     MOVE OHUV-FLEMBORD        TO KVAN-FLEMBORD-IN                        
216700     MOVE ORAD-IDDC            TO KVAN-IDDC-IN                            
216800     MOVE ORAD-IDDISTR         TO KVAN-IDDISTR-IN                         
216900     MOVE ORAD-IDKUNDNR        TO KVAN-IDKUNDNR-IN                        
217000     MOVE ORAD-BERADREF        TO KVAN-BERADREF-IN                        
217100     MOVE ORAD-IDARTNR         TO KVAN-IDARTNR-IN                         
217200                                                                          
217300     MOVE ORAD-IDDISTR         TO TEST-IDDISTR                            
217400     IF DIST18-SKROT OR DIST18-SCRAP-NDC OR                               
217500       ORAD-IDSYSTEM = 'LDC ' OR 'TACD'  OR                               
217600       (DIST35-RETUR AND ORAD-KDKVBRYT = 1) OR                            
217700       WS-TRANSFER = JA                                                   
217800****(DIST35-RETUR AND ORAD-KDKVBRYT = 1) DETTA FÖR ATT                    
217900****LAGEROMRÅDE 98(AUTOMATRETUR) INTE SKALL KVANTANPASSAS                 
218000****TRANSFERS SKALL EJ KVANTANPASSAS                                      
218100       MOVE JA                 TO KVAN-FLORDSPE-IN                        
218200     END-IF                                                               
218300                                                                          
218400     CALL W411KVAN USING KVAN-W411KVAN KVAN-WDB2-PCB KVAN-WDC1-PCB        
218500                                                                          
218600     MOVE KVAN-KDKVBRYT-UT         TO ORAD-KDKVBRYT                       
218700     MOVE KVAN-KVBEART-Q-UT        TO ORAD-KVBEART-Q                      
218800                                                                          
218900     IF KVAN-KDORDBEK-UT > +0                                             
219000        MOVE JA                    TO OBKR-SW                             
219100     END-IF                                                               
219200                                                                          
219300     END-IF                                                               
219400     .                                                                    
219500     EJECT                                                                
219600                                                                          
219700 ECI-KOMPLETTERA-DIREKTLEVERANS SECTION.                                  
219800                                                                          
219900     MOVE 'STA ECI-DLEV   '                TO   WS-PGM-POSITION           
220000     MOVE NEJ  TO SW-DDGS-TPO-OBKR71                                      
220100     IF ALLT-OK                                                           
220200                                                                          
220300       IF ORAD-IDBIL = SPACE OR ORAD-IDSYSTEM = 'SOFT'                    
220400                                                                          
220500         PERFORM S40-HAMTA-WDB6-INFO                                      
220600         IF (DCS-CDC OR DCS-CDC-TR OR                                     
220700             DCS-SDC OR                                                   
220800            (DCS-NDC AND AREG-REDIRLEV = 1.0))                            
220900*    WE MUST ADD ALLOW AN EXCEPTION FOR NDC AND DIRECT DELIVERY           
221000*    FLDDGS IN WDF211 TELLS IF DDGS IS OK OR NOT                          
221100                                                                          
221200           MOVE ZERO                          TO DLEV-KDORDBEK-UT         
221300           MOVE SPACE                         TO DLEV-IDLEVNR-UT          
221400           MOVE ORAD-FLRESTN                  TO DLEV-FLRESTN-UT          
221500           MOVE ORAD-FLSDCLEV                 TO DLEV-FLSDCLEV-UT         
221600           MOVE AREG-REDIRLEV                 TO DLEV-REDIRLEV-IN         
221700           MOVE ORAD-IDDISTR                  TO TEST-IDDISTR             
221800           IF DLEV-REDIRLEV-IN > +0 AND                                   
221900             (AREG-KDUART = SPACE  OR                                     
222000              ORAD-IDKAMPRF = ZERO OR                                     
222100              ORAD-KDTPOTYP = ZERO) AND                                   
222200              OHUV-FLFORBI = NEJ   AND                                    
222300              OHUV-FLOVRLEV = NEJ  AND                                    
222400              OHUV-FLORDSPE = NEJ  AND                                    
222500              WS-TRANSFER = NEJ    AND                                    
222600              NOT DIST35-RETUR                                            
222700                                                                          
222800              MOVE ORAD-IDDISTR      TO DLEV-IDDISTR-IN                   
222900              MOVE OHUV-IDDC-TVS     TO DLEV-IDDC-IN                      
223000              MOVE ORAD-IDDC         TO DLEV-IDDC-ORD-IN                  
223100              MOVE ORAD-IDKUNDNR     TO DLEV-IDKUNDNR-IN                  
223200              MOVE OHUV-KDORDKL      TO DLEV-KDORDKL-IN                   
223300              MOVE ORAD-IDARTNR      TO DLEV-IDARTNR-IN                   
223400              MOVE AREG-IDLEVNR      TO DLEV-IDLEVNR-IN                   
223500              MOVE ORAD-KVBEART-Q    TO DLEV-KVBEART-Q-IN                 
223600              MOVE ORAD-IDKAMPRF     TO DLEV-IDKAMPRF-IN                  
223700              MOVE ORAD-KDTPOTYP     TO DLEV-KDTPOTYP-IN                  
223800              MOVE AREG-KDUART       TO DLEV-KDUART-IN                    
223900              MOVE OHUV-FLFORBI      TO DLEV-FLFORBI-IN                   
224000              MOVE ORAD-FLRESTN      TO DLEV-FLRESTN-IN                   
224100              MOVE AREG-FLREFILL     TO DLEV-FLREFILL-IN                  
224200              MOVE ORAD-KDORDING     TO DLEV-KDORDING-IN                  
224300                                                                          
224400              MOVE OHUV-IDKUNDRF      TO DLEV-IDKUNDRF-IN                 
224500                                                                          
224600              MOVE +1 TO WS-INDEX                                         
224700              PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                     
224800                 MOVE W-GMT-IDDC-CLEAR  (WS-INDEX)                        
224900                                   TO DLEV-IDDC-CLEAR-IN(WS-INDEX)        
225000                 ADD +1 TO WS-INDEX                                       
225100              END-PERFORM                                                 
225200                                                                          
225300              MOVE 1                  TO DLEV-KDCALL                      
225400              IF  MID-IDKUNDRF-RO NOT = SPACE                             
225500                AND NOT TILLKOMMANDE-RAD                                  
225600                MOVE 2                TO DLEV-KDCALL                      
225700              END-IF                                                      
225800              MOVE SPACE             TO DLEV-CLEARGROUP                   
225900              MOVE ORAD-KDOI         TO DLEV-KDOI-UT                      
226000                                                                          
226100              CALL W411DLEV USING DLEV-W411DLEV DLEV-LEVF-PCB             
226200                                                DLEV-LEVG-PCB             
226300                                                DLEV-LEVA-PCB             
226400                                                DLEV-ARTS-PCB             
226500                                                DLEV-WDB6-PCB             
226600                                                TPO2-FILA-PCB             
226700                                                                          
226800*             JUSTERING FÖR DIREKTLEVERERAD TPO-RAD                       
226900              IF DLEV-KDORDBEK-UT = 95                                    
227000                 IF DLEV-KDTPOTYP-IN = +2                                 
227100                    IF OHUV-KDTPOTYP NOT = +2                             
227200                       MOVE 21             TO DLEV-KDORDBEK-UT            
227300                       MOVE SPACE          TO DLEV-IDLEVNR-UT             
227400                    ELSE                                                  
227500                       PERFORM ECIA-KOLLA-TPODAT                          
227600                       MOVE +0             TO ORAD-KDTPOTYP               
227700                       MOVE ZERO           TO ORAD-TITPO                  
227800                       MOVE 'CD'           TO DLEV-KDOI-UT                
227900                    END-IF                                                
228000                 END-IF                                                   
228100              END-IF                                                      
228200                                                                          
228300              IF DLEV-KDORDBEK-UT = 21 OR 53                              
228400                 MOVE JA                   TO OBKR-SW                     
228500                 MOVE NEJ                  TO ALLT-SW                     
228600                 MOVE ZERO                 TO KVAN-KDORDBEK-UT            
228700              ELSE                                                        
228800                 IF DLEV-KDORDBEK-UT = 95                                 
228900                    MOVE JA                TO OBKR-SW                     
229000                 END-IF                                                   
229100                 IF DLEV-IDLEVNR-UT NOT = SPACE                           
229200*FIX KONRAD                                                               
229300                    IF DLEV-IDLEVNR-UT = '6492 '                          
229400                    OR DLEV-IDLEVNR-UT = 'BZFFA'                          
229500                       MOVE SPACE TO AREG-KDUART                          
229600                    END-IF                                                
229700*END FIX KONRAD                                                           
229800                    IF DLEV-IDDC-UT NOT = DLEV-IDDC-IN                    
229900****                   IF OHUV-TIREPDAT = ZERO                            
230000* VI UNDANTAR REPARATIONSORDER                                            
230100* RADEN HAMNAR PÅ HANGING ORDERLINES                                      
230200* OCH KAN EVENTUELLT FLYTTAS TILL DIREKTLEVERANTÖREN DÄR                  
230300                         MOVE DLEV-IDDC-UT TO ORAD-IDDC                   
230400                                              WS-IDDC-SEEK                
230500****                  END-IF                                              
230600                    END-IF                                                
230700                    IF MID-ADLAGOMR-CD(WS-INDEX-MID) NOT = ALL '+'        
230800                    AND MID-ADLAGOMR-CD(WS-INDEX-MID) > ZERO              
230900                                                                          
231000                       MOVE MID-ADLAGOMR-CD(WS-INDEX-MID)                 
231100                                              TO ORAD-ADLAGOMR            
231200                       MOVE MID-ADGANG-CD(WS-INDEX-MID)                   
231300                                              TO ORAD-ADGANG              
231400                       MOVE MID-ADPLATS-CD(WS-INDEX-MID)                  
231500                                              TO ORAD-ADPLATS             
231600                    ELSE                                                  
231700                      MOVE AREG-ADLAGOMR   TO ORAD-ADLAGOMR               
231800                      MOVE AREG-ADGANG     TO ORAD-ADGANG                 
231900                      MOVE AREG-ADPLATS    TO ORAD-ADPLATS                
232000                   END-IF                                                 
232100                   IF OHUV-FLFORBI = SPEC-FORBI                           
232200                      MOVE '07'            TO ORAD-ADLAGOMR               
232300                   END-IF                                                 
232400                 END-IF                                                   
232500                 MOVE DLEV-KDORDSTA-UT     TO                             
232600                                  AVSR-KDORDSTA    (WS-INDEX-WOPS)        
232700                 MOVE DLEV-KDVIA-UT        TO                             
232800                                  AVSR-KDVIA       (WS-INDEX-WOPS)        
232900                 MOVE DLEV-KVDAGAR-DIFF-UT TO                             
233000                                  AVSR-KVDAGAR-DIFF(WS-INDEX-WOPS)        
233100                 MOVE DLEV-TISKEPPN-DDC-UT TO                             
233200                                  AVSR-TISKEPPN-DDC(WS-INDEX-WOPS)        
233300                                                                          
233400                 MOVE DLEV-IDLEVNR-UT      TO ORAD-IDLEVNR                
233500                 MOVE DLEV-FLRESTN-UT      TO ORAD-FLRESTN                
233600                 MOVE DLEV-FLSDCLEV-UT     TO ORAD-FLSDCLEV               
233700                 IF DLEV-FLSDCLEV-UT NOT = JA                             
233800                   MOVE DLEV-KDOI-UT       TO ORAD-KDOI                   
233900                   MOVE DLEV-CLEARGROUP    TO ORAD-CLEARGROUP             
234000                 ELSE                                                     
234100                   MOVE DLEV-IDDC-UT       TO ORAD-IDDC                   
234200                                              WS-IDDC-SEEK                
234300                 END-IF                                                   
234400              END-IF                                                      
234500           END-IF                                                         
234600         ELSE                                                             
234700           IF MID-FLDIRLEV (WS-INDEX-MID ) = JA OR YES                    
234800*** DETTA IDLEVNR BORDE MAN HÄMTA FRÅN K711 NÄR TESTER ÄR KLARA!!         
234900             MOVE '63542'            TO ORAD-IDLEVNR                      
235000             MOVE NEJ                TO ORAD-FLRESTN                      
235100                                        ORAD-FLSDCLEV                     
235200           END-IF                                                         
235300         END-IF                                                           
235400       ELSE                                                               
235500         MOVE ZERO                   TO DLEV-KDORDBEK-UT                  
235600         MOVE '1441 '                TO ORAD-IDLEVNR                      
235700         MOVE NEJ                    TO ORAD-FLRESTN                      
235800                                        ORAD-FLSDCLEV                     
235900       END-IF                                                             
236000       IF DLEV-KDORDBEK-UT = 0 AND AREG-ADLAGOMR = 79                     
236100          AND ORAD-IDDC = WC-CDC-SE                                       
236200          MOVE OHUV-IDDISTR     TO TEST-IDDISTR                           
236300          IF DIST18-SKROT                                                 
236400            CONTINUE                                                      
236500          ELSE                                                            
236600            MOVE 26             TO DLEV-KDORDBEK-UT                       
236700            MOVE JA             TO OBKR-SW                                
236800          END-IF                                                          
236900       END-IF                                                             
237000     END-IF                                                               
237100     .                                                                    
237200     EJECT                                                                
237300 ECIA-KOLLA-TPODAT  SECTION.                                              
237400                                                                          
237500     MOVE 'STA ECIA-KOLLA   '              TO   WS-PGM-POSITION           
237600                                                                          
237700     IF OHUV-TITPO = ZERO                                                 
237800        PERFORM IMS-01-GHU-ORQI-WDQ201                                    
237900        MOVE ORAD-TITPO    TO OHUV-TITPO                                  
238000        PERFORM IMS-REPL-WDQ2-WDQ201                                      
238100     ELSE                                                                 
238200        IF ORAD-TITPO NOT = OHUV-TITPO                                    
238300           MOVE OHUV-TITPO TO ORAD-TITPO                                  
238400           MOVE JA         TO SW-DDGS-TPO-OBKR71                          
238500        END-IF                                                            
238600     END-IF                                                               
238700     .                                                                    
238800     EJECT                                                                
238900 ECK-KOMPLETTERA-ERSATTNING SECTION.                                      
239000                                                                          
239100     MOVE 'STA ECK-ERS    '                TO   WS-PGM-POSITION           
239200     MOVE ORAD-IDDISTR         TO TEST-IDDISTR                            
239300     IF ALLT-OK  AND NOT DIST35-REFILL                                    
239400                 AND NOT DIST35-REFILL-INOM-NDC                           
239500                 AND NOT DIST35-NONVCC-REFILL                             
239600                 AND NOT DIST35-NONVCC-NONVCC-TRANSFER                    
239700                 AND NOT DIST35-NONVCC-VCC-TRANSFER                       
239800                 AND NOT DIST35-RETUR                                     
239900                 AND NOT DIST18-SKROT                                     
240000                 AND NOT DIST18-SCRAP-NDC                                 
240100                                                                          
240200     MOVE ORAD-IDARTNR         TO KERS-IDARTNR                            
240300     MOVE ORAD-IDDC            TO KERS-IDDC                               
240400     MOVE OHUV-FLPRERS         TO KERS-FLPRERS                            
240500     MOVE ORAD-FLPRTILL        TO KERS-FLPRTILL                           
240600     MOVE OHUV-FLFORBI         TO KERS-FLFORBI                            
240700     MOVE ORAD-IDKAMPRF        TO KERS-IDKAMPRF                           
240800     MOVE AREG-KDERS           TO KERS-KDERS                              
240900     MOVE AREG-KDERS-UTG       TO KERS-KDERS-UTG                          
241000     MOVE ORAD-KDPRTYP         TO KERS-KDPRTYP                            
241100     MOVE ORAD-KDTPOTYP        TO KERS-KDTPOTYP                           
241200     MOVE AREG-KDUART          TO KERS-KDUART                             
241300     MOVE ORAD-KVBEART         TO KERS-KVBEART                            
241400     IF MID-IDSYSTEM(1:3) = 'ECO'                                         
241500        MOVE ORAD-PRARTNTO-LOC TO KERS-PRARTNTO                           
241600     ELSE                                                                 
241700        MOVE ORAD-PRARTNTO     TO KERS-PRARTNTO                           
241800     END-IF                                                               
241900     MOVE ORAD-DEAL-PR-LINE    TO KERS-DEAL-PR-LINE                       
242000     MOVE ORAD-TIPRIS          TO KERS-TIPRIS                             
242100                                                                          
242200     IF ORAD-IDBIL = SPACE OR ORAD-IDSYSTEM = 'SOFT'                      
242300       MOVE OHUV-FLORDSPE      TO KERS-FLORDSPE                           
242400     ELSE                                                                 
242500       MOVE JA                 TO KERS-FLORDSPE                           
242600     END-IF                                                               
242700                                                                          
242800     MOVE OHUV-FLOVRLEV        TO KERS-FLOVRLEV                           
242900                                                                          
243000     CALL W411KERS USING KERS-W411KERS TILK-W411TILK                      
243100                         KERS-ARTC-PCB KERS-ERSA-PCB                      
243200                         SDCA-ARTS-PCB CDCA-ARTM-PCB                      
243300                                                                          
243400     IF KERS-KDORDBEK > ZERO   AND                                        
243500        ((KERS-KDERS-UTG > +20 AND KERS-KDERS = +0)  OR                   
243600          KERS-KDERS > 10 )                                               
243700        MOVE JA                  TO OBKR-SW                               
243800        MOVE NEJ                 TO ALLT-SW                               
243900     END-IF                                                               
244000                                                                          
244100     PERFORM S40-HAMTA-WDB6-INFO                                          
244200     IF KERS-KDORDBEK > ZERO                                              
244300      IF DCS-SDC OR DCS-CDC                                               
244400          IF PREPLANED-SW = JA                                            
244500            IF AREG-KDERS = 11 OR 12 OR 14 OR 15 OR 17 OR 18 OR 19        
244600                         OR 21 OR 22 OR 24 OR 25 OR 27 OR 28 OR 29        
244700               MOVE JA            TO KOLLA-ERS-SW                         
244800            END-IF                                                        
244900          ELSE                                                            
245000            IF AREG-KDERS = 11 OR 12 OR 17 OR                             
245100                            21 OR 22 OR 27                                
245200               MOVE JA            TO KOLLA-ERS-SW                         
245300            ELSE                                                          
245400               IF AREG-KDERS = 14 OR 15 OR 18 OR 19 OR                    
245500                               24 OR 25 OR 28 OR 29                       
245600                 MOVE JA          TO ALLT-SW                              
245700               END-IF                                                     
245800            END-IF                                                        
245900          END-IF                                                          
246000      END-IF                                                              
246100                                                                          
246200      IF DCS-NDC                                                          
246300        IF AREG-KDERS > 18                                                
246400          MOVE JA                 TO KOLLA-ERS-SW                         
246500        ELSE                                                              
246600          IF KERS-KDERS-UTG > +20                                         
246700**FIX---TESTA DDETTA FÖR ATT UNDVIKA ABEND I SPÄRR (GE K611)              
246800**      PGA UTGÅNGEN ART BESTÄLLT FRÅN NDC                                
246900            CONTINUE                                                      
247000          ELSE                                                            
247100            MOVE ZERO               TO KERS-KDORDBEK                      
247200            PERFORM S02-RENSA-TILLK-TAB                                   
247300            MOVE JA                 TO ALLT-SW                            
247400          END-IF                                                          
247500        END-IF                                                            
247600      END-IF                                                              
247700     END-IF                                                               
247800                                                                          
247900     IF KERS-KDORDBEK > +0 AND KVAN-KDORDBEK-UT > +0                      
248000        IF KERS-KDERS = 01 AND (AREG-KDSORT = 'L' OR 'M')                 
248100***        CHECK FOR 'KERS-KDERS = 01' AND KDSROT IS 'L' OR 'M' SO        
248200***        THAT QUANTITY ADAPTION DO HAPPEN EVEN WHEN THE                 
248300***        SUPERSESSION CODE IS EQUAL TO 1 AND HENCE Q1 QUANTITY          
248400***        IS NOT BROKEN FURTHER. MIN QTY ORDERED WILL BECOME Q1          
248500           CONTINUE                                                       
248600        ELSE                                                              
248700           MOVE ZERO             TO KVAN-KDORDBEK-UT                      
248800           MOVE ORAD-KVBEART     TO ORAD-KVBEART-Q                        
248900        END-IF                                                            
249000     END-IF                                                               
249100     IF KERS-KDORDBEK > +0 AND DLEV-KDORDBEK-UT > +0                      
249200        MOVE ZERO                TO DLEV-KDORDBEK-UT                      
249300     END-IF                                                               
249400                                                                          
249500     ELSE                                                                 
249600        MOVE +0                  TO KERS-KDERS                            
249700     END-IF                                                               
249800                                                                          
249900     .                                                                    
250000     EJECT                                                                
250100                                                                          
250200 ECL-KOMPLETTERA-SPARRAR SECTION.                                         
250300                                                                          
250400     MOVE 'STA ECL-SPARR  '                TO   WS-PGM-POSITION           
250500     IF ALLT-OK OR KOLLA-ERS                                              
250600                                                                          
250700     MOVE ORAD-BERADREF        TO SPAR-BERADREF                           
250800     IF ORAD-IDBIL = SPACE OR ORAD-IDSYSTEM = 'SOFT'                      
250900       IF ORAD-IDSYSTEM = 'VDI '                                          
251000         MOVE SPACE            TO SPAR-BEKUNDRF                           
251100       ELSE                                                               
251200          MOVE OHUV-BEKUNDRF   TO SPAR-BEKUNDRF                           
251300       END-IF                                                             
251400     ELSE                                                                 
251500       MOVE 'SOFTWARE'         TO SPAR-BEKUNDRF                           
251600     END-IF                                                               
251700     MOVE AREG-FLAVRART        TO SPAR-FLAVRART                           
251800     MOVE OHUV-FLFORBI         TO SPAR-FLFORBI                            
251900     MOVE AREG-FLLSRDEL        TO SPAR-FLLSRDEL                           
252000     MOVE AREG-FLRADREF        TO SPAR-FLRADREF                           
252100     MOVE ORAD-FLRESTN         TO SPAR-FLRESTN                            
252200     MOVE ORAD-IDARTNR         TO SPAR-IDARTNR                            
252300     MOVE AREG-FLIART          TO SPAR-FLIART                             
252400     MOVE AREG-FLMARKSP        TO SPAR-FLMARKSP                           
252500     MOVE ORAD-IDDISTR         TO SPAR-IDDISTR                            
252600     MOVE ORAD-IDKUNDNR        TO SPAR-IDKUNDNR                           
252700     MOVE ORAD-IDKUNDRF-RO     TO SPAR-IDKUNDRF-RO                        
252800     PERFORM S40-HAMTA-WDB6-INFO                                          
252900     IF DCS-DDC                                                           
253000       MOVE OHUV-IDDC-PRIM     TO SPAR-IDDC                               
253100     ELSE                                                                 
253200       MOVE ORAD-IDDC          TO SPAR-IDDC                               
253300     END-IF                                                               
253400     MOVE ORAD-IDSYSTEM        TO SPAR-IDSYSTEM                           
253500     MOVE AREG-KDERS-UTG       TO SPAR-KDERS-UTG                          
253600     MOVE AREG-KDERS           TO SPAR-KDERS                              
253700     MOVE OHUV-KDFAKTYP        TO SPAR-KDFAKTYP                           
253800     MOVE AREG-KDLEVSP         TO SPAR-KDLEVSP                            
253900     MOVE +1                   TO SPAR-KDORDBEH                           
254000     MOVE OHUV-KDORDKL         TO SPAR-KDORDKL                            
254100     MOVE AREG-KDPRODSL        TO SPAR-KDPRODSL                           
254200     MOVE AREG-KDSORT          TO SPAR-KDSORT                             
254300     MOVE ORAD-KDPRTYP         TO SPAR-KDPRTYP                            
254400     MOVE ORAD-KDTPOTYP        TO SPAR-KDTPOTYP                           
254500     MOVE AREG-KDUART          TO SPAR-KDUART                             
254600     MOVE AREG-PRARTSTD        TO SPAR-PRARTSTD                           
254700     MOVE AREG-TIFINLV         TO SPAR-TIFINLV                            
254800     MOVE ORAD-TIRODAT         TO SPAR-TIRODAT                            
254900     MOVE ORAD-TITPO           TO SPAR-TITPO                              
255000     MOVE ORAD-FLSDCLEV        TO SPAR-FLSDCLEV                           
255100     MOVE OHUV-TIREPDAT        TO SPAR-TIREPDAT                           
255200                                                                          
255300     IF ORAD-IDBIL = SPACE OR ORAD-IDSYSTEM = 'SOFT'                      
255400       MOVE OHUV-FLORDSPE      TO SPAR-FLORDSPE                           
255500     ELSE                                                                 
255600       MOVE JA                 TO SPAR-FLORDSPE                           
255700     END-IF                                                               
255800                                                                          
255900     MOVE OHUV-FLOVRLEV        TO SPAR-FLOVRLEV                           
256000     MOVE OHUV-FLEMBORD        TO SPAR-FLEMBORD                           
256100                                                                          
256200     CALL W411SPAR USING SPAR-W411SPAR SPAR-WDF8-PCB                      
256300                                       SPAR-WDF8A-PCB                     
256400                                       SPAR-WDK6-PCB                      
256500                                                                          
256600     IF SPAR-KDORDBEK > ZERO                                              
256700        MOVE JA             TO OBKR-SW                                    
256800        MOVE NEJ            TO ALLT-SW                                    
256900        IF SPAR-KDORDBEK = 51 OR 67 OR 58                                 
257000          MOVE NEJ            TO KOLLA-ERS-SW                             
257100          MOVE ZERO            TO KERS-KDORDBEK                           
257200          PERFORM S02-RENSA-TILLK-TAB                                     
257300        END-IF                                                            
257700        MOVE ZERO           TO XDCA-DAPUBL                                
257400*FOR CODE 67 THERE IS A SPECIAL CHECK IN W411XDCA                         
257500        IF  SPAR-KDORDBEK =67 AND SPAR-FLPUBCDC = YES                     
257600*         MOVE 99999999       TO NDCA-DAPUBL                              
257700          MOVE 99999999       TO XDCA-DAPUBL                              
257800        END-IF                                                            
257900        IF KVAN-KDORDBEK-UT > +0                                          
258000           MOVE ZERO        TO KVAN-KDORDBEK-UT                           
258100           MOVE ORAD-KVBEART TO ORAD-KVBEART-Q                            
258200        END-IF                                                            
258300*FOR KDERS 19/29,SPAR GIVES OCC54.BUT IF STOCKS ARE PRESENT IN A          
258400*DC,ORDERLINE WITH 19/29 CAN BE REGISTERED.                               
258500        IF SPAR-KDORDBEK = 54 AND PREPLANED-SW = NEJ                      
258600                              AND NOT DCS-DDC                             
258700          MOVE JA                TO ALLT-SW                               
258800        END-IF                                                            
258900        IF DLEV-KDORDBEK-UT > +0                                          
259000           MOVE ZERO        TO DLEV-KDORDBEK-UT                           
259100        END-IF                                                            
259200     ELSE                                                                 
259300*       KONTROLL OM REPARATIONSDATUM ÄR FÖR TÄTT INPÅ                     
259400*       GÄLLER ENGLAND, LDC, ORDKL=3                                      
259500        MOVE ORAD-IDDISTR   TO TEST-IDDISTR                               
259600        IF  OHUV-KDORDKL = 3 AND OHUV-TIREPDAT > 0                        
259700        AND (OHUV-IDSYSTEM = 'LDC '                                       
259800         OR (OHUV-IDSYSTEM(1:3) = 'LYN' OR 'ECO' OR 'VOU' OR              
259900                                  'TAD' OR 'ACC' OR 'APA' OR              
260000                                  'APB' OR 'APC' OR 'APD' OR              
260100                                  'APE' OR 'APF' OR 'APG' OR              
260200                                  'APH' OR 'API' OR 'APJ' ))              
260300          AND DIST34-ENGLAND-LDC                                          
260400*       LÄS KUNDREG O KOLLA ENGLANDS-DISTRIKT                             
260500          IF GMT-FLLDCKND = JA                                            
260600            PERFORM S16-KOLLA-REPARATIONSDATUM                            
260700            IF W-REPSW = NEJ                                              
260800              MOVE 68               TO SPAR-KDORDBEK                      
260900              MOVE JA               TO OBKR-SW                            
261000              MOVE NEJ              TO ALLT-SW                            
261100              IF SPAR-KDORDBEK = 51 OR 67 OR 58 OR 68                     
261200                MOVE NEJ            TO KOLLA-ERS-SW                       
261300                MOVE ZERO            TO KERS-KDORDBEK                     
261400                PERFORM S02-RENSA-TILLK-TAB                               
261500              END-IF                                                      
261600              IF KVAN-KDORDBEK-UT > +0                                    
261700                MOVE ZERO        TO KVAN-KDORDBEK-UT                      
261800                MOVE ORAD-KVBEART TO ORAD-KVBEART-Q                       
261900              END-IF                                                      
262000              IF DLEV-KDORDBEK-UT > +0                                    
262100                MOVE ZERO        TO DLEV-KDORDBEK-UT                      
262200              END-IF                                                      
262300            END-IF                                                        
262400          END-IF                                                          
262500        END-IF                                                            
262600     END-IF                                                               
262700                                                                          
262800     IF ORAD-IDBIL NOT = SPACE OR                                         
262900        ORAD-IDSYSTEM = 'SOFT'                                            
263000         CONTINUE                                                         
263100     ELSE                                                                 
263200       IF AREG-KDSORT = 'SW'                                              
263300          MOVE 67                   TO SPAR-KDORDBEK                      
263400          MOVE JA                   TO OBKR-SW                            
263500          MOVE NEJ                  TO ALLT-SW                            
263600          IF KOLLA-ERS-SW = JA                                            
263700            MOVE NEJ            TO KOLLA-ERS-SW                           
263800            MOVE ZERO            TO KERS-KDORDBEK                         
263900            PERFORM S02-RENSA-TILLK-TAB                                   
264000          END-IF                                                          
264100       END-IF                                                             
264200     END-IF                                                               
264300                                                                          
264400     END-IF                                                               
264500     .                                                                    
264600     EJECT                                                                
264700 ECJ-KOMPLETTERA-PRIS SECTION.                                            
264800                                                                          
264900     MOVE 'STA ECJ-PRIS   '                TO   WS-PGM-POSITION           
265000     IF ALLT-OK OR KOLLA-ERS OR SPAR-FLPUBCDC = YES                       
265100                                                                          
265200     IF DIST79-DEALER-PRICE                                               
265300       IF ORAD-PRARTNTO-LOC = +0    OR                                    
265400          ORAD-PRARTBTO-LOC = +0    OR                                    
265500          ORAD-KDVALISO     = SPACE OR                                    
265600          ORAD-KDVALISO     = WS-KDVALISO-NA OR                           
265700          ORAD-KDVAT        = SPACE OR                                    
265800          ORAD-RERAB        = +0    OR                                    
265900          ORAD-KDRAB        = SPACE OR                                    
266000          ORAD-BEART-VIPS   = SPACE                                       
266100                                                                          
266200         IF WS-IDPRQUES                = +0                               
266300           MOVE WS-IDPRQUES           TO PRNO-IDPRQUES-IN                 
266400           MOVE +1                    TO PRNO-KDCALL                      
266500                                                                          
266600           CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                
266700                                                                          
266800           MOVE PRNO-IDPRQUES-UT      TO PRQU-IDPRQUES                    
266900                                         WS-IDPRQUES                      
267000           MOVE +1                    TO PRQU-KDCALL                      
267100         ELSE                                                             
267200           MOVE WS-IDPRQUES           TO PRNO-IDPRQUES-IN                 
267300           MOVE +2                    TO PRNO-KDCALL                      
267400                                                                          
267500           CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                
267600                                                                          
267700           MOVE PRNO-IDPRQUES-UT      TO PRQU-IDPRQUES                    
267800                                         WS-IDPRQUES                      
267900           MOVE +2                    TO PRQU-KDCALL                      
268000         END-IF                                                           
268100                                                                          
268200         MOVE W-IDDISTR                TO PRQU-IDDISTR                    
268300         MOVE W-IDKUNDNR               TO PRQU-IDKUNDNR                   
268400**       IF  TILLKOMMANDE-RAD                                             
268500         IF MID-IDKUNDRF-RO NOT = SPACE                                   
268600            MOVE MID-IDKUNDRF-RO       TO PRQU-IDKUNDRF                   
268700         ELSE                                                             
268800            MOVE W-IDKUNDRF            TO PRQU-IDKUNDRF                   
268900         END-IF                                                           
269000         MOVE ORAD-IDORDER             TO PRQU-IDORDER                    
269100         MOVE ORAD-KDORDKL             TO PRQU-KDORDKL                    
269200         MOVE 'N'                      TO PRQU-KDPRSTA                    
269300         MOVE ORAD-IDARTNR             TO PRQU-IDARTNR                    
269400         MOVE ORAD-KVBEART-Q           TO PRQU-KVBEART-Q                  
269500         MOVE ORAD-PRARTNTO-LOC        TO PRQU-PRARTNTO-LOC               
269600         MOVE +0                       TO PRQU-PRARTNTO-LOCPREL           
269700         MOVE ORAD-IDSYSTEM            TO PRQU-IDSYSTEM                   
269800                                                                          
269900         IF ORAD-KDVALISO  = SPACE OR WS-KDVALISO-NA                      
270000           PERFORM ECJB-HAEMTA-KDVALISO                                   
270100         END-IF                                                           
270200                                                                          
270300         MOVE ORAD-KDVALISO            TO PRQU-KDVALISO                   
270400                                                                          
270500         CALL W335PRQU USING PRQU-W335PRQU PRQU-WDG2-PCB                  
270600                                           PRQU-WDC7-PCB                  
270700                                           PRQU-SJKO-WDK6-PCB             
270800                                                                          
270900         MOVE PRQU-IDPRQUES            TO  ORAD-IDPRQUES                  
271000                                           WS-IDPRQUES                    
271100         MOVE PRQU-FLPRTILL            TO  ORAD-FLPRTILL                  
271200                                                                          
271300         IF ORAD-PRARTNTO-LOC = +0                                        
271400            MOVE PRQU-PRARTNTO-LOCPREL TO  ORAD-PRARTNTO-LOCPREL          
271500         END-IF                                                           
271600                                                                          
271700       END-IF                                                             
271800       IF ORAD-IDSYSTEM = 'OREL'  OR  'OVR '                              
271900          IF ORAD-PRARTNTO-LOC NOT = +0                                   
272000            IF ORAD-KDPRTYP = SPACE                                       
272100              MOVE 'P'            TO ORAD-KDPRTYP                         
272200              MOVE ORAD-TIREGDAT  TO ORAD-TIPRIS                          
272300            END-IF                                                        
272400          END-IF                                                          
272500       END-IF                                                             
272600     ELSE                                                                 
272700*      *NOT DIST79-DEALER-PRICE                                           
272800                                                                          
272900       PERFORM S40-HAMTA-WDB6-INFO                                        
273000                                                                          
273100       IF (ORAD-KDTPOTYP = +0   AND                                       
273200          (AREG-KDUART NOT = 'M' AND 'P' AND 'S' AND 'L'))   OR           
273300          (OHUV-FLOVRLEV = JA AND (AREG-KDUART = 'M' OR 'P'  OR           
273400                                                'S' OR 'L')) OR           
273500          ((OHUV-IDSYSTEM = 'LDCB' OR 'LYNB' OR 'ECOB' OR 'VOUB'          
273600                                   OR 'TADB' OR 'ACCB' OR 'APAB'          
273700                                   OR 'APBB' OR 'APCB' OR 'APDB'          
273800                                   OR 'APEB' OR 'APFB' OR 'APGB'          
273900                                   OR 'APHB' OR 'APIB' OR 'APJB')         
274000          AND                                                             
274100          (AREG-KDUART = 'M' OR 'P' OR 'S' OR 'L')) OR                    
274200           DIST35-RETUR OR                                                
274300           DCS-NDC OR DCS-DDC OR DCS-SDC OR                               
274400           DIST35-REFILL OR                                               
274500           DIST35-REFILL-NA-JAP OR                                        
274600           DIST35-NONVCC-REFILL OR                                        
274700           DIST35-NONVCC-NONVCC-TRANSFER OR                               
274800           DIST35-NONVCC-VCC-TRANSFER OR                                  
274900           DIST18-SKROT                                                   
275000                                                                          
276100           IF ORAD-PRARTNTO NOT = +0                                      
276200             IF OHUV-IDSYSTEM = 'OREL'                                    
276300                AND DIST11-PROFORMA-DUBAI                                 
276400                AND ORAD-IDDC = WC-NDC-AE                                 
276500*            *FETCH PRAVCOST FROM W335PRIS                                
276600               MOVE 1             TO PRIS-KDCALL                          
276700             ELSE                                                         
276800*            *FETCH ONLY KDVALISO FROM W335PRIS                           
276900               MOVE 2                  TO PRIS-KDCALL                     
277000             END-IF                                                       
277100           ELSE                                                           
277200*            *FETCH PRARTNTO/PRAVCOST AND KDVALISO FRON W335PRIS          
277300             MOVE 1                  TO PRIS-KDCALL                       
277400           END-IF                                                         
277600       ELSE                                                               
277700           MOVE 2                  TO PRIS-KDCALL                         
277800       END-IF                                                             
277900                                                                          
278000       MOVE IDPGM                TO PRIS-IDPGM                            
278100       MOVE ORAD-IDARTNR         TO PRIS-IDARTNR                          
278200       MOVE ORAD-IDDISTR         TO PRIS-IDDISTR                          
278300       MOVE ORAD-IDKUNDNR        TO PRIS-IDKUNDNR                         
278400       MOVE ORAD-IDDC            TO PRIS-IDDC                             
278500       IF OHUV-FLOVRLEV = JA                                              
278600*********FIX FÖR ÖVERLEVERANS FÖR ATT FÖRHINDRA KL-1 PÅLÄGG               
278700         MOVE 4                  TO PRIS-KDORDKL                          
278800       ELSE                                                               
278900         MOVE OHUV-KDORDKL       TO PRIS-KDORDKL                          
279000       END-IF                                                             
279100       MOVE ORAD-KVBEART-Q       TO PRIS-KVBEART                          
279200       MOVE ORAD-FLINVEST        TO PRIS-FLINVEST                         
279300                                                                          
279400       CALL W335PRIS USING PRIS-W335PRIS PRIS-ARTC-PCB                    
279500                           PRIS-WDK7-PCB                                  
279600                           PRIS-GMTA-PCB PRIS-BETA-PCB                    
279700                           PRIS-GPRIA-PCB PRIS-GPRIB-PCB                  
279800                           PRIS-COST-WDK6-PCB                             
279900                           PRIS-COST-WDK7-PCB                             
280000                           PRIS-COST-WDF1-PCB                             
280100                           PRIS-COST-9305-PCB                             
280200                           PRIS-COST-WDK72-PCB                            
280300                           PRIS-COST-WDB6-PCB                             
280400                                                                          
280500       IF PRIS-KDSVAR = '2'                                               
280600         MOVE 'DIST/KUND SAKNAS NÄR W335PRIS LÄSER KUNDREG'               
280700                             TO FELTEXT                                   
280800         CALL ABEND USING RKOD-ABEND                                      
280900       END-IF                                                             
281000                                                                          
281100       IF PRIS-KDCALL = 2                                                 
281200         MOVE PRIS-KDVALISO  TO ORAD-KDVALISO                             
281300         IF ORAD-KDPRTYP = SPACE                                          
281400           MOVE 'P'           TO ORAD-KDPRTYP                             
281500           MOVE ORAD-TIREGDAT TO ORAD-TIPRIS                              
281600         END-IF                                                           
281700       ELSE                                                               
282300         IF OHUV-IDSYSTEM = 'OREL' AND                                    
282400            DIST11-PROFORMA-DUBAI  AND                                    
282500            ORAD-IDDC = WC-NDC-AE  AND                                    
282600            ORAD-PRARTNTO NOT = 0                                         
282700*          *OLD NETTOPRICE FROM PROFORMA IS VALID                         
282800           CONTINUE                                                       
282900         ELSE                                                             
283000           MOVE PRIS-PRARTNTO TO ORAD-PRARTNTO                            
283100         END-IF                                                           
283300         MOVE PRIS-FLPRTILL  TO ORAD-FLPRTILL                             
283400         MOVE PRIS-KDPRTYP   TO ORAD-KDPRTYP                              
283500         MOVE PRIS-PRBPRIS   TO ORAD-PRBPRIS                              
283600         MOVE ORAD-TIREGDAT  TO ORAD-TIPRIS                               
283700         MOVE PRIS-KDVALISO  TO ORAD-KDVALISO                             
283800         MOVE PRIS-PRAVCOST  TO ORAD-PRAVCOST                             
283900       END-IF                                                             
284000     END-IF                                                               
284100     END-IF                                                               
284200     .                                                                    
284300     EJECT                                                                
284400                                                                          
284500                                                                          
284600 ECJB-HAEMTA-KDVALISO      SECTION.                                       
284700                                                                          
284800      MOVE 'STA ECJB-HAEMTA'               TO   WS-PGM-POSITION           
284900                                                                          
285000      MOVE GMT-IDPARTNR              TO W-WDB1-IDPARTNR                   
285100      MOVE GMT-IDFTG                 TO W-WDB1-IDFTG                      
285200                                                                          
285300      PERFORM IMS-GU-WDB101                                               
285400      MOVE BET-KDVALISO              TO ORAD-KDVALISO                     
285500      .                                                                   
285600                                                                          
285700 ECM-KOMPLETTERA-TPO1 SECTION.                                            
285800                                                                          
285900     PERFORM S40-HAMTA-WDB6-INFO                                          
286000     MOVE 'STA ECM-TPO1   '                TO   WS-PGM-POSITION           
286100     IF ALLT-OK AND ( DCS-CDC OR DCS-CDC-TR OR                            
286200                     DCS-SDC)                                             
286300        MOVE ZERO                             TO TPO1-KDORDBEK            
286400        MOVE ZERO                             TO TPO1-KVANNANT            
286500        MOVE NEJ                              TO TPO1-FLKLAR              
286600        IF ORAD-IDSYSTEM NOT = 'OREL'                                     
286700                                                                          
286800          IF ORAD-IDBIL = SPACE OR ORAD-IDSYSTEM = 'SOFT'                 
286900                                                                          
287000            IF OHUV-FLORDSPE NOT = JA AND OHUV-FLOVRLEV NOT = JA          
287100               IF ORAD-KDTPOTYP = 1 AND OHUV-FLFORBI = NEJ                
287200                  MOVE ORAD-IDDISTR           TO TEST-IDDISTR             
287300                  IF AREG-FLTPO1 = JA OR                                  
287400                     ((DIST23-TPO1) AND AREG-FLTPO1 = NEJ)                
287500                     MOVE ORAD-IDDISTR        TO TPO1-IDDISTR             
287600                     MOVE ORAD-IDKUNDNR       TO TPO1-IDKUNDNR            
287700                     MOVE ORAD-IDKUNDRF       TO TPO1-IDKUNDRF            
287800                     MOVE ORAD-IDARTNR        TO TPO1-IDARTNR             
287900                     MOVE ORAD-BERADREF       TO TPO1-BERADREF            
288000                     MOVE AREG-IDANSK         TO TPO1-IDANSK              
288100                     MOVE OHUV-IDKONTO        TO TPO1-IDKONTO             
288200                     MOVE OHUV-IDKST          TO TPO1-IDKST               
288300                     MOVE OHUV-IDANALYS       TO TPO1-IDANALYS            
288400                     MOVE ORAD-KDDSP          TO TPO1-KDDSP               
288500                     MOVE OHUV-KDFAKTYP       TO TPO1-KDFAKTYP            
288600                     MOVE ARB-KDFRAKT         TO TPO1-KDFRAKT             
288700                     MOVE ORAD-KDKVBRYT       TO TPO1-KDKVBRYT            
288800                     MOVE ORAD-KDORDING       TO TPO1-KDORDING            
288900                     MOVE OHUV-KDORDKL        TO TPO1-KDORDKL             
289000                     MOVE AREG-KDPRODSL       TO TPO1-KDPRODSL            
289100                     MOVE ORAD-KDVRINFO       TO TPO1-KDVRINFO            
289200                     MOVE ORAD-KVBEART-Q      TO TPO1-KVBEART-Q           
289300                     MOVE AREG-REKSIFFR       TO TPO1-REKSIFFR            
289400                     MOVE ORAD-TITPO          TO TPO1-TITPO               
289500                     IF ORAD-KDPRTYP = 'P'                                
289600                       MOVE ORAD-PRARTNTO     TO TPO1-PRARTNTO            
289700                       MOVE ORAD-DEAL-PR-LINE TO TPO1-DEAL-PR-LINE        
289800                       MOVE ORAD-KDPRTYP      TO TPO1-KDPRTYP             
289900                       MOVE ORAD-FLPRTILL     TO TPO1-FLPRTILL            
290000                     ELSE                                                 
290100                       MOVE ORAD-DEAL-PR-LINE TO TPO1-DEAL-PR-LINE        
290200                       MOVE ZERO          TO TPO1-PRARTNTO                
290300                       MOVE SPACE         TO TPO1-KDPRTYP                 
290400                       MOVE NEJ           TO TPO1-FLPRTILL                
290500                     END-IF                                               
290600                     MOVE ORAD-BEVOLREF       TO TPO1-BEVOLREF            
290700                     MOVE ORAD-IDKAMPRF       TO TPO1-IDKAMPRF            
290800                     MOVE ORAD-IDSYSTEM       TO TPO1-IDSYSTEM            
290900                     MOVE ORAD-FLINVEST       TO TPO1-FLINVEST            
291000                     MOVE ORAD-IDLEVNR        TO TPO1-IDLEVNR             
291100                     MOVE AREG-FLTPO1         TO TPO1-FLTPO1              
291200                     MOVE AREG-KVFRYSTI       TO TPO1-KVFRYSTI            
291300                     MOVE 1                   TO TPO1-KDORDBEH            
291400                     MOVE OHUV-FLFORBI        TO TPO1-FLFORBI             
291500                     MOVE ORAD-KDTPOTYP       TO TPO1-KDTPOTYP            
291600                     MOVE OHUV-BEKUNDRF       TO TPO1-BEKUNDRF            
291700                     MOVE OHUV-FLORDSPE       TO TPO1-FLORDSPE            
291800                     MOVE OHUV-FLOVRLEV       TO TPO1-FLOVRLEV            
291900                     MOVE OHUV-KDORDTYP-LDC   TO TPO1-KDORDTYP-LDC        
292000                     MOVE OHUV-TIREPDAT       TO TPO1-TIREPDAT            
292100                     MOVE ORAD-IDKUNDRF-WIP                               
292200                                              TO TPO1-IDKUNDRF-WIP        
292300                                                                          
292400                     CALL W411TPO1 USING TPO1-W411TPO1                    
292500                          TPO1-ORDP-PCB                                   
292600                          TPO1-ARTM-PCB TPO1-ZZAC-PCB                     
292700                  ELSE                                                    
292800                     MOVE 73                  TO TPO1-KDORDBEK            
292900                     MOVE WC-CDC-SE           TO ORAD-IDDC                
293000                                                  WS-IDDC-SEEK            
293100                  END-IF                                                  
293200                  IF TPO1-KDORDBEK > 0                                    
293300                     MOVE JA               TO OBKR-SW                     
293400                     MOVE NEJ              TO ALLT-SW                     
293500                     MOVE WC-CDC-SE        TO ORAD-IDDC                   
293600                                               WS-IDDC-SEEK               
293700                     IF KVAN-KDORDBEK-UT > +0                             
293800                        MOVE 0             TO KVAN-KDORDBEK-UT            
293900                        MOVE ORAD-KVBEART  TO ORAD-KVBEART-Q              
294000                     END-IF                                               
294100                     IF DLEV-KDORDBEK-UT > +0                             
294200                        MOVE 0             TO DLEV-KDORDBEK-UT            
294300                     END-IF                                               
294400                  ELSE                                                    
294500                     IF TPO1-FLKLAR = JA                                  
294600                        MOVE NEJ           TO ALLT-SW                     
294700                        IF KERS-KDORDBEK > ZERO AND                       
294800                           EJ-TILLKOMMANDE-RAD                            
294900                           MOVE ZERO       TO KERS-KDORDBEK               
295000                           PERFORM S02-RENSA-TILLK-TAB                    
295100                           MOVE NEJ        TO TILLK-SW                    
295200                        END-IF                                            
295300                     END-IF                                               
295400                  END-IF                                                  
295500               END-IF                                                     
295600            END-IF                                                        
295700                                                                          
295800          END-IF                                                          
295900                                                                          
296000        END-IF                                                            
296100                                                                          
296200     END-IF                                                               
296300     .                                                                    
296400     EJECT                                                                
296500 ECN-KOMPLETTERA-TPO2 SECTION.                                            
296600                                                                          
296700     MOVE 'STA ECN-TPO2   '                TO   WS-PGM-POSITION           
296800                                                                          
296900     PERFORM S40-HAMTA-WDB6-INFO                                          
297000     IF ALLT-OK AND (DCS-CDC OR DCS-CDC-TR OR                             
297100                     DCS-SDC)                                             
297200        MOVE ZERO                             TO TPO2-KDORDBEK            
297300        MOVE NEJ                              TO TPO2-FLKLAR              
297400        MOVE ORAD-IDDISTR                     TO TEST-IDDISTR             
297500        IF ORAD-IDSYSTEM NOT = 'OREL'                                     
297600                                                                          
297700          IF ORAD-IDBIL = SPACE OR ORAD-IDSYSTEM = 'SOFT'                 
297800                                                                          
297900            IF OHUV-FLORDSPE NOT = JA AND OHUV-FLOVRLEV NOT = JA          
298000              AND OHUV-FLFORBI = NEJ AND NOT DIST35-REFILL                
298100                                   AND NOT DIST35-NONVCC-REFILL           
298200              AND NOT DIST35-NONVCC-NONVCC-TRANSFER                       
298300              AND NOT DIST35-NONVCC-VCC-TRANSFER                          
298400                                   AND NOT DIST35-REFILL-INOM-NDC         
298500                                   AND NOT DIST35-RETUR                   
298600              IF (AREG-KDUART NOT = SPACE AND                             
298700                  OHUV-KDORDKL NOT = 0) OR                                
298800                  ORAD-KDTPOTYP = 2                                       
298900                                                                          
299000                  MOVE ORAD-IDDISTR         TO TPO2-IDDISTR               
299100                  MOVE ORAD-IDKUNDNR        TO TPO2-IDKUNDNR              
299200                  MOVE ORAD-IDKUNDRF        TO TPO2-IDKUNDRF              
299300                  MOVE ORAD-IDARTNR         TO TPO2-IDARTNR               
299400                  MOVE ORAD-BERADREF        TO TPO2-BERADREF              
299500                  MOVE AREG-IDANSK          TO TPO2-IDANSK                
299600                  MOVE OHUV-IDKONTO         TO TPO2-IDKONTO               
299700                  MOVE OHUV-IDKST           TO TPO2-IDKST                 
299800                  MOVE OHUV-IDANALYS        TO TPO2-IDANALYS              
299900                  MOVE ORAD-KDDSP           TO TPO2-KDDSP                 
300000                  MOVE OHUV-KDFAKTYP        TO TPO2-KDFAKTYP              
300100                  MOVE ARB-KDFRAKT          TO TPO2-KDFRAKT               
300200                  MOVE ORAD-KDKVBRYT        TO TPO2-KDKVBRYT              
300300                  MOVE ORAD-KDORDING        TO TPO2-KDORDING              
300400                  MOVE OHUV-KDORDKL         TO TPO2-KDORDKL               
300500                  MOVE AREG-KDPRODSL        TO TPO2-KDPRODSL              
300600                  MOVE ORAD-KDVRINFO        TO TPO2-KDVRINFO              
300700                  MOVE ORAD-KVBEART-Q       TO TPO2-KVBEART-Q             
300800                  MOVE AREG-REKSIFFR        TO TPO2-REKSIFFR              
300900                  MOVE ORAD-TITPO           TO TPO2-TITPO                 
301000                  MOVE ORAD-PRARTNTO        TO TPO2-PRARTNTO              
301100                  MOVE ORAD-DEAL-PR-LINE    TO TPO2-DEAL-PR-LINE          
301200                  MOVE ORAD-KDPRTYP         TO TPO2-KDPRTYP               
301300                  MOVE ORAD-FLPRTILL        TO TPO2-FLPRTILL              
301400                  MOVE ORAD-BEVOLREF        TO TPO2-BEVOLREF              
301500                  MOVE ORAD-IDKAMPRF        TO TPO2-IDKAMPRF              
301600                  MOVE ORAD-IDSYSTEM        TO TPO2-IDSYSTEM              
301700                  MOVE ORAD-FLINVEST        TO TPO2-FLINVEST              
301800                  MOVE ORAD-IDLEVNR         TO TPO2-IDLEVNR               
301900                  MOVE AREG-KDUART          TO TPO2-KDUART                
302000                  MOVE AREG-KVFRYSTI        TO TPO2-KVFRYSTI              
302100*    SÄTT KDORDBEH TILL 8 FÖR BATCH/MPP. DETTA INNEBÄR ATT EN             
302200*    KDUART-ARTIKEL MED EN GÅNG KOMMER ATT LÄGGAS UPP PÅ RESTORDER        
302300*    REGISTER OCH ANSKAFFNINGSLARM SKAPAS.                                
302400                  MOVE 8                    TO TPO2-KDORDBEH              
302500                  MOVE ORAD-KDTPOTYP        TO TPO2-KDTPOTYP              
302600                  MOVE OHUV-BEKUNDRF        TO TPO2-BEKUNDRF              
302700                  MOVE OHUV-FLORDSPE        TO TPO2-FLORDSPE              
302800                  MOVE OHUV-FLOVRLEV        TO TPO2-FLOVRLEV              
302900                  MOVE OHUV-FLFORBI         TO TPO2-FLFORBI               
303000                  MOVE ORAD-FLTILLK         TO TPO2-FLTILLK               
303100                  MOVE AREG-TIDISPIN        TO TPO2-TIDISPIN              
303200                  MOVE OHUV-BEVARREF        TO TPO2-BEVARREF              
303300*                 MOVE KVAN-KVQPACK-UT      TO TPO2-KVQPACK-1             
303400                  IF KVAN-KVQPACK-UT NUMERIC                              
303500                     MOVE KVAN-KVQPACK-UT   TO TPO2-KVQPACK-1             
303600                  ELSE                                                    
303700                     MOVE ZERO              TO TPO2-KVQPACK-1             
303800                  END-IF                                                  
303900                  MOVE ORAD-KVBEART         TO TPO2-KVBEART               
304000                  MOVE OHUV-KDORDTYP-LDC    TO TPO2-KDORDTYP-LDC          
304100                  MOVE OHUV-TIREPDAT        TO TPO2-TIREPDAT              
304200                  MOVE ORAD-IDKUNDRF-WIP    TO TPO2-IDKUNDRF-WIP          
304300                                                                          
304400                  CALL W411TPO2 USING TPO2-W411TPO2                       
304500                         TPO2-ORDP-PCB                                    
304600                         TPO2-XXBU-PCB TPO2-XXBV-PCB                      
304700                         TPO2-ARTM-PCB TPO2-FILA-PCB TPO2-XXBX-PCB        
304800                         TIME-4437-PCB                                    
304900                  IF TPO2-KDORDBEK > 0                                    
305000                     MOVE JA                TO OBKR-SW                    
305100                     MOVE NEJ               TO ALLT-SW                    
305200                     MOVE WC-CDC-SE         TO ORAD-IDDC                  
305300                                               WS-IDDC-SEEK               
305400                     MOVE TPO2-KDTPOTYP     TO ORAD-KDTPOTYP              
305500                     IF ORAD-KDTPOTYP = 6                                 
305600                        PERFORM S21-UPPDATERA-TPO6                        
305700                     END-IF                                               
305800                  ELSE                                                    
305900                     IF TPO2-FLKLAR = JA                                  
306000                        MOVE NEJ            TO ALLT-SW                    
306100                        IF KERS-KDORDBEK > ZERO AND                       
306200                           EJ-TILLKOMMANDE-RAD                            
306300                           MOVE ZERO        TO KERS-KDORDBEK              
306400                           PERFORM S02-RENSA-TILLK-TAB                    
306500                           MOVE NEJ         TO TILLK-SW                   
306600                        END-IF                                            
306700                     END-IF                                               
306800                  END-IF                                                  
306900               END-IF                                                     
307000            END-IF                                                        
307100                                                                          
307200          END-IF                                                          
307300                                                                          
307400         END-IF                                                           
307500     END-IF                                                               
307600     .                                                                    
307700     EJECT                                                                
307800 ECO-KOMPLETTERA-TPO3 SECTION.                                            
307900                                                                          
308000     MOVE 'STA ECO-TPO3   '                TO   WS-PGM-POSITION           
308100                                                                          
308200     PERFORM S40-HAMTA-WDB6-INFO                                          
308300     IF ALLT-OK AND (DCS-CDC OR DCS-CDC-TR OR                             
308400                     DCS-SDC)                                             
308500        MOVE ZERO                             TO TPO3-KDORDBEK            
308600        MOVE NEJ                              TO TPO3-FLKLAR              
308700        IF ORAD-IDSYSTEM NOT = 'OREL'                                     
308800                                                                          
308900          IF ORAD-IDBIL = SPACE OR ORAD-IDSYSTEM = 'SOFT'                 
309000                                                                          
309100            IF ORAD-KDTPOTYP = 3 AND OHUV-FLFORBI = NEJ                   
309200                                                                          
309300               MOVE ORAD-IDDISTR        TO TPO3-IDDISTR                   
309400               MOVE ORAD-IDKUNDNR       TO TPO3-IDKUNDNR                  
309500               MOVE ORAD-IDKUNDRF       TO TPO3-IDKUNDRF                  
309600               MOVE ORAD-IDARTNR        TO TPO3-IDARTNR                   
309700               MOVE ORAD-BERADREF       TO TPO3-BERADREF                  
309800               MOVE AREG-IDANSK         TO TPO3-IDANSK                    
309900               MOVE OHUV-IDKONTO        TO TPO3-IDKONTO                   
310000               MOVE OHUV-IDKST          TO TPO3-IDKST                     
310100               MOVE OHUV-IDANALYS       TO TPO3-IDANALYS                  
310200               MOVE ORAD-KDDSP          TO TPO3-KDDSP                     
310300               MOVE OHUV-KDFAKTYP       TO TPO3-KDFAKTYP                  
310400               MOVE ARB-KDFRAKT         TO TPO3-KDFRAKT                   
310500               MOVE ORAD-KDKVBRYT       TO TPO3-KDKVBRYT                  
310600               MOVE ORAD-KDORDING       TO TPO3-KDORDING                  
310700               MOVE OHUV-KDORDKL        TO TPO3-KDORDKL                   
310800               MOVE AREG-KDPRODSL       TO TPO3-KDPRODSL                  
310900               MOVE ORAD-KDVRINFO       TO TPO3-KDVRINFO                  
311000               MOVE ORAD-KVBEART-Q      TO TPO3-KVBEART-Q                 
311100               MOVE AREG-REKSIFFR       TO TPO3-REKSIFFR                  
311200               MOVE ORAD-TITPO          TO TPO3-TITPO                     
311300               IF ORAD-KDPRTYP = 'P'                                      
311400                  MOVE ORAD-PRARTNTO    TO TPO3-PRARTNTO                  
311500                  MOVE ORAD-DEAL-PR-LINE TO TPO3-DEAL-PR-LINE             
311600                  MOVE ORAD-KDPRTYP     TO TPO3-KDPRTYP                   
311700                  MOVE ORAD-FLPRTILL    TO TPO3-FLPRTILL                  
311800               ELSE                                                       
311900                  MOVE ORAD-DEAL-PR-LINE TO TPO3-DEAL-PR-LINE             
312000                  MOVE ZERO             TO TPO3-PRARTNTO                  
312100                  MOVE SPACE            TO TPO3-KDPRTYP                   
312200                  MOVE NEJ              TO TPO3-FLPRTILL                  
312300               END-IF                                                     
312400               MOVE ORAD-BEVOLREF       TO TPO3-BEVOLREF                  
312500               MOVE ORAD-IDKAMPRF       TO TPO3-IDKAMPRF                  
312600               MOVE ORAD-IDSYSTEM       TO TPO3-IDSYSTEM                  
312700               MOVE ORAD-FLINVEST       TO TPO3-FLINVEST                  
312800               MOVE ORAD-IDLEVNR        TO TPO3-IDLEVNR                   
312900               MOVE ORAD-KDTPOTYP       TO TPO3-KDTPOTYP                  
313000               MOVE OHUV-BEKUNDRF       TO TPO3-BEKUNDRF                  
313100               MOVE OHUV-FLFORBI        TO TPO3-FLFORBI                   
313200               MOVE OHUV-KDORDTYP-LDC   TO TPO3-KDORDTYP-LDC              
313300               MOVE OHUV-TIREPDAT       TO TPO3-TIREPDAT                  
313400               MOVE ORAD-IDKUNDRF-WIP   TO TPO3-IDKUNDRF-WIP              
313500                                                                          
313600               CALL W411TPO3 USING TPO3-W411TPO3                          
313700                          TPO3-ORDP-PCB TPO3-ZZAC-PCB                     
313800                                                                          
313900               IF TPO3-KDORDBEK > 0                                       
314000                  MOVE JA               TO OBKR-SW                        
314100                  MOVE NEJ              TO ALLT-SW                        
314200                  MOVE WC-CDC-SE        TO ORAD-IDDC                      
314300                                            WS-IDDC-SEEK                  
314400                  IF KVAN-KDORDBEK-UT > 0                                 
314500                     MOVE ZERO          TO KVAN-KDORDBEK-UT               
314600                     MOVE ORAD-KVBEART  TO ORAD-KVBEART-Q                 
314700                  END-IF                                                  
314800                  IF DLEV-KDORDBEK-UT > 0                                 
314900                     MOVE ZERO          TO DLEV-KDORDBEK-UT               
315000                  END-IF                                                  
315100               ELSE                                                       
315200                  IF TPO3-FLKLAR = JA                                     
315300                     MOVE NEJ           TO ALLT-SW                        
315400                     IF KERS-KDORDBEK > ZERO AND                          
315500                        EJ-TILLKOMMANDE-RAD                               
315600                        MOVE ZERO       TO KERS-KDORDBEK                  
315700                        PERFORM S02-RENSA-TILLK-TAB                       
315800                        MOVE NEJ        TO TILLK-SW                       
315900                     END-IF                                               
316000                  END-IF                                                  
316100               END-IF                                                     
316200            END-IF                                                        
316300                                                                          
316400          END-IF                                                          
316500                                                                          
316600        END-IF                                                            
316700                                                                          
316800     END-IF                                                               
316900     .                                                                    
317000     EJECT                                                                
317100 ECP-KOMPLETTERA-KAMPANJER SECTION.                                       
317200                                                                          
317300     MOVE 'STA ECP-KAMP   '                TO   WS-PGM-POSITION           
317400                                                                          
317500     PERFORM S40-HAMTA-WDB6-INFO                                          
317600     IF ALLT-OK AND (DCS-CDC OR DCS-CDC-TR OR                             
317700                     DCS-SDC)                                             
317800        MOVE ZERO                             TO KAMP-KDORDBEK            
317900        MOVE NEJ                              TO KAMP-FLKLAR              
318000        IF ORAD-IDSYSTEM NOT = 'OREL'                                     
318100                                                                          
318200          IF ORAD-IDBIL = SPACE OR ORAD-IDSYSTEM = 'SOFT'                 
318300                                                                          
318400            IF OHUV-FLORDSPE NOT = JA AND OHUV-FLOVRLEV NOT = JA          
318500               IF ORAD-IDKAMPRF > 0 AND OHUV-FLFORBI = NEJ                
318600                                                                          
318700                  MOVE ORAD-IDDISTR        TO KAMP-IDDISTR                
318800                  MOVE ORAD-IDKUNDNR       TO KAMP-IDKUNDNR               
318900                  MOVE ORAD-IDKUNDRF       TO KAMP-IDKUNDRF               
319000                  MOVE ORAD-IDARTNR        TO KAMP-IDARTNR                
319100                  MOVE ORAD-BERADREF       TO KAMP-BERADREF               
319200                  MOVE AREG-IDANSK         TO KAMP-IDANSK                 
319300                  MOVE OHUV-IDKONTO        TO KAMP-IDKONTO                
319400                  MOVE OHUV-IDKST          TO KAMP-IDKST                  
319500                  MOVE ORAD-KDDSP          TO KAMP-KDDSP                  
319600                  MOVE OHUV-KDFAKTYP       TO KAMP-KDFAKTYP               
319700                  MOVE ARB-KDFRAKT         TO KAMP-KDFRAKT                
319800                  MOVE ORAD-KDKVBRYT       TO KAMP-KDKVBRYT               
319900                  MOVE ORAD-KDORDING       TO KAMP-KDORDING               
320000                  MOVE OHUV-KDORDKL        TO KAMP-KDORDKL                
320100                  MOVE AREG-KDPRODSL       TO KAMP-KDPRODSL               
320200                  MOVE ORAD-KDVRINFO       TO KAMP-KDVRINFO               
320300                  MOVE ORAD-KVBEART-Q      TO KAMP-KVBEART-Q              
320400                  MOVE AREG-REKSIFFR       TO KAMP-REKSIFFR               
320500                  MOVE ORAD-TITPO          TO KAMP-TITPO                  
320600                  IF ORAD-KDPRTYP = 'P' OR ORAD-KDTPOTYP = +0             
320700                     MOVE ORAD-PRARTNTO    TO KAMP-PRARTNTO               
320800                     MOVE ORAD-DEAL-PR-LINE TO KAMP-DEAL-PR-LINE          
320900                     MOVE ORAD-KDPRTYP     TO KAMP-KDPRTYP                
321000                     MOVE ORAD-FLPRTILL    TO KAMP-FLPRTILL               
321100                  ELSE                                                    
321200                     MOVE ORAD-DEAL-PR-LINE TO KAMP-DEAL-PR-LINE          
321300                     MOVE ZERO             TO KAMP-PRARTNTO               
321400                     MOVE SPACE            TO KAMP-KDPRTYP                
321500                     MOVE NEJ              TO KAMP-FLPRTILL               
321600                  END-IF                                                  
321700                  MOVE ORAD-BEVOLREF       TO KAMP-BEVOLREF               
321800                  MOVE ORAD-FLINVEST       TO KAMP-FLINVEST               
321900                  MOVE OHUV-BEKUNDRF       TO KAMP-BEKUNDRF               
322000                  MOVE ORAD-IDKAMPRF       TO KAMP-IDKAMPRF               
322100                  MOVE ORAD-IDDC           TO KAMP-IDDC                   
322200                  MOVE ORAD-IDLEVNR        TO KAMP-IDLEVNR                
322300                  MOVE ORAD-IDSYSTEM       TO KAMP-IDSYSTEM               
322400                  MOVE AREG-KVFRYSTI       TO KAMP-KVFRYSTI               
322500                  MOVE ORAD-KDTPOTYP       TO KAMP-KDTPOTYP               
322600                  MOVE OHUV-FLFORBI        TO KAMP-FLFORBI                
322700                  MOVE OHUV-FLORDSPE       TO KAMP-FLORDSPE               
322800                  MOVE OHUV-FLOVRLEV       TO KAMP-FLOVRLEV               
322900                                                                          
323000                  CALL W411KAMP USING KAMP-W411KAMP KAMP-ORDP-PCB         
323100                          KAMP-ZZAC-PCB KAMP-WDM2-PCB                     
323200                  IF KAMP-KDORDBEK > +0                                   
323300                     MOVE JA               TO OBKR-SW                     
323400                     MOVE NEJ              TO ALLT-SW                     
323500                     MOVE WC-CDC-SE        TO ORAD-IDDC                   
323600                                               WS-IDDC-SEEK               
323700                     IF KVAN-KDORDBEK-UT > +0                             
323800                        MOVE +0            TO KVAN-KDORDBEK-UT            
323900                        MOVE ORAD-KVBEART  TO ORAD-KVBEART-Q              
324000                     END-IF                                               
324100                     IF DLEV-KDORDBEK-UT > +0                             
324200                        MOVE +0            TO DLEV-KDORDBEK-UT            
324300                     END-IF                                               
324400                  ELSE                                                    
324500                     IF KAMP-FLKLAR = JA                                  
324600                        MOVE NEJ           TO ALLT-SW                     
324700                        IF KERS-KDORDBEK > ZERO AND                       
324800                           EJ-TILLKOMMANDE-RAD                            
324900                           MOVE ZERO       TO KERS-KDORDBEK               
325000                           PERFORM S02-RENSA-TILLK-TAB                    
325100                           MOVE NEJ        TO TILLK-SW                    
325200                        END-IF                                            
325300                     END-IF                                               
325400                  END-IF                                                  
325500               END-IF                                                     
325600            END-IF                                                        
325700                                                                          
325800          END-IF                                                          
325900                                                                          
326000        END-IF                                                            
326100                                                                          
326200     END-IF                                                               
326300     .                                                                    
326400     EJECT                                                                
326500 ECX-KOMPLETTERA-PROFORMOR SECTION.                                       
326600                                                                          
326700     MOVE 'STA ECX-PROF   '                TO   WS-PGM-POSITION           
326800                                                                          
326900     PERFORM S40-HAMTA-WDB6-INFO                                          
327000     IF ALLT-OK AND (DCS-CDC OR DCS-CDC-TR OR                             
327100                     DCS-SDC)                                             
327200        MOVE ZERO                             TO TPO5-KDORDBEK            
327300        MOVE NEJ                              TO TPO5-FLKLAR              
327400        IF ORAD-KDTPOTYP = 5                                              
327500                                                                          
327600           MOVE ORAD-IDDISTR         TO TPO5-IDDISTR                      
327700           MOVE ORAD-IDKUNDNR        TO TPO5-IDKUNDNR                     
327800           MOVE ORAD-IDKUNDRF        TO TPO5-IDKUNDRF                     
327900           MOVE ORAD-IDARTNR         TO TPO5-IDARTNR                      
328000           MOVE ORAD-BERADREF        TO TPO5-BERADREF                     
328100           MOVE AREG-IDANSK          TO TPO5-IDANSK                       
328200           MOVE OHUV-IDKONTO         TO TPO5-IDKONTO                      
328300           MOVE OHUV-IDKST           TO TPO5-IDKST                        
328400           MOVE OHUV-IDANALYS        TO TPO5-IDANALYS                     
328500           MOVE ORAD-KDDSP           TO TPO5-KDDSP                        
328600           MOVE OHUV-KDFAKTYP        TO TPO5-KDFAKTYP                     
328700           MOVE ARB-KDFRAKT          TO TPO5-KDFRAKT                      
328800           MOVE ORAD-KDKVBRYT        TO TPO5-KDKVBRYT                     
328900           MOVE ORAD-KDORDING        TO TPO5-KDORDING                     
329000           MOVE OHUV-KDORDKL         TO TPO5-KDORDKL                      
329100           MOVE AREG-KDPRODSL        TO TPO5-KDPRODSL                     
329200           MOVE ORAD-KDVRINFO        TO TPO5-KDVRINFO                     
329300           MOVE ORAD-KVBEART-Q       TO TPO5-KVBEART-Q                    
329400           MOVE AREG-REKSIFFR        TO TPO5-REKSIFFR                     
329500           MOVE ORAD-TITPO           TO TPO5-TITPO                        
329600           IF ORAD-KDPRTYP = 'P'                                          
329700              MOVE ORAD-PRARTNTO     TO TPO5-PRARTNTO                     
329800              MOVE ORAD-DEAL-PR-LINE TO TPO5-DEAL-PR-LINE                 
329900              MOVE ORAD-KDPRTYP      TO TPO5-KDPRTYP                      
330000              MOVE ORAD-FLPRTILL     TO TPO5-FLPRTILL                     
330100           ELSE                                                           
330200              MOVE ORAD-DEAL-PR-LINE TO TPO5-DEAL-PR-LINE                 
330300              MOVE ZERO              TO TPO5-PRARTNTO                     
330400              MOVE SPACE             TO TPO5-KDPRTYP                      
330500              MOVE NEJ               TO TPO5-FLPRTILL                     
330600           END-IF                                                         
330700           MOVE ORAD-BEVOLREF        TO TPO5-BEVOLREF                     
330800           MOVE ORAD-IDKAMPRF        TO TPO5-IDKAMPRF                     
330900           MOVE ORAD-IDSYSTEM        TO TPO5-IDSYSTEM                     
331000           MOVE ORAD-FLINVEST        TO TPO5-FLINVEST                     
331100           MOVE ORAD-IDLEVNR         TO TPO5-IDLEVNR                      
331200           MOVE AREG-KVFRYSTI        TO TPO5-KVFRYSTI                     
331300           MOVE ORAD-KDTPOTYP        TO TPO5-KDTPOTYP                     
331400           MOVE OHUV-BEKUNDRF        TO TPO5-BEKUNDRF                     
331500           MOVE OHUV-KDORDTYP-LDC    TO TPO5-KDORDTYP-LDC                 
331600           MOVE OHUV-TIREPDAT        TO TPO5-TIREPDAT                     
331700           MOVE ORAD-IDKUNDRF-WIP    TO TPO5-IDKUNDRF-WIP                 
331800                                                                          
331900           CALL W411TPO5 USING TPO5-W411TPO5 TPO5-ORDP-PCB                
332000                               TPO5-ARTM-PCB                              
332100                                                                          
332200           IF TPO5-KDORDBEK > 0                                           
332300              MOVE JA                TO OBKR-SW                           
332400              MOVE NEJ               TO ALLT-SW                           
332500              MOVE WC-CDC-SE         TO ORAD-IDDC                         
332600                                        WS-IDDC-SEEK                      
332700              IF KVAN-KDORDBEK-UT > +0                                    
332800                 MOVE 0              TO KVAN-KDORDBEK-UT                  
332900                 MOVE ORAD-KVBEART   TO ORAD-KVBEART-Q                    
333000              END-IF                                                      
333100              IF DLEV-KDORDBEK-UT > +0                                    
333200                 MOVE 0              TO DLEV-KDORDBEK-UT                  
333300              END-IF                                                      
333400           END-IF                                                         
333500        END-IF                                                            
333600                                                                          
333700     END-IF                                                               
333800     .                                                                    
333900     EJECT                                                                
334000 ECT-KOMPLETTERA-RELEASESPARR SECTION.                                    
334100                                                                          
334200     MOVE +0                      TO RELS-KDORDBEK                        
334300                                                                          
334400     MOVE 'STA ECT-RELS     '              TO   WS-PGM-POSITION           
334500       IF ALLT-OK AND W-KDORDBEK = 56                                     
334600                                                                          
334700         MOVE ORAD-IDDISTR         TO RELS-IDDISTR                        
334800         MOVE ORAD-IDKUNDNR        TO RELS-IDKUNDNR                       
334900         MOVE ORAD-IDKUNDRF        TO RELS-IDKUNDRF                       
335000         MOVE ORAD-IDARTNR         TO RELS-IDARTNR                        
335100         MOVE ORAD-BERADREF        TO RELS-BERADREF                       
335200         MOVE AREG-IDANSK          TO RELS-IDANSK                         
335300         MOVE OHUV-IDKONTO         TO RELS-IDKONTO                        
335400         MOVE OHUV-IDKST           TO RELS-IDKST                          
335500         MOVE OHUV-IDANALYS        TO RELS-IDANALYS                       
335600         MOVE ORAD-KDDSP           TO RELS-KDDSP                          
335700         MOVE OHUV-KDFAKTYP        TO RELS-KDFAKTYP                       
335800         MOVE ARB-KDFRAKT          TO RELS-KDFRAKT                        
335900         MOVE ORAD-KDKVBRYT        TO RELS-KDKVBRYT                       
336000         MOVE ORAD-KDORDING        TO RELS-KDORDING                       
336100         MOVE OHUV-KDORDKL         TO RELS-KDORDKL                        
336200         MOVE AREG-KDPRODSL        TO RELS-KDPRODSL                       
336300         MOVE ORAD-KDVRINFO        TO RELS-KDVRINFO                       
336400         MOVE ORAD-KVBEART-Q       TO RELS-KVBEART-Q                      
336500         MOVE AREG-REKSIFFR        TO RELS-REKSIFFR                       
336600         MOVE MSGI-TILOKDAT        TO RELS-TITPO                          
336700         MOVE ORAD-PRARTNTO        TO RELS-PRARTNTO                       
336800         MOVE ORAD-PRAVCOST        TO RELS-PRAVCOST                       
336900         MOVE ORAD-DEAL-PR-LINE    TO RELS-DEAL-PR-LINE                   
337000         MOVE ORAD-KDPRTYP         TO RELS-KDPRTYP                        
337100         MOVE ORAD-FLPRTILL        TO RELS-FLPRTILL                       
337200         EJECT                                                            
337300         MOVE ORAD-BEVOLREF        TO RELS-BEVOLREF                       
337400         MOVE ORAD-IDKAMPRF        TO RELS-IDKAMPRF                       
337500         MOVE ORAD-IDSYSTEM        TO RELS-IDSYSTEM                       
337600         MOVE ORAD-FLINVEST        TO RELS-FLINVEST                       
337700         MOVE OHUV-FLORDSPE        TO RELS-FLORDSPE                       
337800         MOVE OHUV-FLOVRLEV        TO RELS-FLOVRLEV                       
337900         MOVE OHUV-FLFORBI         TO RELS-FLFORBI                        
338000         MOVE ORAD-IDLEVNR         TO RELS-IDLEVNR                        
338100         MOVE AREG-KDUART          TO RELS-KDUART                         
338200         MOVE AREG-KVFRYSTI        TO RELS-KVFRYSTI                       
338300         MOVE +1                   TO RELS-KDORDBEH                       
338400         MOVE W-KDTPOTYP           TO RELS-KDTPOTYP                       
338500         MOVE OHUV-BEKUNDRF        TO RELS-BEKUNDRF                       
338600         MOVE ORAD-FLTILLK         TO RELS-FLTILLK                        
338700         MOVE AREG-TIDISPIN        TO RELS-TIDISPIN                       
338800         MOVE OHUV-BEVARREF        TO RELS-BEVARREF                       
338900         MOVE 0                    TO RELS-KVQPACK-1                      
339000         MOVE ORAD-KVBEART         TO RELS-KVBEART                        
339100         MOVE W-KDORDBEK           TO RELS-KDORDBEK                       
339200                                                                          
339300         MOVE SPACE                TO RELS-FLKLAR                         
339400         MOVE OHUV-KDORDTYP-LDC    TO RELS-KDORDTYP-LDC                   
339500         MOVE OHUV-TIREPDAT        TO RELS-TIREPDAT                       
339600         MOVE ORAD-IDKUNDRF-WIP    TO RELS-IDKUNDRF-WIP                   
339700                                                                          
339800         CALL W411RELS USING RELS-W411RELS RELS-ORDP-PCB                  
339900                             RELS-FILA-PCB RELS-ARTM-PCB 2109-PCB         
340000                                                                          
340100         PERFORM ECTA-ANDRA-WDC711                                        
340200         IF RELS-KDORDBEK > +0                                            
340300            MOVE JA                TO OBKR-SW                             
340400            MOVE NEJ               TO ALLT-SW                             
340500            MOVE WC-CDC-SE         TO ORAD-IDDC                           
340600            MOVE ORAD-IDDC         TO WS-IDDC-SEEK                        
340700         ELSE                                                             
340800            IF RELS-FLKLAR = JA                                           
340900               MOVE NEJ            TO ALLT-SW                             
341000               IF KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD            
341100                  MOVE ZERO        TO KERS-KDORDBEK                       
341200                  PERFORM S02-RENSA-TILLK-TAB                             
341300               END-IF                                                     
341400            END-IF                                                        
341500         END-IF                                                           
341600                                                                          
341700         MOVE +0           TO W-KDORDBEK                                  
341800         MOVE SPACE        TO RELS-FLKLAR                                 
341900                                                                          
342000       END-IF                                                             
342100     .                                                                    
342200     EJECT                                                                
342300 ECTA-ANDRA-WDC711 SECTION.                                               
342400                                                                          
342500     IF W-KDTPOTYP = 7 AND RELS-FLOVRLEV NOT = JA                         
342600       IF ORAD-IDDISTR = 0778 AND OHUV-KDORDKL < 3                        
342700         INITIALIZE PRQU-W335PRQU                                         
342800         MOVE ORAD-IDDISTR       TO PRQU-IDDISTR                          
342900         MOVE ORAD-IDKUNDNR      TO PRQU-IDKUNDNR                         
343000         MOVE W-IDKUNDRF         TO PRQU-IDKUNDRF                         
343100         MOVE ORAD-IDPRQUES      TO PRQU-IDPRQUES                         
343200         MOVE 6                  TO PRQU-KDCALL                           
343300         CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                 
343400                                            PRQU-WDC7-PCB                 
343500                                            PRQU-SJKO-WDK6-PCB            
343600         MOVE 'N'                TO ORAD-FLPRTILL                         
343700       END-IF                                                             
343800     END-IF                                                               
343900     .                                                                    
344000     EJECT                                                                
344100 ECG-PREL-AVBOKNING-NDC SECTION.                                          
344200     MOVE 'STA ECG-PREL-AVB'                TO   WS-PGM-POSITION          
344300     IF (ALLT-OK OR KOLLA-ERS OR SPAR-FLPUBCDC = YES)                     
344400        AND OHUV-FLOVRLEV = NEJ                                           
344500                                                                          
344600       IF ORAD-IDBIL = SPACE OR ORAD-IDSYSTEM = 'SOFT'                    
344700                                                                          
344800         PERFORM S40-HAMTA-WDB6-INFO                                      
344900         IF DCS-NDC AND ORAD-IDLEVNR = SPACE                              
345000                                                                          
345100              MOVE JA TO ALLT-SW                                          
345200              PERFORM ECGX-PREL-AVBOKNING-XDC                             
345300                                                                          
345400*          MOVE OHUV-FLFORBI       TO NDCA-FLFORBI                        
345500*          MOVE GMT-FLLDCKND       TO NDCA-FLLDCKND                       
345600*          MOVE OHUV-FLPRELRO      TO NDCA-FLPRELRO                       
345700*          MOVE OHUV-FLRESTN       TO NDCA-FLRESTN                        
345800*          MOVE OHUV-FLORDSPE      TO NDCA-FLORDSPE                       
345900*          MOVE ORAD-IDARTNR       TO NDCA-IDARTNR                        
346000*          MOVE ORAD-IDDC          TO NDCA-IDDC                           
346100*          MOVE OHUV-IDDC-CLEAR-GRP TO NDCA-IDDC-CLEAR-GRP                
346200*          MOVE ORAD-CLEARGROUP    TO NDCA-CLEARGROUP                     
346300*          MOVE OHUV-IDDC-TVS      TO NDCA-IDDC-TVS                       
346400*          MOVE ORAD-IDDISTR       TO NDCA-IDDISTR                        
346500*          MOVE WS-IXDCCLEAR       TO NDCA-IXDCCLEAR                      
346600*          MOVE ORAD-KDARTURS      TO NDCA-KDARTURS                       
346700*          MOVE AREG-KDERS         TO NDCA-KDERS                          
346800*          MOVE ORAD-KDORDING      TO NDCA-KDORDING                       
346900*          MOVE ORAD-KDORDKL       TO NDCA-KDORDKL                        
347000*          MOVE AREG-KDSORT        TO NDCA-KDSORT                         
347100*          MOVE OHUV-KVDAGAR-DOW   TO NDCA-KVDAGAR-DOW                    
347200*          MOVE ORAD-KVBEART-Q     TO NDCA-KVBEART-Q                      
347300*          MOVE AREG-KVQPACK-1     TO NDCA-KVQPACK-1                      
347400*          MOVE ORAD-TIREGDAT      TO NDCA-TIREGDAT                       
347500*          MOVE ORAD-TIREGTID      TO NDCA-TIREGTID                       
347600*          MOVE ORAD-VKART         TO NDCA-VKART                          
347700*          MOVE ORAD-VKART-NTO     TO NDCA-VKART-NTO                      
347800*          MOVE ORAD-VLARTNTO      TO NDCA-VLARTNTO                       
347900*        UPDATE AND NO XDK7                                               
348000*        MOVE +1                   TO NDCA-KDCALL                         
348100*        UPDATE AND USE OF XDK7                                           
348200*        MOVE +3                   TO NDCA-KDCALL                         
348300*        RUNNING NDCA IN PASSIVE MODE                                     
348400*        MOVE +2                   TO NDCA-KDCALL                         
348500*        MOVE SPACE                TO NDCA-XDK7-IDDC                      
348600*        MOVE ZERO                 TO NDCA-XDK7-KDIDDC                    
348700*                                     NDCA-XDK7-KVOKS-DAG                 
348800*                                     NDCA-XDK7-KVOKS-BULK                
348900*                                                                         
349000*          CALL W411NDCA USING NDCA-W411NDCA CLDC-W411CLDC                
349100*                                            NDCA-USEA-PCB                
349200*                                            NDCA-WDK7-PCB                
349300*                                            NDCA-WDL6-PCB                
349400*                                            NDCA-WDB6-PCB                
349500*                                            NDCA-XDK7-W411XDK7           
349600*                                                                         
349700*                                                                         
349800*          IF OHUV-IDUSER = 'PCCN616'                                     
349900*             PERFORM ECGX-CHECK-DIFF                                     
350000*          END-IF                                                         
350100           IF XDCA-KDORDBEK > ZERO                                        
350200             IF SPAR-FLPUBCDC = YES                                       
350300***** SPAR-FLPUBCDC=YES MEANS THERE IS A BLOCK FOR CDC PUBL.DATE          
350400***** IF XDCA-KDORDBEK = 15 MEANS LINE IS MOVED TO ANOTHER DC             
350500***** THEN DAPUBL IS TESTED OK IN W411XDCA                                
350600              IF (XDCA-KDORDBEK = 15 OR 92 OR 98 OR 99)                   
350700                 AND XDCA-DAPUBL > ZERO                                   
350800                   MOVE ZERO TO SPAR-KDORDBEK                             
350900              ELSE                                                        
351000                   MOVE ZERO TO XDCA-KDORDBEK                             
351100                   IF XDCA-DAPUBL = ZERO                                  
351200                      MOVE 0              TO XDCA-KVPREAVB                
351300                                             XDCA-KVPRERO                 
351400                   END-IF                                                 
351500              END-IF                                                      
351600             END-IF                                                       
351700             IF KOLLA-ERS                                                 
351800                IF XDCA-KVPREAVB > 0                                      
351900                  MOVE ZERO          TO KERS-KDORDBEK                     
352000                  PERFORM S02-RENSA-TILLK-TAB                             
352100                  MOVE ZERO          TO SPAR-KDORDBEK                     
352200                ELSE                                                      
352300                  MOVE ZERO          TO XDCA-KDORDBEK                     
352400                END-IF                                                    
352500             ELSE                                                         
352600               IF XDCA-KDORDBEK = 15                                      
352700                  IF SDCA-KDORDBEK-FIRST-SDC = 15                         
352800                     MOVE ZERO       TO SDCA-KDORDBEK-FIRST-SDC           
352900                  END-IF                                                  
353000                  IF SDCA-KDORDBEK-SECOND-SDC = 15                        
353100                     MOVE ZERO       TO SDCA-KDORDBEK-SECOND-SDC          
353200                  END-IF                                                  
353300                  IF SDCA-KDORDBEK = 15                                   
353400                     MOVE ZERO       TO SDCA-KDORDBEK                     
353500                  END-IF                                                  
353600               END-IF                                                     
353700               IF KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD            
353800                   MOVE ZERO         TO XDCA-KDORDBEK                     
353900               END-IF                                                     
354000             END-IF                                                       
354100             MOVE JA                 TO OBKR-SW                           
354200           ELSE                                                           
354300             IF KOLLA-ERS  OR                                             
354400               (KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD)             
354500               IF XDCA-KVPREAVB > 0                                       
354600                 MOVE ZERO           TO KERS-KDORDBEK                     
354700                 PERFORM S02-RENSA-TILLK-TAB                              
354800                 MOVE ZERO           TO SPAR-KDORDBEK                     
354900               ELSE                                                       
355000                 MOVE JA             TO OBKR-SW                           
355100               END-IF                                                     
355200             ELSE                                                         
355300               IF XDCA-KVPREAVB > 0 AND SPAR-KDORDBEK ='54'               
355400                  MOVE ZERO             TO SPAR-KDORDBEK                  
355500               END-IF                                                     
355600             END-IF                                                       
355700             IF SPAR-FLPUBCDC = YES                                       
355800***** SPAR-FLPUBCDC=YES MEANS THERE IS A BLOCK FOR PUBL.DATE ON CD        
355900***** THAT BLOCK SHALL BE REMOVED IF CHECK ON DAPUBL FOR NDC IS OK        
356000               IF XDCA-DAPUBL > 0                                         
356100                 MOVE 0  TO SPAR-KDORDBEK                                 
356200                 MOVE JA  TO ALLT-SW                                      
356300                 MOVE NEJ TO OBKR-SW                                      
356400               ELSE                                                       
356500                 MOVE 0              TO XDCA-KVPREAVB                     
356600                                        XDCA-KVPRERO                      
356700               END-IF                                                     
356800             END-IF                                                       
356900           END-IF                                                         
357000           IF OHUV-FLFORBI = SPEC-FORBI                                   
357100              MOVE '07'              TO ORAD-ADLAGOMR                     
357200           ELSE                                                           
357300              MOVE XDCA-ADLAGOMR     TO ORAD-ADLAGOMR                     
357400           END-IF                                                         
357500           MOVE XDCA-ADGANG          TO ORAD-ADGANG                       
357600           MOVE XDCA-ADPLATS         TO ORAD-ADPLATS                      
357700           MOVE XDCA-IDDC-OUT        TO ORAD-IDDC                         
357800           MOVE XDCA-IDDC-RO         TO ORAD-IDDC-RO                      
357900           MOVE XDCA-KDARTURS        TO ORAD-KDARTURS                     
358000           MOVE XDCA-KDOI            TO ORAD-KDOI                         
358100           MOVE XDCA-CLEARGROUP      TO ORAD-CLEARGROUP                   
358200           MOVE XDCA-KVPREAVB        TO ORAD-KVPREAVB                     
358300           MOVE XDCA-KVPRERO         TO ORAD-KVPRERO                      
358400           MOVE XDCA-TIREGDAT-OUT    TO ORAD-TIREGDAT                     
358500           MOVE XDCA-TIREGTID-OUT    TO ORAD-TIREGTID                     
358600           MOVE XDCA-VKART-OUT       TO ORAD-VKART                        
358700           MOVE XDCA-VKART-NTO       TO ORAD-VKART-NTO                    
358800           MOVE XDCA-VLARTNTO        TO ORAD-VLARTNTO                     
358900           MOVE NEJ                  TO ALLT-SW                           
359000         END-IF                                                           
359100                                                                          
359200       END-IF                                                             
359300                                                                          
359400     END-IF                                                               
359500     .                                                                    
359600     EJECT                                                                
359700 ECGX-PREL-AVBOKNING-XDC SECTION.                                         
359800                                                                          
359900* XDCA-INPUT                                                              
360000      MOVE +1 TO WS-INDEX                                                 
360100      PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                             
360200        MOVE W-GMT-IDDC-CLEAR  (WS-INDEX)                                 
360300                                TO XDCA-IDDC-CLEAR-IN(WS-INDEX)           
360400        ADD +1 TO WS-INDEX                                                
360500      END-PERFORM                                                         
360600                                                                          
360700     MOVE OHUV-IDDC-TVS        TO XDCA-IDDC-TVS                           
360800     MOVE GMT-FLLDCKND         TO XDCA-FLLDCKND                           
360900     MOVE ORAD-IDARTNR         TO XDCA-IDARTNR                            
361000     MOVE ORAD-IDDC            TO XDCA-IDDC                               
361100     MOVE ORAD-IDLEVNR         TO XDCA-IDLEVNR                            
361200     MOVE AREG-KDERS           TO XDCA-KDERS                              
361300     MOVE AREG-KDSORT          TO XDCA-KDSORT                             
361400     MOVE ORAD-KVBEART-Q       TO XDCA-KVBEART-Q                          
361500     MOVE AREG-KVQPACK-1       TO XDCA-KVQPACK-1                          
361600     MOVE ORAD-TIREGDAT        TO XDCA-TIREGDAT                           
361700     MOVE ORAD-TIREGTID        TO XDCA-TIREGTID                           
361800     MOVE ORAD-VKART           TO XDCA-VKART                              
361900*    MOVE +4                   TO XDCA-KDCALL                             
362000*RUNNING XDCA IN ACTIVE MODE                                              
362100     MOVE +1                   TO XDCA-KDCALL                             
362200                                                                          
362300* XDCA-OUTPUT                                                             
362400     MOVE SPACE                TO XDCA-IDDC-OUT                           
362500                                  XDCA-IDDC-RO                            
362600                                  XDCA-KDARTURS                           
362700                                  XDCA-KDOI                               
362800                                  XDCA-CLEARGROUP                         
362900     MOVE ZERO                 TO XDCA-ADLAGOMR                           
363000                                  XDCA-ADGANG                             
363100                                  XDCA-ADPLATS                            
363200                                  XDCA-KDORDBEK                           
363300                                  XDCA-KVPREAVB                           
363400                                  XDCA-KVPRERO                            
363500                                  XDCA-TIREGDAT-OUT                       
363600                                  XDCA-TIREGTID-OUT                       
363700                                  XDCA-VKART-OUT                          
363800                                  XDCA-VKART-NTO                          
363900                                  XDCA-VLARTNTO                           
364100     MOVE ZERO                 TO XDCA-KVOKS-DAG                          
364200                                  XDCA-KVOKS-BULK                         
364300                                                                          
364310     IF XDCA-DAPUBL NOT = 99999999                                        
364320        MOVE ZERO          TO XDCA-DAPUBL                                 
364330     END-IF                                                               
364400     CALL W411XDCA USING OHUV-WDQ201 XDCA-W411XDCA                        
364500          XDCA-USEA-PCB                                                   
364600          XDCA-WDB6-PCB XDCA-WDK6-PCB XDCA-WDK7-PCB                       
364700          XDCA-WDK9-PCB XDCA-WDL6-PCB XDCA-WDQ4B-PCB                      
364800          XDCA-WDQ2-PCB XDCA-WDQ4-PCB XDCA-WDR6-PCB                       
364900          XDCA-WDB6-2-PCB XDCA-WDK6-2-PCB XDCA-WDK7-2-PCB                 
365000          XDCA-WDK7-3-PCB                                                 
365100                                                                          
365200* THIS IS JUST TO BE ABLE TO COMPARE THE RESULT WITH VALUES               
365300* FROM NDCA. IF VALU NOT = SPACE ORAD- VALUES SHULD BE OVERRIDDEN         
365400     IF XDCA-KDARTURS = SPACE                                             
365500        MOVE ORAD-KDARTURS  TO XDCA-KDARTURS                              
365600     END-IF                                                               
365700     IF XDCA-VKART-NTO = ZERO                                             
365800        MOVE ORAD-VKART-NTO TO XDCA-VKART-NTO                             
365900     END-IF                                                               
366000     IF XDCA-VLARTNTO = ZERO                                              
366100        MOVE ORAD-VLARTNTO  TO XDCA-VLARTNTO                              
366200     END-IF                                                               
366300     .                                                                    
366400     EJECT                                                                
366500 ECGX-CHECK-DIFF SECTION.                                                 
366600                                                                          
366700     IF  NDCA-KDORDBEK   = XDCA-KDORDBEK                                  
366800     AND NDCA-KVPREAVB   = XDCA-KVPREAVB                                  
366900     AND NDCA-ADLAGOMR   = XDCA-ADLAGOMR                                  
367000     AND NDCA-ADGANG     = XDCA-ADGANG                                    
367100     AND NDCA-ADPLATS    = XDCA-ADPLATS                                   
367200     AND NDCA-IDDC       = XDCA-IDDC-OUT                                  
367300     AND NDCA-IDDC-RO    = XDCA-IDDC-RO                                   
367400     AND NDCA-KDARTURS   = XDCA-KDARTURS                                  
367500     AND NDCA-KDOI       = XDCA-KDOI                                      
367600     AND NDCA-KVPRERO    = XDCA-KVPRERO                                   
367700     AND NDCA-VKART      = XDCA-VKART-OUT                                 
367800     AND NDCA-VKART-NTO  = XDCA-VKART-NTO                                 
367900     AND NDCA-VLARTNTO   = XDCA-VLARTNTO                                  
368000     AND NDCA-CLEARGROUP = XDCA-CLEARGROUP                                
368100     AND XDCA-KVOKS-DAG     = NDCA-XDK7-KVOKS-DAG                         
368200     AND XDCA-KVOKS-BULK    = NDCA-XDK7-KVOKS-BULK                        
368300         MOVE NEJ TO DIFF-FLSVAR                                          
368400     ELSE                                                                 
368500        MOVE JA           TO DIFF-FLSVAR                                  
368600     END-IF                                                               
368700                                                                          
368800     IF DIFF-FLSVAR = JA                                                  
368900* ORDER LOG INFO                                                          
369000        MOVE IDPGM                TO R6-FIL-IDPGM                         
369100        ACCEPT R6-FIL-TIREGDAT FROM DATE                                  
369200        ACCEPT R6-FIL-TIKLOCK  FROM TIME                                  
369300        MOVE 1                    TO R6-FIL-IDSEKVNR                      
369400        MOVE 'W414'               TO R6-FIL-CT-IDSYSTEM                   
369500        MOVE 'A'                  TO R6-FIL-CT-IDVTYP                     
369600        MOVE 'XDC'                TO R6-FIL-CT-IDPTYP                     
369700                                                                          
369800* ORDER LINE INFO                                                         
369900        MOVE OHUV-IDDISTR         TO DIFF-IDDISTR                         
370000        MOVE OHUV-IDKUNDNR        TO DIFF-IDKUNDNR                        
370100        MOVE OHUV-IDORDNR7        TO DIFF-IDORDNR5                        
370200        MOVE OHUV-KDORDKL         TO DIFF-KDORDKL                         
370300        MOVE ORAD-IDARTNR         TO DIFF-IDARTNR                         
370400        MOVE ORAD-KVBEART-Q       TO DIFF-KVBEART-Q                       
370500        MOVE ORAD-IDDC            TO DIFF-IDDC                            
370600        MOVE '4252'               TO DIFF-IDSYSTEM                        
370700                                                                          
370800* NDCA INFO                                                               
370900        MOVE NDCA-XDK7-KVOKS-DAG  TO DIFF-KVOKS-DAG-NDCA                  
371000        MOVE NDCA-XDK7-KVOKS-BULK TO DIFF-KVOKS-BULK-NDCA                 
371100        MOVE NDCA-KDORDBEK        TO DIFF-KDORDBEK-NDCA                   
371200        MOVE NDCA-KVPREAVB        TO DIFF-KVPREAVB-NDCA                   
371300        MOVE NDCA-ADLAGOMR        TO DIFF-ADLAGOMR-NDCA                   
371400        MOVE NDCA-ADGANG          TO DIFF-ADGANG-NDCA                     
371500        MOVE NDCA-ADPLATS         TO DIFF-ADPLATS-NDCA                    
371600        MOVE NDCA-IDDC            TO DIFF-IDDC-NDCA                       
371700        MOVE NDCA-IDDC-RO         TO DIFF-IDDC-RO-NDCA                    
371800        MOVE NDCA-KDARTURS        TO DIFF-KDARTURS-NDCA                   
371900        MOVE NDCA-KDOI            TO DIFF-KDOI-NDCA                       
372000        MOVE NDCA-KVPRERO         TO DIFF-KVPRERO-NDCA                    
372100        MOVE NDCA-VKART           TO DIFF-VKART-NDCA                      
372200        MOVE NDCA-VKART-NTO       TO DIFF-VKART-NTO-NDCA                  
372300        MOVE NDCA-VLARTNTO        TO DIFF-VLARTNTO-NDCA                   
372400        MOVE NDCA-CLEARGROUP      TO DIFF-OI-CLEAR-GRP-NDCA               
372500                                                                          
372600* XDCA INFO                                                               
372700        MOVE XDCA-KVOKS-DAG       TO DIFF-KVOKS-DAG-XDCA                  
372800        MOVE XDCA-KVOKS-BULK      TO DIFF-KVOKS-BULK-XDCA                 
372900        MOVE XDCA-KDORDBEK        TO DIFF-KDORDBEK-XDCA                   
373000        MOVE XDCA-KVPREAVB        TO DIFF-KVPREAVB-XDCA                   
373100        MOVE XDCA-ADLAGOMR        TO DIFF-ADLAGOMR-XDCA                   
373200        MOVE XDCA-ADGANG          TO DIFF-ADGANG-XDCA                     
373300        MOVE XDCA-ADPLATS         TO DIFF-ADPLATS-XDCA                    
373400        MOVE XDCA-IDDC-OUT        TO DIFF-IDDC-XDCA                       
373500        MOVE XDCA-IDDC-RO         TO DIFF-IDDC-RO-XDCA                    
373600        MOVE XDCA-KDARTURS        TO DIFF-KDARTURS-XDCA                   
373700        MOVE XDCA-KDOI            TO DIFF-KDOI-XDCA                       
373800        MOVE XDCA-KVPRERO         TO DIFF-KVPRERO-XDCA                    
373900        MOVE XDCA-VKART-OUT       TO DIFF-VKART-XDCA                      
374000        MOVE XDCA-VKART-NTO       TO DIFF-VKART-NTO-XDCA                  
374100        MOVE XDCA-VLARTNTO        TO DIFF-VLARTNTO-XDCA                   
374200        MOVE XDCA-CLEARGROUP      TO DIFF-OI-CLEAR-GRP-XDCA               
374300                                                                          
374400        PERFORM IMS-ISRT-WDR601                                           
374500        IF SEGMENT-FINNS-REDAN                                            
374600           PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                          
374700              ADD 1 TO R6-FIL-IDSEKVNR                                    
374800              PERFORM IMS-ISRT-WDR601                                     
374900           END-PERFORM                                                    
375000        END-IF                                                            
375100     END-IF                                                               
375200     .                                                                    
375300     EJECT                                                                
375400 ECW-PREL-AVBOKNING-SDC1 SECTION.                                         
375500                                                                          
375600     MOVE 'STA ECW-PREL-AVB-LDC'           TO   WS-PGM-POSITION           
375700                                                                          
375800     IF LDC-ARTIKELBYTE AND                                               
375900        AREG-REDIRLEV > 0                                                 
376000        MOVE ZERO TO KERS-KDORDBEK                                        
376100        MOVE ZERO TO HELP-KERS-KDORDBEK                                   
376200        MOVE NEJ  TO ALLT-SW                                              
376300     END-IF                                                               
376400                                                                          
376500     IF (ALLT-OK OR KOLLA-ERS) AND                                        
376600        OHUV-FLOVRLEV = NEJ AND                                           
376700        (OHUV-FLORDSPE = NEJ OR (OHUV-FLORDSPE = JA AND                   
376800         OHUV-IDSYSTEM = 'W216'))                                         
376900        AND ORAD-IDBIL = SPACE                                            
377000                                                                          
377100       PERFORM S40-HAMTA-WDB6-INFO                                        
377200       IF  DCS-SDC                                                        
377300                                                                          
377400         MOVE DCS-IDDC             TO DC01-IDDC                           
377500         IF ((ORAD-IDSYSTEM = 'LDC ' OR 'TACD') OR                        
377600             (ORAD-IDSYSTEM(1:3) = 'LYN' OR 'ECO' OR 'VOU' OR             
377700                                   'TAD' OR 'ACC' OR 'APA' OR             
377800                                   'APB' OR 'APC' OR 'APD' OR             
377900                                   'APE' OR 'APF' OR 'APG' OR             
378000                                   'APH' OR 'API' OR 'APJ' ))             
378100         AND ARB-TIRFS > 0                                                
378200         AND (AREG-KDERS = 0 OR                                           
378300            ((ORAD-IDSYSTEM = 'LDC' OR                                    
378400             (ORAD-IDSYSTEM(1:3) = 'LYN' OR 'ECO' OR 'VOU' OR             
378500                                   'TAD' OR 'ACC' OR 'APA' OR             
378600                                   'APB' OR 'APC' OR 'APD' OR             
378700                                   'APE' OR 'APF' OR 'APG' OR             
378800                                   'APH' OR 'API' OR 'APJ' ))             
378900             AND (AREG-KDERS = 01 OR 11)))                                
379000         AND REPAIR-CLEARING                                              
379100           PERFORM S13-CALL-WORKDAY-LDC                                   
379200           IF WORK-TIAAMMDD-FOM <= DAGENS-DATUM                           
379300             MOVE NEJ              TO SDCA-FLORDSPE                       
379400           ELSE                                                           
379500             MOVE JA               TO SDCA-FLORDSPE                       
379600           END-IF                                                         
379700         ELSE                                                             
379800           MOVE NEJ                TO SDCA-FLORDSPE                       
379900         END-IF                                                           
380000                                                                          
380100         MOVE OHUV-IDDISTR         TO SDCA-IDDISTR                        
380200         MOVE OHUV-FLFORBI         TO SDCA-FLFORBI                        
380300         MOVE AREG-FLREFILL        TO SDCA-FLREFILL                       
380400         MOVE ORAD-IDARTNR         TO SDCA-IDARTNR                        
380500         MOVE ORAD-IDDC            TO SDCA-IDDC                           
380600         MOVE OHUV-IDDC-TVS        TO SDCA-IDDC-TVS                       
380700         MOVE ORAD-IDLEVNR         TO SDCA-IDLEVNR                        
380800         MOVE ORAD-IDSYSTEM        TO SDCA-IDSYSTEM                       
380900         MOVE ORAD-KDORDKL         TO SDCA-KDORDKL                        
381000         MOVE ORAD-KDORDING        TO SDCA-KDORDING                       
381100         MOVE AREG-KDPRODSL        TO SDCA-KDPRODSL                       
381200         MOVE AREG-KDSORT          TO SDCA-KDSORT                         
381300         MOVE ORAD-KVBEART-Q       TO SDCA-KVBEART-Q                      
381400         MOVE AREG-KVQPACK-1       TO SDCA-KVQPACK-1                      
381500         MOVE AREG-REDIRLEV        TO SDCA-REDIRLEV                       
381600         MOVE ORAD-FLSDCLEV        TO SDCA-FLSDCLEV                       
381700         MOVE OHUV-TIREPDAT        TO SDCA-TIREPDAT                       
381800         MOVE ZERO                 TO SDCA-KVOKS-PREL                     
381900         MOVE +1                   TO SDCA-KDCALL                         
382000         MOVE +1                   TO SDCA-IXDCCLEAR                      
382100                                                                          
382200         CALL W411SDCA USING SDCA-W411SDCA SDCA-ARTS-PCB                  
382300                                           SDCA-WDB6-PCB                  
382400                                           SDCA-WDK9-PCB                  
382500                                           SDCA-WDR6-PCB                  
382600                                           SDCA-WDK6-PCB                  
382700                                           SDCA-WDQ4B-PCB                 
382800                                           SDCA-WDQ2-PCB                  
382900                                           SDCA-WDQ4-PCB                  
383000                                           SDCA-WDB6-2-PCB                
383100                                           SDCA-WDK6-2-PCB                
383200                                           SDCA-WDK7-2-PCB                
383300                                           SDCA-WDK7-3-PCB                
383400                                                                          
383500         MOVE SDCA-KDORDBEK TO SDCA-KDORDBEK-FIRST-SDC                    
383600         MOVE ZERO          TO SDCA-KDORDBEK                              
383700         IF SDCA-KVOKS-PREL > ZERO                                        
383800            MOVE SDCA-KVOKS-PREL TO ORAD-KVOKS-PREL                       
383900         END-IF                                                           
384000                                                                          
384100         IF SDCA-KDORDBEK-FIRST-SDC > ZERO                                
384200           IF SDCA-FLSDCLEV = JA AND SDCA-KDORDBEK-FIRST-SDC = 80         
384300                MOVE JA        TO OBKR-SW                                 
384400                MOVE NEJ       TO ALLT-SW                                 
384500           ELSE                                                           
384600             IF KOLLA-ERS                                                 
384700               IF PREPLANED-SW = JA                                       
384800                 MOVE ZERO            TO SDCA-KDORDBEK-FIRST-SDC          
384900                 IF SPAR-KDORDBEK = ZERO                                  
385000                   MOVE W-GMT-IDDC-CLEAR(2) TO ORAD-IDDC                  
385100                   MOVE ORAD-IDDC          TO WS-IDDC-SEEK                
385200                 ELSE                                                     
385300                   MOVE NEJ           TO KOLLA-ERS-SW                     
385400                 END-IF                                                   
385500               ELSE                                                       
385600                 IF SPAR-KDORDBEK = ZERO                                  
385700                  IF ORAD-IDDC = W-TILLK-DC                               
385800                     MOVE JA            TO OBKR-SW                        
385900                     MOVE NEJ           TO ALLT-SW                        
386000                                           KOLLA-ERS-SW                   
386100                     MOVE ZERO      TO SDCA-KDORDBEK-FIRST-SDC            
386200                  ELSE                                                    
386300                   MOVE W-GMT-IDDC-CLEAR(2) TO ORAD-IDDC                  
386400                   MOVE ORAD-IDDC     TO DC01-IDDC                        
386500                                         WS-IDDC-SEEK                     
386600                   IF DC01-IDDC = WC-CDC-SE                               
386700                      MOVE JA              TO CDC-MOVE-SW                 
386800                   END-IF                                                 
386900                  END-IF                                                  
387000                 ELSE                                                     
387100                   MOVE NEJ           TO KOLLA-ERS-SW                     
387200                 END-IF                                                   
387300               END-IF                                                     
387400             ELSE                                                         
387500                 IF SDCA-KDORDBEK-FIRST-SDC = 15                          
387600                   IF LDC-ARTIKELBYTE                                     
387700           MOVE 'T'                TO LDC-ARTIKELBYTE-SW                  
387800           MOVE HELP-ORAD-WDQ401   TO ORAD-WDQ401                         
387900           MOVE HELP-OBKR-WDQ101   TO OBKR-WDQ101                         
388000           MOVE HELP-CDCA-W411CDCA TO CDCA-W411CDCA                       
388100           MOVE HELP-DLEV-W411DLEV TO DLEV-W411DLEV                       
388200           MOVE HELP-KAMP-W411KAMP TO KAMP-W411KAMP                       
388300           MOVE HELP-KERS-W411KERS TO KERS-W411KERS                       
388400           MOVE HELP-KVAN-W411KVAN TO KVAN-W411KVAN                       
388500           MOVE HELP-ORFK-W411ORFK TO ORFK-W411ORFK                       
388600*          MOVE HELP-NDCA-W411NDCA TO NDCA-W411NDCA                       
388700           MOVE HELP-XDCA-W411XDCA TO XDCA-W411XDCA                       
388800           MOVE HELP-SDCA-W411SDCA TO SDCA-W411SDCA                       
388900           MOVE HELP-SPAR-W411SPAR TO SPAR-W411SPAR                       
389000           MOVE HELP-STOR-W411STOR TO STOR-W411STOR                       
389100           MOVE HELP-TPO1-W411TPO1 TO TPO1-W411TPO1                       
389200           MOVE HELP-TPO2-W411TPO2 TO TPO2-W411TPO2                       
389300           MOVE HELP-RELS-W411RELS TO RELS-W411RELS                       
389400           MOVE HELP-TILK-W411TILK TO TILK-W411TILK                       
389500           MOVE HELP-AREG-W411AREG TO AREG-W411AREG                       
389600           MOVE ZERO TO OBKR-IDARTNR-TILLK                                
389700                        OBKR-REKSIFFR-TILLK                               
389800           PERFORM ECWC-AVSLUTA-15-CLEARING                               
389900                   ELSE                                                   
390000                    PERFORM ECWA-KOLLA-ARTIKELBYTE-LDC                    
390100                    IF BYT-ARTIKEL                                        
390200                      PERFORM ECWB-LDC-ARTIKELBYTE                        
390300                    ELSE                                                  
390400                      PERFORM ECWC-AVSLUTA-15-CLEARING                    
390500                    END-IF                                                
390600                   END-IF                                                 
390700                 ELSE                                                     
390800                   MOVE JA          TO OBKR-SW                            
390900                   MOVE NEJ         TO ALLT-SW                            
391000                 END-IF                                                   
391100             END-IF                                                       
391200           END-IF                                                         
391300         ELSE                                                             
391400           IF OHUV-FLFORBI = SPEC-FORBI                                   
391500              MOVE '07'              TO ORAD-ADLAGOMR                     
391600           ELSE                                                           
391700              MOVE SDCA-ADLAGOMR     TO ORAD-ADLAGOMR                     
391800           END-IF                                                         
391900           IF LDC-ARTIKELBYTE                                             
392000              MOVE 'B' TO LDC-ARTIKELBYTE-SW                              
392100           END-IF                                                         
392200           MOVE SDCA-ADGANG          TO ORAD-ADGANG                       
392300           MOVE SDCA-ADPLATS         TO ORAD-ADPLATS                      
392400           MOVE SDCA-KVBEART-Q       TO ORAD-KVPREAVB                     
392500           MOVE NEJ                  TO ALLT-SW                           
392600           IF KOLLA-ERS  OR                                               
392700             (KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD)               
392800              MOVE NEJ               TO KOLLA-ERS-SW                      
392900              MOVE ZERO              TO KERS-KDORDBEK                     
393000              PERFORM S02-RENSA-TILLK-TAB                                 
393100              MOVE ZERO              TO SPAR-KDORDBEK                     
393200           END-IF                                                         
393300           IF (KERS-KDERS = +19 OR +29) AND PREPLANED-SW = NEJ            
393400              MOVE ZERO           TO SPAR-KDORDBEK                        
393500           END-IF                                                         
393600         END-IF                                                           
393700         MOVE SDCA-KDOI              TO ORAD-KDOI                         
393800         MOVE SDCA-CLEARGROUP        TO ORAD-CLEARGROUP                   
393900       END-IF                                                             
394000     ELSE                                                                 
394100       IF LDC-ARTIKELBYTE                                                 
394200          MOVE NEJ                TO ALLT-SW                              
394300          MOVE 'T'                TO LDC-ARTIKELBYTE-SW                   
394400          MOVE HELP-ORAD-WDQ401   TO ORAD-WDQ401                          
394500          MOVE HELP-OBKR-WDQ101   TO OBKR-WDQ101                          
394600          MOVE HELP-CDCA-W411CDCA TO CDCA-W411CDCA                        
394700          MOVE HELP-DLEV-W411DLEV TO DLEV-W411DLEV                        
394800          MOVE HELP-KAMP-W411KAMP TO KAMP-W411KAMP                        
394900          MOVE HELP-KERS-W411KERS TO KERS-W411KERS                        
395000          MOVE HELP-KVAN-W411KVAN TO KVAN-W411KVAN                        
395100          MOVE HELP-ORFK-W411ORFK TO ORFK-W411ORFK                        
395200*         MOVE HELP-NDCA-W411NDCA TO NDCA-W411NDCA                        
395300          MOVE HELP-XDCA-W411XDCA TO XDCA-W411XDCA                        
395400          MOVE HELP-SDCA-W411SDCA TO SDCA-W411SDCA                        
395500          MOVE HELP-SPAR-W411SPAR TO SPAR-W411SPAR                        
395600          MOVE HELP-STOR-W411STOR TO STOR-W411STOR                        
395700          MOVE HELP-TPO1-W411TPO1 TO TPO1-W411TPO1                        
395800          MOVE HELP-TPO2-W411TPO2 TO TPO2-W411TPO2                        
395900          MOVE HELP-RELS-W411RELS TO RELS-W411RELS                        
396000          MOVE HELP-TILK-W411TILK TO TILK-W411TILK                        
396100          MOVE HELP-AREG-W411AREG TO AREG-W411AREG                        
396200          PERFORM ECWC-AVSLUTA-15-CLEARING                                
396300       END-IF                                                             
396400     END-IF                                                               
396500     .                                                                    
396600     EJECT                                                                
396700 ECWA-KOLLA-ARTIKELBYTE-LDC SECTION.                                      
396800                                                                          
396900     MOVE JA TO SW-BYT-ARTIKEL                                            
397000                                                                          
397100*    LITE FIX FÖR ATT TESTA VIA WEB                                       
397200*    IF OHUV-IDSYSTEM = 'XCEL' AND                                        
397300*       OHUV-IDDISTR = 778                                                
397400*       MOVE 'LDC ' TO OHUV-IDSYSTEM                                      
397500*       MOVE 190705 TO OHUV-TIREPDAT                                      
397600*    END-IF                                                               
397700     IF (OHUV-IDSYSTEM = 'LDC' OR                                         
397800        (OHUV-IDSYSTEM(1:3) = 'LYN' OR 'ECO' OR 'VOU' OR                  
397900                              'TAD' OR 'ACC' OR 'APA' OR                  
398000                              'APB' OR 'APC' OR 'APD' OR                  
398100                              'APE' OR 'APF' OR 'APG' OR                  
398200                              'APH' OR 'API' OR 'APJ' )) AND              
398300        OHUV-TIREPDAT NOT = ZERO AND                                      
398400        AREG-KDERS = 01 OR 11                                             
398500                                                                          
398600        MOVE 1  TO TILK-IX                                                
398700        PERFORM UNTIL TILK-IX > TILK-IX-MAX                               
398800                   OR TILK-IDARTNR-TILLK(TILK-IX) = ZERO                  
398900                   OR BYT-EJ-ARTIKEL                                      
399000                   OR TILK-FLFINLV(TILK-IX) = NEJ                         
399100           IF TILK-IX > 1                                                 
399200               MOVE NEJ TO SW-BYT-ARTIKEL                                 
399300           ELSE                                                           
399400              MOVE TILK-IDARTNR-TILLK(TILK-IX) TO W-IDARTNR-T             
399500              PERFORM IMS-GU-WDK611-TILK                                  
399600              IF SEGMENT-FINNS                                            
399700                 IF TILK-CLAG-REDIRLEV > ZERO                             
399800                 OR TILK-CLAG-KDERS    > ZERO                             
399900                    MOVE NEJ TO SW-BYT-ARTIKEL                            
400000                 END-IF                                                   
400100              ELSE                                                        
400200                 MOVE NEJ TO SW-BYT-ARTIKEL                               
400300              END-IF                                                      
400400                                                                          
400500              MOVE ORAD-IDDC TO W-IDDC-T                                  
400600              PERFORM IMS-GU-WDK711-TILK                                  
400700              IF SEGMENT-SAKNAS                                           
400800                 MOVE NEJ TO SW-BYT-ARTIKEL                               
400900              ELSE                                                        
401000                 IF SLAG-KVLS = ZERO                                      
401100                    MOVE NEJ TO SW-BYT-ARTIKEL                            
401200                 END-IF                                                   
401300              END-IF                                                      
401400           END-IF                                                         
401500                                                                          
401600           ADD 1 TO TILK-IX                                               
401700        END-PERFORM                                                       
401800        IF TILK-IX = 1                                                    
401900           MOVE NEJ TO SW-BYT-ARTIKEL                                     
402000        END-IF                                                            
402100     ELSE                                                                 
402200        MOVE NEJ TO SW-BYT-ARTIKEL                                        
402300     END-IF                                                               
402400     .                                                                    
402500     EJECT                                                                
402600 ECWB-LDC-ARTIKELBYTE SECTION.                                            
402700                                                                          
402800     MOVE JA TO LDC-ARTIKELBYTE-SW                                        
402900     MOVE NEJ TO ALLT-SW                                                  
403000     MOVE NEJ TO OBKR-SW                                                  
403100     MOVE ORAD-WDQ401   TO HELP-ORAD-WDQ401                               
403200     MOVE OBKR-WDQ101   TO HELP-OBKR-WDQ101                               
403300     MOVE CDCA-W411CDCA TO HELP-CDCA-W411CDCA                             
403400     MOVE DLEV-W411DLEV TO HELP-DLEV-W411DLEV                             
403500     MOVE KAMP-W411KAMP TO HELP-KAMP-W411KAMP                             
403600     MOVE KERS-W411KERS TO HELP-KERS-W411KERS                             
403700     MOVE KVAN-W411KVAN TO HELP-KVAN-W411KVAN                             
403800     MOVE ORFK-W411ORFK TO HELP-ORFK-W411ORFK                             
403900*    MOVE NDCA-W411NDCA TO HELP-NDCA-W411NDCA                             
404000     MOVE XDCA-W411XDCA TO HELP-XDCA-W411XDCA                             
404100     MOVE SDCA-W411SDCA TO HELP-SDCA-W411SDCA                             
404200     MOVE SPAR-W411SPAR TO HELP-SPAR-W411SPAR                             
404300     MOVE STOR-W411STOR TO HELP-STOR-W411STOR                             
404400     MOVE TPO1-W411TPO1 TO HELP-TPO1-W411TPO1                             
404500     MOVE TPO2-W411TPO2 TO HELP-TPO2-W411TPO2                             
404600     MOVE RELS-W411RELS TO HELP-RELS-W411RELS                             
404700     MOVE TILK-W411TILK TO HELP-TILK-W411TILK                             
404800     MOVE AREG-W411AREG TO HELP-AREG-W411AREG                             
404900     .                                                                    
405000     EJECT                                                                
405100 ECWC-AVSLUTA-15-CLEARING SECTION.                                        
405200                                                                          
405300*    IF ORAD-IDDC = '21' AND                                              
405400*       AREG-REDIRLEV = 1                                                 
405500*       CONTINUE                                                          
405600*    ELSE                                                                 
405700     MOVE W-GMT-IDDC-CLEAR(2) TO ORAD-IDDC                                
405800                                WS-IDDC-SEEK                              
405900     MOVE JA      TO OBKR-SW                                              
406000*    END-IF                                                               
406100     .                                                                    
406200     EJECT                                                                
406300 ECH-PREL-AVBOKNING-SDC2 SECTION.                                         
406400                                                                          
406500     MOVE 'STA ECH-PREL-AVB-SDC2'          TO   WS-PGM-POSITION           
406600     IF (ALLT-OK OR KOLLA-ERS) AND                                        
406700        OHUV-FLOVRLEV = NEJ AND                                           
406800        (OHUV-FLORDSPE = NEJ OR (OHUV-FLORDSPE = JA AND                   
406900         OHUV-IDSYSTEM = 'W216'))                                         
407000        AND ORAD-IDBIL = SPACE                                            
407100                                                                          
407200       PERFORM S40-HAMTA-WDB6-INFO                                        
407300       IF  DCS-SDC                                                        
407400                                                                          
407500         MOVE DCS-IDDC             TO DC01-IDDC                           
407600         IF (ORAD-IDSYSTEM = 'LDC ' OR 'TACD')                            
407700         AND ARB-TIRFS > 0                                                
407800         AND AREG-KDERS = 0                                               
407900         AND REPAIR-CLEARING                                              
408000           PERFORM S13-CALL-WORKDAY-LDC                                   
408100           IF WORK-TIAAMMDD-FOM <= DAGENS-DATUM                           
408200             MOVE NEJ              TO SDCA-FLORDSPE                       
408300           ELSE                                                           
408400             MOVE JA               TO SDCA-FLORDSPE                       
408500           END-IF                                                         
408600         ELSE                                                             
408700           MOVE NEJ                TO SDCA-FLORDSPE                       
408800         END-IF                                                           
408900         MOVE OHUV-IDDISTR         TO SDCA-IDDISTR                        
409000         MOVE OHUV-FLFORBI         TO SDCA-FLFORBI                        
409100         MOVE AREG-FLREFILL        TO SDCA-FLREFILL                       
409200         MOVE ORAD-IDARTNR         TO SDCA-IDARTNR                        
409300         MOVE ORAD-IDDC            TO SDCA-IDDC                           
409400         MOVE OHUV-IDDC-TVS        TO SDCA-IDDC-TVS                       
409500         MOVE ORAD-IDLEVNR         TO SDCA-IDLEVNR                        
409600         MOVE ORAD-IDSYSTEM        TO SDCA-IDSYSTEM                       
409700         MOVE ORAD-KDORDKL         TO SDCA-KDORDKL                        
409800         MOVE ORAD-KDORDING        TO SDCA-KDORDING                       
409900         MOVE AREG-KDPRODSL        TO SDCA-KDPRODSL                       
410000         MOVE ORAD-KVBEART-Q       TO SDCA-KVBEART-Q                      
410100         MOVE AREG-REDIRLEV        TO SDCA-REDIRLEV                       
410200         MOVE ORAD-FLSDCLEV        TO SDCA-FLSDCLEV                       
410300         MOVE +1                   TO SDCA-KDCALL                         
410400         MOVE +2                   TO SDCA-IXDCCLEAR                      
410500                                                                          
410600         CALL W411SDCA USING SDCA-W411SDCA SDCA-ARTS-PCB                  
410700                                           SDCA-WDB6-PCB                  
410800                                           SDCA-WDK9-PCB                  
410900                                           SDCA-WDR6-PCB                  
411000                                           SDCA-WDK6-PCB                  
411100                                           SDCA-WDQ4B-PCB                 
411200                                           SDCA-WDQ2-PCB                  
411300                                           SDCA-WDQ4-PCB                  
411400                                           SDCA-WDB6-2-PCB                
411500                                           SDCA-WDK6-2-PCB                
411600                                           SDCA-WDK7-2-PCB                
411700                                           SDCA-WDK7-3-PCB                
411800                                                                          
411900         MOVE SDCA-KDORDBEK TO SDCA-KDORDBEK-SECOND-SDC                   
412000         MOVE ZERO          TO SDCA-KDORDBEK                              
412100                                                                          
412200         IF SDCA-KDORDBEK-SECOND-SDC > ZERO                               
412300           IF SDCA-FLSDCLEV = JA AND SDCA-KDORDBEK-SECOND-SDC = 80        
412400*             IF OHUV-IDDC-CLEAR(3) > '19'                                
412500*               MOVE ZERO            TO SDCA-KDORDBEK-SECOND-SDC          
412600*               MOVE OHUV-IDDC-CLEAR(3) TO ORAD-IDDC                      
412700*               MOVE ORAD-IDDC          TO WS-IDDC-SEEK                   
412800*               IF SDCA-KDOI = 'XX'                                       
412900*                  MOVE JA     TO SDCA-FLCLEAR(SDCA-IXDCCLEAR)            
413000*               END-IF                                                    
413100*             ELSE                                                        
413200                MOVE JA        TO OBKR-SW                                 
413300                MOVE NEJ       TO ALLT-SW                                 
413400*             END-IF                                                      
413500           ELSE                                                           
413600             IF KOLLA-ERS                                                 
413700               IF PREPLANED-SW = JA                                       
413800                 MOVE ZERO            TO SDCA-KDORDBEK-SECOND-SDC         
413900                 IF SPAR-KDORDBEK = ZERO                                  
414000                   MOVE W-GMT-IDDC-CLEAR(3) TO ORAD-IDDC                  
414100                   MOVE ORAD-IDDC          TO WS-IDDC-SEEK                
414200                 ELSE                                                     
414300                   MOVE NEJ           TO KOLLA-ERS-SW                     
414400                 END-IF                                                   
414500               ELSE                                                       
414600                 MOVE ZERO            TO SDCA-KDORDBEK-FIRST-SDC          
414700                 IF SPAR-KDORDBEK = ZERO                                  
414800                  IF ORAD-IDDC = W-TILLK-DC                               
414900                     MOVE JA            TO OBKR-SW                        
415000                     MOVE NEJ           TO ALLT-SW                        
415100                                           KOLLA-ERS-SW                   
415200                    MOVE ZERO         TO SDCA-KDORDBEK-SECOND-SDC         
415300                  ELSE                                                    
415400                   MOVE W-GMT-IDDC-CLEAR(3) TO ORAD-IDDC                  
415500                   MOVE ORAD-IDDC     TO DC01-IDDC                        
415600                                         WS-IDDC-SEEK                     
415700                   IF DC01-IDDC = WC-CDC-SE                               
415800                      MOVE JA              TO CDC-MOVE-SW                 
415900                   END-IF                                                 
416000                  END-IF                                                  
416100                 ELSE                                                     
416200                   MOVE NEJ           TO KOLLA-ERS-SW                     
416300                 END-IF                                                   
416400               END-IF                                                     
416500             ELSE                                                         
416600               IF SDCA-KDORDBEK-SECOND-SDC = 15                           
416700                  IF SDCA-KDORDBEK-FIRST-SDC = 15                         
416800                     MOVE ZERO TO SDCA-KDORDBEK-FIRST-SDC                 
416900                  END-IF                                                  
417000                  MOVE W-GMT-IDDC-CLEAR(3) TO ORAD-IDDC                   
417100                                             WS-IDDC-SEEK                 
417200                  MOVE JA          TO OBKR-SW                             
417300               ELSE                                                       
417400                  MOVE JA          TO OBKR-SW                             
417500                  MOVE NEJ         TO ALLT-SW                             
417600               END-IF                                                     
417700             END-IF                                                       
417800           END-IF                                                         
417900         ELSE                                                             
418000           IF OHUV-FLFORBI = SPEC-FORBI                                   
418100              MOVE '07'              TO ORAD-ADLAGOMR                     
418200           ELSE                                                           
418300              MOVE SDCA-ADLAGOMR     TO ORAD-ADLAGOMR                     
418400           END-IF                                                         
418500           MOVE SDCA-ADGANG          TO ORAD-ADGANG                       
418600           MOVE SDCA-ADPLATS         TO ORAD-ADPLATS                      
418700           MOVE SDCA-KVBEART-Q       TO ORAD-KVPREAVB                     
418800           MOVE NEJ                  TO ALLT-SW                           
418900           IF KOLLA-ERS  OR                                               
419000             (KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD)               
419100              MOVE NEJ               TO KOLLA-ERS-SW                      
419200              MOVE ZERO              TO KERS-KDORDBEK                     
419300              PERFORM S02-RENSA-TILLK-TAB                                 
419400              MOVE ZERO              TO SPAR-KDORDBEK                     
419500           END-IF                                                         
419600           IF (KERS-KDERS = +19 OR +29) AND PREPLANED-SW = NEJ            
419700              MOVE ZERO           TO SPAR-KDORDBEK                        
419800           END-IF                                                         
419900         END-IF                                                           
420000         MOVE SDCA-KDOI              TO ORAD-KDOI                         
420100         MOVE SDCA-CLEARGROUP        TO ORAD-CLEARGROUP                   
420200       END-IF                                                             
420300                                                                          
420400     END-IF                                                               
420500     .                                                                    
420600     EJECT                                                                
420700 ECX-PREL-AVBOKNING-SDC3 SECTION.                                         
420800                                                                          
420900     MOVE 'STA ECX-PREL-AVB-SDC3'          TO   WS-PGM-POSITION           
421000     IF (ALLT-OK OR KOLLA-ERS) AND                                        
421100        OHUV-FLOVRLEV = NEJ AND                                           
421200        (OHUV-FLORDSPE = NEJ OR (OHUV-FLORDSPE = JA AND                   
421300         OHUV-IDSYSTEM = 'W216'))                                         
421400        AND ORAD-IDBIL = SPACE                                            
421500                                                                          
421600       PERFORM S40-HAMTA-WDB6-INFO                                        
421700       IF  DCS-SDC                                                        
421800                                                                          
421900         MOVE NEJ                  TO SDCA-FLORDSPE                       
422000         MOVE OHUV-IDDISTR         TO SDCA-IDDISTR                        
422100         MOVE OHUV-FLFORBI         TO SDCA-FLFORBI                        
422200         MOVE AREG-FLREFILL        TO SDCA-FLREFILL                       
422300         MOVE ORAD-IDARTNR         TO SDCA-IDARTNR                        
422400         MOVE ORAD-IDDC            TO SDCA-IDDC                           
422500         MOVE OHUV-IDDC-TVS        TO SDCA-IDDC-TVS                       
422600         MOVE ORAD-IDLEVNR         TO SDCA-IDLEVNR                        
422700         MOVE ORAD-IDSYSTEM        TO SDCA-IDSYSTEM                       
422800         MOVE ORAD-KDORDKL         TO SDCA-KDORDKL                        
422900         MOVE ORAD-KDORDING        TO SDCA-KDORDING                       
423000         MOVE AREG-KDPRODSL        TO SDCA-KDPRODSL                       
423100         MOVE ORAD-KVBEART-Q       TO SDCA-KVBEART-Q                      
423200         MOVE AREG-REDIRLEV        TO SDCA-REDIRLEV                       
423300         MOVE ORAD-FLSDCLEV        TO SDCA-FLSDCLEV                       
423400         MOVE +1                   TO SDCA-KDCALL                         
423500         MOVE +3                   TO SDCA-IXDCCLEAR                      
423600                                                                          
423700         CALL W411SDCA USING SDCA-W411SDCA SDCA-ARTS-PCB                  
423800                                           SDCA-WDB6-PCB                  
423900                                           SDCA-WDK9-PCB                  
424000                                           SDCA-WDR6-PCB                  
424100                                           SDCA-WDK6-PCB                  
424200                                           SDCA-WDQ4B-PCB                 
424300                                           SDCA-WDQ2-PCB                  
424400                                           SDCA-WDQ4-PCB                  
424500                                           SDCA-WDB6-2-PCB                
424600                                           SDCA-WDK6-2-PCB                
424700                                           SDCA-WDK7-2-PCB                
424800                                           SDCA-WDK7-3-PCB                
424900                                                                          
425000         IF SDCA-KDORDBEK > ZERO                                          
425100           IF SDCA-FLSDCLEV = JA AND SDCA-KDORDBEK = 80                   
425200              MOVE JA        TO OBKR-SW                                   
425300              MOVE NEJ       TO ALLT-SW                                   
425400           ELSE                                                           
425500             IF KOLLA-ERS                                                 
425600               IF PREPLANED-SW = JA                                       
425700                  MOVE NEJ             TO KOLLA-ERS-SW                    
425800                  MOVE ZERO            TO SDCA-KDORDBEK                   
425900                  IF SPAR-KDORDBEK = ZERO                                 
426000                       MOVE WC-CDC-SE TO ORAD-IDDC                        
426100                    MOVE ORAD-IDDC     TO WS-IDDC-SEEK                    
426200                  END-IF                                                  
426300               ELSE                                                       
426400                  MOVE ZERO         TO SDCA-KDORDBEK-SECOND-SDC           
426500                  IF SPAR-KDORDBEK = ZERO                                 
426600                   IF ORAD-IDDC = W-TILLK-DC                              
426700                      MOVE JA            TO OBKR-SW                       
426800                      MOVE NEJ           TO ALLT-SW                       
426900                                            KOLLA-ERS-SW                  
427000                      MOVE ZERO            TO SDCA-KDORDBEK               
427100                   ELSE                                                   
427200                    MOVE WC-CDC-SE     TO ORAD-IDDC                       
427300                    MOVE ORAD-IDDC     TO WS-IDDC-SEEK                    
427400                    MOVE JA            TO CDC-MOVE-SW                     
427500                   END-IF                                                 
427600                  END-IF                                                  
427700               END-IF                                                     
427800             ELSE                                                         
427900               IF SDCA-KDORDBEK = 15                                      
428000                  IF SDCA-KDORDBEK-SECOND-SDC = 15                        
428100                     MOVE ZERO TO SDCA-KDORDBEK-SECOND-SDC                
428200                  END-IF                                                  
428300                     MOVE WC-CDC-SE TO ORAD-IDDC                          
428400                                        WS-IDDC-SEEK                      
428500                  MOVE JA          TO OBKR-SW                             
428600               ELSE                                                       
428700                  MOVE JA          TO OBKR-SW                             
428800                  MOVE NEJ         TO ALLT-SW                             
428900               END-IF                                                     
429000             END-IF                                                       
429100           END-IF                                                         
429200         ELSE                                                             
429300           IF OHUV-FLFORBI = SPEC-FORBI                                   
429400              MOVE '07'              TO ORAD-ADLAGOMR                     
429500           ELSE                                                           
429600              MOVE SDCA-ADLAGOMR     TO ORAD-ADLAGOMR                     
429700           END-IF                                                         
429800           MOVE SDCA-ADGANG          TO ORAD-ADGANG                       
429900           MOVE SDCA-ADPLATS         TO ORAD-ADPLATS                      
430000           MOVE SDCA-KVBEART-Q       TO ORAD-KVPREAVB                     
430100           MOVE NEJ                  TO ALLT-SW                           
430200           IF KOLLA-ERS  OR                                               
430300             (KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD)               
430400              MOVE NEJ               TO KOLLA-ERS-SW                      
430500              MOVE ZERO              TO KERS-KDORDBEK                     
430600              PERFORM S02-RENSA-TILLK-TAB                                 
430700              MOVE ZERO              TO SPAR-KDORDBEK                     
430800           END-IF                                                         
430900           IF (KERS-KDERS = +19 OR +29) AND PREPLANED-SW = NEJ            
431000              MOVE ZERO           TO SPAR-KDORDBEK                        
431100           END-IF                                                         
431200         END-IF                                                           
431300                                                                          
431400         MOVE SDCA-KDOI                TO ORAD-KDOI                       
431500         MOVE SDCA-CLEARGROUP          TO ORAD-CLEARGROUP                 
431600       END-IF                                                             
431700                                                                          
431800     END-IF                                                               
431900     .                                                                    
432000     EJECT                                                                
432100 ECT-KOMPLETTERA-RANSONERING SECTION.                                     
432200                                                                          
432300     MOVE 'STA ECT-RANS        '           TO   WS-PGM-POSITION           
432400     IF ALLT-OK OR LDC-ARTIKELBYTE                                        
432500                OR (CDC-MOVE AND PREPLANED-SW = NEJ)                      
432600     MOVE ORAD-BERADREF        TO RANS-BERADREF                           
432700     MOVE OHUV-FLEMBORD        TO RANS-FLEMBORD                           
432800     MOVE OHUV-FLFORBI         TO RANS-FLFORBI                            
432900                                                                          
433000     IF ORAD-IDBIL = SPACE OR ORAD-IDSYSTEM = 'SOFT'                      
433100       MOVE OHUV-FLORDSPE      TO RANS-FLORDSPE                           
433200     ELSE                                                                 
433300       MOVE JA                 TO RANS-FLORDSPE                           
433400     END-IF                                                               
433500                                                                          
433600     MOVE OHUV-FLOVRLEV        TO RANS-FLOVRLEV                           
433700     MOVE ORAD-IDKAMPRF        TO RANS-IDKAMPRF                           
433800     MOVE ORAD-IDARTNR         TO RANS-IDARTNR                            
433900     MOVE ORAD-IDLEVNR         TO RANS-IDLEVNR                            
434000     MOVE OHUV-IDRFTAB         TO RANS-IDRFTAB                            
434100     MOVE ORAD-TIRODAT         TO RANS-TIRODAT                            
434200     MOVE OHUV-KDORDKL         TO RANS-KDORDKL                            
434300     MOVE +1                   TO RANS-KDORDBEH                           
434400     MOVE ORAD-KVBEART-Q       TO RANS-KVBEART-Q                          
434500     MOVE ORAD-KDTPOTYP        TO RANS-KDTPOTYP                           
434600     MOVE AREG-KDERS           TO RANS-KDERS                              
434700     MOVE AREG-KVLS            TO RANS-KVLS                               
434800     MOVE AREG-KVPB-SATS       TO RANS-KVPB-SATS                          
434900     MOVE AREG-KVPB-SEP        TO RANS-KVPB-SEP                           
435000     MOVE AREG-REDIRLEV        TO RANS-REDIRLEV                           
435100     MOVE AREG-KVRESS          TO RANS-KVRESS                             
435200     MOVE AREG-KVSPANT         TO RANS-KVSPANT                            
435300     MOVE AREG-KVUTRS          TO RANS-KVUTRS                             
435400     MOVE AREG-TIDISPIN        TO RANS-TIDISPIN                           
435500                                                                          
435600     MOVE AREG-KDPRODSL        TO TEST-KDPRODSL                           
435700     IF KDPRODSL-BIMA                                                     
435800         MOVE 1                TO ORAD-RERF-RAD                           
435900                                  RANS-RERF-RAD-UT                        
436000         MOVE ZERO             TO RANS-SUTPO-PB-UT                        
436100                                  RANS-SUTPO-EJPB-UT                      
436200                                  RANS-RERF-ART-UT                        
436300     ELSE                                                                 
436400     CALL W411RANS USING RANS-W411RANS RANS-XXKM-PCB RANS-ARTM-PCB        
436500                                       RANS-ARTS-PCB                      
436600                                                                          
436700       MOVE RANS-RERF-RAD-UT     TO ORAD-RERF-RAD                         
436800                                                                          
436900     END-IF                                                               
437000     END-IF                                                               
437100     .                                                                    
437200     EJECT                                                                
437300                                                                          
437400 ECQ-KOMPLETTERA-STORA-UTTAG SECTION.                                     
437500                                                                          
437600     MOVE 'STA ECQ-STOR        '           TO   WS-PGM-POSITION           
437700     IF ALLT-OK                                                           
437800        MOVE ZERO                             TO STOR-KDORDBEK            
437900        MOVE SPACE                            TO STOR-KDPROTYP            
438000        MOVE ORAD-IDDISTR                     TO TEST-IDDISTR             
438100        IF OHUV-FLFORBI              = JA     OR                          
438200           OHUV-FLFORBI              = SPEC-FORBI OR                      
438300           OHUV-FLOVRLEV             = JA     OR                          
438400           OHUV-FLORDSPE             = JA     OR                          
438500           DIST35-REFILL                      OR                          
438600           DIST35-REFILL-INOM-NDC             OR                          
438700           DIST35-NONVCC-REFILL               OR                          
438800           DIST35-NONVCC-NONVCC-TRANSFER      OR                          
438900           DIST35-NONVCC-VCC-TRANSFER         OR                          
439000           DIST35-RETUR                       OR                          
439100           STOR-KDPROTYP             = 'F'    OR                          
439200           ORAD-KVBEART-Q            < +3     OR                          
439300           OHUV-IDKAMPRF             > ZERO   OR                          
439400           AREG-KDERS                > ZERO   OR                          
439500           ORAD-IDLEVNR          NOT = SPACE  OR                          
439600          (ORAD-IDKUNDRF-RO      NOT = '0000000   ') OR                   
439700           OHUV-KDORDKL              = ZERO   OR                          
439800           ORAD-IDSYSTEM             = 'OREL' OR                          
439900           ORAD-IDSYSTEM             = 'LDC ' OR                          
440000          (ORAD-IDSYSTEM             = 'VIPS' AND                         
439900           GMT-KDKUNDKAT             = 3)     OR                          
440000           ORAD-IDSYSTEM             = 'TACD' OR                          
440100           ORAD-IDSYSTEM (1:3)       = 'LYN'  OR                          
440200           ORAD-IDSYSTEM (1:3)       = 'POL'  OR                          
440300           ORAD-IDSYSTEM (1:3)       = 'ECO'  OR                          
440400           ORAD-IDSYSTEM (1:3)       = 'VOU' OR                           
440500           ORAD-IDSYSTEM (1:3)       = 'TAD' OR                           
440600           ORAD-IDSYSTEM (1:3)       = 'ACC' OR                           
440700           ORAD-IDSYSTEM (1:3)       = 'APA' OR                           
440800           ORAD-IDSYSTEM (1:3)       = 'APB' OR                           
440900           ORAD-IDSYSTEM (1:3)       = 'APC' OR                           
441000           ORAD-IDSYSTEM (1:3)       = 'APD' OR                           
441100           ORAD-IDSYSTEM (1:3)       = 'APE' OR                           
441200           ORAD-IDSYSTEM (1:3)       = 'APF' OR                           
441300           ORAD-IDSYSTEM (1:3)       = 'APG' OR                           
441400           ORAD-IDSYSTEM (1:3)       = 'APH' OR                           
441500           ORAD-IDSYSTEM (1:3)       = 'API' OR                           
441600           ORAD-IDSYSTEM (1:3)       = 'APJ'                              
441700                                                                          
441800           OR ORAD-IDBIL         NOT = SPACE                              
441900                                                                          
442000           CONTINUE                                                       
442100        ELSE                                                              
442200           MOVE ORAD-IDSYSTEM        TO STOR-IDSYSTEM                     
442300           MOVE ORAD-IDLEVNR         TO STOR-IDLEVNR                      
442400           MOVE ORAD-IDKUNDRF-RO     TO STOR-IDKUNDRF-RO                  
442500           MOVE ORAD-BERADREF        TO STOR-BERADREF                     
442600           MOVE OHUV-FLFORBI         TO STOR-FLFORBI                      
442700           MOVE OHUV-FLORDSPE        TO STOR-FLORDSPE                     
442800           MOVE OHUV-FLOVRLEV        TO STOR-FLOVRLEV                     
442900           MOVE OHUV-KDORDKL         TO STOR-KDORDKL                      
443000           MOVE SPACE                TO STOR-KDPROTYP                     
443100           MOVE AREG-KDERS           TO STOR-KDERS                        
443200           MOVE AREG-KDVVKL          TO STOR-KDVVKL                       
443300           MOVE ORAD-KVBEART-Q       TO STOR-KVBEART-Q                    
443400           MOVE AREG-KVPB-SEP        TO STOR-KVPB-SEP                     
443500           MOVE AREG-KVSLUTKP        TO STOR-KVSLUTKP                     
443600           MOVE AREG-KDPRODSL        TO STOR-KDPRODSL                     
443700                                                                          
443800           PERFORM S40-HAMTA-WDB6-INFO                                    
443900                                                                          
444000           IF (DIST18-SKROT                                               
444100             OR DIST35-NL-CDC-RETUR-Q                                     
444200             OR DIST35-ES-CDC-RETUR-Q                                     
444300             OR DIST35-AT-CDC-RETUR-Q)                                    
444400             AND (((DCS-CDC OR DCS-CDC-TR) AND                            
444500                (CLAG-KDLEVSP > 19 OR                                     
444600                 CLAG-KVSPARR-KVAL > 0)) OR                               
444700                (DCS-SDC AND                                              
444800                (SLAG-KDLEVSP > 19 OR                                     
444900                 SLAG-KVSPARR-KVAL > 0)))                                 
445000             MOVE ZERO               TO STOR-RERF-ART                     
445100           ELSE                                                           
445200             MOVE RANS-RERF-ART-UT   TO STOR-RERF-ART                     
445300           END-IF                                                         
445400           MOVE OHUV-IDKAMPRF        TO STOR-IDKAMPRF                     
445500           MOVE ORAD-IDDISTR         TO STOR-IDDISTR                      
445600                                                                          
445700           CALL W411STOR USING STOR-W411STOR                              
445800                                                                          
445900           IF STOR-KDORDBEK > +0                                          
446000              MOVE +6                   TO ORAD-KDTPOTYP                  
446100              PERFORM S21-UPPDATERA-TPO6                                  
446200              MOVE JA                   TO OBKR-SW                        
446300              MOVE NEJ                  TO ALLT-SW                        
446400           END-IF                                                         
446500        END-IF                                                            
446600                                                                          
446700     END-IF                                                               
446800     .                                                                    
446900     EJECT                                                                
447000                                                                          
447100                                                                          
447200 ECR-PREL-AVBOKNING-CDC SECTION.                                          
447300                                                                          
447400     MOVE 'STA ECR-PREL-AVB-CDC'           TO   WS-PGM-POSITION           
447500     IF ALLT-OK OR LDC-ARTIKEL-TILLBAKA                                   
447600                OR (CDC-MOVE AND PREPLANED-SW = NEJ)                      
447700                                                                          
447800     MOVE TILK-FLFINLV(WS-INDEX-TILLK)                                    
447900                               TO CDCA-FLFINLV-IN                         
448000     MOVE OHUV-FLFORBI         TO CDCA-FLFORBI-IN                         
448100     MOVE OHUV-FLPRELRO        TO CDCA-FLPRELRO-IN                        
448200     MOVE ORAD-FLRESTN         TO CDCA-FLRESTN-IN                         
448300     MOVE ORFK-FLSLATT (WS-INDEX-MID)                                     
448400                               TO CDCA-FLSLATT-IN                         
448500     MOVE ORAD-IDARTNR         TO CDCA-IDARTNR-IN                         
448600     MOVE OHUV-IDDISTR         TO CDCA-IDDISTR-IN                         
448700     MOVE ORAD-IDDC            TO CDCA-IDDC-IN                            
448800     MOVE ORAD-IDKAMPRF        TO CDCA-IDKAMPRF-IN                        
448900     MOVE ORAD-IDKUNDRF-RO     TO CDCA-IDKUNDRF-RO-IN                     
449000     MOVE ORAD-IDSYSTEM        TO CDCA-IDSYSTEM-IN                        
449100     MOVE ORAD-IDLEVNR         TO CDCA-IDLEVNR-IN                         
449200     MOVE AREG-KDERS           TO CDCA-KDERS-IN                           
449300     MOVE ORAD-KDKVBRYT        TO CDCA-KDKVBRYT-IN                        
449400     MOVE ORAD-KDORDKL         TO CDCA-KDORDKL-IN                         
449500     MOVE ORAD-KDPRODSL        TO CDCA-KDPRODSL-IN                        
449600     MOVE AREG-KDSORT          TO CDCA-KDSORT-IN                          
449700     MOVE SPACE                TO CDCA-KDPROTYP-IN                        
449800     MOVE ORAD-KDTPOTYP        TO CDCA-KDTPOTYP-IN                        
449900     MOVE AREG-KDUART          TO CDCA-KDUART-IN                          
450000     MOVE ORAD-KVBEART         TO CDCA-KVBEART-IN                         
450100     MOVE ORAD-KVBEART-Q       TO CDCA-KVBEART-Q-IN                       
450200* FOR VR ORDERS (IDYSTEM=VR AND KDORDURS=G VIA VIPS) INCLUDE              
450300* QUANTITY ADAPTATION TO NOT USE Q0 THEREFORE MOVE ZERO TO Q0             
450400     IF (MID-IDSYSTEM = 'VR  ' OR MID-KDORDURS = 'G')                     
450500       MOVE 0                  TO CDCA-KVQPACK-0-IN                       
450600     ELSE                                                                 
450700       MOVE AREG-KVQPACK-0     TO CDCA-KVQPACK-0-IN                       
450800     END-IF                                                               
450900     MOVE AREG-KVQPACK-1       TO CDCA-KVQPACK-1-IN                       
451000     MOVE AREG-IDFKNGRP        TO CDCA-IDFKNGRP-IN                        
451100                                                                          
451200     PERFORM S40-HAMTA-WDB6-INFO                                          
451300     IF (DIST18-SKROT                                                     
451400       OR DIST35-NL-CDC-RETUR-Q                                           
451500       OR DIST35-ES-CDC-RETUR-Q                                           
451600       OR DIST35-AT-CDC-RETUR-Q)                                          
451700       AND (((DCS-CDC OR DCS-CDC-TR) AND                                  
451800          (CLAG-KDLEVSP > 19 OR                                           
451900           CLAG-KVSPARR-KVAL > 0)) OR                                     
452000          (DCS-SDC AND                                                    
452100          (SLAG-KDLEVSP > 19 OR                                           
452200           SLAG-KVSPARR-KVAL > 0)))                                       
452300       MOVE ZERO               TO CDCA-RERF-RAD-IN                        
452400                                  CDCA-RERF-ART-IN                        
452500     ELSE                                                                 
452600       MOVE RANS-RERF-RAD-UT   TO CDCA-RERF-RAD-IN                        
452700       MOVE RANS-RERF-ART-UT   TO CDCA-RERF-ART-IN                        
452800     END-IF                                                               
452900     MOVE OHUV-RESLATT         TO CDCA-RESLATT-IN                         
453000                                                                          
453100     IF ORAD-IDBIL = SPACE OR ORAD-IDSYSTEM = 'SOFT'                      
453200       MOVE OHUV-FLORDSPE      TO CDCA-FLORDSPE-IN                        
453300     ELSE                                                                 
453400       MOVE JA                 TO CDCA-FLORDSPE-IN                        
453500     END-IF                                                               
453600                                                                          
453700     MOVE OHUV-FLOVRLEV        TO CDCA-FLOVRLEV-IN                        
453800     MOVE AREG-KVAKS-CDC       TO CDCA-KVAKS-CDC-IN                       
453900     MOVE AREG-KVAKS-PAV       TO CDCA-KVAKS-PAV-IN                       
454000     MOVE AREG-KVLS            TO CDCA-KVLS-IN                            
454100     MOVE AREG-KVRESS          TO CDCA-KVRESS-IN                          
454200     MOVE AREG-KVSPANT         TO CDCA-KVSPANT-IN                         
454300     MOVE AREG-KVUTRS          TO CDCA-KVUTRS-IN                          
454400     MOVE AREG-KVSPARR-KVAL    TO CDCA-KVSPARR-KVAL-IN                    
454500     MOVE ORAD-IDKUNDNR        TO CDCA-IDKUNDNR-IN                        
454600     MOVE ORAD-BERADREF        TO CDCA-BERADREF-IN                        
454700     MOVE +1                   TO CDCA-KDCALL                             
454800                                                                          
454900     CALL W411CDCA USING CDCA-W411CDCA CDCA-ARTM-PCB                      
455000                                       CDCA-INLB-PCB                      
455100                                       CDCA-WDB2-PCB                      
455200                                       CDCA-WDC1-PCB                      
455300                                                                          
455400     MOVE CDCA-FLAKPLOC-UT     TO ORAD-FLAKPLOC                           
455500                                                                          
455600     IF OHUV-FLOVRLEV = JA OR OHUV-FLORDSPE = JA                          
455700                                                                          
455800       OR ORAD-IDBIL NOT = SPACE                                          
455900                                                                          
456000       CONTINUE                                                           
456100     ELSE                                                                 
456200       IF (KERS-KDERS = 0) OR (KERS-KDERS > 10 AND                        
456300                            KERS-KDORDBEK = 0)                            
456400          CONTINUE                                                        
456500       ELSE                                                               
456600          IF KERS-KDERS > 0 AND < 10                                      
456700             IF CDCA-KVPREAVB-UT = +0 AND CDCA-KVPRERO-UT = +0            
456800                MOVE CDCA-KVBEART-Q-UT TO CDCA-KVPRERO-UT                 
456900             END-IF                                                       
457000             PERFORM S02-RENSA-TILLK-TAB                                  
457100             MOVE ZERO           TO KERS-KDORDBEK                         
457200             MOVE NEJ            TO TILLK-SW                              
457300          ELSE                                                            
457400**FOR KDERS>10,DONT SWITCH IF A HAS STOCKS IN CDC                         
457500           IF PREPLANED-SW = NEJ                                          
457600             IF CDCA-KVPREAVB-UT > 0                                      
457700               IF KOLLA-ERS    OR                                         
457800                 (KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD)           
457900                  MOVE NEJ               TO KOLLA-ERS-SW                  
458000                  MOVE ZERO              TO KERS-KDORDBEK                 
458100                  PERFORM S02-RENSA-TILLK-TAB                             
458200                  MOVE ZERO              TO SPAR-KDORDBEK                 
458300               ELSE                                                       
458400                  IF KERS-KDERS = +19 OR +29                              
458500                     MOVE ZERO           TO SPAR-KDORDBEK                 
458600                  END-IF                                                  
458700               END-IF                                                     
458800             ELSE                                                         
458900               IF (CDCA-KVPREAVB-UT <= 0) AND                             
459000                  (CDCA-KDORDBEK-UT = 92 OR 99)                           
459100                  MOVE ZEROES        TO CDCA-KDORDBEK-UT                  
459200               END-IF                                                     
459300               MOVE ZEROES           TO CDCA-KVPREAVB-UT                  
459400                                        CDCA-KVPRERO-UT                   
459500               IF (KOLLA-ERS AND KERS-KDORDBEK > ZERO)                    
459600               OR (KERS-KDORDBEK = 61 AND EJ-TILLKOMMANDE-RAD)            
459700               OR SPAR-KDORDBEK = 54                                      
459800                  PERFORM S12-SPACE-SDCA-KDORDBEK                         
459900               END-IF                                                     
460000             END-IF                                                       
460100           END-IF                                                         
460200          END-IF                                                          
460300       END-IF                                                             
460400                                                                          
460500       MOVE CDCA-KVPREAVB-UT     TO ORAD-KVPREAVB                         
460600       MOVE CDCA-KVPRERO-UT      TO ORAD-KVPRERO                          
460700       MOVE CDCA-KVSLATT-UT      TO ORAD-KVSLATT                          
460800       MOVE CDCA-RERF-RAD-UT     TO ORAD-RERF-RAD                         
460900       IF ORAD-IDLEVNR NOT = SPACE                                        
461000          CONTINUE                                                        
461100       ELSE                                                               
461200          MOVE CDCA-KVBEART-UT   TO ORAD-KVBEART                          
461300          MOVE CDCA-KVBEART-Q-UT TO ORAD-KVBEART-Q                        
461400       END-IF                                                             
461500     END-IF                                                               
461600                                                                          
461700     IF CDCA-KDORDBEK-UT > ZERO                                           
461800        MOVE JA                TO OBKR-SW                                 
461900     END-IF                                                               
462000     IF  MID-ADLAGOMR-CD(WS-INDEX-MID) NOT = ALL '+'                      
462100     AND MID-ADLAGOMR-CD(WS-INDEX-MID) > ZERO                             
462200        MOVE MID-ADLAGOMR-CD(WS-INDEX-MID)                                
462300                               TO ORAD-ADLAGOMR                           
462400        MOVE MID-ADGANG-CD(WS-INDEX-MID)                                  
462500                               TO ORAD-ADGANG                             
462600        MOVE MID-ADPLATS-CD(WS-INDEX-MID)                                 
462700                               TO ORAD-ADPLATS                            
462800     ELSE                                                                 
462900        MOVE AREG-ADLAGOMR     TO ORAD-ADLAGOMR                           
463000        MOVE AREG-ADGANG       TO ORAD-ADGANG                             
463100        MOVE AREG-ADPLATS      TO ORAD-ADPLATS                            
463200     END-IF                                                               
463300     IF OHUV-FLFORBI = SPEC-FORBI                                         
463400        MOVE '07'              TO ORAD-ADLAGOMR                           
463500     END-IF                                                               
463600                                                                          
463700     END-IF                                                               
463800     .                                                                    
463900     EJECT                                                                
464000 ECZ-CHECK-KDERS-IN-DC SECTION.                                           
464100                                                                          
464200     MOVE WS-INDEX-TILLK    TO WS-SAVE-INDEX                              
464300     MOVE +1                TO WS-INDEX-TILLK                             
464400                               IDDC-IX                                    
464500     MOVE NEJ               TO BAL-DC-FND-SW                              
464600                               TILLK-BAL-DC-FND-SW                        
464700                               KDERS-CHAIN-SW                             
464800     MOVE AREG-W411AREG-001 TO ORFK-W411AREG-001(WS-INDEX-MID)            
464900                                                                          
465000     PERFORM UNTIL WS-INDEX-TILLK > WS-INDEX-TILLK-MAX OR                 
465100                   TILK-IDARTNR(WS-INDEX-TILLK) = ZERO OR                 
465200                   BAL-DC-FND                          OR                 
465300                   TILLK-BAL-DC-FND                    OR                 
465400                   KDERS-CHAIN                                            
465500        PERFORM ECZD-CHECK-KDERS-CHAIN                                    
465600        IF KDERS-CHAIN-SW = NEJ                                           
465700           IF TILK-IDARTNR(WS-INDEX-TILLK) > ZERO AND                     
465800              TILK-IDARTNR-TILLK(WS-INDEX-TILLK) > ZERO AND               
465900              TILK-FLTILLK-X(WS-INDEX-TILLK) = JA                         
466000               PERFORM UNTIL W-GMT-IDDC-CLEAR(IDDC-IX) = SPACES           
466100                     OR IDDC-IX > IX-DCCLEAR-MAX                          
466200                     OR BAL-DC-FND                                        
466300                     OR TILLK-BAL-DC-FND                                  
466400                  MOVE TILK-IDARTNR(WS-INDEX-TILLK)                       
466500                                           TO W-IDARTNR-SDCA              
466600                  PERFORM ECZB-CALL-SDCA                                  
466700                  IF SDCA-KDORDBEK > 0                                    
466800                     PERFORM ECZA-GET-TILLK-DATA                          
466900                     MOVE TILK-IDARTNR-TILLK(WS-INDEX-TILLK)              
467000                                           TO W-IDARTNR-SDCA              
467100                     PERFORM ECZB-CALL-SDCA                               
467200                     IF SDCA-KDORDBEK = 0                                 
467300                       MOVE JA             TO TILLK-BAL-DC-FND-SW         
467400                       MOVE W-GMT-IDDC-CLEAR(IDDC-IX)                     
467500                                           TO W-TILLK-DC                  
467600                     END-IF                                               
467700                  ELSE                                                    
467800                     MOVE JA                TO BAL-DC-FND-SW              
467900                  END-IF                                                  
468000                  ADD +1       TO IDDC-IX                                 
468100               END-PERFORM                                                
468200           END-IF                                                         
468300        END-IF                                                            
468400        ADD +1              TO WS-INDEX-TILLK                             
468500     END-PERFORM                                                          
468600                                                                          
468700     MOVE WS-SAVE-INDEX     TO WS-INDEX-TILLK                             
468800     MOVE ORFK-W411AREG-001(WS-INDEX-MID) TO AREG-W411AREG-001            
468900     MOVE ZEROES            TO SDCA-KDORDBEK                              
469000                                                                          
469100     IF KDERS-CHAIN-SW = NEJ                                              
469200        IF BAL-DC-FND-SW = NEJ AND TILLK-BAL-DC-FND-SW = NEJ              
469300           MOVE '11'           TO W-TILLK-DC                              
469400        END-IF                                                            
469500     END-IF                                                               
469600     .                                                                    
469700     EJECT                                                                
469800*****************************************************************         
469900*IF A(KDERS 22) SUPERSEEDED BY B(KDERS-25) AND IS SUPERSEEDED             
470000*BY C1(KDERS 00) AND C2(KDERS 00),C1,C2 WILL BE SKIPPED AND               
470100*A WILL BE CHECKED FOR STOCKS, IF NOT OCC61                               
470200*****************************************************************         
470300 ECZD-CHECK-KDERS-CHAIN SECTION.                                          
470400                                                                          
470500     IF TILK-IDARTNR(WS-INDEX-TILLK) > ZERO AND                           
470600        TILK-IDARTNR-TILLK(WS-INDEX-TILLK) > ZERO AND                     
470700        TILK-FLTILLK-X(WS-INDEX-TILLK) = NEJ AND                          
470800       (TILK-KDERS(WS-INDEX-TILLK) = 14 OR 15 OR 18 OR                    
470900                                     24 OR 25 OR 28)                      
471000          MOVE JA              TO KDERS-CHAIN-SW                          
471100     END-IF                                                               
471200     .                                                                    
471300     EJECT                                                                
471400 ECZA-GET-TILLK-DATA SECTION.                                             
471500                                                                          
471600     MOVE AREG-FLREFILL        TO W-FLREFILL-MAIN                         
471700     MOVE AREG-KDPRODSL        TO W-KDPRODSL-MAIN                         
471800     MOVE AREG-KDSORT          TO W-KDSORT-MAIN                           
471900     MOVE AREG-KVQPACK-1       TO W-KVQPACK-1-MAIN                        
472000     MOVE AREG-REDIRLEV        TO W-REDIRLEV-MAIN                         
472100                                                                          
472200     PERFORM ED-LAES-TILLK-DATA                                           
472300                                                                          
472400     MOVE AREG-FLREFILL        TO W-FLREFILL-REPL                         
472500     MOVE AREG-KDPRODSL        TO W-KDPRODSL-REPL                         
472600     MOVE AREG-KDSORT          TO W-KDSORT-REPL                           
472700     MOVE AREG-KVQPACK-1       TO W-KVQPACK-1-REPL                        
472800     MOVE AREG-REDIRLEV        TO W-REDIRLEV-REPL                         
472900                                                                          
473000     MOVE W-FLREFILL-MAIN      TO AREG-FLREFILL                           
473100     MOVE W-KDPRODSL-MAIN      TO AREG-KDPRODSL                           
473200     MOVE W-KDSORT-MAIN        TO AREG-KDSORT                             
473300     MOVE W-KVQPACK-1-MAIN     TO AREG-KVQPACK-1                          
473400     MOVE W-REDIRLEV-MAIN      TO AREG-REDIRLEV                           
473500     .                                                                    
473600     EJECT                                                                
473700 ECZB-CALL-SDCA   SECTION.                                                
473800                                                                          
473900     IF ORAD-IDARTNR = W-IDARTNR-SDCA                                     
474000        MOVE ORAD-IDARTNR         TO SDCA-IDARTNR                         
474100        MOVE AREG-FLREFILL        TO SDCA-FLREFILL                        
474200        MOVE AREG-KDPRODSL        TO SDCA-KDPRODSL                        
474300        MOVE AREG-KDSORT          TO SDCA-KDSORT                          
474400        MOVE AREG-KVQPACK-1       TO SDCA-KVQPACK-1                       
474500        MOVE AREG-REDIRLEV        TO SDCA-REDIRLEV                        
474600     ELSE                                                                 
474700        MOVE W-IDARTNR-SDCA       TO SDCA-IDARTNR                         
474800        MOVE W-FLREFILL-REPL      TO SDCA-FLREFILL                        
474900        MOVE W-KDPRODSL-REPL      TO SDCA-KDPRODSL                        
475000        MOVE W-KDSORT-REPL        TO SDCA-KDSORT                          
475100        MOVE W-KVQPACK-1-REPL     TO SDCA-KVQPACK-1                       
475200        MOVE W-REDIRLEV-REPL      TO SDCA-REDIRLEV                        
475300     END-IF                                                               
475400                                                                          
475500     MOVE OHUV-IDDISTR         TO SDCA-IDDISTR                            
475600     MOVE OHUV-FLFORBI         TO SDCA-FLFORBI                            
475700     MOVE NEJ                  TO SDCA-FLORDSPE                           
475800     MOVE W-GMT-IDDC-CLEAR(IDDC-IX)                                       
475900                               TO SDCA-IDDC                               
476000     MOVE OHUV-IDDC-TVS        TO SDCA-IDDC-TVS                           
476100     MOVE ORAD-IDLEVNR         TO SDCA-IDLEVNR                            
476200     MOVE ORAD-IDSYSTEM        TO SDCA-IDSYSTEM                           
476300     MOVE ORAD-KDORDKL         TO SDCA-KDORDKL                            
476400     MOVE ORAD-KDORDING        TO SDCA-KDORDING                           
476500     MOVE ORAD-KVBEART-Q       TO SDCA-KVBEART-Q                          
476600     MOVE ORAD-FLSDCLEV        TO SDCA-FLSDCLEV                           
476700     MOVE +0                   TO SDCA-TIREPDAT                           
476800     MOVE +0                   TO SDCA-KVOKS-PREL                         
476900     MOVE +2                   TO SDCA-KDCALL                             
477000     MOVE +1                   TO SDCA-IXDCCLEAR                          
477100                                                                          
477200     CALL W411SDCA USING SDCA-W411SDCA SDCA-ARTS-PCB                      
477300                                       SDCA-WDB6-PCB                      
477400                                       SDCA-WDK9-PCB                      
477500                                       SDCA-WDR6-PCB                      
477600                                       SDCA-WDK6-PCB                      
477700                                       SDCA-WDQ4B-PCB                     
477800                                       SDCA-WDQ2-PCB                      
477900                                       SDCA-WDQ4-PCB                      
478000                                       SDCA-WDB6-2-PCB                    
478100                                       SDCA-WDK6-2-PCB                    
478200                                       SDCA-WDK7-2-PCB                    
478300                                       SDCA-WDK7-3-PCB                    
478400     .                                                                    
478500     EJECT                                                                
478600 S12-SPACE-SDCA-KDORDBEK  SECTION.                                        
478700                                                                          
478800     IF SDCA-KDORDBEK-FIRST-SDC > 0                                       
478900         MOVE ZEROES  TO SDCA-KDORDBEK-FIRST-SDC                          
479000     ELSE                                                                 
479100        IF SDCA-KDORDBEK-SECOND-SDC > 0                                   
479200           MOVE ZEROES  TO SDCA-KDORDBEK-SECOND-SDC                       
479300        ELSE                                                              
479400           IF SDCA-KDORDBEK > 0                                           
479500              MOVE ZEROES  TO SDCA-KDORDBEK                               
479600           END-IF                                                         
479700        END-IF                                                            
479800     END-IF                                                               
479900     .                                                                    
480000                                                                          
480100     EJECT                                                                
480200 ECS-SKRIV-OBKR-OCH-VOR-RAD SECTION.                                      
480300                                                                          
480400     MOVE 'STA ECS-SKRIV-OBKR  '           TO   WS-PGM-POSITION           
480500*--- EFTERSOM KVPREAVB OCH KVPRERO ENDAST SKALL SKRIVAS PÅ                
480600*    DEN SISTA ORDERBEKRÄFTELSERADEN 'SLÄPAR' ISRT AV RADEN               
480700*    OBKR-SW SÄTTS = JA NÄR EN ORDERBEKRÄFTELSE RAD SKALL                 
480800*    SKRIVAS.  DENNA TESTAR VI PÅ SIST I SECTIONEN FÖR ATT                
480900*    FÅ MED DEN SISTA ORDERBEKRÄFTELSEN MED KVPREAVB OCH KVPRERO          
481000*---                                                                      
481100     PERFORM ECSA-REDIGERA-OBKR-RAD                                       
481200                                                                          
481300     IF TILLKOMMANDE-RAD                                                  
481400        IF KERS-KDORDBEK = 41                                             
481500           MOVE KERS-KDORDBEK     TO OBKR-KDORDBEK                        
481600           MOVE '4252KER1'        TO OBKR-IDPGM                           
481700           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
481800                                  TO OBKR-KVBEART-TILLK                   
481900           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
482000              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
482100             MOVE +0              TO OBKR-DIERS-KVOT                      
482200           ELSE                                                           
482300             COMPUTE OBKR-DIERS-KVOT =                                    
482400                                  TILK-DIERS-TILLK(WS-INDEX-TILLK)        
482500                                  / TILK-DIERS-ERS(WS-INDEX-TILLK)        
482600           END-IF                                                         
482700           MOVE 'S'               TO OBKR-SW                              
482800        END-IF                                                            
482900     END-IF                                                               
483000                                                                          
483100     IF ORFK-KDORDBEK(WS-INDEX-MID) > 0                                   
483200       AND ORFK-KDORDBEK(WS-INDEX-MID) NOT = 56                           
483300*----(KOD  58, 59, 98)                                                    
483400        IF OBKR-SKRIVEN                                                   
483500           PERFORM IMS-25-ISRT-WLORQM01-WDQ101                            
483600           ADD +1              TO OBKR-IDSEKVNR                           
483700        END-IF                                                            
483800                                                                          
483900        MOVE ORFK-KDORDBEK(WS-INDEX-MID)                                  
484000                               TO OBKR-KDORDBEK                           
484100        MOVE '4252ORFK'        TO OBKR-IDPGM                              
484200        MOVE 'S'               TO OBKR-SW                                 
484300     END-IF                                                               
484400     EJECT                                                                
484500                                                                          
484600     IF KVAN-KDORDBEK-UT > +0                                             
484700*----(KOD 43, 44)                                                         
484800        IF OBKR-SKRIVEN                                                   
484900           PERFORM IMS-25-ISRT-WLORQM01-WDQ101                            
485000           ADD +1              TO OBKR-IDSEKVNR                           
485100        END-IF                                                            
485200        IF TILLKOMMANDE-RAD                                               
485300           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
485400                               TO OBKR-KVBEART-TILLK                      
485500           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
485600              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
485700             MOVE +0              TO OBKR-DIERS-KVOT                      
485800           ELSE                                                           
485900             COMPUTE OBKR-DIERS-KVOT =                                    
486000                                  TILK-DIERS-TILLK(WS-INDEX-TILLK)        
486100                                  / TILK-DIERS-ERS(WS-INDEX-TILLK)        
486200           END-IF                                                         
486300        END-IF                                                            
486400        MOVE KVAN-KDORDBEK-UT  TO OBKR-KDORDBEK                           
486500        MOVE '4252KVAN'        TO OBKR-IDPGM                              
486600        MOVE KVAN-KVBEART-IN   TO OBKR-KVBEART                            
486700        MOVE KVAN-KVBEART-Q-UT TO OBKR-KVBEART-Q                          
486800        MOVE 'S'               TO OBKR-SW                                 
486900     END-IF                                                               
487000     EJECT                                                                
487100                                                                          
487200     IF DLEV-KDORDBEK-UT > ZERO                                           
487300*----(KOD 21, 53, 95) , 26                                                
487400        IF OBKR-SKRIVEN                                                   
487500           PERFORM IMS-25-ISRT-WLORQM01-WDQ101                            
487600           ADD +1              TO OBKR-IDSEKVNR                           
487700        END-IF                                                            
487800        IF TILLKOMMANDE-RAD                                               
487900           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
488000                               TO OBKR-KVBEART-TILLK                      
488100           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
488200              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
488300             MOVE +0              TO OBKR-DIERS-KVOT                      
488400           ELSE                                                           
488500             COMPUTE OBKR-DIERS-KVOT =                                    
488600                                  TILK-DIERS-TILLK(WS-INDEX-TILLK)        
488700                                  / TILK-DIERS-ERS(WS-INDEX-TILLK)        
488800           END-IF                                                         
488900        END-IF                                                            
489000        MOVE DLEV-KDORDBEK-UT  TO OBKR-KDORDBEK                           
489100        MOVE '4252DLEV'        TO OBKR-IDPGM                              
489200        MOVE 'S'               TO OBKR-SW                                 
489300     END-IF                                                               
489400     EJECT                                                                
489500                                                                          
489600     IF KERS-KDORDBEK > +0                                                
489700*----(KOD 41, 61)                                                         
489800                                                                          
489900        IF KERS-KDORDBEK = 61                                             
490000*----FÖR KOD 41 OCH 42 SKER ORDERBEKRÄFTELSE ENDAST PÅ TILL-              
490100*----KOMMANDE ARTIKLAR EFTER DET ATT DESSA HAR GENOMGÅTT                  
490200*----RADBEHANDLINGEN                                                      
490300           IF OBKR-SKRIVEN                                                
490400              PERFORM IMS-25-ISRT-WLORQM01-WDQ101                         
490500              ADD +1           TO OBKR-IDSEKVNR                           
490600           END-IF                                                         
490700           MOVE KERS-KDORDBEK  TO OBKR-KDORDBEK                           
490800           MOVE '4252KER2'     TO OBKR-IDPGM                              
490900           MOVE 'S'            TO OBKR-SW                                 
491000           PERFORM S20-OBKR-FRAN-TILLK-TAB                                
491100           PERFORM S02-RENSA-TILLK-TAB                                    
491200        ELSE                                                              
491300           IF NOT TILLKOMMANDE-RAD                                        
491400              IF OBKR-SKRIVEN                                             
491500                 PERFORM IMS-25-ISRT-WLORQM01-WDQ101                      
491600                 ADD +1        TO OBKR-IDSEKVNR                           
491700              END-IF                                                      
491800              MOVE 'S'            TO OBKR-SW                              
491900              MOVE KERS-KDORDBEK  TO OBKR-KDORDBEK                        
492000              MOVE '4252KER3'     TO OBKR-IDPGM                           
492100           END-IF                                                         
492200        END-IF                                                            
492300     END-IF                                                               
492400     EJECT                                                                
492500                                                                          
492600     IF SPAR-KDORDBEK > ZERO                                              
492700*----(KOD 51, 52, 53, 54, 55, 57, 58, 66, 67, 68, 80, 90)                 
492800        IF OBKR-SKRIVEN                                                   
492900           PERFORM IMS-25-ISRT-WLORQM01-WDQ101                            
493000           ADD +1              TO OBKR-IDSEKVNR                           
493100        END-IF                                                            
493200        IF TILLKOMMANDE-RAD                                               
493300           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
493400                               TO OBKR-KVBEART-TILLK                      
493500           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
493600              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
493700             MOVE +0              TO OBKR-DIERS-KVOT                      
493800           ELSE                                                           
493900             COMPUTE OBKR-DIERS-KVOT =                                    
494000                                  TILK-DIERS-TILLK(WS-INDEX-TILLK)        
494100                                  / TILK-DIERS-ERS(WS-INDEX-TILLK)        
494200           END-IF                                                         
494300        END-IF                                                            
494400        MOVE SPAR-KDORDBEK                                                
494500                               TO OBKR-KDORDBEK                           
494600        MOVE '4252SPAR'        TO OBKR-IDPGM                              
494700        MOVE 'S'               TO OBKR-SW                                 
494800     END-IF                                                               
494900     EJECT                                                                
495000                                                                          
495100     IF TPO1-KDORDBEK > +0                                                
495200*----(KOD 72, 73, 74, 85)                                                 
495300        IF OBKR-SKRIVEN                                                   
495400           PERFORM IMS-25-ISRT-WLORQM01-WDQ101                            
495500           ADD +1              TO OBKR-IDSEKVNR                           
495600        END-IF                                                            
495700        IF TILLKOMMANDE-RAD                                               
495800           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
495900                               TO OBKR-KVBEART-TILLK                      
496000           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
496100              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
496200             MOVE +0              TO OBKR-DIERS-KVOT                      
496300           ELSE                                                           
496400             COMPUTE OBKR-DIERS-KVOT =                                    
496500                                  TILK-DIERS-TILLK(WS-INDEX-TILLK)        
496600                                  / TILK-DIERS-ERS(WS-INDEX-TILLK)        
496700           END-IF                                                         
496800        END-IF                                                            
496900        MOVE TPO1-KDORDBEK     TO OBKR-KDORDBEK                           
497000        MOVE '4252TPO1'        TO OBKR-IDPGM                              
497100        IF TPO1-KDORDBEK = 85                                             
497200           MOVE TPO1-KVANNANT  TO OBKR-KVANNANT                           
497300        END-IF                                                            
497400        MOVE 'S'               TO OBKR-SW                                 
497500     END-IF                                                               
497600     EJECT                                                                
497700                                                                          
497800     IF TPO2-KDORDBEK > +0                                                
497900*----(KOD 70)                                                             
498000        IF OBKR-SKRIVEN                                                   
498100           PERFORM IMS-25-ISRT-WLORQM01-WDQ101                            
498200           ADD +1              TO OBKR-IDSEKVNR                           
498300        END-IF                                                            
498400        IF TILLKOMMANDE-RAD                                               
498500           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
498600                               TO OBKR-KVBEART-TILLK                      
498700           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
498800              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
498900             MOVE +0              TO OBKR-DIERS-KVOT                      
499000           ELSE                                                           
499100             COMPUTE OBKR-DIERS-KVOT =                                    
499200                                  TILK-DIERS-TILLK(WS-INDEX-TILLK)        
499300                                  / TILK-DIERS-ERS(WS-INDEX-TILLK)        
499400           END-IF                                                         
499500        END-IF                                                            
499600        MOVE TPO2-KDORDBEK     TO OBKR-KDORDBEK                           
499700        MOVE '4252TPO2'        TO OBKR-IDPGM                              
499800        MOVE 'S'               TO OBKR-SW                                 
499900     END-IF                                                               
500000     EJECT                                                                
500100                                                                          
500200     IF TPO3-KDORDBEK > +0                                                
500300*----(KOD 72)                                                             
500400        IF OBKR-SKRIVEN                                                   
500500           PERFORM IMS-25-ISRT-WLORQM01-WDQ101                            
500600           ADD +1              TO OBKR-IDSEKVNR                           
500700        END-IF                                                            
500800        IF TILLKOMMANDE-RAD                                               
500900           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
501000                               TO OBKR-KVBEART-TILLK                      
501100           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
501200              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
501300             MOVE +0              TO OBKR-DIERS-KVOT                      
501400           ELSE                                                           
501500             COMPUTE OBKR-DIERS-KVOT =                                    
501600                                  TILK-DIERS-TILLK(WS-INDEX-TILLK)        
501700                                  / TILK-DIERS-ERS(WS-INDEX-TILLK)        
501800           END-IF                                                         
501900        END-IF                                                            
502000        MOVE TPO3-KDORDBEK     TO OBKR-KDORDBEK                           
502100        MOVE '4252TPO3'        TO OBKR-IDPGM                              
502200        MOVE 'S'               TO OBKR-SW                                 
502300     END-IF                                                               
502400     EJECT                                                                
502500                                                                          
502600     IF KAMP-KDORDBEK > +0                                                
502700*----(KOD 72, 75, 76)                                                     
502800        IF OBKR-SKRIVEN                                                   
502900           PERFORM IMS-25-ISRT-WLORQM01-WDQ101                            
503000           ADD +1              TO OBKR-IDSEKVNR                           
503100        END-IF                                                            
503200        IF TILLKOMMANDE-RAD                                               
503300           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
503400                               TO OBKR-KVBEART-TILLK                      
503500           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
503600              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
503700             MOVE +0              TO OBKR-DIERS-KVOT                      
503800           ELSE                                                           
503900             COMPUTE OBKR-DIERS-KVOT =                                    
504000                                  TILK-DIERS-TILLK(WS-INDEX-TILLK)        
504100                                  / TILK-DIERS-ERS(WS-INDEX-TILLK)        
504200           END-IF                                                         
504300        END-IF                                                            
504400        MOVE KAMP-KDORDBEK     TO OBKR-KDORDBEK                           
504500        MOVE '4252KAMP'        TO OBKR-IDPGM                              
504600        MOVE 'S'               TO OBKR-SW                                 
504700     END-IF                                                               
504800     EJECT                                                                
504900                                                                          
505000     IF RELS-KDORDBEK > 0                                                 
505100*----(KOD 56)                                                             
505200          IF OBKR-SKRIVEN                                                 
505300             PERFORM IMS-25-ISRT-WLORQM01-WDQ101                          
505400             ADD +1              TO OBKR-IDSEKVNR                         
505500          END-IF                                                          
505600          MOVE RELS-KDORDBEK     TO OBKR-KDORDBEK                         
505700          MOVE '4252ORFK'        TO OBKR-IDPGM                            
505800          MOVE 'S'               TO OBKR-SW                               
505900       END-IF                                                             
506000    EJECT                                                                 
506100     IF TPO5-KDORDBEK > +0                                                
506200*----(KOD 73)                                                             
506300        IF OBKR-SKRIVEN                                                   
506400           PERFORM IMS-25-ISRT-WLORQM01-WDQ101                            
506500           ADD +1              TO OBKR-IDSEKVNR                           
506600        END-IF                                                            
506700        IF TILLKOMMANDE-RAD                                               
506800           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
506900                               TO OBKR-KVBEART-TILLK                      
507000           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
507100              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
507200             MOVE +0              TO OBKR-DIERS-KVOT                      
507300           ELSE                                                           
507400             COMPUTE OBKR-DIERS-KVOT =                                    
507500                                  TILK-DIERS-TILLK(WS-INDEX-TILLK)        
507600                                  / TILK-DIERS-ERS(WS-INDEX-TILLK)        
507700           END-IF                                                         
507800        END-IF                                                            
507900        MOVE TPO5-KDORDBEK     TO OBKR-KDORDBEK                           
508000        MOVE '4252TPO5'        TO OBKR-IDPGM                              
508100        IF TPO5-KDORDBEK = 85                                             
508200           MOVE TPO5-KVBEART-Q TO OBKR-KVANNANT                           
508300        END-IF                                                            
508400        MOVE 'S'               TO OBKR-SW                                 
508500     END-IF                                                               
508600     EJECT                                                                
508700                                                                          
508800     IF XDCA-KDORDBEK > ZERO                                              
508900*----(KOD 15, 53, 55, 80, 92, 98, 99)                                     
509000        IF OBKR-SKRIVEN                                                   
509100           PERFORM IMS-25-ISRT-WLORQM01-WDQ101                            
509200           ADD +1              TO OBKR-IDSEKVNR                           
509300        END-IF                                                            
509400        IF TILLKOMMANDE-RAD                                               
509500           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
509600                               TO OBKR-KVBEART-TILLK                      
509700           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
509800              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
509900             MOVE +0              TO OBKR-DIERS-KVOT                      
510000           ELSE                                                           
510100             COMPUTE OBKR-DIERS-KVOT =                                    
510200                                  TILK-DIERS-TILLK(WS-INDEX-TILLK)        
510300                                  / TILK-DIERS-ERS(WS-INDEX-TILLK)        
510400           END-IF                                                         
510500        END-IF                                                            
510600                                                                          
510700        IF XDCA-KDORDBEK NOT = 15                                         
510800          IF OHUV-IDDC-TVS = SPACE                                        
510900            IF OHUV-IDDC-PRIM   NOT = XDCA-IDDC-OUT                       
511000              MOVE 15          TO OBKR-KDORDBEK                           
511100              MOVE IDPGM       TO OBKR-IDPGM                              
511200              MOVE 'S'         TO OBKR-SW                                 
511300                                                                          
511400* FÖR ATT SLIPPA DUBBLA 15-BEKRÄFTELSER NOLLAR VI EV. SDC                 
511500              IF SDCA-KDORDBEK-FIRST-SDC = 15                             
511600                 MOVE ZERO     TO SDCA-KDORDBEK-FIRST-SDC                 
511700              END-IF                                                      
511800              IF SDCA-KDORDBEK-SECOND-SDC = 15                            
511900                 MOVE ZERO     TO SDCA-KDORDBEK-SECOND-SDC                
512000              END-IF                                                      
512100              IF SDCA-KDORDBEK = 15                                       
512200                 MOVE ZERO     TO SDCA-KDORDBEK                           
512300              END-IF                                                      
512400            END-IF                                                        
512500            IF OBKR-SKRIVEN                                               
512600              PERFORM IMS-25-ISRT-WLORQM01-WDQ101                         
512700              ADD +1           TO OBKR-IDSEKVNR                           
512800            END-IF                                                        
512900          END-IF                                                          
513000        END-IF                                                            
513100        IF XDCA-KDORDBEK = 80                                             
513200           MOVE ORAD-KVBEART-Q TO OBKR-KVANNANT                           
513300        END-IF                                                            
513400        MOVE XDCA-KDORDBEK     TO OBKR-KDORDBEK                           
513500        MOVE '4252XDCA'        TO OBKR-IDPGM                              
513600        MOVE 'S'               TO OBKR-SW                                 
513700     END-IF                                                               
513800     EJECT                                                                
513900                                                                          
514000     IF SDCA-KDORDBEK-SECOND-SDC > ZERO                                   
514100*----(KOD 15, 53, 80, 92)                                                 
514200        IF OBKR-SKRIVEN                                                   
514300           PERFORM IMS-25-ISRT-WLORQM01-WDQ101                            
514400           ADD +1              TO OBKR-IDSEKVNR                           
514500        END-IF                                                            
514600        IF TILLKOMMANDE-RAD                                               
514700           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
514800                               TO OBKR-KVBEART-TILLK                      
514900           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
515000              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
515100             MOVE +0              TO OBKR-DIERS-KVOT                      
515200           ELSE                                                           
515300             COMPUTE OBKR-DIERS-KVOT =                                    
515400                                  TILK-DIERS-TILLK(WS-INDEX-TILLK)        
515500                                  / TILK-DIERS-ERS(WS-INDEX-TILLK)        
515600           END-IF                                                         
515700        END-IF                                                            
515800        IF SDCA-KDORDBEK-SECOND-SDC = 80                                  
515900           MOVE ORAD-KVBEART-Q TO OBKR-KVANNANT                           
516000        END-IF                                                            
516100        IF SDCA-KDORDBEK-SECOND-SDC = 80 OR 92                            
516200           MOVE 0              TO OBKR-IDARTNR-TILLK                      
516300        END-IF                                                            
516400        MOVE SDCA-KDORDBEK-SECOND-SDC TO OBKR-KDORDBEK                    
516500        MOVE '4252SDCA'              TO OBKR-IDPGM                        
516600        MOVE 'S'                     TO OBKR-SW                           
516700     END-IF                                                               
516800     EJECT                                                                
516900                                                                          
517000     IF SDCA-KDORDBEK-FIRST-SDC > ZERO                                    
517100     OR LDC-ARTIKEL-BYTT                                                  
517200*----(KOD 15, 53, 80, 92)                                                 
517300        IF OBKR-SKRIVEN                                                   
517400           IF LDC-ARTIKEL-BYTT                                            
517500              MOVE 'N'  TO OBKR-FLTILLK                                   
517600              MOVE ZERO TO OBKR-IDARTNR-TILLK                             
517700           ELSE                                                           
517800           PERFORM IMS-25-ISRT-WLORQM01-WDQ101                            
517900           ADD +1              TO OBKR-IDSEKVNR                           
518000           END-IF                                                         
518100        END-IF                                                            
518200        IF SDCA-KDORDBEK-FIRST-SDC > ZERO                                 
518300        OR LDC-ARTIKEL-TILLBAKA                                           
518400           IF TILLKOMMANDE-RAD                                            
518500              MOVE TILK-KVBEART(WS-INDEX-TILLK)                           
518600                                  TO OBKR-KVBEART-TILLK                   
518700              IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                  
518800                 TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                       
518900                MOVE +0              TO OBKR-DIERS-KVOT                   
519000              ELSE                                                        
519100                COMPUTE OBKR-DIERS-KVOT =                                 
519200                                  TILK-DIERS-TILLK(WS-INDEX-TILLK)        
519300                                  / TILK-DIERS-ERS(WS-INDEX-TILLK)        
519400              END-IF                                                      
519500           END-IF                                                         
519600           IF SDCA-KDORDBEK-FIRST-SDC = 80                                
519700              MOVE ORAD-KVBEART-Q TO OBKR-KVANNANT                        
519800           END-IF                                                         
519900           IF SDCA-KDORDBEK-FIRST-SDC = 80 OR 92                          
520000              MOVE 0              TO OBKR-IDARTNR-TILLK                   
520100           END-IF                                                         
520200           MOVE SDCA-KDORDBEK-FIRST-SDC TO OBKR-KDORDBEK                  
520300           MOVE '4252SDCA'           TO OBKR-IDPGM                        
520400           MOVE 'S'                  TO OBKR-SW                           
520500        END-IF                                                            
520600     END-IF                                                               
520700     EJECT                                                                
520800                                                                          
520900     IF SDCA-KDORDBEK > ZERO                                              
521000*----(KOD 15, 53, 80, 92)                                                 
521100        IF OBKR-SKRIVEN                                                   
521200           PERFORM IMS-25-ISRT-WLORQM01-WDQ101                            
521300           ADD +1              TO OBKR-IDSEKVNR                           
521400        END-IF                                                            
521500        IF TILLKOMMANDE-RAD                                               
521600           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
521700                               TO OBKR-KVBEART-TILLK                      
521800           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
521900              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
522000             MOVE +0              TO OBKR-DIERS-KVOT                      
522100           ELSE                                                           
522200             COMPUTE OBKR-DIERS-KVOT =                                    
522300                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
522400                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
522500           END-IF                                                         
522600        END-IF                                                            
522700        IF SDCA-KDORDBEK = 80                                             
522800           MOVE ORAD-KVBEART-Q TO OBKR-KVANNANT                           
522900        END-IF                                                            
523000        IF SDCA-KDORDBEK = 80 OR 92                                       
523100           MOVE 0              TO OBKR-IDARTNR-TILLK                      
523200        END-IF                                                            
523300        MOVE SDCA-KDORDBEK     TO OBKR-KDORDBEK                           
523400        MOVE '4252SDCA'        TO OBKR-IDPGM                              
523500        MOVE 'S'               TO OBKR-SW                                 
523600     END-IF                                                               
523700                                                                          
523800     EJECT                                                                
523900     IF STOR-KDORDBEK > +0                                                
524000*----(KOD 70)                                                             
524100        IF OBKR-SKRIVEN                                                   
524200           PERFORM IMS-25-ISRT-WLORQM01-WDQ101                            
524300           ADD +1              TO OBKR-IDSEKVNR                           
524400        END-IF                                                            
524500        IF TILLKOMMANDE-RAD                                               
524600           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
524700                               TO OBKR-KVBEART-TILLK                      
524800           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
524900              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
525000             MOVE +0              TO OBKR-DIERS-KVOT                      
525100           ELSE                                                           
525200             COMPUTE OBKR-DIERS-KVOT =                                    
525300                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
525400                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
525500           END-IF                                                         
525600        END-IF                                                            
525700        MOVE STOR-KDORDBEK     TO OBKR-KDORDBEK                           
525800        MOVE '4252STOR'        TO OBKR-IDPGM                              
525900        MOVE 'S'               TO OBKR-SW                                 
526000     END-IF                                                               
526100     EJECT                                                                
526200     IF CDCA-KDORDBEK-UT > +0                                             
526300     AND NOT LDC-ARTIKEL-TILLBAKA                                         
526400*----(KOD 80, 92, 99)                                                     
526500        IF OBKR-SKRIVEN                                                   
526600           PERFORM IMS-25-ISRT-WLORQM01-WDQ101                            
526700           ADD +1              TO OBKR-IDSEKVNR                           
526800        END-IF                                                            
526900        IF TILLKOMMANDE-RAD                                               
527000           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
527100                               TO OBKR-KVBEART-TILLK                      
527200           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
527300              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
527400             MOVE +0              TO OBKR-DIERS-KVOT                      
527500           ELSE                                                           
527600             COMPUTE OBKR-DIERS-KVOT =                                    
527700                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
527800                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
527900           END-IF                                                         
528000        END-IF                                                            
528100        IF CDCA-KDORDBEK-UT = +80                                         
528200           MOVE CDCA-KVANNANT-UT  TO OBKR-KVANNANT                        
528300        END-IF                                                            
528400        MOVE CDCA-KDORDBEK-UT  TO OBKR-KDORDBEK                           
528500        MOVE '4252CDCA'        TO OBKR-IDPGM                              
528600        MOVE 'S'               TO OBKR-SW                                 
528700     END-IF                                                               
528800*                                                                         
528900* PÅ SISTA RADEN FÖR KUNDENS NORMALA CL LÄGGS DE AVBOKADE ANTALEN!        
529000     IF OBKR-SKRIVEN                                                      
529100     AND NOT LDC-ARTIKELBYTE                                              
529200        IF OBKR-KDORDBEK = 61 AND                                         
529300         (OBKR-KVPREAVB = 0 AND OBKR-KVPRERO = 0)                         
529400           CONTINUE                                                       
529500        ELSE                                                              
529600           MOVE ORAD-KVPREAVB     TO OBKR-KVPREAVB                        
529700           MOVE ORAD-KVPRERO      TO OBKR-KVPRERO                         
529800        END-IF                                                            
529900******** TA BORT FRÅGAN FÖR ERSATT ARTIKEL                                
530000        IF KERS-KDORDBEK > 0 AND NOT TILLKOMMANDE-RAD                     
530100           PERFORM S23-DELETE-PRICE-Q-LINE                                
530200           INITIALIZE OBKR-DEAL-PR-LINE                                   
530300           MOVE 'N/A'          TO OBKR-KDVALISO                           
530400        END-IF                                                            
530500*************TL 030514                                                    
530600         IF LDC-ARTIKEL-TILLBAKA                                          
530700            MOVE NEJ          TO OBKR-FLTILLK                             
530800            MOVE ZERO         TO OBKR-IDARTNR-TILLK                       
530900                                 OBKR-REKSIFFR-TILLK                      
531000         ELSE                                                             
531100            IF LDC-ARTIKEL-BYTT                                           
531200               MOVE JA            TO OBKR-FLTILLK                         
531300               MOVE ORAD-IDARTNR  TO OBKR-IDARTNR-TILLK                   
531400               MOVE ORAD-REKSIFFR TO OBKR-REKSIFFR-TILLK                  
531500            END-IF                                                        
531600         END-IF                                                           
531700         PERFORM IMS-25-ISRT-WLORQM01-WDQ101                              
531800         ADD +1                TO OBKR-IDSEKVNR                           
531900        IF OBKR-KVPREAVB = +0 AND OBKR-KVPRERO = +0                       
532000          PERFORM ECSB-BACKA-FORBI-REFILL                                 
532100        END-IF                                                            
532200     END-IF                                                               
532300     IF OHUV-KDORDKL = +0    AND                                          
532400         (OBKR-KDORDBEK = 51 OR 52 OR 53 OR 54 OR 55 OR 57 OR             
532500                          67 OR 92 OR 98)                                 
532600        PERFORM S11-SKRIV-VOR-RAD                                         
532700     END-IF                                                               
532800                                                                          
532900*** TILLÄGGSTPO SKAPAS                                                    
533000                                                                          
533100     IF DDGS-TPO-OBKR71                                                   
533200        PERFORM ECSC-SKAPA-TPO2                                           
533300     END-IF                                                               
533400     .                                                                    
533500     EJECT                                                                
533600 ECSA-REDIGERA-OBKR-RAD SECTION.                                          
533700                                                                          
533800     MOVE 'STA ECSA-RED-OBKR   '           TO   WS-PGM-POSITION           
533900     MOVE ORAD-IDORDER         TO OBKR-IDORDER                            
534000     MOVE ORFK-IDARTNR(WS-INDEX-MID)                                      
534100                               TO OBKR-IDARTNR                            
534200     MOVE ZERO                 TO OBKR-KDERS                              
534300     IF NOT TILLKOMMANDE-RAD                                              
534400     OR OBKR-IDLOPNR NOT NUMERIC                                          
534500        MOVE OBKR-IDORDER      TO W-IDORDER-Q1-MIN                        
534600                                  W-IDORDER-Q1-MAX                        
534700        MOVE OBKR-IDARTNR      TO W-IDARTNR-Q1-MIN                        
534800                                  W-IDARTNR-Q1-MAX                        
534900        MOVE +1                TO W-IDLOPNR-Q1-MIN                        
535000                                  W-IDLOPNR-Q1-MAX                        
535100                                  W-IDSEKVNR-Q1-MIN                       
535200                                  W-IDSEKVNR-Q1-MAX                       
535300        PERFORM IMS-07-GU-ORQM-WDQ101                                     
535400        PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                        
535500           ADD +1              TO W-IDLOPNR-Q1-MIN                        
535600                                  W-IDLOPNR-Q1-MAX                        
535700           PERFORM IMS-07-GN-ORQM-WDQ101                                  
535800        END-PERFORM                                                       
535900        MOVE W-IDLOPNR-Q1-MIN  TO OBKR-IDLOPNR                            
536000        MOVE +1                TO OBKR-IDSEKVNR                           
536100     END-IF                                                               
536200     MOVE ORAD-IDDC            TO OBKR-IDDC                               
536300     MOVE +0                   TO OBKR-KDORDBEK                           
536400     MOVE SPACE                TO OBKR-BEERS                              
536500     MOVE SPACE                TO OBKR-IDBIL                              
536600     MOVE OHUV-BEKUNDRF        TO OBKR-BEKUNDRF                           
536700     MOVE ORAD-BERADREF        TO OBKR-BERADREF                           
536800     MOVE ORAD-BEVOLREF        TO OBKR-BEVOLREF                           
536900     MOVE ORAD-IDKAMPRF        TO OBKR-IDKAMPRF                           
537000     MOVE +0                   TO OBKR-DIERS-KVOT                         
537100     MOVE ORAD-FLAKPLOC        TO OBKR-FLAKPLOC                           
537200     MOVE ORAD-FLINVEST        TO OBKR-FLINVEST                           
537300     MOVE JA                   TO OBKR-FLOBOK                             
537400     MOVE ORAD-FLOBTRAN        TO OBKR-FLOBTRAN                           
537500     MOVE NEJ                  TO OBKR-FLOBPRT                            
537600     MOVE ORAD-FLPRTILL        TO OBKR-FLPRTILL                           
537700     MOVE ORAD-FLRESTN         TO OBKR-FLRESTN                            
537800     MOVE ORFK-FLSLATT (WS-INDEX-MID)                                     
537900                               TO OBKR-FLSLATT                            
538000     MOVE ORAD-FLTILLK         TO OBKR-FLTILLK                            
538100     IF ORFK-KDORDBEK (WS-INDEX-MID) = 59                                 
538200        MOVE ORAD-REKSIFFR     TO OBKR-REKSIFFR                           
538300     ELSE                                                                 
538400        MOVE ORFK-REKSIFFR(WS-INDEX-MID)                                  
538500                               TO OBKR-REKSIFFR                           
538600     END-IF                                                               
538700     IF TILLKOMMANDE-RAD                                                  
538800        MOVE TILK-IDARTNR-TILLK(WS-INDEX-TILLK)                           
538900                               TO OBKR-IDARTNR-TILLK                      
539000        MOVE TILK-REKSIFFR-TILLK(WS-INDEX-TILLK)                          
539100                               TO OBKR-REKSIFFR-TILLK                     
539200     ELSE                                                                 
539300        MOVE +0                TO OBKR-IDARTNR-TILLK                      
539400        MOVE +0                TO OBKR-REKSIFFR-TILLK                     
539500     END-IF                                                               
539600     MOVE ORAD-IDDC-RO         TO OBKR-IDDC-RO                            
539700     MOVE ORAD-IDGMTREF        TO OBKR-IDGMTREF                           
539800     MOVE ORAD-IDKUNDRF-RO     TO OBKR-IDKUNDRF-RO                        
539900     MOVE ORAD-IDLEVNR         TO OBKR-IDLEVNR                            
540000     MOVE ORAD-IDLOPNR-RO      TO OBKR-IDLOPNR-RO                         
540100     MOVE ORAD-IDSYSTEM        TO OBKR-IDSYSTEM                           
540200     MOVE ORAD-KDDSP           TO OBKR-KDDSP                              
540300     IF NOT TILLKOMMANDE-RAD                                              
540400     OR LDC-ARTIKEL-BYTT                                                  
540500        MOVE AREG-KDERS        TO OBKR-KDERS                              
540600     END-IF                                                               
540700     MOVE ORAD-KDKVBRYT        TO OBKR-KDKVBRYT                           
540800     MOVE ORAD-KDOI            TO OBKR-KDOI                               
540900     MOVE ORAD-CLEARGROUP      TO OBKR-CLEARGROUP                         
541000     MOVE ORAD-KDPRTYP         TO OBKR-KDPRTYP                            
541100     MOVE ORAD-KDTPOTYP        TO OBKR-KDTPOTYP                           
541200     MOVE ORAD-KDVRINFO        TO OBKR-KDVRINFO                           
541300     MOVE +0                   TO OBKR-KVANNANT                           
541400     MOVE +0                   TO OBKR-KVAVBART                           
541500     MOVE ORAD-KVBEART         TO OBKR-KVBEART                            
541600     MOVE ORAD-KVBEART-Q       TO OBKR-KVBEART-Q                          
541700     MOVE +0                   TO OBKR-KVBEART-TILLK                      
541800     MOVE +0                   TO OBKR-KVPREAVB                           
541900     MOVE +0                   TO OBKR-KVPRERO                            
542000     IF KVAN-KVQPACK-UT NUMERIC                                           
542100        MOVE KVAN-KVQPACK-UT   TO OBKR-KVQPACK                            
542200     ELSE                                                                 
542300        MOVE ZERO              TO OBKR-KVQPACK                            
542400     END-IF                                                               
542500     MOVE +0                   TO OBKR-KVRO                               
542600     MOVE ORAD-KVSLATT         TO OBKR-KVSLATT                            
542700     MOVE ORAD-PRARTNTO        TO OBKR-PRARTNTO                           
542800     MOVE ORAD-DEAL-PR-LINE    TO OBKR-DEAL-PR-LINE                       
542900     MOVE ORAD-PRBPRIS         TO OBKR-PRBPRIS                            
543000     MOVE ORAD-RERF-RAD        TO OBKR-RERF-RAD                           
543100     MOVE AREG-TIDISPIN        TO OBKR-TIDISPIN                           
543200     MOVE OHUV-TIREGDAT        TO OBKR-TIORDREG                           
543300     MOVE ORAD-TIPRIS          TO OBKR-TIPRIS                             
543400     MOVE ORAD-TIREGDAT        TO OBKR-TIREGDAT                           
543500     MOVE ORAD-TIREGTID        TO OBKR-TIREGTID                           
543600     MOVE +0                   TO OBKR-TIRODAT                            
543700     MOVE ORAD-TIREGDAT        TO WS-AAMMDD-9KOMPL                        
543800     IF WS-AAMMDD-9KOMPL(1:2) < 50                                        
543900       MOVE 20                 TO WS-SEKEL-9KOMPL                         
544000     ELSE                                                                 
544100       MOVE 19                 TO WS-SEKEL-9KOMPL                         
544200     END-IF                                                               
544300     COMPUTE OBKR-TITIREGD-9KOMPL = 999999999 - WS-TITIREGD-9KOMPL        
544400     MOVE ORAD-TITPO           TO OBKR-TITPO                              
544500     MOVE OHUV-TIREGDAT        TO WS-AAMMDD-9KOMPL                        
544600     IF WS-AAMMDD-9KOMPL(1:2) < 50                                        
544700       MOVE 20                 TO WS-SEKEL-9KOMPL                         
544800     ELSE                                                                 
544900       MOVE 19                 TO WS-SEKEL-9KOMPL                         
545000     END-IF                                                               
545100     COMPUTE OBKR-TITIORDD-9KOMPL = 999999999 - WS-TITIREGD-9KOMPL        
545200     MOVE ARB-KDFRAKT          TO OBKR-KDFRAKT                            
545300     MOVE OHUV-KDORDKL         TO OBKR-KDORDKL                            
545400                                                                          
545500     MOVE ORAD-IDBIL           TO OBKR-IDBIL                              
545600     MOVE OHUV-KDORDTYP-LDC    TO OBKR-KDORDTYP-LDC                       
545700     MOVE OHUV-TIREPDAT        TO OBKR-TIREPDAT                           
545800     MOVE ORAD-IDKUNDRF-WIP    TO OBKR-IDKUNDRF-WIP                       
545900     MOVE ZERO                 TO OBKR-TIDLEVDAT                          
546000     MOVE ORAD-PRAVCOST        TO OBKR-PRAVCOST                           
546100     MOVE ORAD-KDVALISO        TO OBKR-KDVALISO                           
546200     .                                                                    
546300     EJECT                                                                
546400 ECSB-BACKA-FORBI-REFILL SECTION.                                         
546500                                                                          
546600     MOVE 'STA ECSB-BACKA      '           TO   WS-PGM-POSITION           
546700                                                                          
546800******************************************************************        
546900*                                                                         
547000*  KOLLA OM TRANSFER (GER SVARET RADER-FINNS)                             
547100*  GÄLLER I PRAKTIKEN BARA EU-TRANSFER NOV-05                             
547200*                                                                         
547300******************************************************************        
547400                                                                          
547500     MOVE ORAD-IDDISTR         TO W-TP4TRAN-IDDISTR                       
547600                                                                          
547700     PERFORM DB2-SELECT-TP4TRAN                                           
547800                                                                          
547900     MOVE ORAD-IDDISTR      TO DIST35-IDDISTR                             
548000     IF  DIST35-REFILL           OR                                       
548100         DIST35-REFILL-INOM-NDC  OR                                       
548200         DIST35-NONVCC-NONVCC-REFILL      OR                              
548300         DIST35-NONVCC-NONVCC-TRANSFER OR                                 
548400         DIST35-NA-TRANSFER      OR                                       
548500         DIST35-NA-NDC-RETURNS   OR                                       
548600         DIST35-PACIFIC-TRANSFER OR                                       
548700         DIST35-REFILL-INOM-JP   OR                                       
548800         DIST35-CN-TRANSFER      OR                                       
548900         DIST35-NONVCC-VCC-REFILL OR                                      
549000         DIST35-NONVCC-VCC-TRANSFER OR                                    
549100         RADER-FINNS                                                      
549200                                                                          
549300       IF RADER-FINNS                                                     
549400         MOVE TP4TRAN-IDDC-REC                                            
549500                             TO W-IDDC                                    
549600       ELSE                                                               
549700                                                                          
549800         SEARCH ALL DIST57-REFILL-DC                                      
549900            AT END                                                        
550000               MOVE 'EJ TRÄFF I REFILLTAB WWDIST57'                       
550100                                TO FELTEXT                                
550200               CALL FELLOG USING RKOD-ABEND                               
550300            WHEN DIST57-SOK-IDDISTR(DIST57-IX) = ORAD-IDDISTR             
550400               MOVE DIST57-REFILL-TO-DC(DIST57-IX) TO W-IDDC              
550500         END-SEARCH                                                       
550600       END-IF                                                             
550700                                                                          
550800       MOVE ORAD-IDARTNR TO W-IDARTNR                                     
550900                                                                          
551000       PERFORM IMS-GHU-WDK711                                             
551100       COMPUTE SLAG-KVBEART = SLAG-KVBEART - ORAD-KVBEART-Q               
551200                                                                          
551300       PERFORM IMS-REPL-WDK7                                              
551400     ELSE                                                                 
551500        IF DIST35-NONVCC-CDC-REFILL                                       
551600           SEARCH ALL DIST57-REFILL-DC                                    
551700             AT END                                                       
551800               MOVE 'EJ TRÄFF I REFILLTAB WWDIST57'                       
551900                                TO FELTEXT                                
552000               CALL FELLOG USING RKOD-ABEND                               
552100             WHEN DIST57-SOK-IDDISTR(DIST57-IX) = ORAD-IDDISTR            
552200               MOVE DIST57-REFILL-TO-DC(DIST57-IX) TO W-IDDC              
552300           END-SEARCH                                                     
552400                                                                          
552500           MOVE ORAD-IDARTNR TO W-IDARTNR                                 
552600                                                                          
552700           PERFORM IMS-GHU-WDK611                                         
552800           COMPUTE CLAG-KVBEART = CLAG-KVBEART - ORAD-KVBEART-Q           
552900                                                                          
553000           PERFORM IMS-REPL-WDK611                                        
553100        END-IF                                                            
553200     END-IF                                                               
553300     .                                                                    
553400     EJECT                                                                
553500 ECSC-SKAPA-TPO2 SECTION.                                                 
553600                                                                          
553700     MOVE WC-CDC-SE            TO W-IDDC-WDB3                             
553800                                  W-IDDC-WDB3-DEF                         
553900     MOVE OBKR-IDDISTR         TO W-IDDISTR-WDB3                          
554000                                  W-IDDISTR-WDB3-DEF                      
554100     MOVE OBKR-IDKUNDNR        TO W-IDKUNDNR-WDB3                         
554200     PERFORM IMS-GU-WDB301                                                
554300     IF SEGMENT-FINNS                                                     
554400        IF OBKR-KDORDKL = 1                                               
554500           MOVE DC-KDGENFRA-DO TO OBKR-KDFRAKT                            
554600        ELSE                                                              
554700           MOVE DC-KDGENFRA-MO TO OBKR-KDFRAKT                            
554800        END-IF                                                            
554900     END-IF                                                               
555000     MOVE +2                   TO OBKR-KDTPOTYP                           
555100     MOVE +71                  TO OBKR-KDORDBEK                           
555200     MOVE +0                   TO OBKR-KVPREAVB                           
555300     MOVE +0                   TO OBKR-KVPRERO                            
555400     MOVE IDPGM                TO OBKR-IDPGM                              
555500     IF DDGS-TPO-OBKR71                                                   
555600        MOVE OHUV-TITPO        TO OBKR-TITPO                              
555700        MOVE SPACE             TO OBKR-KDOI                               
555800     ELSE                                                                 
555900        MOVE OBKR-TIORDREG     TO OBKR-TITPO                              
556000        MOVE ORAD-KDOI         TO OBKR-KDOI                               
556100     END-IF                                                               
556200     MOVE ORAD-CLEARGROUP      TO OBKR-CLEARGROUP                         
556300     MOVE NEJ                  TO ALLT-SW                                 
556400     IF TILLKOMMANDE-RAD                                                  
556500        MOVE TILK-KVBEART(WS-INDEX-TILLK)                                 
556600                               TO OBKR-KVBEART-TILLK                      
556700        IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                        
556800           TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                             
556900          MOVE +0              TO OBKR-DIERS-KVOT                         
557000        ELSE                                                              
557100          COMPUTE OBKR-DIERS-KVOT =                                       
557200                               TILK-DIERS-TILLK(WS-INDEX-TILLK)           
557300                               / TILK-DIERS-ERS(WS-INDEX-TILLK)           
557400        END-IF                                                            
557500     END-IF                                                               
557600     MOVE 'S'                  TO OBKR-SW                                 
557700     PERFORM IMS-25-ISRT-WLORQM01-WDQ101                                  
557800     ADD +1                    TO OBKR-IDSEKVNR                           
557900                                                                          
558000     IF EJ-DDGS-TPO-OBKR71                                                
558100        MOVE ORAD-IDDISTR        TO TPO2-IDDISTR                          
558200        MOVE ORAD-IDKUNDNR       TO TPO2-IDKUNDNR                         
558300        IF ORAD-IDKUNDRF-RO = '0000000   '                                
558400           MOVE ORAD-IDKUNDRF    TO TPO2-IDKUNDRF                         
558500        ELSE                                                              
558600           MOVE ORAD-IDKUNDRF-RO TO TPO2-IDKUNDRF                         
558700        END-IF                                                            
558800        MOVE ORAD-IDARTNR        TO TPO2-IDARTNR                          
558900        MOVE ORAD-BERADREF       TO TPO2-BERADREF                         
559000        MOVE AREG-IDANSK         TO TPO2-IDANSK                           
559100        MOVE OHUV-IDKONTO        TO TPO2-IDKONTO                          
559200        MOVE OHUV-IDKST          TO TPO2-IDKST                            
559300        MOVE OHUV-IDANALYS       TO TPO2-IDANALYS                         
559400        MOVE ORAD-KDDSP          TO TPO2-KDDSP                            
559500        MOVE OHUV-KDFAKTYP       TO TPO2-KDFAKTYP                         
559600        MOVE OBKR-KDFRAKT        TO TPO2-KDFRAKT                          
559700        MOVE ORAD-KDKVBRYT       TO TPO2-KDKVBRYT                         
559800        MOVE ORAD-KDORDING       TO TPO2-KDORDING                         
559900        MOVE OHUV-KDORDKL        TO TPO2-KDORDKL                          
560000        MOVE AREG-KDPRODSL       TO TPO2-KDPRODSL                         
560100        MOVE ORAD-KDVRINFO       TO TPO2-KDVRINFO                         
560200        MOVE ORAD-KVBEART-Q      TO TPO2-KVBEART-Q                        
560300        MOVE AREG-REKSIFFR       TO TPO2-REKSIFFR                         
560400        MOVE ORAD-TITPO          TO TPO2-TITPO                            
560500        MOVE ORAD-PRARTNTO       TO TPO2-PRARTNTO                         
560600        MOVE ORAD-DEAL-PR-LINE   TO TPO2-DEAL-PR-LINE                     
560700        MOVE ORAD-KDPRTYP        TO TPO2-KDPRTYP                          
560800        MOVE ORAD-FLPRTILL       TO TPO2-FLPRTILL                         
560900        MOVE ORAD-BEVOLREF       TO TPO2-BEVOLREF                         
561000        MOVE ORAD-IDKAMPRF       TO TPO2-IDKAMPRF                         
561100        MOVE ORAD-IDSYSTEM       TO TPO2-IDSYSTEM                         
561200        MOVE ORAD-FLINVEST       TO TPO2-FLINVEST                         
561300        MOVE ORAD-IDLEVNR        TO TPO2-IDLEVNR                          
561400        MOVE AREG-KDUART         TO TPO2-KDUART                           
561500        MOVE AREG-KVFRYSTI       TO TPO2-KVFRYSTI                         
561600        MOVE 3                   TO TPO2-KDORDBEH                         
561700        MOVE 2                   TO TPO2-KDTPOTYP                         
561800        MOVE OHUV-BEKUNDRF       TO TPO2-BEKUNDRF                         
561900        MOVE OHUV-FLORDSPE       TO TPO2-FLORDSPE                         
562000        MOVE OHUV-FLOVRLEV       TO TPO2-FLOVRLEV                         
562100        MOVE OHUV-FLFORBI        TO TPO2-FLFORBI                          
562200        MOVE ORAD-FLTILLK        TO TPO2-FLTILLK                          
562300        MOVE AREG-TIDISPIN       TO TPO2-TIDISPIN                         
562400        MOVE OHUV-BEVARREF       TO TPO2-BEVARREF                         
562500*       MOVE KVAN-KVQPACK-UT     TO TPO2-KVQPACK-1                        
562600        IF KVAN-KVQPACK-UT NUMERIC                                        
562700           MOVE KVAN-KVQPACK-UT  TO TPO2-KVQPACK-1                        
562800        ELSE                                                              
562900           MOVE ZERO             TO TPO2-KVQPACK-1                        
563000        END-IF                                                            
563100        MOVE ORAD-KVBEART        TO TPO2-KVBEART                          
563200        MOVE OHUV-KDORDTYP-LDC   TO TPO2-KDORDTYP-LDC                     
563300        MOVE OHUV-TIREPDAT       TO TPO2-TIREPDAT                         
563400        MOVE ORAD-IDKUNDRF-WIP   TO TPO2-IDKUNDRF-WIP                     
563500                                                                          
563600        CALL W411TPO2 USING TPO2-W411TPO2                                 
563700                            TPO2-ORDP-PCB                                 
563800                            TPO2-XXBU-PCB TPO2-XXBV-PCB                   
563900                            TPO2-ARTM-PCB TPO2-FILA-PCB                   
564000                            TPO2-XXBX-PCB TIME-4437-PCB                   
564100     END-IF                                                               
564200     .                                                                    
564300     EJECT                                                                
564400                                                                          
564500 ED-LAES-TILLK-DATA SECTION.                                              
564600                                                                          
564700     MOVE 'STA ED-LAES-TILLK   '           TO   WS-PGM-POSITION           
564800     MOVE TILK-IDARTNR-TILLK(WS-INDEX-TILLK)                              
564900                               TO AREG-IDARTNR                            
565000                                                                          
565100     CALL W411AREG USING AREG-W411AREG                                    
565200                         AREG-WDK6-PCB                                    
565300                         AREG-WDK7-PCB                                    
565400     .                                                                    
565500     EJECT                                                                
565600 F-BIPACKNING-ORDERAVSLUT SECTION.                                        
565700                                                                          
565800     MOVE 'STA F-BIPACK        '           TO   WS-PGM-POSITION           
565900     IF OHUV-FLOVRLEV = JA OR OHUV-FLORDSPE = JA OR                       
566000                              OHUV-KDORDKL  = +0 OR                       
566100                              OHUV-IDKAMPRF > +0 OR                       
566200                              OHUV-KDTPOTYP > +0 OR                       
566300**LDC INGEN BIPACKNING ÄVEN FÖR RADER SOM HAMNAR DIREKT PÅ CDC            
566400                              OHUV-IDSYSTEM = 'LDC ' OR                   
566500                              OHUV-IDSYSTEM = 'TACD' OR                   
566600                              (AVSR-KDROPACK = ZERO OR SPACE)             
566700                                                                          
566800        PERFORM FA-STARTA-ORDERAVSLUT                                     
566900     ELSE                                                                 
567000        PERFORM FB-STARTA-BIPACKNING-4299                                 
567100     END-IF                                                               
567200     .                                                                    
567300     EJECT                                                                
567400                                                                          
567500 FA-STARTA-ORDERAVSLUT SECTION.                                           
567600                                                                          
567700     MOVE 'STA FA-STA-ORD-AVSLU'           TO   WS-PGM-POSITION           
567800     MOVE W-IDDISTR            TO 4298-MID-IDDISTR                        
567900     MOVE W-IDKUNDNR           TO 4298-MID-IDKUNDNR                       
568000     MOVE W-IDKUNDRF           TO 4298-MID-IDKUNDRF                       
568100     MOVE OHUV-IDORDER         TO 4298-MID-IDORDER                        
568200                                                                          
568300     COMPUTE MSG-KVLL = LENGTH OF 4298-MID-W4I29801 + 17                  
568400     MOVE 'W4T298X '           TO MSG-KDTRANS-1                           
568500     MOVE '4252'               TO MSG-IDTRANS-1                           
568600     MOVE SPACE                TO MSG-KDMFSFOR-1                          
568700                                                                          
568800     PERFORM IMS-INSERT-4298-MSG                                          
568900     .                                                                    
569000     EJECT                                                                
569100                                                                          
569200 FB-STARTA-BIPACKNING-4299 SECTION.                                       
569300                                                                          
569400     MOVE 'STA FB-4299         '           TO   WS-PGM-POSITION           
569500     MOVE OHUV-BEKUNDRF        TO 4299-MID-BEKUNDRF                       
569600     MOVE OHUV-FLEMBORD        TO 4299-MID-FLEMBORD                       
569700     MOVE OHUV-FLFORBI         TO 4299-MID-FLFORBI                        
569800     MOVE OHUV-FLOVRLEV        TO 4299-MID-FLOVRLEV                       
569900     MOVE OHUV-FLPRELRO        TO 4299-MID-FLPRELRO                       
570000     MOVE W-IDDISTR            TO 4299-MID-IDDISTR                        
570100     MOVE OHUV-IDKAMPRF        TO 4299-MID-IDKAMPRF                       
570200     MOVE OHUV-IDKONTO         TO 4299-MID-IDKONTO                        
570300     MOVE OHUV-IDKST           TO 4299-MID-IDKST                          
570400     MOVE OHUV-IDANALYS        TO 4299-MID-IDANALYS                       
570500     MOVE W-IDKUNDNR           TO 4299-MID-IDKUNDNR                       
570600     MOVE W-IDKUNDRF           TO 4299-MID-IDKUNDRF                       
570700     MOVE OHUV-IDRFTAB         TO 4299-MID-IDRFTAB                        
570800     MOVE OHUV-IDORDER         TO 4299-MID-IDORDER                        
570900     MOVE MID-IDSYSTEM         TO 4299-MID-IDSYSTEM                       
571000     MOVE OHUV-KDFAKTYP        TO 4299-MID-KDFAKTYP                       
571100     MOVE ARB-KDFDKRAV         TO 4299-MID-KDFDKRAV                       
571200     MOVE OHUV-KDORDING        TO 4299-MID-KDORDING                       
571300     MOVE OHUV-KDORDKL         TO 4299-MID-KDORDKL                        
571400     MOVE OHUV-KDTPOTYP        TO 4299-MID-KDTPOTYP                       
571500     MOVE OHUV-RESLATT         TO 4299-MID-RESLATT                        
571600     MOVE OHUV-TIREGDAT        TO 4299-MID-TIREGDAT                       
571700     MOVE OHUV-BEVARREF        TO 4299-MID-BEVARREF                       
571800     MOVE OHUV-IDBIPREF        TO 4299-MID-IDBIPREF                       
571900     MOVE ARB-KDROPACK         TO 4299-MID-KDROPACK                       
572000     MOVE ARB-KDFRAKT          TO 4299-MID-KDFRAKT                        
572100     IF OHUV-IDDC-TVS NOT = SPACE                                         
572200       MOVE OHUV-IDDC-TVS      TO 4299-MID-IDDC                           
572300     ELSE                                                                 
572400       MOVE SPACE              TO 4299-MID-IDDC                           
572500     END-IF                                                               
572600                                                                          
572700     COMPUTE MSG-KVLL = LENGTH OF 4299-MID-W4I29901 + 17                  
572800     MOVE 'W4T299X '           TO MSG-KDTRANS-1                           
572900     MOVE '4252'               TO MSG-IDTRANS-1                           
573000     MOVE SPACE                TO MSG-KDMFSFOR-1                          
573100                                                                          
573200     PERFORM IMS-INSERT-4299-MSG                                          
573300     .                                                                    
573400     EJECT                                                                
573500                                                                          
573600 G-SKICKA-PRISFRAGA SECTION.                                              
573700                                                                          
573800     MOVE 1                      TO 3039-REQU-IDMSGVER                    
573900     MOVE SPACE                  TO 3039-REQU-KDPGMACT                    
574000     MOVE 'W4025200'             TO 3039-REQU-IDUSER                      
574100                                                                          
574200     MOVE SPACE                  TO 3039-MID-IDBUNDLE                     
574300     MOVE ORAD-IDDISTR           TO 3039-MID-IDDISTR                      
574400     MOVE ORAD-IDKUNDNR          TO 3039-MID-IDKUNDNR                     
574500     IF MID-IDKUNDRF-RO NOT = SPACE                                       
574600       MOVE MID-IDKUNDRF-RO      TO 3039-MID-IDBUNDLE                     
574700     ELSE                                                                 
574800       MOVE ORAD-IDORDNR7        TO 3039-MID-IDBUNDLE                     
574900     END-IF                                                               
575000     MOVE ZERO                   TO 3039-MID-IDPRQUES                     
575100                                                                          
575200     PERFORM S04-SKICKA-OPEN                                              
575300     PERFORM S04-SKICKA-MEDDELANDE                                        
575400     PERFORM S04-SKICKA-CLOSE                                             
575500                                                                          
575600     .                                                                    
575700     EJECT                                                                
575800 Z-FINIT  SECTION.                                                        
575900                                                                          
576000*    SKRIV MED TILL MPP DISPATCHERN                                       
576100     IF MSG-KOM-IDMFSMED = SPACE                                          
576200        MOVE OK-BEHANDLAD      TO MSG-KOM-IDMFSMED                        
576300     END-IF                                                               
576400     PERFORM IMS-INSERT-DISP-MSG                                          
576500     .                                                                    
576600     EJECT                                                                
576700 S02-RENSA-TILLK-TAB SECTION.                                             
576800                                                                          
576900     MOVE 'STA S02-RENSA       '           TO   WS-PGM-POSITION           
577000     MOVE +1              TO WS-INDEX-TILLK                               
577100     PERFORM UNTIL WS-INDEX-TILLK > WS-INDEX-TILLK-MAX                    
577200        MOVE +0           TO TILK-IDARTNR(WS-INDEX-TILLK)                 
577300        MOVE +0           TO TILK-IDARTNR-TILLK(WS-INDEX-TILLK)           
577400        MOVE SPACE        TO TILK-BEERS(WS-INDEX-TILLK)                   
577500        MOVE +0           TO TILK-DIERS-ERS(WS-INDEX-TILLK)               
577600        MOVE +0           TO TILK-DIERS-TILLK(WS-INDEX-TILLK)             
577700        MOVE SPACE        TO TILK-FLFINLV(WS-INDEX-TILLK)                 
577800        MOVE SPACE        TO TILK-FLPRTILL(WS-INDEX-TILLK)                
577900        MOVE SPACE        TO TILK-FLTILLK-X(WS-INDEX-TILLK)               
578000        MOVE +0           TO TILK-KDERS(WS-INDEX-TILLK)                   
578100        MOVE SPACE        TO TILK-KDPRTYP(WS-INDEX-TILLK)                 
578200        MOVE +0           TO TILK-KVBEART(WS-INDEX-TILLK)                 
578300        MOVE +0           TO TILK-PRARTNTO(WS-INDEX-TILLK)                
578400        INITIALIZE           TILK-DEAL-PR-LINE(WS-INDEX-TILLK)            
578500        MOVE +0           TO TILK-TIPRIS(WS-INDEX-TILLK)                  
578600        MOVE +0           TO TILK-REKSIFFR-TILLK(WS-INDEX-TILLK)          
578700        ADD +1            TO WS-INDEX-TILLK                               
578800     END-PERFORM                                                          
578900     MOVE +1              TO WS-INDEX-TILLK                               
579000     .                                                                    
579100     EJECT                                                                
579200                                                                          
579300 S04-SKICKA-OPEN SECTION.                                                 
579400                                                                          
579500     MOVE 'OPEN'                     TO SEND-KDFUNC                       
579600     MOVE 'CARPARTS.PULS.PRQRY' TO SEND-ADDISPABS                         
579700     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-OPEN-AREA                 
579800                                                                          
579900     IF SEND-KDRC > 0                                                     
580000       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
580100       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
580200       DELIMITED BY SIZE INTO FELTEXT                                     
580300       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
580400     END-IF                                                               
580500     .                                                                    
580600     SKIP3                                                                
580700 S04-SKICKA-MEDDELANDE SECTION.                                           
580800                                                                          
580900     MOVE 'PUT'                      TO SEND-KDFUNC                       
581000     MOVE LENGTH OF SEND-AREA        TO SEND-KVDLEN                       
581100     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-KVDLEN SEND-AREA          
581200                                                                          
581300     IF SEND-KDRC > 0                                                     
581400       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
581500       STRING 'WZ01SEND GET ERROR RC=' KDRC-DISPLAY                       
581600       DELIMITED BY SIZE INTO FELTEXT                                     
581700       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
581800     END-IF                                                               
581900     .                                                                    
582000     SKIP3                                                                
582100 S04-SKICKA-CLOSE SECTION.                                                
582200                                                                          
582300     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
582400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
582500                                                                          
582600     IF SEND-KDRC > 0                                                     
582700       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
582800       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
582900       DELIMITED BY SIZE INTO FELTEXT                                     
583000       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
583100     END-IF                                                               
583200     .                                                                    
583300     EJECT                                                                
583400                                                                          
583500                                                                          
583600 S05-NOLLSTALL-OBKR-KODER SECTION.                                        
583700                                                                          
583800     MOVE 'STA S05-NOLLA       '           TO   WS-PGM-POSITION           
583900     MOVE ZERO                 TO KVAN-KDORDBEK-UT                        
584000                                  DLEV-KDORDBEK-UT                        
584100                                  KERS-KDERS                              
584200     IF NOT TILLKOMMANDE-RAD                                              
584300        MOVE ZERO              TO KERS-KDORDBEK                           
584400     ELSE                                                                 
584500        MOVE JA                TO OBKR-SW                                 
584600     END-IF                                                               
584700     MOVE ZERO                 TO TPO1-KDORDBEK                           
584800                                  TPO2-KDORDBEK                           
584900                                  RELS-KDORDBEK                           
585000                                  TPO3-KDORDBEK                           
585100                                  TPO5-KDORDBEK                           
585200                                  TPO6-KDORDBEK                           
585300                                  KAMP-KDORDBEK                           
585400                                  STOR-KDORDBEK                           
585500*                                 NDCA-KDORDBEK                           
585600                                  XDCA-KDORDBEK                           
585700                                  SDCA-KDORDBEK                           
585800                                  SDCA-KDORDBEK-FIRST-SDC                 
585900                                  SDCA-KDORDBEK-SECOND-SDC                
586000                                  CDCA-KDORDBEK-UT                        
586100                                  SPAR-KDORDBEK                           
586200                                                                          
586300     MOVE JA                   TO ALLT-SW                                 
586400     MOVE NEJ                  TO KOLLA-ERS-SW                            
586500                                                                          
586600******************************************************************        
586700*                                                                         
586800*  KOLLA OM TRANSFER (GER SVARET RADER-FINNS)                             
586900*  GÄLLER I PRAKTIKEN BARA EU-TRANSFER NOV-05                             
587000*                                                                         
587100******************************************************************        
587200                                                                          
587300     MOVE ORFK-IDDISTR         TO W-TP4TRAN-IDDISTR                       
587400                                                                          
587500     PERFORM DB2-SELECT-TP4TRAN                                           
587600                                                                          
587700     IF RADER-FINNS                                                       
587800       MOVE JA     TO WS-TRANSFER                                         
587900     ELSE                                                                 
588000       MOVE NEJ    TO WS-TRANSFER                                         
588100     END-IF                                                               
588200                                                                          
588300     IF ORFK-KDORDBEK(WS-INDEX-MID) = 56                                  
588400       IF DIST35-RETUR           OR DIST35-REFILL            OR           
588500          DIST35-REFILL-INOM-NDC OR DIST35-NONVCC-REFILL     OR           
588600          DIST35-NONVCC-VCC-TRANSFER OR                                   
588700          DIST35-NONVCC-NONVCC-TRANSFER OR                                
588800          DIST35-REFILL-NA-JAP   OR DIST35-NA-CDC-RETURN     OR           
588900          DIST35-CDC-RETURNS-NON-VCC OR                                   
589000          DIST35-CN-NDC-RETURNS  OR DIST35-NA-NDC-RETURNS    OR           
589100          DIST35-NA-TRANSFER     OR DIST35-PACIFIC-TRANSFER  OR           
589200          DIST35-REFILL-INOM-JP  OR                                       
589300          DIST35-CN-TRANSFER     OR                                       
589400          DIST35-ST-CDC          OR DIST35-NL-SITTARD-OBJEKT OR           
589500          DIST18-SKROT           OR DIST18-SCRAP-NDC         OR           
589600          RADER-FINNS                                                     
589700                                                                          
589800         MOVE 0 TO ORFK-KDORDBEK(WS-INDEX-MID)                            
589900        END-IF                                                            
590000     END-IF                                                               
590100                                                                          
590200     IF ORFK-KDORDBEK(WS-INDEX-MID) = 56                                  
590300       MOVE ORFK-KDORDBEK(WS-INDEX-MID) TO W-KDORDBEK                     
590400       MOVE +7                          TO W-KDTPOTYP                     
590500     ELSE                                                                 
590600       IF ORFK-KDORDBEK(WS-INDEX-MID) > 0                                 
590700          MOVE NEJ               TO ALLT-SW                               
590800          MOVE JA                TO OBKR-SW                               
590900       END-IF                                                             
591000     END-IF                                                               
591100     .                                                                    
591200     EJECT                                                                
591300 S07-RO-TVINGANDE-TILLAGG SECTION.                                        
591400                                                                          
591500     MOVE ORAD-IDDISTR         TO WDA5-RAD-IDDISTR                        
591600     MOVE ORAD-IDKUNDNR        TO WDA5-RAD-IDKUNDNR                       
591700     MOVE MID-IDKUNDRF-RO(3:5) TO WDA5-RAD-IDKUNDRF                       
591800     MOVE MID-IDORDNR(3:5)     TO WDA5-RAD-IDKUNDRF-LEV                   
591900     MOVE ORAD-IDDC            TO WDA5-RAD-IDDC                           
592000     MOVE ORAD-IDDC-RO         TO WDA5-RAD-IDDC-RO                        
592100     MOVE ORAD-KDOI            TO WDA5-RAD-KDOI                           
592200     MOVE ORAD-CLEARGROUP      TO WDA5-RAD-CLEARGROUP                     
592300     MOVE ORAD-IDARTNR         TO WDA5-RAD-IDARTNR                        
592400     MOVE +1                   TO WDA5-RAD-IDLOPNR                        
592500     MOVE OHUV-BEKUNDRF        TO WDA5-RAD-BEKUNDRF                       
592600     MOVE ORAD-BERADREF        TO WDA5-RAD-BERADREF                       
592700     MOVE NEJ                  TO WDA5-RAD-FLERS                          
592800                                                                          
592900     MOVE OHUV-IDKONTO         TO WDA5-RAD-IDKONTO                        
593000     MOVE OHUV-IDKST           TO WDA5-RAD-IDKST                          
593100     MOVE OHUV-IDANALYS        TO WDA5-RAD-IDANALYS                       
593200     MOVE ORAD-KDDSP           TO WDA5-RAD-KDDSP                          
593300     MOVE OHUV-KDFAKTYP        TO WDA5-RAD-KDFAKTYP                       
593400     MOVE ARB-KDFRAKT          TO WDA5-RAD-KDFRAKT                        
593500     MOVE ORAD-KDKVBRYT        TO WDA5-RAD-KDKVBRYT                       
593600     MOVE 3                    TO WDA5-RAD-KDORDING                       
593700     MOVE OHUV-KDORDKL         TO WDA5-RAD-KDORDKL                        
593800     MOVE ORAD-KDPRODSL        TO WDA5-RAD-KDPRODSL                       
593900     MOVE ZERO                 TO WDA5-RAD-KDROO                          
594000     MOVE ORAD-KVBEART         TO WDA5-RAD-KVRO                           
594100     MOVE ORAD-DEAL-PR-LINE    TO WDA5-RAD-DEAL-PR-LINE                   
594200     MOVE '4'                  TO WDA5-RAD-KDSTARAD                       
594300     MOVE ORFK-KDTPOTYP        TO WDA5-RAD-KDTPOTYP                       
594400     MOVE ZERO                 TO WDA5-RAD-KDVRINFO                       
594500     MOVE ORAD-KVBEART         TO WDA5-RAD-KVART                          
594600     MOVE ORAD-PRARTNTO        TO WDA5-RAD-PRARTNTO                       
594700     IF ORFK-KDORDBEK (WS-INDEX-MID) = 59                                 
594800        MOVE ORAD-REKSIFFR     TO WDA5-RAD-REKSIFFR                       
594900     ELSE                                                                 
595000        MOVE ORFK-REKSIFFR(WS-INDEX-MID)                                  
595100                               TO WDA5-RAD-REKSIFFR                       
595200     END-IF                                                               
595300     MOVE ZERO                 TO WDA5-RAD-TIAVBOKN                       
595400     MOVE ORAD-TIREGDAT        TO WDA5-RAD-TIREGDAT                       
595500     MOVE ZERO                 TO WDA5-RAD-TIRES                          
595600     MOVE ORAD-TIREGDAT        TO WDA5-RAD-DARODAT                        
595700     MOVE DAGENS-DATUM         TO WDA5-RAD-TITPO                          
595800     MOVE ORAD-KDPRTYP         TO WDA5-RAD-KDPRTYP                        
595900     MOVE ORAD-BEVOLREF        TO WDA5-RAD-BEVOLREF                       
596000     MOVE ORAD-FLINVEST        TO WDA5-RAD-FLINVEST                       
596100     MOVE ORAD-FLPRTILL        TO WDA5-RAD-FLPRTILL                       
596200     MOVE JA                   TO WDA5-RAD-FLTPOBEK                       
596300     MOVE ORAD-IDKAMPRF        TO WDA5-RAD-IDKAMPRF                       
596400     MOVE ORAD-IDLEVNR         TO WDA5-RAD-IDLEVNR                        
596500     MOVE ORAD-IDSYSTEM        TO WDA5-RAD-IDSYSTEM                       
596600     MOVE ORAD-KVBEART-Q       TO WDA5-RAD-KVBEART-Q                      
596700     MOVE ORAD-TIREGTID        TO WDA5-RAD-TIREGTID                       
596800     MOVE 999                  TO WDA5-RAD-DASENDAT                       
596900                                  WDA5-RAD-TISENBEK-KL                    
597000                                                                          
597100     MOVE OHUV-KDORDTYP-LDC    TO WDA5-RAD-KDORDTYP-LDC                   
597200     MOVE OHUV-TIREPDAT        TO WDA5-RAD-TIREPDAT                       
597300     MOVE ORAD-IDKUNDRF-WIP    TO WDA5-RAD-IDKUNDRF-WIP                   
597400                                                                          
597500     IF  ALLT-OK                                                          
597600        MOVE AREG-IDANSK       TO WDA5-RAD-IDANSK                         
597700     ELSE                                                                 
597800        MOVE ZERO              TO WDA5-RAD-IDANSK                         
597900     END-IF                                                               
598000     MOVE ORAD-PRAVCOST        TO WDA5-RAD-PRAVCOST                       
598100     MOVE ORAD-KDVALISO        TO WDA5-RAD-KDVALISO                       
598200     MOVE ARB-KDROPACK         TO WDA5-RAD-KDROPACK                       
598300     MOVE ORAD-IDARBREF        TO WDA5-RAD-IDARBREF                       
598400                                                                          
598500     PERFORM S07A-HAMTA-PRIORITETSKOD                                     
598600                                                                          
598700     PERFORM IMS-ISRT-WDA501                                              
598800                                                                          
598900     PERFORM UNTIL SEGMENT-FINNS                                          
599000        ADD 1 TO WDA5-RAD-IDLOPNR                                         
599100        PERFORM IMS-ISRT-WDA501                                           
599200     END-PERFORM                                                          
599300                                                                          
599400     MOVE '0000000   '      TO ORAD-IDKUNDRF-RO                           
599500     MOVE WDA5-RAD-IDKUNDRF(1:5)                                          
599600                            TO ORAD-IDKUNDRF-RO(3:5)                      
599700     MOVE WDA5-RAD-IDLOPNR  TO ORAD-IDLOPNR-RO                            
599800     MOVE NEJ               TO ORAD-FLORDING                              
599900     .                                                                    
600000                                                                          
600100 S07A-HAMTA-PRIORITETSKOD SECTION.                                        
600200                                                                          
600300     MOVE WDA5-RAD-KDTPOTYP      TO W-4512-KDTPOTYP                       
600400     MOVE OHUV-KDORDKL           TO W-4512-KDORDKL                        
600500     MOVE OHUV-IDDISTR           TO W-4512-IDDISTR-FOM                    
600600                                    W-4512-IDDISTR-TOM                    
600700                                                                          
600800     PERFORM IMS-GU-XXJN-WLXXJN11                                         
600900     MOVE 4512-KDRAPRIO          TO WDA5-RAD-KDRAPRIO                     
601000     .                                                                    
601100                                                                          
601200 S08-OBKR-TVINGANDE-TILLAGG   SECTION.                                    
601300                                                                          
601400     MOVE OHUV-IDORDER         TO OBKR-IDORDER                            
601500     MOVE ORFK-IDARTNR(WS-INDEX-MID)                                      
601600                               TO OBKR-IDARTNR                            
601700     IF NOT TILLKOMMANDE-RAD                                              
601800        MOVE OBKR-IDORDER      TO W-IDORDER-Q1-MIN                        
601900                                  W-IDORDER-Q1-MAX                        
602000        MOVE OBKR-IDARTNR      TO W-IDARTNR-Q1-MIN                        
602100                                  W-IDARTNR-Q1-MAX                        
602200        MOVE +1                TO W-IDLOPNR-Q1-MIN                        
602300                                  W-IDLOPNR-Q1-MAX                        
602400                                  W-IDSEKVNR-Q1-MIN                       
602500                                  W-IDSEKVNR-Q1-MAX                       
602600        PERFORM IMS-07-GU-ORQM-WDQ101                                     
602700        PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                        
602800           ADD +1              TO W-IDLOPNR-Q1-MIN                        
602900                                  W-IDLOPNR-Q1-MAX                        
603000           PERFORM IMS-07-GN-ORQM-WDQ101                                  
603100        END-PERFORM                                                       
603200        MOVE W-IDLOPNR-Q1-MIN  TO OBKR-IDLOPNR                            
603300        MOVE +1                TO OBKR-IDSEKVNR                           
603400     END-IF                                                               
603500     MOVE 10                   TO OBKR-KDORDBEK                           
603600     MOVE IDPGM                TO OBKR-IDPGM                              
603700     MOVE OHUV-IDKUNDRF        TO OBKR-IDKUNDRF                           
603800     MOVE MID-IDKUNDRF-RO      TO OBKR-IDKUNDRF-RO                        
603900     MOVE ORAD-KVBEART         TO OBKR-KVBEART-Q                          
604000     MOVE NEJ                  TO OBKR-FLOBTRAN                           
604100                                                                          
604200     MOVE ORAD-IDDC            TO OBKR-IDDC                               
604300     MOVE SPACE                TO OBKR-BEERS                              
604400     MOVE SPACE                TO OBKR-IDBIL                              
604500     MOVE OHUV-BEKUNDRF        TO OBKR-BEKUNDRF                           
604600     MOVE ORAD-BERADREF        TO OBKR-BERADREF                           
604700     MOVE ORAD-BEVOLREF        TO OBKR-BEVOLREF                           
604800     MOVE ORAD-IDKAMPRF        TO OBKR-IDKAMPRF                           
604900     MOVE +0                   TO OBKR-DIERS-KVOT                         
605000     MOVE ORAD-FLAKPLOC        TO OBKR-FLAKPLOC                           
605100     MOVE ORAD-FLINVEST        TO OBKR-FLINVEST                           
605200     MOVE JA                   TO OBKR-FLOBOK                             
605300     MOVE NEJ                  TO OBKR-FLOBPRT                            
605400     MOVE ORAD-FLPRTILL        TO OBKR-FLPRTILL                           
605500     MOVE ORAD-FLRESTN         TO OBKR-FLRESTN                            
605600     MOVE ORFK-FLSLATT (WS-INDEX-MID)                                     
605700                               TO OBKR-FLSLATT                            
605800     MOVE ORAD-FLTILLK         TO OBKR-FLTILLK                            
605900     IF ORFK-KDORDBEK (WS-INDEX-MID) = 59                                 
606000        MOVE ORAD-REKSIFFR     TO OBKR-REKSIFFR                           
606100     ELSE                                                                 
606200        MOVE ORFK-REKSIFFR(WS-INDEX-MID)                                  
606300                               TO OBKR-REKSIFFR                           
606400     END-IF                                                               
606500     IF TILLKOMMANDE-RAD                                                  
606600        MOVE TILK-IDARTNR-TILLK(WS-INDEX-TILLK)                           
606700                               TO OBKR-IDARTNR-TILLK                      
606800        MOVE TILK-REKSIFFR-TILLK(WS-INDEX-TILLK)                          
606900                               TO OBKR-REKSIFFR-TILLK                     
607000     ELSE                                                                 
607100        MOVE +0                TO OBKR-IDARTNR-TILLK                      
607200        MOVE +0                TO OBKR-REKSIFFR-TILLK                     
607300     END-IF                                                               
607400     MOVE ORAD-IDDC-RO         TO OBKR-IDDC-RO                            
607500     MOVE ORAD-IDGMTREF        TO OBKR-IDGMTREF                           
607600     MOVE ORAD-IDLEVNR         TO OBKR-IDLEVNR                            
607700     MOVE ORAD-IDLOPNR-RO      TO OBKR-IDLOPNR-RO                         
607800     MOVE ORAD-IDSYSTEM        TO OBKR-IDSYSTEM                           
607900     MOVE ORAD-KDDSP           TO OBKR-KDDSP                              
608000     IF NOT TILLKOMMANDE-RAD                                              
608100        MOVE AREG-KDERS        TO OBKR-KDERS                              
608200     END-IF                                                               
608300     MOVE ORAD-KDKVBRYT        TO OBKR-KDKVBRYT                           
608400     MOVE ORAD-KDOI            TO OBKR-KDOI                               
608500     MOVE ORAD-CLEARGROUP      TO OBKR-CLEARGROUP                         
608600     MOVE ORAD-KDPRTYP         TO OBKR-KDPRTYP                            
608700     MOVE ORAD-KDTPOTYP        TO OBKR-KDTPOTYP                           
608800     MOVE ORAD-KDVRINFO        TO OBKR-KDVRINFO                           
608900     MOVE +0                   TO OBKR-KVANNANT                           
609000     MOVE +0                   TO OBKR-KVAVBART                           
609100     MOVE ORAD-KVBEART         TO OBKR-KVBEART                            
609200     MOVE +0                   TO OBKR-KVBEART-TILLK                      
609300     MOVE +0                   TO OBKR-KVPREAVB                           
609400     MOVE +0                   TO OBKR-KVPRERO                            
609500*    MOVE KVAN-KVQPACK-UT      TO OBKR-KVQPACK                            
609600     IF KVAN-KVQPACK-UT NUMERIC                                           
609700        MOVE KVAN-KVQPACK-UT   TO OBKR-KVQPACK                            
609800     ELSE                                                                 
609900        MOVE ZERO              TO OBKR-KVQPACK                            
610000     END-IF                                                               
610100     MOVE +0                   TO OBKR-KVRO                               
610200     MOVE ORAD-KVSLATT         TO OBKR-KVSLATT                            
610300     MOVE ORAD-PRARTNTO        TO OBKR-PRARTNTO                           
610400     MOVE ORAD-DEAL-PR-LINE    TO OBKR-DEAL-PR-LINE                       
610500     MOVE ORAD-PRBPRIS         TO OBKR-PRBPRIS                            
610600     MOVE ORAD-RERF-RAD        TO OBKR-RERF-RAD                           
610700     MOVE AREG-TIDISPIN        TO OBKR-TIDISPIN                           
610800     MOVE OHUV-TIREGDAT        TO OBKR-TIORDREG                           
610900     MOVE ORAD-TIPRIS          TO OBKR-TIPRIS                             
611000     MOVE ORAD-TIREGDAT        TO OBKR-TIREGDAT                           
611100     MOVE ORAD-TIREGTID        TO OBKR-TIREGTID                           
611200     MOVE +0                   TO OBKR-TIRODAT                            
611300     MOVE ORAD-TIREGDAT        TO WS-AAMMDD-9KOMPL                        
611400     IF WS-AAMMDD-9KOMPL(1:2) < 50                                        
611500       MOVE 20                 TO WS-SEKEL-9KOMPL                         
611600     ELSE                                                                 
611700       MOVE 19                 TO WS-SEKEL-9KOMPL                         
611800     END-IF                                                               
611900     COMPUTE OBKR-TITIREGD-9KOMPL = 999999999 - WS-TITIREGD-9KOMPL        
612000     MOVE ORAD-TITPO           TO OBKR-TITPO                              
612100     MOVE OHUV-TIREGDAT        TO WS-AAMMDD-9KOMPL                        
612200     IF WS-AAMMDD-9KOMPL(1:2) < 50                                        
612300       MOVE 20                 TO WS-SEKEL-9KOMPL                         
612400     ELSE                                                                 
612500       MOVE 19                 TO WS-SEKEL-9KOMPL                         
612600     END-IF                                                               
612700     COMPUTE OBKR-TITIORDD-9KOMPL = 999999999 - WS-TITIREGD-9KOMPL        
612800     MOVE ARB-KDFRAKT          TO OBKR-KDFRAKT                            
612900     MOVE OHUV-KDORDKL         TO OBKR-KDORDKL                            
613000                                                                          
613100     MOVE ORAD-IDBIL           TO OBKR-IDBIL                              
613200     MOVE OHUV-KDORDTYP-LDC    TO OBKR-KDORDTYP-LDC                       
613300     MOVE OHUV-TIREPDAT        TO OBKR-TIREPDAT                           
613400     MOVE ORAD-IDKUNDRF-WIP    TO OBKR-IDKUNDRF-WIP                       
613410     MOVE ZERO                 TO OBKR-TIDLEVDAT                          
613420     MOVE ORAD-PRAVCOST        TO OBKR-PRAVCOST                           
613500     PERFORM IMS-25-ISRT-WLORQM01-WDQ101                                  
613600     ADD  +1                   TO OBKR-IDSEKVNR                           
613700                                                                          
613800     MOVE 77                   TO OBKR-KDORDBEK                           
613900     MOVE 2                    TO OBKR-KDTPOTYP                           
614000     MOVE EMPTY-OHUV-IDORDER   TO OBKR-IDORDER                            
614100     MOVE MID-IDKUNDRF-RO      TO OBKR-IDKUNDRF                           
614200     MOVE '0000000   '         TO OBKR-IDKUNDRF-RO                        
614300     MOVE ORAD-KVBEART         TO OBKR-KVBEART-Q                          
614400     MOVE DAGENS-DATUM         TO OBKR-TITPO                              
614500     MOVE ZERO                 TO OBKR-TIDLEVDAT                          
614600     MOVE ORAD-PRAVCOST        TO OBKR-PRAVCOST                           
614700     MOVE ORAD-KDVALISO        TO OBKR-KDVALISO                           
614800     PERFORM IMS-25-ISRT-WLORQM01-WDQ101                                  
614900                                                                          
615000     PERFORM S08A-SKAPA-WDR6-TRANS                                        
615100     .                                                                    
615200                                                                          
615300                                                                          
615400 S08A-SKAPA-WDR6-TRANS      SECTION.                                      
615500                                                                          
615600     MOVE 'STA S08A-SKAPA-WDR6 '       TO WS-PGM-POSITION                 
615700                                                                          
615800     MOVE IDPGM                        TO  FIL-IDPGM                      
615900     ACCEPT FIL-TIREGDAT             FROM  DATE                           
616000     ACCEPT FIL-TIKLOCK              FROM  TIME                           
616100     MOVE ZERO                         TO FIL-IDSEKVNR                    
616200     MOVE 'W414'                       TO FIL-CT-IDSYSTEM                 
616300     MOVE 'A'                          TO FIL-CT-IDVTYP                   
616400     MOVE '200'                        TO FIL-CT-IDPTYP                   
616500     MOVE SPACE                        TO FIL-WDR601-DATA                 
616600     MOVE ORAD-IDARTNR                 TO 203-IDARTNR                     
616700     MOVE ORAD-BERADREF                TO 203-BERADREF                    
616800     MOVE ORAD-BEVOLREF                TO 203-BEVOLREF                    
616900     MOVE ORAD-FLTILLK                 TO 203-FLTILLK                     
617000     MOVE ORAD-IDDISTR                 TO 203-IDDISTR                     
617100     MOVE EMPTY-OHUV-IDKONTO           TO 203-IDKONTO                     
617200     MOVE EMPTY-OHUV-IDKST             TO 203-IDKST                       
617300     MOVE ORAD-IDKUNDNR                TO 203-IDKUNDNR                    
617400     MOVE EMPTY-OHUV-IDKUNDRF          TO 203-IDKUNDRF                    
617500     MOVE ORAD-IDSYSTEM                TO 203-IDSYSTEM                    
617600     MOVE ORAD-KDDSP                   TO 203-KDDSP                       
617700     MOVE EMPTY-OHUV-KDFAKTYP          TO 203-KDFAKTYP                    
617800     MOVE ARB-KDFRAKT                  TO 203-KDFRAKT                     
617900     MOVE ORAD-KDKVBRYT                TO 203-KDKVBRYT                    
618000     MOVE 77                           TO 203-KDORDBEK                    
618100     MOVE ORAD-KDORDING                TO 203-KDORDING                    
618200     IF 203-KDORDING = +1                                                 
618300        MOVE +2                        TO 203-KDORDING                    
618400     END-IF                                                               
618500     MOVE ORAD-KDORDKL                 TO 203-KDORDKL                     
618600     MOVE ORAD-KDTPOTYP                TO 203-KDTPOTYP                    
618700     MOVE ORAD-KDVRINFO                TO 203-KDVRINFO                    
618800     MOVE ORAD-KVBEART-Q               TO 203-KVBEART-Q                   
618900     MOVE ORAD-PRARTNTO                TO 203-PRARTNTO                    
619000     MOVE AREG-REKSIFFR                TO 203-REKSIFFR                    
619100     MOVE AREG-TIDISPIN                TO 203-TIDISPIN                    
619200     MOVE ORAD-KVBEART                 TO 203-KVBEART                     
619300*    MOVE KVAN-KVQPACK-UT              TO 203-KVQPACK-1                   
619400     IF KVAN-KVQPACK-UT NUMERIC                                           
619500        MOVE KVAN-KVQPACK-UT           TO 203-KVQPACK-1                   
619600     ELSE                                                                 
619700        MOVE ZERO                      TO 203-KVQPACK-1                   
619800     END-IF                                                               
619900     MOVE EMPTY-OHUV-BEVARREF          TO 203-BEVARREF                    
620000     MOVE ORAD-KDPRTYP                 TO 203-KDPRTYP                     
620100     MOVE ORAD-FLPRTILL                TO 203-FLPRTILL                    
620200     MOVE ORAD-FLINVEST                TO 203-FLINVEST                    
620300     MOVE ORAD-KDPRODSL                TO 203-KDPRODSL                    
620400     MOVE EMPTY-OHUV-TIREGDAT          TO 203-TIREGDAT                    
620500                                          203-TITPO                       
620600                                                                          
620700     ADD +1                            TO FIL-IDSEKVNR                    
620800     MOVE '203'                        TO FIL-CT-IDPTYP                   
620900     PERFORM IMS-ISRT-FILA-WDR6                                           
621000     PERFORM UNTIL SEGMENT-FINNS                                          
621100        ADD +1 TO FIL-IDSEKVNR                                            
621200        PERFORM IMS-ISRT-FILA-WDR6                                        
621300     END-PERFORM                                                          
621400     .                                                                    
621500                                                                          
621600                                                                          
621700 S09-KONTROLLERA-ENHETSLAST SECTION.                                      
621800                                                                          
621900     MOVE 'STA S09-ENHETSLAST  '           TO   WS-PGM-POSITION           
622000     MOVE ZERO                 TO LAST-ADGANG-UT                          
622100     MOVE ZERO                 TO LAST-ADLAGOMR-UT                        
622200     MOVE ZERO                 TO LAST-KVANTAL-UT                         
622300     MOVE ZERO                 TO LAST-KVBEART-UT                         
622400                                                                          
622500     IF MID-ADLAGOMR-CD(WS-INDEX-MID) NOT = ALL '+'                       
622600     AND MID-ADLAGOMR-CD(WS-INDEX-MID) > ZERO                             
622700       CONTINUE                                                           
622800     ELSE                                                                 
622900       PERFORM S40-HAMTA-WDB6-INFO                                        
623000       IF DCS-CDC             AND                                         
623100          OHUV-FLFORBI = NEJ  AND OHUV-FLORDSPE = NEJ AND                 
623200          OHUV-FLOVRLEV = NEJ AND ORAD-IDLEVNR = SPACE AND                
623300         (AREG-KVQPACK-3 > +0 OR AREG-KVQPACK-4 > +0)                     
623400                                                                          
623500          AND ORAD-IDBIL = SPACE                                          
623600                                                                          
623700                                                                          
623800          MOVE ORAD-ADLAGOMR        TO LAST-ADLAGOMR                      
623900          MOVE OHUV-FLFORBI         TO LAST-FLFORBI                       
624000          MOVE OHUV-FLOVRLEV        TO LAST-FLOVRLEV                      
624100          MOVE OHUV-FLORDSPE        TO LAST-FLORDSPE                      
624200          MOVE ORAD-IDLEVNR         TO LAST-IDLEVNR                       
624300          MOVE ORAD-IDDC            TO LAST-IDDC                          
624400          MOVE ARB-KDFDKRAV         TO LAST-KDFDKRAV                      
624500          MOVE ORAD-KVPREAVB        TO LAST-KVPREAVB                      
624600          MOVE AREG-KVQPACK-3       TO LAST-KVQPACK-3                     
624700          MOVE AREG-KVQPACK-4       TO LAST-KVQPACK-4                     
624800                                                                          
624900          CALL W411LAST USING LAST-W411LAST                               
625000       END-IF                                                             
625100     END-IF                                                               
625200     .                                                                    
625300     EJECT                                                                
625400 S10-BERAKNA-WOPS-SKRIV-ORAD SECTION.                                     
625500                                                                          
625600     MOVE 'STA S10-WOPS        '           TO   WS-PGM-POSITION           
625700     IF LAST-ADLAGOMR-UT = +0 AND                                         
625800        LAST-KVANTAL-UT  = +0 AND                                         
625900        LAST-KVBEART-UT  = +0                                             
626000*------------------------------------------------------------*            
626100*-----ALLT MOT VANLIGT LAGEROMRÅDE ( 'VANLIG' ORDERRAD)------*            
626200*------------------------------------------------------------*            
626300        PERFORM S10A-FIXA-LAGEROMR-PLATS                                  
626400        PERFORM S10B-REDIGERA-WOPS-AREA                                   
626500        PERFORM IMS-22-ISRT-ORQF-WDQ401                                   
626600        PERFORM UNTIL SEGMENT-FINNS                                       
626700           ADD +1           TO ORAD-IDLOPNR                               
626800           PERFORM IMS-22-ISRT-ORQF-WDQ401                                
626900        END-PERFORM                                                       
627000     ELSE                                                                 
627100                                                                          
627200        IF LAST-KVANTAL-UT > +0 AND LAST-KVBEART-UT > +0                  
627300*------------------------------------------------------------*            
627400*--------- RADEN MOT VANLIGT LAGEROMRÅDE (KVBEART)-----------*            
627500*------------------------------------------------------------*            
627600                                                                          
627700           MOVE LAST-KVBEART-UT      TO ORAD-KVPREAVB                     
627800           COMPUTE ORAD-KVBEART-Q = ORAD-KVPREAVB +                       
627900                                    ORAD-KVPRERO                          
628000           MOVE ORAD-KVSLATT         TO SPAR-ORAD-KVSLATT                 
628100           PERFORM S10C-BERAEKNA-KVSLATT                                  
628200           PERFORM S10A-FIXA-LAGEROMR-PLATS                               
628300           MOVE ORAD-ADLAGOMR        TO WS-ADLAGOMR                       
628400           MOVE ORAD-ADGANG          TO WS-ADGANG                         
628500           MOVE ORAD-ADPLATS         TO WS-ADPLATS                        
628600           PERFORM S10B-REDIGERA-WOPS-AREA                                
628700           PERFORM IMS-22-ISRT-ORQF-WDQ401                                
628800           PERFORM UNTIL SEGMENT-FINNS                                    
628900              ADD +1        TO ORAD-IDLOPNR                               
629000              PERFORM IMS-22-ISRT-ORQF-WDQ401                             
629100           END-PERFORM                                                    
629200****************************************************************          
629300*                                                                         
629400*------------------------------------------------------------*            
629500*--------- RADEN MOT 'ENHETS' LAGEROMRÅDE (KVANTAL)----------*            
629600*------------------------------------------------------------*            
629700                                                                          
629800           MOVE WS-ADLAGOMR          TO ORAD-ADLAGOMR                     
629900           MOVE WS-ADGANG            TO ORAD-ADGANG                       
630000           MOVE WS-ADPLATS           TO ORAD-ADPLATS                      
630100                                                                          
630200           MOVE +0                   TO ORAD-KVBEART                      
630300           MOVE LAST-KVANTAL-UT      TO ORAD-KVBEART-Q                    
630400                                        ORAD-KVPREAVB                     
630500           IF MID-ADLAGOMR-CD(WS-INDEX-MID) NOT = ALL '+'                 
630600           AND MID-ADLAGOMR-CD(WS-INDEX-MID) > ZERO                       
630700                                                                          
630800              MOVE MID-ADLAGOMR-CD(WS-INDEX-MID)                          
630900                                     TO ORAD-ADLAGOMR                     
631000           ELSE                                                           
631100             MOVE LAST-ADLAGOMR-UT   TO ORAD-ADLAGOMR                     
631200           END-IF                                                         
631300           IF OHUV-FLFORBI = SPEC-FORBI                                   
631400              MOVE '07'              TO ORAD-ADLAGOMR                     
631500           END-IF                                                         
631600           MOVE +0                   TO ORAD-KVPRERO                      
631700           MOVE 1.0000               TO ORAD-RERF-RAD                     
631800           COMPUTE ORAD-KVSLATT = SPAR-ORAD-KVSLATT - ORAD-KVSLATT        
631900           PERFORM S10A-FIXA-LAGEROMR-PLATS                               
632000           PERFORM S10B-REDIGERA-WOPS-AREA                                
632100           PERFORM IMS-22-ISRT-ORQF-WDQ401                                
632200           PERFORM UNTIL SEGMENT-FINNS                                    
632300              ADD +1        TO ORAD-IDLOPNR                               
632400              PERFORM IMS-22-ISRT-ORQF-WDQ401                             
632500           END-PERFORM                                                    
632600        ELSE                                                              
632700*------------------------------------------------------------*            
632800*-----ALLT MOT 'ENHETS' LAGEROMRÅDE -------------------------*            
632900*------------------------------------------------------------*            
633000           IF MID-ADLAGOMR-CD(WS-INDEX-MID) NOT = ALL '+'                 
633100           AND MID-ADLAGOMR-CD(WS-INDEX-MID) > ZERO                       
633200                                                                          
633300              MOVE MID-ADLAGOMR-CD(WS-INDEX-MID)                          
633400                                   TO ORAD-ADLAGOMR                       
633500           ELSE                                                           
633600             MOVE LAST-ADLAGOMR-UT TO ORAD-ADLAGOMR                       
633700           END-IF                                                         
633800           IF OHUV-FLFORBI = SPEC-FORBI                                   
633900              MOVE '07'            TO ORAD-ADLAGOMR                       
634000           END-IF                                                         
634100           IF LAST-ADLAGOMR-UT = 62 OR 63 OR 64                           
634200             CONTINUE                                                     
634300           ELSE                                                           
634400             IF LAST-ADGANG-UT > ZERO                                     
634500               MOVE LAST-ADGANG-UT   TO ORAD-ADGANG                       
634600             END-IF                                                       
634700           END-IF                                                         
634800           MOVE 1.0000            TO ORAD-RERF-RAD                        
634900           PERFORM S10A-FIXA-LAGEROMR-PLATS                               
635000           PERFORM S10B-REDIGERA-WOPS-AREA                                
635100           PERFORM IMS-22-ISRT-ORQF-WDQ401                                
635200           PERFORM UNTIL SEGMENT-FINNS                                    
635300              ADD +1        TO ORAD-IDLOPNR                               
635400              PERFORM IMS-22-ISRT-ORQF-WDQ401                             
635500           END-PERFORM                                                    
635600        END-IF                                                            
635700     END-IF                                                               
635800     .                                                                    
635900     EJECT                                                                
636000 S10A-FIXA-LAGEROMR-PLATS SECTION.                                        
636100                                                                          
636200     MOVE 'STA S10A-LAGER      '           TO   WS-PGM-POSITION           
636300     MOVE ORAD-ADLAGOMR        TO ADRS-ADLAGOMR-IN                        
636400     MOVE ORAD-ADPLATS         TO ADRS-ADPLATS-IN                         
636500     MOVE OHUV-BEVARREF        TO ADRS-BEVARREF-IN                        
636600                                                                          
636700* TO HANDLE BEVARREF FROM EXCEL - ORDER UPLOAD                            
636800* TO MAKE IT POSSIBLE TO STEER ORDERS TO PARTICULAR PRC                   
636900* THIS DONE USING W413WHFA                                                
637000     IF OHUV-IDSYSTEM = ('XCEL' OR 'SPX') AND BEVARREF-I-HFAK-TAB         
637100       MOVE JA                 TO ADRS-FLFORBI-IN                         
637200     ELSE                                                                 
637300       MOVE OHUV-FLFORBI       TO ADRS-FLFORBI-IN                         
637400     END-IF                                                               
637500                                                                          
637600     MOVE OHUV-IDDISTR         TO ADRS-IDDISTR-IN                         
637700     MOVE 1                    TO ADRS-KDCALL-IN                          
637800     MOVE ORAD-IDDC            TO ADRS-IDDC-IN                            
637900     MOVE OHUV-KDORDKL         TO ADRS-KDORDKL-IN                         
638000     MOVE ORAD-KVBEART-Q       TO ADRS-KVBEART-Q-IN                       
638100     MOVE ORAD-VLARTNTO        TO ADRS-VLARTNTO-IN                        
638200                                                                          
638300     CALL W413ADRS USING ADRS-W413ADRS                                    
638400                                                                          
638500     IF MID-ADLAGOMR-CD(WS-INDEX-MID) NOT = ALL '+'                       
638600     AND MID-ADLAGOMR-CD(WS-INDEX-MID) > ZERO                             
638700                                                                          
638800        MOVE MID-ADLAGOMR-CD(WS-INDEX-MID)                                
638900                               TO ORAD-ADLAGOMR                           
639000        MOVE MID-ADPLATS-CD(WS-INDEX-MID)                                 
639100                               TO ORAD-ADPLATS                            
639200     ELSE                                                                 
639300       MOVE ADRS-ADLAGOMR-UT   TO ORAD-ADLAGOMR                           
639400       MOVE ADRS-ADPLATS-UT    TO ORAD-ADPLATS                            
639500     END-IF                                                               
639600     IF OHUV-FLFORBI = SPEC-FORBI                                         
639700        MOVE '07'              TO ORAD-ADLAGOMR                           
639800     END-IF                                                               
639900                                                                          
640000*- KAMPANJORDRAR FÅR ETT FIKTIVT LAGEROMRÅDE.E'TRACKER 2218613.           
640100     IF OHUV-IDKAMPRF > 0                                                 
640200       MOVE 8                  TO ORAD-ADLAGOMR                           
640300     END-IF                                                               
640400     .                                                                    
640500     EJECT                                                                
640600 S10B-REDIGERA-WOPS-AREA SECTION.                                         
640700                                                                          
640800     MOVE 'STA S10B-WOPS       '           TO   WS-PGM-POSITION           
640900     IF ((OHUV-IDSYSTEM = 'LDC ' OR 'TACD' OR 'LDCB' OR 'LDCD') OR        
641000        (OHUV-TIREPDAT > 0 AND                                            
641100        (OHUV-IDSYSTEM(1:3) = 'LYN' OR 'ECO' OR 'VOU' OR                  
641200                              'TAD' OR 'ACC' OR 'APA' OR                  
641300                              'APB' OR 'APC' OR 'APD' OR                  
641400                              'APE' OR 'APF' OR 'APG' OR                  
641500                              'APH' OR 'API' OR 'APJ' )))                 
641600     AND (OHUV-KDORDKL  = +3 OR +2)                                       
641700       MOVE +9                 TO AVSR-KDCALL                             
641800     ELSE                                                                 
641900       MOVE +1                 TO AVSR-KDCALL                             
642000       IF MID-IDKUNDRF-RO NOT = SPACE                                     
642100         MOVE +8               TO AVSR-KDCALL                             
642200       END-IF                                                             
642300     END-IF                                                               
642400     MOVE OHUV-IDORDER         TO AVSR-IDORDER                            
642500     MOVE OHUV-KDORDKL         TO AVSR-KDORDKL                            
642600     MOVE ARB-KDFRAKT          TO AVSR-KDFRAKT                            
642700     MOVE ARB-KDROPACK         TO AVSR-KDROPACK                           
642800     MOVE MSGI-TILOKDAT        TO AVSR-TIREGDAT                           
642900     MOVE MSGI-TILOKTID        TO AVSR-TIHHMM                             
643000                                                                          
643100     MOVE ORAD-ADLAGOMR        TO AVSR-ADLAGOMR(WS-INDEX-WOPS)            
643200     IF OHUV-FLEMBORD = JA OR OHUV-FLOVRLEV = JA                          
643300        MOVE SPACE             TO AVSR-IDLEVNR(WS-INDEX-WOPS)             
643400     ELSE                                                                 
643500        MOVE ORAD-IDLEVNR      TO AVSR-IDLEVNR(WS-INDEX-WOPS)             
643600     END-IF                                                               
643700     MOVE ORAD-IDDC            TO AVSR-IDDC(WS-INDEX-WOPS)                
643800*LDC URSÄKTA FIX-LÖSNINGEN FÖR RFS                                        
643900     IF (OHUV-IDSYSTEM = 'LDC ' OR 'TACD' OR 'LYNK'                       
644000                                OR 'ECOM' OR 'VOUI' OR 'TAD '             
644100                                OR 'ACC ' OR 'APA ' OR 'APB '             
644200                                OR 'APC ' OR 'APD ' OR 'APE '             
644300                                OR 'APF ' OR 'APG ' OR 'APH '             
644400                                OR 'API ' OR 'APJ '  )                    
644500        AND ORAD-IDDC = WC-CDC-SE                                         
644600        AND ARB-TIRFS > 0                                                 
644700       PERFORM S15-CALL-WORKDAY-LDC-CDC-RFS                               
644800       MOVE WORK-TIAAMMDD-FOM TO AVSR-TISKEPPN-DDC(WS-INDEX-WOPS)         
644900     END-IF                                                               
645000     MOVE ORAD-KDSPEEMB        TO AVSR-KDSPEEMB(WS-INDEX-WOPS)            
645100     MOVE +0                   TO AVSR-KVANNANT(WS-INDEX-WOPS)            
645200     MOVE ORAD-KVBEART-Q       TO AVSR-KVBEART-Q(WS-INDEX-WOPS)           
645300     MOVE ORAD-PRARTNTO        TO AVSR-PRARTNTO(WS-INDEX-WOPS)            
645400     MOVE ORAD-PRAVCOST        TO AVSR-PRAVCOST(WS-INDEX-WOPS)            
645500     MOVE ORAD-DEAL-PR-LINE    TO AVSR-DEAL-PR-LINE(WS-INDEX-WOPS)        
645600     MOVE ORAD-VKART           TO AVSR-VKART(WS-INDEX-WOPS)               
645700     MOVE ORAD-VLARTNTO        TO AVSR-VLARTNTO(WS-INDEX-WOPS)            
645800     MOVE AREG-KDVSOP          TO AVSR-KDVSOP(WS-INDEX-WOPS)              
645900     MOVE AREG-KDFARLIG        TO AVSR-KDFARLIG(WS-INDEX-WOPS)            
646000                                                                          
646100     ADD +1                    TO WS-INDEX-WOPS                           
646200                                                                          
646300     IF WS-INDEX-WOPS > WS-INDEX-WOPS-MAX                                 
646400                                                                          
646500        CALL W413AVSR USING AVSR-W413AVSR AVSR-LIST-PCB                   
646600          AVSR-ORQI-PCB AVSR-GMTB-PCB AVSR-GMTC-PCB                       
646700          AVSR-WDB2-PCB AVSR-WDB6-PCB                                     
646800          TRAN-XXKB-PCB                                                   
646900                                                                          
647000        PERFORM AA-NOLLA-WOPS-TABELL                                      
647100     END-IF                                                               
647200     .                                                                    
647300     EJECT                                                                
647400 S10C-BERAEKNA-KVSLATT SECTION.                                           
647500                                                                          
647600     MOVE 'STA S10C-KVSLATT    '           TO   WS-PGM-POSITION           
647700     IF ORAD-FLRESTN = JA AND OHUV-RESLATT > +0                           
647800                                                                          
647900        COMPUTE WS-RESLATT = OHUV-RESLATT / 100                           
648000                                                                          
648100        COMPUTE ORAD-KVSLATT ROUNDED =                                    
648200               (ORAD-KVPREAVB + ORAD-KVPRERO) * WS-RESLATT                
648300     END-IF                                                               
648400     .                                                                    
648500     EJECT                                                                
648600                                                                          
648700 S11-SKRIV-VOR-RAD SECTION.                                               
648800                                                                          
648900     MOVE 'STA S11-VOR         '           TO   WS-PGM-POSITION           
649000     MOVE OBKR-IDDISTR         TO 4542-IDDISTR                            
649100     MOVE OBKR-IDDC            TO WS-IDDC-SEEK                            
649200                                                                          
649300     IF OBKR-IDARTNR-TILLK > +0                                           
649400       MOVE OBKR-IDARTNR-TILLK TO W-IDARTNR                               
649500                                  4542-IDARTNR                            
649600     ELSE                                                                 
649700        MOVE OBKR-IDARTNR      TO W-IDARTNR                               
649800                                  4542-IDARTNR                            
649900     END-IF                                                               
650000                                                                          
650100     MOVE AREG-IDANSK          TO 4542-IDANSK                             
650200     MOVE +1                   TO 4542-IDLOPNR                            
650300     MOVE OBKR-IDORDER         TO 4542-IDORDER                            
650400     MOVE OBKR-BERADREF        TO 4542-BERADREF                           
650500     MOVE OBKR-IDDC            TO W-IDDC                                  
650600                                  4542-IDDC                               
650700     MOVE OBKR-IDKUNDNR        TO 4542-IDKUNDNR                           
650800     MOVE OBKR-IDKUNDRF        TO 4542-IDKUNDRF                           
650900     MOVE OHUV-IDUSER          TO 4542-IDUSER                             
651000     MOVE OBKR-KDORDBEK        TO 4542-KDORDBEK                           
651100     MOVE OBKR-KDPRTYP         TO 4542-KDPRTYP                            
651200     MOVE ZERO                 TO 4542-KDVORATG                           
651300     IF 4542-KDORDBEK = 92 OR 98                                          
651400       COMPUTE 4542-KVBEART = OBKR-KVPREAVB + OBKR-KVPRERO                
651500       MOVE 4542-KVBEART         TO 4542-KVBEART-Q                        
651600       MOVE OBKR-KVPREAVB        TO 4542-KVPREAVB                         
651700     ELSE                                                                 
651800       MOVE OBKR-KVBEART         TO 4542-KVBEART-Q                        
651900       MOVE +0                   TO 4542-KVPREAVB                         
652000     END-IF                                                               
652100     EJECT                                                                
652200     MOVE OBKR-PRARTNTO        TO 4542-PRARTNTO                           
652300     MOVE OBKR-DEAL-PR-LINE    TO 4542-DEAL-PR-LINE                       
652400     MOVE SPACE                TO 4542-TEVORMRK                           
652500     MOVE MSGI-TILOKDAT        TO 4542-TIREGDAT                           
652600     MOVE MSGI-TILOKTID        TO WS-TIHHMM                               
652700     MOVE WS-TIHHMMSS          TO 4542-TIREGTID                           
652800     MOVE +0                   TO 4542-TIUPPDAT                           
652900     MOVE +0                   TO 4542-TIUPPTID                           
653000     MOVE OBKR-IDLEVNR         TO 4542-IDLEVNR                            
653100                                                                          
653200*---------------------------------------UPPLÄGG TILL NY VORKÖ             
653300*                                       SKER I W40293                     
653400     PERFORM S40-HAMTA-WDB6-INFO                                          
653500     IF  DCS-NDC                                                          
653600         PERFORM IMS-17-ISRT-4541-WDR411                                  
653700         PERFORM UNTIL SEGMENT-FINNS                                      
653800                                                                          
653900            ADD +1             TO 4542-IDLOPNR                            
654000            PERFORM IMS-17-ISRT-4541-WDR411                               
654100         END-PERFORM                                                      
654200                                                                          
654300         PERFORM IMS-GHU-WDK711                                           
654400         IF  DCS-NDC-CN                                                   
654500         OR (DCS-NDC-NA AND DCS-USA)                                      
654600            IF SLAG-IDDC-REF = SPACE                                      
654700               PERFORM IMS-GU-WDK722                                      
654800               IF SEGMENT-FINNS AND XLAG-IDANSK > 0                       
654900                  MOVE XLAG-IDANSK TO WS-IDANSK                           
655000               ELSE                                                       
655100                  PERFORM IMS-GHU-WDK611                                  
655200                  MOVE XLAG-IDANSK TO WS-IDANSK                           
655300               END-IF                                                     
655400               PERFORM S11A-STARTA-W2T191X                                
655500            END-IF                                                        
655600         END-IF                                                           
655700     END-IF                                                               
655800                                                                          
655900     IF (4542-KDORDBEK NOT = 92 AND 98) AND                               
656000        4542-IDLEVNR = SPACE                                              
656100       IF DCS-CDC                                                         
656200         MOVE 4542-IDARTNR    TO W-IDARTNR                                
656300         PERFORM IMS-10-GHU-WLARTM-WDK901                                 
656400         COMPUTE ART-KVOKS-VOR =                                          
656500           ART-KVOKS-VOR + (4542-KVBEART-Q - 4542-KVPREAVB)               
656600         PERFORM IMS-11-REPL-ARTM-WDK901                                  
656700       END-IF                                                             
656800     END-IF                                                               
656900     PERFORM S24-CHANGE-PRICE-Q-LINE                                      
657000     .                                                                    
657100     EJECT                                                                
657200                                                                          
657300 S11A-STARTA-W2T191X   SECTION.                                           
657400                                                                          
657500     MOVE +1                    TO 2191-MID-KDCLAGER                      
657600     MOVE 4542-IDARTNR          TO 2191-MID-IDARTNR                       
657700     MOVE ZERO                  TO 2191-MID-TISENBEK-DAG                  
657800                                   2191-MID-TISENBEK-KL                   
657900     MOVE SPACE                 TO 2191-MID-IDKR                          
658000     MOVE WS-IDANSK             TO 2191-MID-IDANSK                        
658100     MOVE '500'                 TO 2191-MID-KDLARM                        
658200     MOVE 4542-IDDISTR          TO WS-IDDISTR-NUM4                        
658300     MOVE WS-IDDISTR-NUM4       TO 2191-MID-IDDISTR                       
658400     MOVE 4542-IDKUNDNR         TO WS-IDKUNDNR-NUM6                       
658500     MOVE WS-IDKUNDNR-NUM6      TO 2191-MID-IDKUNDNR                      
658600     MOVE 4542-IDKUNDRF         TO 2191-MID-IDKUNDRF                      
658700     MOVE 'J'                   TO 2191-MID-FLNYLARM                      
658800     MOVE SLAG-IDDC             TO 2191-MID-IDDC                          
658900     MOVE SLAG-IDLEVNR          TO 2191-MID-IDLEVNR                       
659000                                                                          
659100     COMPUTE MSG-KVLL = LENGTH OF 2191-MID-W2I19101 + 17                  
659200     MOVE 'W2T191X '            TO MSG-KDTRANS-1                          
659300     MOVE '4252'                TO MSG-IDTRANS-1                          
659400     MOVE '1'                   TO MSG-KDMFSFOR-1                         
659500     MOVE 2191-MID-W2I19101     TO MSG-INDATA-MINUS-1-TRANSKOD            
659600                                                                          
659700     PERFORM IMS-PURG-MSG-2191                                            
659800                                                                          
659900     MOVE SPACE                 TO 2191-MID-W2I19101                      
660000     .                                                                    
660100     EJECT                                                                
660200 S13-CALL-WORKDAY-LDC SECTION.                                            
660300                                                                          
660400     MOVE ORAD-IDDC             TO W-IDDC                                 
660500** RÄKNAR X+1 ARBETSDAGAR BAKÅT                                           
660600     MOVE WC-CDC-SE             TO WORK-IDDC                              
660700     MOVE +003                  TO WORK-KDCALL                            
660800     COMPUTE WORK-KVWORKD       = GMT-KVDAGAR-CDC + 1                     
660900     MOVE ARB-TIRFS             TO WS-TIRFS                               
661000     MOVE WS-TIRFS-DAT          TO WORK-TIAAMMDD-TOM                      
661100     CALL WORKDAY               USING WORK-KDCALL                         
661200                                      WORK-DATE-AREA                      
661300                                      WORK-KDSVAR                         
661400     IF WORK-KDSVAR-FEL                                                   
661500        MOVE 'SECT S13-, DATUM SAKNAS I WORKDAY'                          
661600                                TO    FELTEXT                             
661700        CALL ABEND              USING RKOD-ABEND                          
661800     END-IF                                                               
661900     .                                                                    
662000     EJECT                                                                
662100 S15-CALL-WORKDAY-LDC-CDC-RFS SECTION.                                    
662200     MOVE WC-CDC-SE             TO WORK-IDDC                              
662300     MOVE +003                  TO WORK-KDCALL                            
662400     MOVE +001                  TO WORK-KVWORKD                           
662500     MOVE ARB-TIRFS             TO WS-TIRFS                               
662600     MOVE WS-TIRFS-DAT          TO WORK-TIAAMMDD-TOM                      
662700                                                                          
662800     PERFORM S15A-CHK-DC11-HOLIDAY                                        
662900                                                                          
663000     CALL WORKDAY               USING WORK-KDCALL                         
663100                                      WORK-DATE-AREA                      
663200                                      WORK-KDSVAR                         
663300     IF WORK-KDSVAR-FEL                                                   
663400        MOVE 'SECT S15-, DATUM SAKNAS I WORKDAY'                          
663500                                TO    FELTEXT                             
663600        CALL ABEND              USING RKOD-ABEND                          
663700     END-IF                                                               
663800     .                                                                    
663900     EJECT                                                                
664000 S15A-CHK-DC11-HOLIDAY SECTION.                                           
664100                                                                          
664200                                                                          
664300     MOVE NEJ               TO HOLIDAY-SW                                 
664400                                                                          
664500     CALL WORKDAY                USING WORK-KDCALL                        
664600                                       WORK-DATE-AREA                     
664700                                       WORK-KDSVAR                        
664800                                                                          
664900     IF WORK-TIAAMMDD-FOM   = WORK-TIAAMMDD-TOM                           
665000        CONTINUE                                                          
665100     ELSE                                                                 
665200        SET ITS-HOLIDAY TO TRUE                                           
665300        PERFORM UNTIL HOLIDAY-SW = NEJ                                    
665400          COMPUTE WORK-TIAAMMDD-TOM = WORK-TIAAMMDD-TOM + 1               
665500          CALL WORKDAY                USING WORK-KDCALL                   
665600                                       WORK-DATE-AREA                     
665700                                       WORK-KDSVAR                        
665800          IF WORK-TIAAMMDD-FOM   = WORK-TIAAMMDD-TOM                      
665900            MOVE NEJ        TO HOLIDAY-SW                                 
666000          END-IF                                                          
666100        END-PERFORM                                                       
666200     END-IF                                                               
666300     .                                                                    
666400     EJECT                                                                
666500 S16-KOLLA-REPARATIONSDATUM SECTION.                                      
666600                                                                          
666700     MOVE JA                TO W-REPSW                                    
666800**   MOVE WC-CDC-SE         TO WORK-IDDC                                  
666900     MOVE ORAD-IDDC         TO WORK-IDDC                                  
667000     IF ORAD-IDDC = 'SE' OR 'BE' OR 'DE' OR 'NO' OR                       
667000                    'GB' OR 'AU'                                          
667100       MOVE WC-CDC-SE       TO WORK-IDDC                                  
667200     END-IF                                                               
667300     MOVE OHUV-TIREPDAT     TO WORK-TIAAMMDD-FOM                          
667400     MOVE +002              TO WORK-KDCALL                                
667500     MOVE +001              TO WORK-KVWORKD                               
667600     CALL WORKDAY           USING WORK-KDCALL                             
667700                                  WORK-DATE-AREA                          
667800                                  WORK-KDSVAR                             
667900     IF WORK-KDSVAR-FEL                                                   
668000        MOVE 'SECT S16-, DATUM SAKNAS I WORKDAY'                          
668100                            TO    FELTEXT                                 
668200        CALL ABEND          USING RKOD-ABEND                              
668300     ELSE                                                                 
668400        MOVE +003                   TO WORK-KDCALL                        
668500*       REPAIRDAY - ANTAL RFSDAY - DAGAR (WEDN-MOND)                      
668600        MOVE GMT-KVDAGAR-RFS-DEF   TO WORK-KVWORKD                        
668700        PERFORM                                                           
668800        VARYING RFS-IX FROM 1 BY 1                                        
668900          UNTIL RFS-IX > MAX-RFS-IX                                       
669000          IF GMT-IDDC-RFS (RFS-IX) = WORK-IDDC                            
669100            MOVE GMT-KVDAGAR-RFS (RFS-IX)                                 
669200                                   TO WORK-KVWORKD                        
669300          END-IF                                                          
669400        END-PERFORM                                                       
669500        ADD +2  TO WORK-KVWORKD                                           
669600        CALL WORKDAY           USING WORK-KDCALL                          
669700                                     WORK-DATE-AREA                       
669800                                     WORK-KDSVAR                          
669900        IF WORK-KDSVAR-FEL                                                
670000           MOVE 'SECT S16-2, DATUM SAKNAS I WORKDAY'                      
670100                               TO    FELTEXT                              
670200           CALL ABEND          USING RKOD-ABEND                           
670300        ELSE                                                              
670400           IF WORK-TIAAMMDD-FOM < DAGENS-DATUM                            
670500             MOVE NEJ            TO W-REPSW                               
670600           END-IF                                                         
670700        END-IF                                                            
670800     END-IF                                                               
670900     .                                                                    
671000     EJECT                                                                
671100 S20-OBKR-FRAN-TILLK-TAB SECTION.                                         
671200                                                                          
671300     MOVE 'STA S20-OBKR-TILLK  '           TO   WS-PGM-POSITION           
671400     MOVE +1                   TO WS-INDEX-TILLK                          
671500     PERFORM UNTIL WS-INDEX-TILLK > WS-INDEX-TILLK-MAX OR                 
671600                TILK-IDARTNR(WS-INDEX-TILLK) = ZERO                       
671700        IF TILK-FLTILLK-X(WS-INDEX-TILLK) = JA                            
671800           IF OBKR-SKRIVEN                                                
671900              PERFORM IMS-25-ISRT-WLORQM01-WDQ101                         
672000              ADD +1              TO OBKR-IDSEKVNR                        
672100           END-IF                                                         
672200           MOVE KERS-KDORDBEK  TO OBKR-KDORDBEK                           
672300           MOVE '4252KER4'     TO OBKR-IDPGM                              
672400           MOVE TILK-IDARTNR-TILLK(WS-INDEX-TILLK)                        
672500                               TO OBKR-IDARTNR-TILLK                      
672600           MOVE TILK-REKSIFFR-TILLK(WS-INDEX-TILLK)                       
672700                               TO OBKR-REKSIFFR-TILLK                     
672800           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
672900                               TO OBKR-KVBEART-TILLK                      
673000           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
673100              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
673200             MOVE +0              TO OBKR-DIERS-KVOT                      
673300           ELSE                                                           
673400             COMPUTE OBKR-DIERS-KVOT =                                    
673500                   TILK-DIERS-TILLK(WS-INDEX-TILLK) /                     
673600                   TILK-DIERS-ERS(WS-INDEX-TILLK)                         
673700           END-IF                                                         
673800           MOVE TILK-BEERS(WS-INDEX-TILLK)                                
673900                               TO OBKR-BEERS                              
674000           MOVE ZEROES         TO OBKR-KVPREAVB                           
674100                                  OBKR-KVPRERO                            
674200           MOVE 'S'            TO OBKR-SW                                 
674300        END-IF                                                            
674400        ADD +1                 TO WS-INDEX-TILLK                          
674500     END-PERFORM                                                          
674600     IF WS-INDEX-TILLK = +1                                               
674700        MOVE +0                TO OBKR-KDERS                              
674800     END-IF                                                               
674900     .                                                                    
675000     EJECT                                                                
675100 S21-UPPDATERA-TPO6 SECTION.                                              
675200                                                                          
675300     MOVE 'STA S21-TPO6        '           TO   WS-PGM-POSITION           
675400     MOVE ORAD-IDDISTR         TO TPO6-IDDISTR                            
675500     MOVE ORAD-IDKUNDNR        TO TPO6-IDKUNDNR                           
675600     MOVE ORAD-IDKUNDRF        TO TPO6-IDKUNDRF                           
675700     MOVE ORAD-IDARTNR         TO TPO6-IDARTNR                            
675800     MOVE OHUV-IDDC-PRIM       TO TPO6-IDDC-DAY                           
675900     MOVE AREG-FLREFILL        TO TPO6-FLREFILL                           
676000     MOVE AREG-KDUART          TO TPO6-KDUART                             
676100     MOVE AREG-REDIRLEV        TO TPO6-REDIRLEV                           
676200     MOVE ORAD-BERADREF        TO TPO6-BERADREF                           
676300     MOVE AREG-IDANSK          TO TPO6-IDANSK                             
676400     MOVE OHUV-IDKONTO         TO TPO6-IDKONTO                            
676500     MOVE OHUV-IDKST           TO TPO6-IDKST                              
676600     MOVE OHUV-IDANALYS        TO TPO6-IDANALYS                           
676700     MOVE ORAD-KDDSP           TO TPO6-KDDSP                              
676800     MOVE OHUV-KDFAKTYP        TO TPO6-KDFAKTYP                           
676900     MOVE ARB-KDFRAKT          TO TPO6-KDFRAKT                            
677000     MOVE ORAD-KDKVBRYT        TO TPO6-KDKVBRYT                           
677100     MOVE ORAD-KDORDING        TO TPO6-KDORDING                           
677200     MOVE OHUV-KDORDKL         TO TPO6-KDORDKL                            
677300     MOVE AREG-KDPRODSL        TO TPO6-KDPRODSL                           
677400     MOVE ORAD-KDVRINFO        TO TPO6-KDVRINFO                           
677500     MOVE ORAD-KVBEART-Q       TO TPO6-KVBEART-Q                          
677600     MOVE AREG-REKSIFFR        TO TPO6-REKSIFFR                           
677700     IF ORAD-KDPRTYP = 'P'                                                
677800        MOVE ORAD-PRARTNTO     TO TPO6-PRARTNTO                           
677900        MOVE ORAD-DEAL-PR-LINE TO TPO6-DEAL-PR-LINE                       
678000        MOVE ORAD-KDPRTYP      TO TPO6-KDPRTYP                            
678100        MOVE ORAD-FLPRTILL     TO TPO6-FLPRTILL                           
678200     ELSE                                                                 
678300        MOVE ORAD-DEAL-PR-LINE TO TPO6-DEAL-PR-LINE                       
678400        MOVE +0                TO TPO6-PRARTNTO                           
678500        MOVE SPACE             TO TPO6-KDPRTYP                            
678600        MOVE NEJ               TO TPO6-FLPRTILL                           
678700     END-IF                                                               
678800     MOVE ORAD-BEVOLREF        TO TPO6-BEVOLREF                           
678900     MOVE ORAD-IDKAMPRF        TO TPO6-IDKAMPRF                           
679000     MOVE ORAD-IDSYSTEM        TO TPO6-IDSYSTEM                           
679100     MOVE ORAD-FLINVEST        TO TPO6-FLINVEST                           
679200     MOVE ORAD-IDLEVNR         TO TPO6-IDLEVNR                            
679300     MOVE ORAD-KDTPOTYP        TO TPO6-KDTPOTYP                           
679400     MOVE OHUV-BEKUNDRF        TO TPO6-BEKUNDRF                           
679500     MOVE OHUV-FLFORBI         TO TPO6-FLFORBI                            
679600                                                                          
679700     MOVE ZERO                 TO TPO6-KDORDBEK                           
679800     MOVE SPACE                TO TPO6-FLKLAR                             
679900     MOVE OHUV-KDORDTYP-LDC    TO TPO6-KDORDTYP-LDC                       
680000     MOVE OHUV-TIREPDAT        TO TPO6-TIREPDAT                           
680100     MOVE ORAD-IDKUNDRF-WIP    TO TPO6-IDKUNDRF-WIP                       
680200                                                                          
680300     MOVE ORAD-KDOI            TO TPO6-KDOI                               
680400     MOVE ORAD-CLEARGROUP      TO TPO6-CLEARGROUP                         
680500                                                                          
680600     CALL W411TPO6 USING TPO6-W411TPO6 2109-PCB TPO6-ORDP-PCB             
680700                         TPO6-XXBU-PCB TPO6-XXBV-PCB                      
680800                         TPO6-XXBX-PCB TPO6-ARTS-PCB TIME-4437-PCB        
680900                                                                          
681000     .                                                                    
681100     EJECT                                                                
681200                                                                          
681300                                                                          
681400 S22-DATA-TILL-DEL-NOTE SECTION.                                          
681500                                                                          
681600     MOVE 'STA S22-DEL-NOTE    '           TO   WS-PGM-POSITION           
681700     MOVE OHUV-IDDISTR             TO TEST-IDDISTR                        
681800     IF DIST07-USA-RETAILER-DNOTE                                         
681900     OR DIST07-CAN-RETAILER                                               
682000     INITIALIZE DNOT-ORDER-INFO                                           
682100                                                                          
682200        MOVE IDPGM                    TO DNOT-IDPGM                       
682300        MOVE OHUV-IDORDER             TO DNOT-IDORDER                     
682400        MOVE ORAD-IDARTNR             TO DNOT-IDARTNR                     
682500        MOVE ORAD-IDDC                TO DNOT-IDDC                        
682600        MOVE OHUV-ADGMT-GATA          TO DNOT-ADGMT-GATA                  
682700        MOVE OHUV-ADGMT-PADR          TO DNOT-ADGMT-PADR                  
682800        MOVE OHUV-ADGMT-LAND          TO DNOT-ADGMT-LAND                  
682900        MOVE OHUV-BEGMT-RAD1          TO DNOT-BEGMT-RAD1                  
683000        MOVE OHUV-BEGMT-RAD2          TO DNOT-BEGMT-RAD2                  
683100        MOVE OHUV-BEKUNDRF            TO DNOT-BEKUNDRF                    
683200        MOVE ORAD-BERADREF            TO DNOT-BERADREF                    
683300        MOVE OHUV-IDGMTREF            TO DNOT-IDGMTREF                    
683400        MOVE OHUV-IDDC-PRIM           TO DNOT-IDDC-PRIM                   
683500        MOVE ORAD-IDKUNDRF-RO         TO DNOT-IDKUNDRF-RO                 
683600        MOVE ORAD-FLTILLK             TO DNOT-FLTILLK                     
683700        MOVE OHUV-KDORDKL             TO DNOT-KDORDKL                     
683800        MOVE ARB-KDFRAKT              TO DNOT-KDFRAKT                     
683900        MOVE ORAD-KVBEART             TO DNOT-KVBEART                     
684000        MOVE ORAD-KVBEART-Q           TO DNOT-KVBEART-Q                   
684100        MOVE ORAD-REKSIFFR            TO DNOT-REKSIFFR                    
684200        MOVE ORAD-TIREGDAT            TO DNOT-TIREGDAT                    
684300        MOVE ORAD-TIREGTID            TO DNOT-TIREGTID                    
684400        IF MID-FLDIRLEV (WS-INDEX-MID ) = JA OR YES                       
684500           MOVE JA                    TO DNOT-FLDIRLEV                    
684600        ELSE                                                              
684700           MOVE NEJ                   TO DNOT-FLDIRLEV                    
684800        END-IF                                                            
684900                                                                          
685000        IF MID-FLSLUT = JA                                                
685100           IF WS-INDEX-MID = WS-INDEX-MID-MAX                             
685200           OR MID-IDARTNR(WS-INDEX-MID + 1) = ALL '+'                     
685300              MOVE JA              TO DNOT-FL-ORAD-LAST                   
685400           END-IF                                                         
685500        END-IF                                                            
685600                                                                          
685700        CALL W411DNOT USING DNOT-W411DNOT                                 
685800                            DNOT-ORQP-PCB                                 
685900                            DNOT-ORQP2-PCB                                
686000                            DNOT-ORQP3-PCB                                
686100                            DNOT-4013-PCB                                 
686200                            DNOT-BENA-PCB                                 
686300     END-IF                                                               
686400     .                                                                    
686500     EJECT                                                                
686600 S23-DELETE-PRICE-Q-LINE  SECTION.                                        
686700                                                                          
686800     IF DIST79-DEALER-PRICE                                               
686900       IF ORAD-IDPRQUES > ZERO                                            
687000         INITIALIZE PRQU-W335PRQU                                         
687100         MOVE OHUV-IDDISTR       TO PRQU-IDDISTR                          
687200         MOVE OHUV-IDKUNDNR      TO PRQU-IDKUNDNR                         
687300         MOVE OHUV-IDKUNDRF      TO PRQU-IDKUNDRF                         
687400         MOVE ORAD-IDPRQUES      TO PRQU-IDPRQUES                         
687500         MOVE 4                  TO PRQU-KDCALL                           
687600         CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                 
687700                                            PRQU-WDC7-PCB                 
687800                                            PRQU-SJKO-WDK6-PCB            
687900       END-IF                                                             
688000     END-IF                                                               
688100     .                                                                    
688200     EJECT                                                                
688300 S24-CHANGE-PRICE-Q-LINE SECTION.                                         
688400                                                                          
688500     IF DIST79-DEALER-PRICE                                               
688600       IF ORAD-IDPRQUES > ZERO                                            
688700         INITIALIZE PRQU-W335PRQU                                         
688800         MOVE OHUV-IDDISTR       TO PRQU-IDDISTR                          
688900         MOVE OHUV-IDKUNDNR      TO PRQU-IDKUNDNR                         
689000         MOVE OHUV-IDKUNDRF      TO PRQU-IDKUNDRF                         
689100         MOVE ORAD-IDPRQUES      TO PRQU-IDPRQUES                         
689200         MOVE ORAD-KVBEART-Q     TO PRQU-KVBEART-Q                        
689300         MOVE 5                  TO PRQU-KDCALL                           
689400         CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                 
689500                                            PRQU-WDC7-PCB                 
689600                                            PRQU-SJKO-WDK6-PCB            
689700       END-IF                                                             
689800     END-IF                                                               
689900     .                                                                    
690000     EJECT                                                                
690100 S27-SLACK-OBKR  SECTION.                                                 
690200                                                                          
690300     MOVE ZERO             TO KVAN-KDORDBEK-UT                            
690400                              DLEV-KDORDBEK-UT                            
690500     MOVE NEJ              TO ALLT-SW                                     
690600                              OBKR-SW                                     
690700                                                                          
690800                                                                          
690900                                                                          
691000     .                                                                    
691100     EJECT                                                                
691200 S28-RAD-TILL-WDR5 SECTION.                                               
691300     MOVE '4251'                   TO 4251-IDHTYP                         
691400     MOVE MID-IDDISTR              TO 4251-IDDISTR                        
691500     MOVE MID-IDKUNDNR             TO 4251-IDKUNDNR                       
691600     MOVE MID-IDKUNDRF-RO          TO 4251-IDKUNDRF                       
691700     MOVE SPACE                    TO 4251-LOWVALUE                       
691800     PERFORM IMS-ISRT-4251                                                
691900                                                                          
692000     MOVE 1                        TO 4252-IDLOPNR                        
692100**   MOVE MID-RADER(WS-INDEX-MID)  TO 4252-RADER                          
692200     MOVE SPAR-MID-RADER(WS-INDEX-MID)  TO 4252-RADER                     
692300     PERFORM IMS-ISRT-4252                                                
692400     IF SEGMENT-FINNS-REDAN                                               
692500       PERFORM UNTIL SEGMENT-FINNS                                        
692600       ADD 1                       TO 4252-IDLOPNR                        
692700       PERFORM IMS-ISRT-4252                                              
692800       END-PERFORM                                                        
692900     END-IF                                                               
693000     .                                                                    
693100     EJECT                                                                
693200 S30-STARTA-MPP-NY SECTION.                                               
693300*     OM TILLÄGG STARTA NY TRANS 'W4T257X'                                
693400     IF MID-IDKUNDRF-RO NOT = SPACE                                       
693500       PERFORM S31-OPEN-WZ01                                              
693600       PERFORM S32-SEND-WZ01                                              
693700       PERFORM S33-CLOSE-WZ01                                             
693800     END-IF                                                               
693900     .                                                                    
694000     EJECT                                                                
694100 S31-OPEN-WZ01 SECTION.                                                   
694200     MOVE 'S31-OPEN-WZ01'       TO WS-PGM-POSITION                        
694300                                                                          
694400     MOVE 'OPEN'                     TO SEND-KDFUNC                       
694500     MOVE 'CARPARTS.PULS.DLEVRAD'    TO SEND-ADDISPABS                    
694600     MOVE SPACE                      TO SEND-ADDISPABS-RETURN             
694700                                                                          
694800     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
694900                                SEND-OPEN-AREA                            
695000     IF SEND-KDRC > 0                                                     
695100       MOVE SEND-KDRC           TO KDRC-DISP                              
695200       STRING 'WZ01SEND-OPEN RC-ERR = ' KDRC-DISP                         
695300            DELIMITED BY SIZE INTO FELTEXT                                
695400       CALL FELLOG                                                        
695500     ELSE                                                                 
695600       MOVE SEND-IDCOM               TO WS-IDCOM                          
695700     END-IF                                                               
695800     .                                                                    
695900     EJECT                                                                
696000 S32-SEND-WZ01 SECTION.                                                   
696100     MOVE 'S32-SEND-WZ01'    TO WS-PGM-POSITION                           
696200                                                                          
696300     MOVE 'PUT'                 TO SEND-KDFUNC                            
696400     COMPUTE SEND-KVDLEN = LENGTH OF 4257-MID-W4I25701                    
696500     MOVE MID-IDDISTR        TO 4257-MID-IDDISTR                          
696600     MOVE MID-IDKUNDNR       TO 4257-MID-IDKUNDNR                         
696700     MOVE MID-IDKUNDRF-RO    TO 4257-MID-IDORDNR                          
696800                                                                          
696900     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
697000                                SEND-KVDLEN                               
697100                                4257-MID-W4I25701                         
697200     IF SEND-KDRC > 0                                                     
697300       MOVE SEND-KDRC           TO KDRC-DISP                              
697400       STRING 'WZ01SEND-PUT RC-ERR = ' KDRC-DISP                          
697500            DELIMITED BY SIZE INTO FELTEXT                                
697600       CALL FELLOG                                                        
697700     END-IF                                                               
697800     .                                                                    
697900     EJECT                                                                
698000 S33-CLOSE-WZ01  SECTION.                                                 
698100     MOVE 'S33-CLOSE-WZ01'      TO WS-PGM-POSITION                        
698200                                                                          
698300     MOVE 'CLOSE'               TO SEND-KDFUNC                            
698400                                                                          
698500     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
698600     IF SEND-KDRC > 0                                                     
698700       MOVE SEND-KDRC           TO KDRC-DISP                              
698800       STRING 'WZ01SEND-CLOSE RC-ERR = ' KDRC-DISP                        
698900            DELIMITED BY SIZE INTO FELTEXT                                
699000       CALL FELLOG                                                        
699100     END-IF                                                               
699200     .                                                                    
699300     EJECT                                                                
699400 S40-HAMTA-WDB6-INFO      SECTION.                                        
699500                                                                          
699600     IF WS-CLDC-IX > IX-DCCLEAR-MAX                                       
699700       CONTINUE                                                           
699800     ELSE                                                                 
699900       IF CLDC-IDDC (WS-CLDC-IX) NOT = WS-IDDC-SEEK                       
700000                                                                          
700100          MOVE 1 TO WS-CLDC-IX                                            
700200          PERFORM UNTIL WS-CLDC-IX > IX-DCCLEAR-MAX OR                    
700300                        CLDC-IDDC (WS-CLDC-IX) = WS-IDDC-SEEK OR          
700400                        CLDC-IDDC (WS-CLDC-IX) = SPACE                    
700500             ADD 1 TO WS-CLDC-IX                                          
700600          END-PERFORM                                                     
700700                                                                          
700800       END-IF                                                             
700900     END-IF                                                               
701000     IF WS-CLDC-IX > IX-DCCLEAR-MAX OR                                    
701100        CLDC-IDDC (WS-CLDC-IX) = SPACE                                    
701200        MOVE WS-IDDC-SEEK TO W-IDDC-B6                                    
701300        PERFORM IMS-GU-WDB601                                             
701400     ELSE                                                                 
701500        MOVE CLDC-WDB601 (WS-CLDC-IX)  TO DLI-IO-AREA-B601                
701600     END-IF                                                               
701700     .                                                                    
701800     EJECT                                                                
701900                                                                          
702000 S01-KOLLA-I-HFAK-TAB SECTION.                                            
702100                                                                          
702200     IF WS-HFAK-REF-X10 NOT = SPACE                                       
702300        MOVE 1 TO HFAK-TAB-IX                                             
702400        PERFORM UNTIL HFAK-TAB-IX > MAX-HFAK-IX                           
702500           IF WS-HFAK-REF-X10 (1:3) = HFAK-BERADREF (HFAK-TAB-IX)         
702600           OR (WS-HFAK-REF-X10 (1:1) = '#'                                
702700           AND WS-HFAK-REF-X10 (2:2) NOT = SPACE                          
702800           AND HFAK-BERADREF-1 (HFAK-TAB-IX) = '#')                       
702900              MOVE JA TO BEVARREF-I-HFAK-TAB-SW                           
703000              MOVE 99 TO HFAK-TAB-IX                                      
703100           END-IF                                                         
703200           ADD 1 TO HFAK-TAB-IX                                           
703300        END-PERFORM                                                       
703400     END-IF                                                               
703500     .                                                                    
703600     EJECT                                                                
703700                                                                          
703800* --- IMS SEKTIONER ---                                                   
703900                                                                          
704000 IMS-GET-MSG SECTION.                                                     
704100                                                                          
704200     MOVE '  QC' TO GODK-STATUSKODER                                      
704300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
704400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
704500     PERFORM IMS-STATUSKONTROLL                                           
704600     .                                                                    
704700     SKIP2                                                                
704800 IMS-GN-MSG SECTION.                                                      
704900                                                                          
705000     MOVE '  '   TO GODK-STATUSKODER                                      
705100     CALL CBLTDLI USING GN MSG-PCB KOM-IO-AREA                            
705200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
705300     PERFORM IMS-STATUSKONTROLL                                           
705400     .                                                                    
705500     SKIP2                                                                
705600 IMS-INSERT-DISP-MSG SECTION.                                             
705700                                                                          
705800     MOVE SPACE TO GODK-STATUSKODER                                       
705900     CALL CBLTDLI USING ISRT DISP-PCB KOM-IO-AREA                         
706000     MOVE DISP-STATUS-CODE TO STATUS-WS                                   
706100     PERFORM IMS-STATUSKONTROLL                                           
706200     .                                                                    
706300     EJECT                                                                
706400                                                                          
706500 IMS-INSERT-4298-MSG SECTION.                                             
706600                                                                          
706700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
706800     MOVE SPACE TO GODK-STATUSKODER                                       
706900     CALL CBLTDLI USING ISRT 4298-PCB MSG-IO-AREA                         
707000     MOVE 4298-STATUS-CODE TO STATUS-WS                                   
707100     PERFORM IMS-STATUSKONTROLL                                           
707200     .                                                                    
707300     SKIP3                                                                
707400 IMS-INSERT-4299-MSG SECTION.                                             
707500                                                                          
707600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
707700     MOVE SPACE TO GODK-STATUSKODER                                       
707800     CALL CBLTDLI USING ISRT 4299-PCB MSG-IO-AREA                         
707900     MOVE 4299-STATUS-CODE TO STATUS-WS                                   
708000     PERFORM IMS-STATUSKONTROLL                                           
708100     .                                                                    
708200     EJECT                                                                
708300                                                                          
708400 IMS-PURG-MSG-2191  SECTION.                                              
708500     MOVE SPACE TO GODK-STATUSKODER                                       
708600     CALL  CBLTDLI  USING PURG 2191-PCB MSG-IO-AREA                       
708700     MOVE 2191-STATUS-CODE TO STATUS-WS                                   
708800     PERFORM IMS-STATUSKONTROLL                                           
708900     .                                                                    
709000     EJECT                                                                
709100                                                                          
709200 IMS-01-GHU-ORQI-WDQ201 SECTION.                                          
709300                                                                          
709400     STRING 'WLORQI01(WDQ2CSEQ =' W-IDGMTREF-X ')'                        
709500          DELIMITED BY SIZE INTO SSA1                                     
709600     MOVE '  GE'               TO GODK-STATUSKODER                        
709700     CALL CBLTDLI USING GHU ORQI-PCB DLI-IO-AREA-OHUV SSA1                
709800     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
709900     PERFORM IMS-STATUSKONTROLL                                           
710000     .                                                                    
710100     SKIP2                                                                
710200                                                                          
710300 IMS-REPL-WDQ2-WDQ201 SECTION.                                            
710400                                                                          
710500     MOVE '    '               TO GODK-STATUSKODER                        
710600     CALL CBLTDLI USING REPL ORQI-PCB DLI-IO-AREA-OHUV                    
710700     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
710800     PERFORM IMS-STATUSKONTROLL                                           
710900     .                                                                    
711000     EJECT                                                                
711100                                                                          
711200 IMS-02-GU-ORQI-WDQ201-EMPTY SECTION.                                     
711300                                                                          
711400     STRING 'WLORQI01(WDQ2CSEQ =' W-IDGMTREF-X ')'                        
711500          DELIMITED BY SIZE INTO SSA1                                     
711600     MOVE '  '                 TO GODK-STATUSKODER                        
711700     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-AREA-OHUV-EMPTY SSA1           
711800     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
711900     PERFORM IMS-STATUSKONTROLL                                           
712000     .                                                                    
712100     SKIP2                                                                
712200 IMS-03-GNP-ORQI-WDQ212 SECTION.                                          
712300                                                                          
712400     MOVE   'WLORQI12'         TO SSA1                                    
712500     MOVE '  GE'               TO GODK-STATUSKODER                        
712600     CALL CBLTDLI USING GNP  ORQI-PCB DLI-IO-AREA-ARB SSA1                
712700     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
712800     PERFORM IMS-STATUSKONTROLL                                           
712900     .                                                                    
713000     EJECT                                                                
713100 IMS-07-GU-ORQM-WDQ101 SECTION.                                           
713200                                                                          
713300     STRING 'WLORQM01(WDQ101KY >' W-WDQ101KY-MIN-X                        
713400                    '&WDQ101KY <' W-WDQ101KY-MAX-X ')'                    
713500          DELIMITED BY SIZE INTO SSA1                                     
713600     MOVE '  GE'               TO GODK-STATUSKODER                        
713700     CALL CBLTDLI USING GU   ORQM-PCB DLI-IO-AREA-OBKR SSA1               
713800     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
713900     PERFORM IMS-STATUSKONTROLL                                           
714000     .                                                                    
714100     SKIP2                                                                
714200 IMS-07-GN-ORQM-WDQ101 SECTION.                                           
714300                                                                          
714400     STRING 'WLORQM01(WDQ101KY >' W-WDQ101KY-MIN-X                        
714500                    '&WDQ101KY <' W-WDQ101KY-MAX-X ')'                    
714600          DELIMITED BY SIZE INTO SSA1                                     
714700     MOVE '  GEGB'             TO GODK-STATUSKODER                        
714800     CALL CBLTDLI USING GN   ORQM-PCB DLI-IO-AREA-OBKR SSA1               
714900     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
715000     PERFORM IMS-STATUSKONTROLL                                           
715100     .                                                                    
715200     SKIP2                                                                
715300 IMS-09-GU-WLARTM-WDK901 SECTION.                                         
715400                                                                          
715500     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
715600          DELIMITED BY SIZE INTO SSA1                                     
715700     MOVE '  GE'               TO GODK-STATUSKODER                        
715800     CALL CBLTDLI USING GU ARTM-PCB DLI-IO-AREA-ART SSA1                  
715900     MOVE ARTM-STATUS-CODE     TO STATUS-WS                               
716000     PERFORM IMS-STATUSKONTROLL                                           
716100     .                                                                    
716200     EJECT                                                                
716300 IMS-10-GHU-WLARTM-WDK901 SECTION.                                        
716400                                                                          
716500     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
716600          DELIMITED BY SIZE INTO SSA1                                     
716700     MOVE '    '               TO GODK-STATUSKODER                        
716800     CALL CBLTDLI USING GHU ARTM-PCB DLI-IO-AREA-ART SSA1                 
716900     MOVE ARTM-STATUS-CODE     TO STATUS-WS                               
717000     PERFORM IMS-STATUSKONTROLL                                           
717100     .                                                                    
717200                                                                          
717300 IMS-11-REPL-ARTM-WDK901 SECTION.                                         
717400                                                                          
717500     MOVE '    '               TO GODK-STATUSKODER                        
717600     CALL CBLTDLI USING REPL ARTM-PCB DLI-IO-AREA-ART                     
717700     MOVE ARTM-STATUS-CODE     TO STATUS-WS                               
717800     PERFORM IMS-STATUSKONTROLL                                           
717900     .                                                                    
718000     EJECT                                                                
718100 IMS-GHU-WDK611 SECTION.                                                  
718200                                                                          
718300     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
718400          DELIMITED BY SIZE INTO SSA1                                     
718500     MOVE 'WDK611   '         TO SSA2                                     
718600     MOVE '    '              TO GODK-STATUSKODER                         
718700     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-AREA-WDK611 SSA1 SSA2         
718800     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
718900     PERFORM IMS-STATUSKONTROLL                                           
719000     .                                                                    
719100 IMS-GU-WDK611-TILK SECTION.                                              
719200                                                                          
719300     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-T-X ')'                       
719400          DELIMITED BY SIZE INTO SSA1                                     
719500     MOVE 'WDK611   '         TO SSA2                                     
719600     MOVE '    '              TO GODK-STATUSKODER                         
719700     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-WDK611-T SSA1 SSA2        
719800     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
719900     PERFORM IMS-STATUSKONTROLL                                           
720000     .                                                                    
720100 IMS-REPL-WDK611 SECTION.                                                 
720200                                                                          
720300     MOVE '    '               TO GODK-STATUSKODER                        
720400     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-AREA-WDK611                  
720500     MOVE WDK6-STATUS-CODE     TO STATUS-WS                               
720600     PERFORM IMS-STATUSKONTROLL                                           
720700     .                                                                    
720800     EJECT                                                                
720900 IMS-GHU-WDK711 SECTION.                                                  
721000                                                                          
721100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
721200          DELIMITED BY SIZE INTO SSA1                                     
721300     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
721400          DELIMITED BY SIZE INTO SSA2                                     
721500     MOVE '    '               TO GODK-STATUSKODER                        
721600     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-AREA-WDK7 SSA1 SSA2           
721700     MOVE WDK7-STATUS-CODE     TO STATUS-WS                               
721800     PERFORM IMS-STATUSKONTROLL                                           
721900     .                                                                    
722000                                                                          
722100 IMS-GU-WDK711-TILK SECTION.                                              
722200                                                                          
722300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-T-X ')'                       
722400          DELIMITED BY SIZE INTO SSA1                                     
722500     STRING 'WDK711  (IDDC     =' W-IDDC-T-X ')'                          
722600          DELIMITED BY SIZE INTO SSA2                                     
722700     MOVE '  GE'               TO GODK-STATUSKODER                        
722800     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK7 SSA1 SSA2            
722900     MOVE WDK7-STATUS-CODE     TO STATUS-WS                               
723000     PERFORM IMS-STATUSKONTROLL                                           
723100     .                                                                    
723200 IMS-GU-WDK722 SECTION.                                                   
723300                                                                          
723400     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
723500          DELIMITED BY SIZE INTO SSA1                                     
723600     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
723700          DELIMITED BY SIZE INTO SSA2                                     
723800     MOVE 'WDK722 '           TO SSA3                                     
723900     MOVE '  GE'               TO GODK-STATUSKODER                        
724000     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3          
724100     MOVE WDK7-STATUS-CODE     TO STATUS-WS                               
724200     PERFORM IMS-STATUSKONTROLL                                           
724300     .                                                                    
724400 IMS-REPL-WDK7 SECTION.                                                   
724500                                                                          
724600     MOVE '    '               TO GODK-STATUSKODER                        
724700     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-AREA-WDK7                    
724800     MOVE WDK7-STATUS-CODE     TO STATUS-WS                               
724900     PERFORM IMS-STATUSKONTROLL                                           
725000     .                                                                    
725100     EJECT                                                                
725200 IMS-17-ISRT-4541-WDR411 SECTION.                                         
725300                                                                          
725400     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-4541-X ')'                    
725500          DELIMITED BY SIZE INTO SSA1                                     
725600     MOVE 'WDGX4542 '          TO SSA2                                    
725700     MOVE '  II'               TO GODK-STATUSKODER                        
725800     CALL CBLTDLI USING ISRT 4541-PCB DLI-IO-AREA-4541 SSA1 SSA2          
725900     MOVE 4541-STATUS-CODE     TO STATUS-WS                               
726000     PERFORM IMS-STATUSKONTROLL                                           
726100     .                                                                    
726200     SKIP2                                                                
726300                                                                          
726400 IMS-22-ISRT-ORQF-WDQ401 SECTION.                                         
726500                                                                          
726600     MOVE 'WLORQF01 '          TO SSA1                                    
726700     MOVE '  II'               TO GODK-STATUSKODER                        
726800     CALL CBLTDLI USING ISRT ORQF-PCB DLI-IO-AREA-ORAD SSA1               
726900     MOVE ORQF-STATUS-CODE     TO STATUS-WS                               
727000     PERFORM IMS-STATUSKONTROLL                                           
727100     .                                                                    
727200     SKIP2                                                                
727300 IMS-25-ISRT-WLORQM01-WDQ101 SECTION.                                     
727400                                                                          
727500     MOVE 'WLORQM01 '          TO SSA1                                    
727600     MOVE '    '               TO GODK-STATUSKODER                        
727700     CALL CBLTDLI USING ISRT ORQM-PCB DLI-IO-AREA-OBKR SSA1               
727800     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
727900     PERFORM IMS-STATUSKONTROLL                                           
728000     .                                                                    
728100                                                                          
728200 IMS-GU-WDB201 SECTION.                                                   
728300                                                                          
728400     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
728500            DELIMITED BY SIZE INTO SSA1                                   
728600     MOVE '    '               TO GODK-STATUSKODER                        
728700     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-AREA-WDB201 SSA1               
728800     MOVE WDB2-STATUS-CODE     TO STATUS-WS                               
728900     PERFORM IMS-STATUSKONTROLL                                           
729000     .                                                                    
729100     SKIP2                                                                
729200 IMS-GU-WDB101 SECTION.                                                   
729300                                                                          
729400     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
729500          DELIMITED BY SIZE INTO SSA1                                     
729600     MOVE '  '                 TO GODK-STATUSKODER                        
729700     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-AREA-WDB101 SSA1               
729800     MOVE WDB1-STATUS-CODE     TO STATUS-WS                               
729900     PERFORM IMS-STATUSKONTROLL                                           
730000     .                                                                    
730100     SKIP2                                                                
730200 IMS-GU-WDB601    SECTION.                                                
730300     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
730400          DELIMITED BY SIZE INTO SSA1                                     
730500     MOVE '  GE' TO GODK-STATUSKODER                                      
730600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
730700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
730800     PERFORM IMS-STATUSKONTROLL                                           
730900     IF SEGMENT-SAKNAS                                                    
731000        MOVE SPACE TO DCS-KDDC                                            
731100     END-IF                                                               
731200     .                                                                    
731300     EJECT                                                                
731400 DB2-SELECT-TP4TRAN     SECTION.                                          
731500     MOVE 'DB2-SELECT-TP4TRAN   ' TO  WS-DB2-SEKTION                      
731600                                                                          
731700     MOVE 000100 TO GODK-SQLCODEKODER                                     
731800                                                                          
731900     EXEC SQL                                                             
732000           SELECT  DISTINCT                                               
732100                   IDDC_REC                                               
732200                                                                          
732300           INTO   :TP4TRAN-IDDC-REC                                       
732400                                                                          
732500           FROM    TP4TRAN                                                
732600                                                                          
732700           WHERE IDDISTR   = :W-TP4TRAN-IDDISTR                           
732800     END-EXEC                                                             
732900                                                                          
733000     MOVE SQLCODE TO SQLCODE-WS                                           
733100     PERFORM DB2-STATUSKONTROLL                                           
733200     .                                                                    
733300     EJECT                                                                
733400                                                                          
733500 IMS-ISRT-WDA501 SECTION.                                                 
733600                                                                          
733700     MOVE 'WDA501   ' TO SSA1                                             
733800     MOVE '  II' TO GODK-STATUSKODER                                      
733900     CALL CBLTDLI USING ISRT WDA5-PCB DLI-IO-WDA501 SSA1                  
734000     MOVE WDA5-STATUS-CODE TO STATUS-WS                                   
734100     PERFORM IMS-STATUSKONTROLL                                           
734200     .                                                                    
734300     EJECT                                                                
734400 IMS-ISRT-4251 SECTION.                                                   
734500                                                                          
734600     MOVE 'WDR501   '      TO SSA1                                        
734700     MOVE '  II'           TO GODK-STATUSKODER                            
734800     CALL CBLTDLI USING ISRT 4251-PCB DLI-IO-4251 SSA1                    
734900     MOVE 4251-STATUS-CODE TO STATUS-WS                                   
735000     PERFORM IMS-STATUSKONTROLL                                           
735100     .                                                                    
735200     EJECT                                                                
735300                                                                          
735400 IMS-ISRT-4252 SECTION.                                                   
735500                                                                          
735600     STRING 'WDR501  (WDGXKEY  =' 4251-WDGX4251 ')'                       
735700          DELIMITED BY SIZE INTO SSA1                                     
735800     MOVE 'WDGX4252 '          TO SSA2                                    
735900     MOVE '  II'               TO GODK-STATUSKODER                        
736000     CALL CBLTDLI USING ISRT 4251-PCB DLI-IO-4252 SSA1 SSA2               
736100     MOVE 4251-STATUS-CODE     TO STATUS-WS                               
736200     PERFORM IMS-STATUSKONTROLL                                           
736300     .                                                                    
736400     EJECT                                                                
736500                                                                          
736600 IMS-GU-XXJN-WLXXJN11 SECTION.                                            
736700                                                                          
736800     STRING 'WLXXJN01(WDGXKEY  =' W-4511-IDHTYP-X  ')'                    
736900          DELIMITED BY SIZE INTO SSA1                                     
737000     STRING 'WLXXJN11(KDTPOTYP =' W-4512-KDTPOTYP-X                       
737100                    '&KDORDKL  =' W-4512-KDORDKL-X                        
737200                    '&IDDISTRF<=' W-4512-IDDISTR-FOM-X                    
737300                    '&IDDISTRT>=' W-4512-IDDISTR-TOM-X ')'                
737400          DELIMITED BY SIZE INTO SSA2                                     
737500     MOVE '    ' TO GODK-STATUSKODER                                      
737600     CALL CBLTDLI USING GU XXJN-PCB DLI-IO-AREA-SUB SSA1 SSA2             
737700     MOVE XXJN-STATUS-CODE TO STATUS-WS                                   
737800     PERFORM IMS-STATUSKONTROLL                                           
737900     .                                                                    
738000     EJECT                                                                
738100 IMS-ISRT-FILA-WDR6 SECTION.                                              
738200                                                                          
738300     MOVE   'WLFILA01 ' TO SSA1                                           
738400     MOVE '  II' TO GODK-STATUSKODER                                      
738500     CALL CBLTDLI USING ISRT FILA-PCB DLI-IO-AREA2 SSA1                   
738600     MOVE FILA-STATUS-CODE TO STATUS-WS                                   
738700     PERFORM IMS-STATUSKONTROLL                                           
738800     .                                                                    
738900                                                                          
739000 IMS-GU-WDB301 SECTION.                                                   
739100                                                                          
739200     STRING 'WDB301  (WDB301KY =' W-WDB301KY-X                            
739300                    '!WDB301KY =' W-WDB301KY-DEF-X  ')'                   
739400          DELIMITED BY SIZE INTO SSA1                                     
739500     MOVE '  GE'              TO GODK-STATUSKODER                         
739600     CALL CBLTDLI USING GU WDB3-PCB DLI-IO-AREA-WDB301 SSA1               
739700     MOVE WDB3-STATUS-CODE    TO STATUS-WS                                
739800     PERFORM IMS-STATUSKONTROLL                                           
739900     .                                                                    
740000     EJECT                                                                
740100                                                                          
740200 IMS-ISRT-WDR601 SECTION.                                                 
740300                                                                          
740400     MOVE 'WDR601' TO SSA1                                                
740500     MOVE '  II' TO GODK-STATUSKODER                                      
740600     CALL CBLTDLI USING ISRT WDR6-PCB DLI-IO-AREA-R601 SSA1               
740700     MOVE WDR6-STATUS-CODE TO STATUS-WS                                   
740800     PERFORM IMS-STATUSKONTROLL                                           
740900     .                                                                    
741000     EJECT                                                                
741100                                                                          
741200 IMS-STATUSKONTROLL SECTION.                                              
741300                                                                          
741400     SET STATUS-IX TO 1                                                   
741500     SEARCH GODK-STATUS                                                   
741600       AT END CALL FELLOG                                                 
741700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
741800     END-SEARCH                                                           
741900     .                                                                    
742000 DB2-STATUSKONTROLL  SECTION.                                             
742100                                                                          
742200     SET SQLCODE-IX TO 1                                                  
742300     SEARCH GODK-SQLCODE                                                  
742400       AT END                                                             
742500          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
742600          DELIMITED BY SIZE INTO FELTEXT                                  
742700          CALL ABEND USING RKOD-ABEND-DB2                                 
742800       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
742900     END-SEARCH                                                           
743000     .                                                                    
