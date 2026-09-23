000100 PROCESS DYNAM                                                            
000201 ID DIVISION.                                                             
000301     SKIP2                                                                
000401 PROGRAM-ID.     W4025800.                                                
000501 AUTHOR.         PRIYA RC                                                 
000601 DATE-WRITTEN.   MAR 2024.                                                
000701                                                                          
000801     REMARKS.                                                             
000901**                                                                        
001001*    FUNKTION.                                                            
001101*        PROGRAM TO ADD/UPDATE PARTNUMBER,QUANTITY,DC                     
001201*        FOR AN EXISTING ORDERLINE.                                       
003401*                                                                         
003501*        DATABASE READS/UPDATES.                                          
003601*        PROGRAM READS                   WDK6                             
003602*        PROGRAM READS                   WDB2                             
003603*        PROGRAM READS                   WDB1                             
003604*        PROGRAM READS                   WDF5                             
003801*        PROGRAM READS/UPDATES           WDQ2                             
003901*        PROGRAM UPDATES/INSERTS/DELETES WDQ4                             
004001*        PROGRAM UPDATES/INSERTS         WDQ1                             
006201*                                                                         
006301*    INDATA.                                                              
006401*        TRANSACTION: W4T258X                                             
006501*        MID:         W4I25801                                            
006601*                     WMSGKOM                                             
006701*    UTDATA.                                                              
006801*        MOD:         WMSGMOD    FEL/KLAR MED TILL DISPATCHER             
007001*                                                                         
007101* CHANGE LOG:  STORY 3573926:PROGRAM CALLED FROM W400ORUP.                
008501     EJECT                                                                
008601 ENVIRONMENT DIVISION.                                                    
008701                                                                          
008801 DATA DIVISION.                                                           
008901 WORKING-STORAGE SECTION.                                                 
009001*    -- CHECKED BY WY2000                                                 
009002*    -COPY WY2000W1                                                       
009003     SKIP3                                                                
009101 77  IDPGM                       PIC X(08)   VALUE 'W4025800'.            
009202 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
009302 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
009501*01  -COPY WWDCKONS                                                       
009601                                                                          
009701*01  -COPY WWPRODSL                                                       
009801                                                                          
009901*    DC FÖR CLEARING AV VERKSTADSORDER                                    
010001*01  -COPY WWDC01                                                         
010101                                                                          
010102*    BYT PARTNUMBERS                                                      
010103*01 -COPY WWBYT03                                                         
010104                                                                          
010201 77  FELTEXT                     PIC X(64)   VALUE SPACE.                 
010301 77  KDRC-DISPLAY                PIC Z(5)    VALUE ZERO.                  
010401 77  YES                         PIC X(1)    VALUE 'Y'.                   
010501 77  JA                          PIC X(1)    VALUE 'J'.                   
010601 77  NEJ                         PIC X(1)    VALUE 'N'.                   
010701 77  SPEC-FORBI                  PIC X(1)    VALUE 'S'.                   
010801 77  RKOD-ABEND                  PIC S9(3)   COMP SYNC VALUE +33.         
010901 77  RKOD-ABEND-MED-DUMP         PIC S9(4)  VALUE +1000 COMP SYNC.        
011001 77  WS-INDEX                    PIC S9(9)   COMP SYNC VALUE ZERO.        
011101 77  WS-INDEX-ORFK               PIC S9(9)   COMP SYNC VALUE ZERO.        
011201 77  WS-INDEX-ORFK-MAX           PIC S9(9)   COMP SYNC VALUE +13.         
011202 77  WS-INDEX-ORFK-TOT           PIC S9(9)   COMP SYNC VALUE +14.         
011301 77  WS-INDEX-TILLK              PIC S9(9)   COMP SYNC VALUE ZERO.        
011401 77  WS-INDEX-TILLK-MAX          PIC S9(9)   COMP SYNC VALUE +20.         
011501 77  WS-INDEX-WOPS               PIC S9(9)   COMP SYNC VALUE ZERO.        
011601 77  WS-INDEX-WOPS-MAX           PIC S9(9)   COMP SYNC VALUE +100.        
011602 77  AVSR-INDX                   PIC S9(4)   VALUE +0 COMP SYNC.          
011603 77  UPD-INDX                    PIC S9(4)   VALUE +0 COMP SYNC.          
011604 77  ADD-INDX                    PIC S9(4)   VALUE +0 COMP SYNC.          
011605 77  SEARCH-INDX                 PIC S9(4)   VALUE +0 COMP SYNC.          
011701 77  WS-RESLATT                  PIC S9(3)   VALUE +0  COMP-3.            
011801 77  WS-IDPRQUES                 PIC S9(7)   VALUE +0.                    
011901 77  WS-IDANSK                   PIC 9(3)    VALUE  0.                    
012001 77  SPAR-ORAD-KVSLATT           PIC S9(7)   VALUE +0  COMP-3.            
012101 77  W-KDORDBEK                  PIC S9(2)   VALUE +0.                    
012201 77  W-KDTPOTYP                  PIC S9      VALUE +0.                    
012301 77  KDRC-DISP                   PIC 9(4)    VALUE ZERO.                  
012401 77  WS-ADLAGOMR                 PIC S9(3)   VALUE ZERO COMP-3.           
012501 77  WS-ADGANG                   PIC S9(3)   VALUE ZERO COMP-3.           
012601 77  WS-ADPLATS                  PIC S9(5)   VALUE ZERO COMP-3.           
012701 77  WS-IXDCCLEAR                PIC S9(5)   VALUE ZERO COMP-3.           
012801 77  WS-CLDC-IX                  PIC S9(5)   VALUE +1   COMP-3.           
012901 77  WS-CLDC-IX-MAX              PIC S9(5)   VALUE +7   COMP-3.           
013001 77  WS-IDDISTR-NUM4             PIC 9(4).                                
013101 77  WS-IDKUNDNR-NUM6            PIC 9(6).                                
013201 77  WS-KDVALISO-NA              PIC X(3)    VALUE 'N/A'.                 
013301                                                                          
013401 77  IX                          PIC S9(3)  VALUE ZERO COMP-3.            
013501 77  MAX-IX                      PIC S9(3)  VALUE +7   COMP-3.            
013601 77  TILK-IX                     PIC S9(3)  VALUE ZERO COMP-3.            
013701 77  TILK-IX-MAX                 PIC S9(3)  VALUE +20  COMP-3.            
013801 77  RFS-IX                      PIC S9(4)   COMP SYNC VALUE ZERO.        
013901 77  MAX-RFS-IX                  PIC S9(4)   COMP SYNC VALUE +4.          
014001 77  WS-SAVE-INDEX               PIC S9(9)   COMP SYNC VALUE ZERO.        
014101 77  IDDC-IX                     PIC S9(3)   VALUE ZERO COMP-3.           
014201 77  W-TILLK-DC                  PIC X(2)    VALUE SPACE.                 
014301                                                                          
014401 77  WS-HFAK-REF-X10             PIC X(10)   VALUE SPACE.                 
014501 77  HFAK-TAB-IX                 PIC S9(9)   VALUE +0   COMP SYNC.        
014601 77  BEVARREF-I-HFAK-TAB-SW      PIC X       VALUE 'N'.                   
014701     88  BEVARREF-I-HFAK-TAB                 VALUE 'J'.                   
014901 01  WS-TIHHMMSS                 PIC 9(6)   VALUE ZERO.                   
015001 01  FILLER REDEFINES WS-TIHHMMSS.                                        
015101     03 WS-TIHHMM                PIC 9(4).                                
015201     03 FILLER                   PIC 9(2).                                
015301     EJECT                                                                
015401                                                                          
015501 77  ALLT-SW                     PIC X       VALUE 'J'.                   
015601     88  ALLT-OK                             VALUE 'J'.                   
015701                                                                          
015705 77  DIRLEV-KOLL                 PIC X       VALUE 'N'.                   
015706                                                                          
015801 77  ACTION-SW                   PIC X       VALUE 'J'.                   
015901     88  ADD-ORDERLINE                       VALUE 'J'.                   
016001     88  UPD-ORDERLINE                       VALUE 'N'.                   
016002                                                                          
016003 77  UPD-ACTION-SW               PIC X       VALUE 'N'.                   
016004     88  UPD-ACTION                          VALUE 'J'.                   
016101                                                                          
016102 77  PREPLANED-SW                PIC X       VALUE 'N'.                   
016103     88  PREPLANED                           VALUE 'J'.                   
016104                                                                          
016105 77  DUP-FND-SW                  PIC X       VALUE 'N'.                   
016106     88  DUP-FND                             VALUE 'J'.                   
016107                                                                          
016108 77  SW-SOFTWARE-ORDER           PIC X       VALUE 'N'.                   
016109     88  SOFTWARE-ORDER                      VALUE 'J'.                   
016110                                                                          
016111 77  SW-IDARTNR-FND              PIC X       VALUE 'N'.                   
016112     88  IDARTNR-FND                         VALUE 'J'.                   
016113                                                                          
016114 77  TILLK-SW                    PIC X       VALUE 'N'.                   
016115     88  TILLKOMMANDE-RAD                    VALUE 'J'.                   
016116     88  EJ-TILLKOMMANDE-RAD                 VALUE 'N'.                   
016120                                                                          
016201 77  KOLLA-ERS-SW                PIC X       VALUE 'N'.                   
016301     88  KOLLA-ERS                           VALUE 'J'.                   
016401                                                                          
016501 77  OBKR-SW                     PIC X       VALUE 'N'.                   
016601     88  SKRIV-OBKR                          VALUE 'J'.                   
016701     88  OBKR-SKRIVEN                        VALUE 'S'.                   
016801                                                                          
016901 77  SW-BYT-ARTIKEL              PIC X       VALUE 'N'.                   
017001     88  BYT-ARTIKEL                         VALUE 'J'.                   
017101     88  BYT-EJ-ARTIKEL                      VALUE 'N'.                   
017201                                                                          
017701 77  LDC-ARTIKELBYTE-SW          PIC X       VALUE 'N'.                   
017801     88  LDC-INGET-BYTE                      VALUE 'N'.                   
017901     88  LDC-ARTIKELBYTE                     VALUE 'J'.                   
018001     88  LDC-ARTIKEL-BYTT                    VALUE 'B'.                   
018101     88  LDC-ARTIKEL-TILLBAKA                VALUE 'T'.                   
018201                                                                          
018301 77  BAL-DC-FND-SW               PIC X       VALUE 'N'.                   
018401     88  BAL-DC-FND                          VALUE 'J'.                   
018501                                                                          
018601 77  TILLK-BAL-DC-FND-SW         PIC X       VALUE 'N'.                   
018701     88  TILLK-BAL-DC-FND                    VALUE 'J'.                   
018801                                                                          
018901 77  CDC-MOVE-SW                 PIC X       VALUE 'N'.                   
019001     88  CDC-MOVE                            VALUE 'J'.                   
019002                                                                          
019003 77  CDC-CALL-SW                 PIC X       VALUE 'N'.                   
019004     88  CDC-CALL                            VALUE 'J'.                   
019005                                                                          
019006 77  AVSR-SW                     PIC X       VALUE 'J'.                   
019007     88  AVSR-OK                             VALUE 'J'.                   
019008                                                                          
019101                                                                          
019402 77  SW-KDORDSTA-O-ALL-SPACE-FLAG PIC X(1)   VALUE 'J'.                   
019403                                                                          
019601 77  KDERS-CHAIN-SW              PIC X       VALUE 'N'.                   
019701     88  KDERS-CHAIN                         VALUE 'J'.                   
019801                                                                          
019901*01  -COPY WDQ401   -PRE HELP-                                            
020001*01  -COPY WDQ101   -PRE HELP-                                            
020101*01  -COPY W411CDCA -PRE HELP-                                            
020401*01  -COPY W411KERS -PRE HELP-                                            
020501*01  -COPY W411KVAN -PRE HELP-                                            
020601*01  -COPY W411ORFK -PRE HELP-                                            
020701*01  -COPY W411NDCA -PRE HELP-                                            
020801*01  -COPY W411XDCA -PRE HELP-                                            
020901*01  -COPY W411SDCA -PRE HELP-                                            
021001*01  -COPY W411SPAR -PRE HELP-                                            
021101*01  -COPY W411STOR -PRE HELP-                                            
021401*01  -COPY W411RELS -PRE HELP-                                            
021501*01  -COPY W411TILK -PRE HELP-                                            
021601*01  -COPY W411AREG -PRE HELP-                                            
021701                                                                          
021801 77  W-REPSW                     PIC X       VALUE 'J'.                   
021901                                                                          
022001 77  WS-TRANSFER                 PIC X       VALUE 'N'.                   
022101                                                                          
022201 01  WS-ALFA-1.                                                           
022301     03  WS-NUM-1                PIC 9(1).                                
022401 01  WS-ALFA-6.                                                           
022501     03  WS-NUM-6                PIC 9(6).                                
022601                                                                          
023201 01  WS-TITIREGD-9KOMPL          PIC 9(8).                                
023301 01  FILLER REDEFINES WS-TITIREGD-9KOMPL.                                 
023401     03  WS-SEKEL-9KOMPL         PIC 9(2).                                
023501     03  WS-AAMMDD-9KOMPL        PIC 9(6).                                
023601                                                                          
023701 01  DAGENS-DATUM                PIC 9(6)  VALUE ZERO.                    
023801 01  FILLER                REDEFINES DAGENS-DATUM.                        
023901     03  DAGENS-AA         PIC  9(2).                                     
024001     03  DAGENS-MM         PIC  9(2).                                     
024101     03  DAGENS-DD         PIC  9(2).                                     
024102 01  WS-LOCAL-DATE-AAMMDD  PIC 9(6) VALUE 0.                              
024201                                                                          
024301 01  WS-TIRFS                    PIC 9(10).                               
024401 01  FILLER REDEFINES WS-TIRFS.                                           
024501     03  WS-TIRFS-DAT            PIC 9(6).                                
024601     03  WS-TIRFS-TID            PIC 9(4).                                
024701 01  W-WORK-VAR.                                                          
024702     03 W-IDTRANS                PIC X(4)    VALUE '4258'.                
024801     03 W-FLREFILL-MAIN          PIC X       VALUE SPACE.                 
024901     03 W-KDPRODSL-MAIN          PIC S9(3)   COMP-3 VALUE 0.              
025001     03 W-KDSORT-MAIN            PIC X(2)    VALUE SPACE.                 
025101     03 W-KVQPACK-1-MAIN         PIC S9(5)   COMP-3 VALUE 0.              
025201     03 W-REDIRLEV-MAIN          PIC S9V9(2) COMP-3 VALUE 0.              
025301     03 W-FLREFILL-REPL          PIC X       VALUE SPACE.                 
025401     03 W-KDPRODSL-REPL          PIC S9(3)   COMP-3 VALUE 0.              
025501     03 W-KDSORT-REPL            PIC X(2)    VALUE SPACE.                 
025601     03 W-KVQPACK-1-REPL         PIC S9(5)   COMP-3 VALUE 0.              
025701     03 W-REDIRLEV-REPL          PIC S9V9(2) COMP-3 VALUE 0.              
025801     03 W-IDARTNR-SDCA           PIC S9(9)   COMP-3 VALUE 0.              
025802     03 W-SAVE-IDORDER           PIC S9(7)   COMP-3 VALUE 0.              
025803     03 W-SAVE-IDARTNR           PIC S9(9)   COMP-3 VALUE 0.              
025804     03 W-IDARTNR-CNT            PIC 9(5)    VALUE 0.                     
025901     03 WS-IDSYSTEM              PIC  X(4)   VALUE SPACE.                 
026001     03 WS-IDSYSTEM-LYNK         PIC  X(4)   VALUE 'LYNK'.                
026101     03 WS-IDSYSTEM-POLE         PIC  X(4)   VALUE 'POLE'.                
026201     03 WS-IDSYSTEM-ECOM         PIC  X(4)   VALUE 'ECOM'.                
026301     03 WS-IDSYSTEM-VOUI         PIC  X(4)   VALUE 'VOUI'.                
026401     03 WS-IDSYSTEM-TAD          PIC  X(4)   VALUE 'TAD '.                
026501     03 WS-IDSYSTEM-ACC          PIC  X(4)   VALUE 'ACC '.                
026601     03 WS-IDSYSTEM-APA          PIC  X(4)   VALUE 'APA '.                
026701     03 WS-IDSYSTEM-APB          PIC  X(4)   VALUE 'APB '.                
026801     03 WS-IDSYSTEM-APC          PIC  X(4)   VALUE 'APC '.                
026901     03 WS-IDSYSTEM-APD          PIC  X(4)   VALUE 'APD '.                
027001     03 WS-IDSYSTEM-APE          PIC  X(4)   VALUE 'APE '.                
027101     03 WS-IDSYSTEM-APF          PIC  X(4)   VALUE 'APF '.                
027201     03 WS-IDSYSTEM-APG          PIC  X(4)   VALUE 'APG '.                
027301     03 WS-IDSYSTEM-APH          PIC  X(4)   VALUE 'APH '.                
027401     03 WS-IDSYSTEM-API          PIC  X(4)   VALUE 'API '.                
027501     03 WS-IDSYSTEM-APJ          PIC  X(4)   VALUE 'APJ '.                
027601     03 IDDC-VARIABLES.                                                   
027701        05 WS-IDDC-RECV          PIC X(2)    VALUE SPACES.                
027801        05 WS-IDDC-SEND          PIC X(2)    VALUE SPACES.                
027802     03 WS-NUM-7                 PIC 9(7)    VALUE ZERO.                  
027803     03 MID-PART-IX              PIC 9(5)    VALUE ZERO.                  
027804     03 W-BLANKS                 PIC  9(5)   VALUE ZERO.                  
027805     03 W-LENGTH                 PIC  9(5)   VALUE ZERO.                  
027806*    03 WC-CDC-SE                PIC X(2)    VALUE '11'.                  
027807     03 IX-DCCLEAR-MAX           PIC S9(3)   COMP SYNC VALUE +99.         
027808     03 W-GMT-IDDC-CLEAR-GRP.                                             
027809*                                      GRUPP AV IDDC-CLEAR                
027810        05 W-GMT-IDDC-CLEAR   OCCURS 99 TIMES                             
027811                                      PIC X(2)    VALUE SPACE.            
027820     03 W-IDARTNR-CHAR.                                                   
027830       05  W-IDARTNR-NUM             PIC  9(8)   VALUE ZERO.              
027880     03  WS-IDARTNR.                                                      
027890         05  WS-IDARTNR-1-9          PIC X(9).                            
027900         05  WS-IDARTNR-10           PIC X(1) VALUE '-'.                  
027901         05  WS-IDARTNR-11           PIC X(1).                            
027902     03  WS-IDDC-SEEK                PIC X(2)    VALUE SPACE.             
027903     03  WS-KDORDSTA-O               PIC X(2)    VALUE SPACE.             
027906     03  WS-ORAD-KVBEART-Q           PIC S9(7)   VALUE +0  COMP-3.        
027907     03  WS-ORAD-KVPRERO             PIC S9(7)   VALUE +0  COMP-3.        
027908     03  WS-ORAD-KVPREAVB            PIC S9(7)   VALUE +0  COMP-3.        
027909     03  2109-INDX                   PIC S9(4) VALUE +0 COMP SYNC.        
027920     03  MAX-2109-INDX              PIC S9(4) VALUE +18 COMP SYNC.        
027930     03 WS-UPD-REQUEST.                                                   
027940        05 WS-UPD-ARTNR-DATA        OCCURS 13 TIMES.                      
027950           07 WS-UPD-IDORDER       PIC S9(7)  COMP-3.                     
027960           07 WS-UPD-IDDC        PIC X(2).                                
027970           07 WS-UPD-ADLAGOMR    PIC S9(3)  COMP-3.                       
027980           07 WS-UPD-ADGANG      PIC S9(3)  COMP-3.                       
027990           07 WS-UPD-ADPLATS     PIC S9(5)  COMP-3.                       
028000           07 WS-UPD-IDARTNR     PIC S9(9)  COMP-3.                       
028001           07 WS-UPD-IDLOPNR     PIC S9(3)  COMP-3.                       
028002     03 WS-ADD-REQUEST.                                                   
028003        05 WS-ADD-IDARTNR     PIC S9(9) COMP-3 OCCURS 13 TIMES.           
028011                                                                          
028020 01 NYCKLAR-TP4TRAN.                                                      
028101     03 FILLER                   PIC X(16)   VALUE                        
028201                                             'WS-DB2-SEKTION'.            
028301     03 WS-DB2-SEKTION           PIC X(24)   VALUE SPACE.                 
028401                                                                          
028501     EJECT                                                                
028601 01 NYCKLAR-TP4TRAN.                                                      
028701     03 W-TP4TRAN-IDDISTR        PIC S9(5)   COMP-3 VALUE ZERO.           
028801                                                                          
028901 01  TEST-IDDISTR                PIC  9(5)   COMP-3.                      
029001*01  FILLER   -COPY WWDIST07    -RED TEST-IDDISTR.                        
029101     EJECT                                                                
029201*01  FILLER   -COPY WWDIST11    -RED TEST-IDDISTR.                        
029301     EJECT                                                                
029401*01  FILLER   -COPY WWDIST18    -RED TEST-IDDISTR.                        
029501     EJECT                                                                
029601*01  FILLER   -COPY WWDIST23    -RED TEST-IDDISTR.                        
029701     EJECT                                                                
029801*01  FILLER   -COPY WWDIST34    -RED TEST-IDDISTR.                        
029901     EJECT                                                                
030001*01  FILLER   -COPY WWDIST35    -RED TEST-IDDISTR.                        
030101     EJECT                                                                
030201*01  FILLER   -COPY WWDIST79    -RED TEST-IDDISTR.                        
030301*    ----DISTR-DEALER-PRICE----                                           
030401     EJECT                                                                
030501                                                                          
030502 01  FILLER            PIC X(16)   VALUE '2109-MSG-IO-AREA'.              
030503 01  W-PROG-TO-PROG-SW-2.                                                 
030504     03  2109-KVLL                PIC S9(4)  COMP SYNC.                   
030505     03  2109-Z1                   PIC X.                                 
030506     03  2109-Z2                   PIC X.                                 
030507     03  2109-TRANSKOD             PIC X(8)  VALUE 'W2T109X '.            
030508     03  2109-IDTRANS              PIC X(4)  VALUE '4258'.                
030509     03  2109-KDMFSFOR             PIC X.                                 
030510*    03  -COPY W2I10902    -PRE 2109-                                     
030520     EJECT                                                                
030530                                                                          
030601 01  TEST-ARTIKEL                PIC 9(9)    COMP-3.                      
030701*01 FILLER  -COPY WWART04       -RED  TEST-ARTIKEL.                       
030801     EJECT                                                                
030901 01  FILLER                   PIC X(16) VALUE 'REFILLTAB-DC '.            
031001*    -COPY WWDIST57                                                       
031101     EJECT                                                                
031201 01  FILLER                   PIC X(16) VALUE 'TILLKOMMANDE-TAB'.         
031301*    -COPY W411TILK                                                       
031401                                                                          
031501*   -COPY W413WHFA                                                        
031601                                                                          
031701*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
031801 01  GENERELLA-SUBPROGRAM.                                                
031901     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
032001     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
032101     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
032201     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
032301     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
032401     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
032501*                                                                         
032601*                                                                         
032701*    --- PARAMETRAR TILL GENERELLA SUBPROGRAM                             
032801 01 FILLER                       PIC X(8) VALUE 'W005INIT'.               
032901*   -COPY WMSGINIT                                                        
033001     EJECT                                                                
033101 01  FILLER                  PIC X(16)   VALUE 'WORKAREA   '.             
033201*   -COPY WORKAREA                                                        
033301     EJECT                                                                
033401*                                                                         
033501 01  GEMENSAMMA-SUBPROGRAM.                                               
033601     03  W335PRIS                PIC X(8)    VALUE 'W335PRIS'.            
033701*        PRISTILLÄMPNING                                                  
033801     03  W335PRNO                PIC X(8)    VALUE 'W335PRNO'.            
033901*        HÄMTA PRISFRÅGENR                                                
034001     03  W335PRQU                PIC X(8)    VALUE 'W335PRQU'.            
034101*        DEALER PRISFRÅGABEHANDLING                                       
034201     03  W411AREG                PIC X(8)    VALUE 'W411AREG'.            
034301*        LÄSNING ARTIKELREGISTER                                          
034401     03  W411ARTM                PIC X(8)    VALUE 'W411ARTM'.            
034501*        NYUPPLÄGG AV TOM ROT PÅ WDK9                                     
034801     03  W411DNOT                PIC X(8)    VALUE 'W411DNOT'.            
034901*        DATA TILL DEL NOTE NDC                                           
035201     03  W411KERS                PIC X(8)    VALUE 'W411KERS'.            
035301*        KONTROLL ERSÄTTNINGAR                                            
035401     03  W411KVAN                PIC X(8)    VALUE 'W411KVAN'.            
035501*        KONTROLL KVANTANPASSNING                                         
035601     03  W411LAST                PIC X(8)    VALUE 'W411LAST'.            
035701*        KONTROLL ENHETSLAST                                              
035801     03  W411ORFK                PIC X(8)    VALUE 'W411ORFK'.            
035901*        FORMELLA KONTROLLER AV INDATA                                    
036001     03  W411CDCA                PIC X(8)    VALUE 'W411CDCA'.            
036101*        KONTROLL PRELIMINÄRAVBOKNING-CDC                                 
036201     03  W411RANS                PIC X(8)    VALUE 'W411RANS'.            
036301*        BERÄKNA RANSONERING                                              
036401     03  W411NDCA                PIC X(8)    VALUE 'W411NDCA'.            
036501*        KONTROLL PRELIMINÄRAVBOKNING-NDC                                 
036601     03  W411XDCA                PIC X(8)    VALUE 'W411XDCA'.            
036701*        KONTROLL PRELIMINÄRAVBOKNING-XDC                                 
036801     03  W411SDCA                PIC X(8)    VALUE 'W411SDCA'.            
036901*        KONTROLL PRELIMINÄRAVBOKNING-SDC                                 
037001     03  W411SPAR                PIC X(8)    VALUE 'W411SPAR'.            
037101*        KONTROLL SPÄRRAR                                                 
037201     03  W411STOR                PIC X(8)    VALUE 'W411STOR'.            
037301*        KONTROLL STORA UTTAG                                             
037801     03  W411RELS                PIC X(8)    VALUE 'W411RELS'.            
037901*        KONTROLL RELS                                                    
038601     03  W411RODC                PIC X(8)    VALUE 'W411RODC'.            
038701*        BERÄKNING AV RO-DC                                               
038801     03  W411CLDC                PIC X(8)    VALUE 'W411CLDC'.            
038901*        WDB601-SEGMENT FÖR CLARING-DC                                    
039001     03  W413AVSR                PIC X(8)    VALUE 'W413AVSR'.            
039101*        WOPS RADBEHANDLING                                               
039102     03  W413AVSO                PIC X(8)    VALUE 'W413AVSO'.            
039103*        HANDLING PRC,RFS,ORDERPARTS                                      
039201     03  W413ADRS                PIC X(8)    VALUE 'W413ADRS'.            
039301*        OMVANDLING AV LAGOMR + PLATS                                     
039401     EJECT                                                                
039501                                                                          
039601 01  MESSAGE-CODES.                                                       
039602     03  OK-BEHANDLAD            PIC X(3)    VALUE '101'.                 
039603     03  ERR-INVALID-IDARTNR     PIC X(3)    VALUE '017'.                 
039604     03  ERR-ORDER-ANNULL        PIC X(3)    VALUE '052'.                 
039605     03  ERR-SAKNAS-KREG         PIC X(3)    VALUE '063'.                 
039606     03  ERR-EJ-TILLAEGG         PIC X(3)    VALUE '077'.                 
039607     03  ERR-FORM-IDDISTR-FEL    PIC X(3)    VALUE '94A'.                 
039608     03  ERR-FORM-IDORDNR-FEL    PIC X(3)    VALUE '94C'.                 
039610     03  ERR-LINES-PRINTED       PIC X(3)    VALUE '124'.                 
039611     03  ERR-INV-QUANTITY        PIC X(3)    VALUE '181'.                 
039620     03  ERR-STD-PRICE-MISSING   PIC X(3)    VALUE '301'.                 
039901     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
040303     03  ERR-INV-ORDERTYP        PIC X(3)    VALUE '442'.                 
040304     03  ERR-EJ-ARBETSTABELL     PIC X(3)    VALUE '443'.                 
040305     03  ERR-ADD-PART-FIRST      PIC X(3)    VALUE '444'.                 
040306     03  ERR-SEND-ADD-REQ        PIC X(3)    VALUE '445'.                 
040307     03  ERR-INVALID-ACTION      PIC X(3)    VALUE '446'.                 
041002     03  ERR-RFS-NEAR            PIC X(3)    VALUE '447'.                 
041003     03  ERR-REFILL-BLOCKED      PIC X(3)    VALUE '448'.                 
041004     03  ERR-DDGS-PART-BLOCKED   PIC X(3)    VALUE '449'.                 
041006     03  ERR-SS-PART-ADD-INSTEAD PIC X(3)    VALUE '450'.                 
041007     03  ERR-ORDER-NT-R-STATUS   PIC X(3)    VALUE '451'.                 
041008     03  ERR-NO-SW-PARTS         PIC X(3)    VALUE '452'.                 
041010     03  ERR-DUP-UPD-REQ         PIC X(3)    VALUE '453'.                 
041011     03  ERR-DUP-ADD-UPD-REQ     PIC X(3)    VALUE '454'.                 
041012     03  ERR-ORDER-MISSING       PIC X(3)    VALUE '701'.                 
041013     03  ERR-ORDER-AVSLUTAD      PIC X(3)    VALUE '718'.                 
041301     EJECT                                                                
041401*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
041501 01 FILLER                       PIC X(8) VALUE 'W335PRIS'.               
041601*   -COPY W335PRIS                                                        
041701     EJECT                                                                
041801 01 FILLER                       PIC X(8) VALUE 'W335PRNO'.               
041901*   -COPY W335PRNO                                                        
042001     EJECT                                                                
042101 01 FILLER                       PIC X(8) VALUE 'W335PRQU'.               
042201*   -COPY W335PRQU                                                        
042301     EJECT                                                                
042401 01 FILLER                       PIC X(8) VALUE 'W411AREG'.               
042501*   -COPY W411AREG                                                        
042601     EJECT                                                                
042701 01 FILLER                       PIC X(8) VALUE 'W411ARTM'.               
042801*   -COPY W411ARTM                                                        
042901     EJECT                                                                
043301 01 FILLER                       PIC X(8) VALUE 'W411DNOT'.               
043401*   -COPY W411DNOT                                                        
043501     EJECT                                                                
043901 01 FILLER                       PIC X(8) VALUE 'W411KERS'.               
044001*   -COPY W411KERS                                                        
044101     EJECT                                                                
044201 01 FILLER                       PIC X(8) VALUE 'W411KVAN'.               
044301*   -COPY W411KVAN                                                        
044401     EJECT                                                                
044501 01 FILLER                       PIC X(8) VALUE 'W411LAST'.               
044601*   -COPY W411LAST                                                        
044701     EJECT                                                                
044801 01 FILLER                       PIC X(8) VALUE 'W411ORFK'.               
044901*   -COPY W411ORFK                                                        
045001     EJECT                                                                
045101 01 FILLER                       PIC X(8) VALUE 'W411CDCA'.               
045201*   -COPY W411CDCA                                                        
045301     EJECT                                                                
045401 01 FILLER                       PIC X(8) VALUE 'W411RANS'.               
045501*   -COPY W411RANS                                                        
045601     EJECT                                                                
045701 01 FILLER                       PIC X(8) VALUE 'W411NDCA'.               
045801*   -COPY W411NDCA                                                        
045901     EJECT                                                                
046001 01 FILLER                       PIC X(8) VALUE 'W411XDCA'.               
046101*   -COPY W411XDCA                                                        
046201     EJECT                                                                
046301 01 FILLER                       PIC X(8) VALUE 'W411XDK7'.               
046401*   -COPY W411XDK7 -PRE NDCA-                                             
046501     EJECT                                                                
046601 01 FILLER                       PIC X(8) VALUE 'W411SDCA'.               
046701*   -COPY W411SDCA                                                        
046801     EJECT                                                                
046901 01 FILLER                       PIC X(8) VALUE 'W411SPAR'.               
047001*   -COPY W411SPAR                                                        
047101     EJECT                                                                
047201 01 FILLER                       PIC X(8) VALUE 'W411STOR'.               
047301*   -COPY W411STOR                                                        
047401     EJECT                                                                
048101 01 FILLER                       PIC X(8) VALUE 'W411RELS'.               
048201*   -COPY W411RELS                                                        
048301     EJECT                                                                
049301 01 FILLER                       PIC X(8) VALUE 'W411RODC'.               
049401*   -COPY W411RODC                                                        
049501     EJECT                                                                
049601 01 FILLER                       PIC X(8) VALUE 'W411CLDC'.               
049701*   -COPY W411CLDC                                                        
049801     EJECT                                                                
049901 01 FILLER                       PIC X(8) VALUE 'W413AVSR'.               
050001*   -COPY W413AVSR                                                        
050101     SKIP2                                                                
050102 01 FILLER                       PIC X(8) VALUE 'W413AVSO'.               
050103*   -COPY W413AVSO                                                        
050104     SKIP2                                                                
050201 01 FILLER                       PIC X(8) VALUE 'W413ADRS'.               
050301*   -COPY W413ADRS                                                        
050401     EJECT                                                                
050501 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
050601     SKIP3                                                                
050701 01  MID-AREA.                                                            
050801*03  MID -COPY W4I25801                                                   
050901     EJECT                                                                
051001 01  SPAR-MID-AREA.                                                       
051101*03  MID -COPY W4I25801   -PRE SPAR-                                      
051201     EJECT                                                                
051301 01  FILLER                      PIC X(16)  VALUE 'MSG-IO-AREA'.          
051401     SKIP3                                                                
051501*01  -COPY WMSGAREA                                                       
051601     EJECT                                                                
051701*    05  -COPY W4I29801  -PRE 4298-  -RED MSG-MID-OUT                     
051801     EJECT                                                                
051901*    05  -COPY W4I29901  -PRE 4299-  -RED MSG-MID-OUT                     
052001     EJECT                                                                
052101*    05  -COPY W4I25701  -PRE 4257-  -RED MSG-MID-OUT                     
052201     EJECT                                                                
052301******************************************************************        
052401*    MID-AREA FÖR W2T191                                         *        
052501******************************************************************        
052601*01  -COPY  W2I19101  -PRE 2191-                                          
052701     EJECT                                                                
052801 01  FILLER                      PIC X(16)  VALUE 'KOM-IO-AREA'.          
052901     SKIP3                                                                
053001 01  KOM-IO-AREA.                                                         
053101*03  -COPY WMSGKOM                                                        
053201     EJECT                                                                
053301*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
053401*                                                                         
053501 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
053601     SKIP3                                                                
053701 01  NYCKLAR-TILL-DLI.                                                    
053801                                                                          
054302     03  W-WDQ2CSEQ-X.                                                    
054303         05  W-WDQ2C-IDDISTR      PIC S9(5)   COMP-3 VALUE +0.            
054304         05  W-WDQ2C-IDKUNDNR     PIC S9(7)   COMP-3 VALUE +0.            
054305         05  W-WDQ2C-IDKUNDRF     PIC X(10)   VALUE SPACE.                
054306                                                                          
054401     03  W-IDORDER-X.                                                     
054501         05  W-IDORDER           PIC S9(7)   VALUE ZERO COMP-3.           
054601                                                                          
054701     03  W-IDARTNR-X.                                                     
054801         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
054901                                                                          
055001     03  W-IDARTNR-T-X.                                                   
055101         05  W-IDARTNR-T         PIC S9(9)   VALUE ZERO COMP-3.           
055201                                                                          
055301     03  W-IDDC-WDQ212-X.                                                 
055401         05  W-IDDC-WDQ212       PIC X(2)    VALUE SPACE.                 
055501                                                                          
055502     03  W-IDDC-WDQ221-X.                                                 
055503         05  W-IDDC-WDQ221       PIC X(2)    VALUE SPACE.                 
055504                                                                          
055505     03  W-IDDC-X.                                                        
055506         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
055507                                                                          
055601     03  W-IDDC-B6-X.                                                     
055701         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
055801                                                                          
055901     03  W-IDDC-REF-B6-X.                                                 
056001         05  W-IDDC-REF-B6       PIC X(2)    VALUE SPACE.                 
056101                                                                          
056201     03  W-KDSEGKEY-X.                                                    
056301         05  W-KDSEGKEY          PIC X(01)   VALUE '1'.                   
056401                                                                          
056501     03  W-IDDC-T-X.                                                      
056601         05  W-IDDC-T            PIC X(2)    VALUE SPACE.                 
056701                                                                          
056801     03  W-WDGXKEY-4541-X.                                                
056901         05  W-IDHTYP            PIC X(04)   VALUE '4541'.                
057001         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
057101                                                                          
057201     03  W-WDQ101KY-MIN-X.                                                
057301         05  W-IDORDER-Q1-MIN    PIC S9(7)   COMP-3.                      
057401         05  W-IDARTNR-Q1-MIN    PIC S9(9)   COMP-3.                      
057501         05  W-IDLOPNR-Q1-MIN    PIC S9(3)   COMP-3.                      
057601         05  W-IDSEKVNR-Q1-MIN   PIC S9(3)   COMP-3.                      
057701         05  FILLER              PIC X(04)   VALUE LOW-VALUE.             
057801                                                                          
057901     03  W-WDQ101KY-MAX-X.                                                
058001         05  W-IDORDER-Q1-MAX    PIC S9(7)   COMP-3.                      
058101         05  W-IDARTNR-Q1-MAX    PIC S9(9)   COMP-3.                      
058201         05  W-IDLOPNR-Q1-MAX    PIC S9(3)   COMP-3.                      
058301         05  W-IDSEKVNR-Q1-MAX   PIC S9(3)   COMP-3.                      
058401         05  FILLER              PIC X(04)   VALUE HIGH-VALUE.            
058501                                                                          
058601     03  W-IDGMT-X.                                                       
058701         05  W-IDDISTR-WDB2      PIC S9(5) VALUE ZERO COMP-3.             
058801         05  W-IDKUNDNR-WDB2     PIC S9(7) VALUE ZERO COMP-3.             
058901*                                                                         
059001     03  W-IDGMT-MIN-X.                                                   
059101         05  W-IDDISTR-WDB2-MIN  PIC S9(5) VALUE ZERO COMP-3.             
059201         05  W-IDKUNDNR-WDB2-MIN PIC S9(7) VALUE ZERO COMP-3.             
059301*                                                                         
059401     03  W-IDGMT-MAX-X.                                                   
059501         05  W-IDDISTR-WDB2-MAX  PIC S9(5) VALUE ZERO COMP-3.             
059601         05  W-IDKUNDNR-WDB2-MAX PIC S9(7) VALUE ZERO COMP-3.             
059701                                                                          
059801     03  W-WDB101KY-X.                                                    
059901         05  W-WDB1-IDPARTNR     PIC X(9)    VALUE SPACE.                 
060001         05  W-WDB1-IDFTG        PIC 9(2)    VALUE ZERO.                  
060101                                                                          
060102     03  W-WDQ4A1KY-MIN-X.                                                
060103         05  W-Q4SEQA-IDORDER-MIN PIC S9(7)    VALUE ZERO COMP-3.         
060104         05  W-Q4SEQA-IDARTNR-MIN PIC S9(9)    VALUE ZERO COMP-3.         
060105         05  FILLER               PIC X(11)    VALUE LOW-VALUE.           
060106     03  W-WDQ4A1KY-MAX-X.                                                
060107         05  W-Q4SEQA-IDORDER-MAX PIC S9(7)    VALUE ZERO COMP-3.         
060108         05  W-Q4SEQA-IDARTNR-MAX PIC S9(9)    VALUE ZERO COMP-3.         
060109         05  FILLER               PIC X(11)    VALUE HIGH-VALUE.          
060118     03  W-WDQ401KY-X.                                                    
060120         05  W-Q401-IDORDER     PIC S9(7)      VALUE ZERO COMP-3.         
060130         05  W-Q401-IDDC        PIC X(2)       VALUE SPACE.               
060140         05  W-Q401-ADLAGOMR    PIC S9(3)      VALUE ZERO COMP-3.         
060150         05  W-Q401-ADGANG      PIC S9(3)      VALUE ZERO COMP-3.         
060160         05  W-Q401-ADPLATS     PIC S9(5)      VALUE ZERO COMP-3.         
060170         05  W-Q401-IDARTNR     PIC S9(9)      VALUE ZERO COMP-3.         
060180         05  W-Q401-IDLOPNR     PIC S9(3)      VALUE ZERO COMP-3.         
060190                                                                          
060501     03  W-4511-IDHTYP-X.                                                 
060601         05  W-4511-IDHTYP       PIC  X(04) VALUE '4511'.                 
060701         05  W-LOW-VALUE         PIC  X(26) VALUE LOW-VALUE.              
060801                                                                          
060901     03  W-4512-KDTPOTYP-X.                                               
061001         05  W-4512-KDTPOTYP     PIC  S9(1) COMP-3.                       
061101                                                                          
061201     03  W-4512-KDORDKL-X.                                                
061301         05  W-4512-KDORDKL      PIC  S9(1) COMP-3.                       
061401                                                                          
061501     03  W-4512-IDDISTR-FOM-X.                                            
061601         05  W-4512-IDDISTR-FOM  PIC  S9(5) COMP-3.                       
061701                                                                          
061801     03  W-4512-IDDISTR-TOM-X.                                            
061901         05  W-4512-IDDISTR-TOM  PIC  S9(5) COMP-3.                       
062001*                                                                         
062101     03  W-WDB301KY-X.                                                    
062201         05  W-IDDC-WDB3          PIC X(2)    VALUE SPACE.                
062301         05  W-IDDISTR-WDB3       PIC S9(5)   COMP-3 VALUE ZERO.          
062401         05  W-IDKUNDNR-WDB3      PIC S9(7)   COMP-3 VALUE ZERO.          
062501*                                                                         
062601     03  W-WDB301KY-DEF-X.                                                
062701         05  W-IDDC-WDB3-DEF      PIC X(2)    VALUE SPACE.                
062801         05  W-IDDISTR-WDB3-DEF   PIC S9(5)   COMP-3 VALUE ZERO.          
062901         05  W-IDKUNDNR-WDB3-DEF  PIC S9(7) VALUE +9999999 COMP-3.        
063001*                                                                         
063501*    --- STATUS-KOD FRÅN IMS                                              
063601 01  STATUS-WS                   PIC XX.                                  
063701     88  SEGMENT-FINNS                       VALUE '  '.                  
063801     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
063901     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
064001     88  BASEN-SLUT                          VALUE 'GB'.                  
064101     SKIP2                                                                
064201 01  GODK-STATUSKODER.                                                    
064301     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
064401                                                                          
064501 01  ALL-SSA.                                                             
064601     03  SSA1                        PIC X(160).                          
064701     03  SSA2                        PIC X(96).                           
064801     03  SSA3                        PIC X(64).                           
064901     EJECT                                                                
065001*                            DB2 FUNKTIONSKODER                           
065101 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
065201       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
065301                                                                          
065401 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
065501 01  DB2-WS.                                                              
065601     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
065701         88  CURSOR-OK                       VALUE 000.                   
065801         88  RADER-FINNS                     VALUE 000.                   
065901         88  RADER-SAKNAS                    VALUE 100.                   
066001         88  ATKOMST-FEL                     VALUE 904.                   
066101     03  GODK-SQLCODEKODER.                                               
066201         05  GODK-SQLCODE OCCURS 5                                        
066301             INDEXED BY SQLCODE-IX PIC 9(3).                              
066401 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
066501     EJECT                                                                
066601*    --- IMS FUNKTIONSKODER                                               
066701*01  -COPY W0003                                                          
066801     EJECT                                                                
066901*    ---  DLI INPUT-OUTPUT AREA                                           
067001 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
067101     EJECT                                                                
067201 01  FILLER                      PIC X(16)   VALUE 'WDQ101-AREA'.         
067301 01  DLI-IO-AREA-WDQ1.                                                    
067401*    03 -COPY WDQ101                                                      
067601     EJECT                                                                
067701 01  FILLER                      PIC X(16)   VALUE 'WDQ201-AREA'.         
067801 01  DLI-IO-AREA-WDQ2.                                                    
067901     03  WLORQI01.                                                        
068001*        05  -COPY WDQ201                                                 
068101     EJECT                                                                
068201 01  FILLER                      PIC X(16)   VALUE 'WDQ201-EMPTY'.        
068301 01  DLI-IO-AREA-WDQ2-EMPTY.                                              
068401     03  WLORQI01.                                                        
068501*        05  -COPY WDQ201 -PRE EMPTY-                                     
068601     EJECT                                                                
068701 01  FILLER                      PIC X(16)   VALUE 'WDQ212-AREA'.         
068801 01  DLI-IO-AREA-ARB.                                                     
068901     03  WLORQI12.                                                        
069001*        05  -COPY WDQ212                                                 
069101     EJECT                                                                
069102 01  FILLER                      PIC X(16)   VALUE 'WDQ211-AREA'.         
069103 01  DLI-IO-AREA-DIRL.                                                    
069104     03  WLORQI11.                                                        
069105*        05  -COPY WDQ211                                                 
069106     EJECT                                                                
069107 01  FILLER                      PIC X(16)   VALUE 'WDQ221-AREA'.         
069108 01  DLI-IO-AREA-LOR.                                                     
069109     03  WLORQI21.                                                        
069110*        05  -COPY WDQ221                                                 
069120     EJECT                                                                
069602 01  FILLER                      PIC X(16)   VALUE 'IO-WDQ401  '.         
069603                                                                          
069604 01  DLI-IO-AREA-WDQ4.                                                    
069605*    03  -COPY WDQ401                                                     
069607     EJECT                                                                
069608 01  FILLER                      PIC X(16)   VALUE 'IO-WDQ4A1  '.         
069609                                                                          
069610 01  DLI-IO-AREA-WDQ4A.                                                   
069620*    03  -COPY WDQ4A1                                                     
069640     EJECT                                                                
069650                                                                          
069701 01  FILLER                      PIC X(16)   VALUE 'WDK901-AREA'.         
070002 01  DLI-IO-WDK901.                                                       
070003*    03  -COPY WDK901                                                     
070101     EJECT                                                                
070201 01  FILLER                      PIC X(16)   VALUE '454111-AREA'.         
070301 01  DLI-IO-AREA-4541.                                                    
070401     03  WL454111.                                                        
070501*        05  -COPY WDGX4542                                               
070601     EJECT                                                                
070701 01  FILLER                      PIC X(16)   VALUE 'WDK711-AREA'.         
070801 01  DLI-IO-AREA-WDK7.                                                    
070901     03  WDK711.                                                          
071001*        05  -COPY WDK711                                                 
071101 01  FILLER                      PIC X(16)   VALUE 'WDK722-AREA'.         
071201 01  DLI-IO-WDK722.                                                       
071301     03  WDK722.                                                          
071401*        05  -COPY WDK722                                                 
071501     EJECT                                                                
071601 01  FILLER                      PIC X(16)   VALUE 'WDK601-AREA'.         
071701 01  DLI-IO-WDK601.                                                       
071801*    03  -COPY WDK601                                                     
071901     EJECT                                                                
071902 01  FILLER                      PIC X(16)   VALUE 'WDK611-AREA'.         
071903 01  DLI-IO-WDK611.                                                       
071904*    03  -COPY WDK611                                                     
071905     EJECT                                                                
072001 01  FILLER                      PIC X(16)   VALUE 'WDK611-T-AR'.         
072101 01  DLI-IO-WDK611-T.                                                     
072201*    03  -COPY WDK611 -PRE TILK-                                          
072301     EJECT                                                                
072401 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDB201'.          
072501 01  DLI-IO-WDB201.                                                       
072601*    03  -COPY WDB201                                                     
072801     EJECT                                                                
072901 01  FILLER                      PIC X(16)   VALUE 'WDB101-AREA'.         
073001 01  DLI-IO-AREA-WDB101.                                                  
073101     03  WLBETC01.                                                        
073201         05  -COPY WDB101                                                 
073301                                                                          
073401 01  FILLER                      PIC X(16)   VALUE 'WDB601 AREA'.         
073501 01   DLI-IO-WDB601.                                                      
073601*     03  -COPY WDB601                                                    
073701                                                                          
073801 01  FILLER                   PIC X(16) VALUE 'DLI-IO-WDA5  '.            
073901 01  DLI-IO-WDA501.                                                       
074001*    03  WDA5 -COPY WDA501 -PRE WDA5-                                     
074101     EJECT                                                                
074201                                                                          
074301 01  FILLER                   PIC X(16)   VALUE 'WDB301 AREA'.            
074401 01   DLI-IO-AREA-WDB301.                                                 
074501*     03  -COPY WDB301                                                    
074601     EJECT                                                                
074701 01  FILLER                   PIC X(16)   VALUE '4251 AREA'.              
074801 01  DLI-IO-4251.                                                         
074901*    03  -COPY WDGX4251                                                   
075001     EJECT                                                                
075101 01  FILLER                   PIC X(16)   VALUE '4252 AREA'.              
075201 01  DLI-IO-4252.                                                         
075301*    03  -COPY WDGX4252                                                   
075401     EJECT                                                                
075501 01  FILLER                    PIC X(11)  VALUE 'IO-AREA-SUB'.            
075601 01  DLI-IO-AREA-SUB.                                                     
075701     03  WLXXJN11.                                                        
075801*        05  -COPY WDGX4512                                               
075901 01  FILLER                    PIC X(16)   VALUE 'SEND-CONTROL'.          
076001     SKIP3                                                                
076101 01  WS-IDCOM           PIC S9(9)  VALUE ZERO  COMP-3.                    
076201 01  -COPY WZ01SEND                                                       
076301     EJECT                                                                
076401 01  FILLER                    PIC X(16)   VALUE 'SEND-AREA'.             
076501     SKIP3                                                                
076601 01  SEND-AREA.                                                           
076701*    03  -COPY WZ01REQU  -PRE 3039-                                       
076801*    03  -COPY W30391I1  -PRE 3039-                                       
076901     EJECT                                                                
077001 01  FILLER                      PIC X(16)  VALUE 'TP4TRAN-AREA'.         
077101                                                                          
077201*01  -COPY TP4TRAN -PRE TP4TRAN-                                          
077301     EJECT                                                                
077401     EXEC SQL INCLUDE TP4TRAN END-EXEC.                                   
077501     EJECT                                                                
077601 01  DLI-IO-AREA2.                                                        
077701     03  IO-AREA2                PIC X(300)  VALUE SPACE.                 
077801     SKIP3                                                                
077901*    03  WLFILA01     -COPY WDR601           -RED IO-AREA2.               
078001*    07  W414203A     -COPY W414203A         -RED FIL-WDR601-DATA.        
078101                                                                          
078201 01  FILLER               PIC X(16)   VALUE 'WDR601 AREA'.                
078301 01   DLI-IO-AREA-R601.                                                   
078401*     03  -COPY WDR601 -PRE R6-                                           
078501*     05  -COPY W414XDCA      -RED R6-FIL-WDR601-DATA                     
078601     EJECT                                                                
078701 LINKAGE SECTION.                                                         
078801                                                                          
078901*01  -COPY W0009   -PRE MSG-                                              
079001                                                                          
079101*01  -COPY W0009   -PRE DISP-                                             
079201     EJECT                                                                
079301*01  -COPY W0009   -PRE 4298-                                             
079401     EJECT                                                                
079501*01  -COPY W0009   -PRE 4299-                                             
079601     EJECT                                                                
079602*01  -COPY W0009   -PRE 2109-                                             
079603     EJECT                                                                
079701*01  -COPY W0009   -PRE 2191-                                             
079801     EJECT                                                                
079901*01  -COPY W0009   -PRE PRQRY-                                            
080001     SKIP2                                                                
080002*01  -COPY W0009   -PRE DLEV-                                             
080003     SKIP2                                                                
080301*01  -COPY W0008   -PRE USEA-                                             
080401     05  FILLER                  PIC X.                                   
080501     SKIP2                                                                
080502*01  -COPY W0008   -PRE WDQ4-                                             
080503     05  FILLER                  PIC X.                                   
080504     SKIP2                                                                
080805*01  -COPY W0008   -PRE WDQ4A-                                            
080806     05  FILLER                  PIC X.                                   
080807     EJECT                                                                
080901*01  -COPY W0008   -PRE WDQ2-                                             
081001     05  FILLER                  PIC X.                                   
081101     EJECT                                                                
081201*01  -COPY W0008   -PRE WDQ1-                                             
081301     05  FILLER                  PIC X.                                   
081401     SKIP2                                                                
081501*01  -COPY W0008   -PRE WDK9-                                             
081601     05  FILLER                  PIC X.                                   
081701     EJECT                                                                
081801*01  -COPY W0008   -PRE 4541-                                             
081901     05  FILLER                  PIC X.                                   
082001     EJECT                                                                
082101*01  -COPY W0008   -PRE WDK7-                                             
082201     05  FILLER                  PIC X.                                   
082301     EJECT                                                                
082401*01  -COPY W0008   -PRE WDK6-                                             
082501     05  FILLER                  PIC X.                                   
082601     EJECT                                                                
082701*01  -COPY W0008   -PRE WDB2-                                             
082801     05  FILLER                  PIC X.                                   
082901     EJECT                                                                
083001*01  -COPY W0008   -PRE WDB1-                                             
083101     05  FILLER                  PIC X.                                   
083201     EJECT                                                                
083301*01  -COPY W0008   -PRE WDB6-                                             
083401     05  FILLER                  PIC X.                                   
083501     EJECT                                                                
083601*01  -COPY W0008   -PRE WDA5-                                             
083701     05  FILLER                  PIC X.                                   
083801     EJECT                                                                
083901*01  -COPY W0008   -PRE XXJN-                                             
084001     05  FILLER                  PIC X.                                   
084101     EJECT                                                                
084201*01  -COPY W0008   -PRE FILA-                                             
084301     05  FILLER                  PIC X.                                   
084401     EJECT                                                                
084501*01  -COPY W0008   -PRE WDB3-                                             
084601     05  FILLER                  PIC X.                                   
084701     EJECT                                                                
084801*01  -COPY W0008   -PRE 4251-                                             
084901     05  FILLER                  PIC X.                                   
085001     EJECT                                                                
085101*01  -COPY W0008   -PRE WDR6-                                             
085201     05  FILLER                  PIC X.                                   
085301     EJECT                                                                
085801 01  PRIS-ARTC-PCB               PIC X.                                   
085901 01  PRIS-WDK7-PCB               PIC X.                                   
086001 01  PRIS-GMTA-PCB               PIC X.                                   
086101 01  PRIS-BETA-PCB               PIC X.                                   
086201 01  PRIS-GPRIA-PCB              PIC X.                                   
086301 01  PRIS-GPRIB-PCB              PIC X.                                   
086401 01  PRIS-COST-WDK6-PCB          PIC X.                                   
086501 01  PRIS-COST-WDK7-PCB          PIC X.                                   
086601 01  PRIS-COST-WDF1-PCB          PIC X.                                   
086701 01  PRIS-COST-9305-PCB          PIC X.                                   
086801 01  PRIS-COST-WDK72-PCB         PIC X.                                   
086901 01  PRIS-COST-WDB6-PCB          PIC X.                                   
087001                                                                          
087101 01  PRNO-3107-PCB               PIC X.                                   
087201 01  PRQU-WDC7-PCB               PIC X.                                   
087301 01  PRQU-WDG2-PCB               PIC X.                                   
087401 01  PRQU-SJKO-WDK6-PCB          PIC X.                                   
087501                                                                          
087601 01  AREG-WDK6-PCB               PIC X.                                   
087701 01  AREG-WDK7-PCB               PIC X.                                   
087801                                                                          
087901 01  ARTM-ARTM-PCB               PIC X.                                   
088001                                                                          
088701 01  SPAR-WDF8-PCB               PIC X.                                   
088801 01  SPAR-WDF8A-PCB              PIC X.                                   
088901 01  SPAR-WDK6-PCB               PIC X.                                   
089001                                                                          
089101 01  DNOT-ORQP-PCB               PIC X.                                   
089201 01  DNOT-ORQP2-PCB              PIC X.                                   
089301 01  DNOT-ORQP3-PCB              PIC X.                                   
089401 01  DNOT-4013-PCB               PIC X.                                   
089501 01  DNOT-BENA-PCB               PIC X.                                   
089601                                                                          
090101 01  KERS-ARTC-PCB               PIC X.                                   
090201 01  KERS-ERSA-PCB               PIC X.                                   
090301                                                                          
090401 01  NDCA-USEA-PCB               PIC X.                                   
090501 01  NDCA-WDK7-PCB               PIC X.                                   
090601 01  NDCA-WDL6-PCB               PIC X.                                   
090701 01  NDCA-WDB6-PCB               PIC X.                                   
090801                                                                          
090901 01  RODC-WDK7-PCB               PIC X.                                   
091001                                                                          
091101 01  SDCA-ARTS-PCB               PIC X.                                   
091201 01  SDCA-WDB6-PCB               PIC X.                                   
091301 01  SDCA-WDK9-PCB               PIC X.                                   
091401 01  SDCA-WDR6-PCB               PIC X.                                   
091501 01  SDCA-WDK6-PCB               PIC X.                                   
091601 01  SDCA-WDQ4B-PCB              PIC X.                                   
091701 01  SDCA-WDQ2-PCB               PIC X.                                   
091801 01  SDCA-WDQ4-PCB               PIC X.                                   
091901 01  SDCA-WDB6-2-PCB             PIC X.                                   
092001 01  SDCA-WDK6-2-PCB             PIC X.                                   
092101 01  SDCA-WDK7-2-PCB             PIC X.                                   
092201 01  SDCA-WDK7-3-PCB             PIC X.                                   
092301                                                                          
092401 01  CDCA-ARTM-PCB               PIC X.                                   
092501 01  CDCA-INLB-PCB               PIC X.                                   
092601 01  CDCA-WDB2-PCB               PIC X.                                   
092701 01  CDCA-WDC1-PCB               PIC X.                                   
092801                                                                          
092901 01  RANS-XXKM-PCB               PIC X.                                   
093001 01  RANS-ARTM-PCB               PIC X.                                   
093101 01  RANS-ARTS-PCB               PIC X.                                   
093201                                                                          
094401 01  RELS-ORDP-PCB               PIC X.                                   
094501 01  RELS-FILA-PCB               PIC X.                                   
094601 01  RELS-ARTM-PCB               PIC X.                                   
094701                                                                          
096101 01  TIME-4437-PCB               PIC X.                                   
096201                                                                          
096301 01  AVSR-LIST-PCB               PIC X.                                   
096401 01  AVSR-ORQI-PCB               PIC X.                                   
096501 01  AVSR-GMTB-PCB               PIC X.                                   
096601 01  AVSR-GMTC-PCB               PIC X.                                   
096701 01  AVSR-WDB2-PCB               PIC X.                                   
096801 01  AVSR-WDB6-PCB               PIC X.                                   
096901                                                                          
097101 01  KVAN-WDB2-PCB               PIC X.                                   
097201 01  KVAN-WDC1-PCB               PIC X.                                   
097301                                                                          
097401 01  XDCA-USEA-PCB               PIC X.                                   
097501 01  XDCA-WDB6-PCB               PIC X.                                   
097601 01  XDCA-WDK6-PCB               PIC X.                                   
097701 01  XDCA-WDK7-PCB               PIC X.                                   
097801 01  XDCA-WDK9-PCB               PIC X.                                   
097901 01  XDCA-WDL6-PCB               PIC X.                                   
098001 01  XDCA-WDQ4B-PCB              PIC X.                                   
098101 01  XDCA-WDQ2-PCB               PIC X.                                   
098201 01  XDCA-WDQ4-PCB               PIC X.                                   
098301 01  XDCA-WDR6-PCB               PIC X.                                   
098401 01  XDCA-WDB6-2-PCB             PIC X.                                   
098501 01  XDCA-WDK6-2-PCB             PIC X.                                   
098601 01  XDCA-WDK7-2-PCB             PIC X.                                   
098701 01  XDCA-WDK7-3-PCB             PIC X.                                   
098702*----> SUBPROGRAM W413AVSO.                                               
098703 01  AVSO-WDE6-PCB               PIC X.                                   
098704 01  AVSO-ORQA-PCB               PIC X.                                   
098705 01  AVSO-WDQ2-PCB               PIC X.                                   
098706 01  AVSO-GMTB-PCB               PIC X.                                   
098707 01  AVSO-XXKA-PCB               PIC X.                                   
098708 01  AVSO-4437-PCB               PIC X.                                   
098709 01  AVSO-XXKE-PCB               PIC X.                                   
098710 01  AVSO-XXKF-PCB               PIC X.                                   
098720 01  AVSO-XXKG-PCB               PIC X.                                   
098730 01  AVSO-XXKH-PCB               PIC X.                                   
098740 01  AVSO-XXKI-PCB               PIC X.                                   
098750 01  AVSO-XXKP-PCB               PIC X.                                   
098760 01  AVSO-WDB2-PCB               PIC X.                                   
098770 01  AVSO-WDB6-PCB               PIC X.                                   
098780 01  AVSO-WDP7-PCB               PIC X.                                   
098790 01  TRAN-XXKB-PCB               PIC X.                                   
098800 01  ORDN-ORQL-PCB               PIC X.                                   
098801 01  ORDN-PROC-PCB               PIC X.                                   
098802 01  ORDN-ORQI-PCB               PIC X.                                   
098803 01  ORDN-WDQ3-PCB               PIC X.                                   
098805     EJECT                                                                
098901 PROCEDURE DIVISION  USING MSG-PCB DISP-PCB 4298-PCB                      
099001      4299-PCB AVSR-LIST-PCB 2109-PCB 2191-PCB PRQRY-PCB                  
099101      DLEV-PCB USEA-PCB WDQ4-PCB                                          
099201      WDQ4A-PCB WDQ2-PCB                                                  
099202      WDQ1-PCB WDK9-PCB 4541-PCB WDK7-PCB                                 
099301      WDK6-PCB WDB2-PCB WDB1-PCB WDB6-PCB                                 
099401      WDA5-PCB XXJN-PCB FILA-PCB WDB3-PCB 4251-PCB WDR6-PCB               
099601      PRIS-ARTC-PCB PRIS-WDK7-PCB                                         
099701      PRIS-GMTA-PCB PRIS-BETA-PCB                                         
099801      PRIS-GPRIA-PCB PRIS-GPRIB-PCB                                       
099901      PRIS-COST-WDK6-PCB                                                  
100001      PRIS-COST-WDK7-PCB                                                  
100101      PRIS-COST-WDF1-PCB                                                  
100201      PRIS-COST-9305-PCB                                                  
100301      PRIS-COST-WDK72-PCB                                                 
100401      PRIS-COST-WDB6-PCB                                                  
100501      PRNO-3107-PCB                                                       
100601      PRQU-WDG2-PCB                                                       
100701      PRQU-WDC7-PCB                                                       
100801      PRQU-SJKO-WDK6-PCB                                                  
100901      AREG-WDK6-PCB                                                       
101001      AREG-WDK7-PCB                                                       
101101      ARTM-ARTM-PCB                                                       
101701      SPAR-WDF8-PCB                                                       
101801      SPAR-WDF8A-PCB                                                      
101901      SPAR-WDK6-PCB                                                       
102001      DNOT-ORQP-PCB                                                       
102101      DNOT-ORQP2-PCB                                                      
102201      DNOT-ORQP3-PCB                                                      
102301      DNOT-4013-PCB                                                       
102401      DNOT-BENA-PCB                                                       
102601      KERS-ARTC-PCB KERS-ERSA-PCB                                         
102701      NDCA-USEA-PCB NDCA-WDK7-PCB NDCA-WDL6-PCB NDCA-WDB6-PCB             
102801      RODC-WDK7-PCB                                                       
102901      SDCA-ARTS-PCB SDCA-WDB6-PCB SDCA-WDK9-PCB SDCA-WDR6-PCB             
103001      SDCA-WDK6-PCB SDCA-WDQ4B-PCB SDCA-WDQ2-PCB SDCA-WDQ4-PCB            
103101      SDCA-WDB6-2-PCB SDCA-WDK6-2-PCB SDCA-WDK7-2-PCB                     
103201      SDCA-WDK7-3-PCB                                                     
103301      CDCA-ARTM-PCB CDCA-INLB-PCB CDCA-WDB2-PCB CDCA-WDC1-PCB             
103401      RANS-XXKM-PCB RANS-ARTM-PCB RANS-ARTS-PCB                           
103801      RELS-ORDP-PCB RELS-FILA-PCB RELS-ARTM-PCB                           
104301      TIME-4437-PCB                                                       
104401      AVSR-ORQI-PCB AVSR-GMTB-PCB AVSR-GMTC-PCB                           
104501      AVSR-WDB2-PCB AVSR-WDB6-PCB                                         
104701      KVAN-WDB2-PCB                                                       
104801      KVAN-WDC1-PCB                                                       
104901      XDCA-USEA-PCB                                                       
105001        XDCA-WDB6-PCB XDCA-WDK6-PCB XDCA-WDK7-PCB                         
105101        XDCA-WDK9-PCB XDCA-WDL6-PCB XDCA-WDQ4B-PCB                        
105201        XDCA-WDQ2-PCB XDCA-WDQ4-PCB XDCA-WDR6-PCB                         
105301        XDCA-WDB6-2-PCB XDCA-WDK6-2-PCB XDCA-WDK7-2-PCB                   
105401        XDCA-WDK7-3-PCB                                                   
105402        AVSO-WDE6-PCB AVSO-ORQA-PCB                                       
105403        AVSO-WDQ2-PCB                                                     
105404        AVSO-GMTB-PCB AVSO-XXKA-PCB                                       
105405        AVSO-4437-PCB AVSO-XXKE-PCB                                       
105406        AVSO-XXKF-PCB AVSO-XXKG-PCB                                       
105407        AVSO-XXKH-PCB AVSO-XXKI-PCB                                       
105408        AVSO-XXKP-PCB AVSO-WDB2-PCB                                       
105409        AVSO-WDB6-PCB                                                     
105410        AVSO-WDP7-PCB TRAN-XXKB-PCB                                       
105420        ORDN-ORQL-PCB ORDN-PROC-PCB                                       
105430        ORDN-ORQI-PCB ORDN-WDQ3-PCB.                                      
105501                                                                          
105601     EJECT                                                                
105701                                                                          
105801     ENTRY 'DLITCBL' USING MSG-PCB DISP-PCB 4298-PCB                      
105901      4299-PCB AVSR-LIST-PCB 2109-PCB 2191-PCB PRQRY-PCB                  
106001      DLEV-PCB USEA-PCB WDQ4-PCB                                          
106102      WDQ4A-PCB WDQ2-PCB                                                  
106103      WDQ1-PCB WDK9-PCB 4541-PCB WDK7-PCB                                 
106201      WDK6-PCB WDB2-PCB WDB1-PCB WDB6-PCB                                 
106301      WDA5-PCB XXJN-PCB FILA-PCB WDB3-PCB 4251-PCB WDR6-PCB               
106501      PRIS-ARTC-PCB PRIS-WDK7-PCB                                         
106601      PRIS-GMTA-PCB PRIS-BETA-PCB                                         
106701      PRIS-GPRIA-PCB PRIS-GPRIB-PCB                                       
106801      PRIS-COST-WDK6-PCB                                                  
106901      PRIS-COST-WDK7-PCB                                                  
107001      PRIS-COST-WDF1-PCB                                                  
107101      PRIS-COST-9305-PCB                                                  
107201      PRIS-COST-WDK72-PCB                                                 
107301      PRIS-COST-WDB6-PCB                                                  
107401      PRNO-3107-PCB                                                       
107501      PRQU-WDG2-PCB                                                       
107601      PRQU-WDC7-PCB                                                       
107701      PRQU-SJKO-WDK6-PCB                                                  
107801      AREG-WDK6-PCB                                                       
107901      AREG-WDK7-PCB                                                       
108001      ARTM-ARTM-PCB                                                       
108601      SPAR-WDF8-PCB                                                       
108701      SPAR-WDF8A-PCB                                                      
108801      SPAR-WDK6-PCB                                                       
108901      DNOT-ORQP-PCB                                                       
109001      DNOT-ORQP2-PCB                                                      
109101      DNOT-ORQP3-PCB                                                      
109201      DNOT-4013-PCB                                                       
109301      DNOT-BENA-PCB                                                       
109501      KERS-ARTC-PCB KERS-ERSA-PCB                                         
109601      NDCA-USEA-PCB NDCA-WDK7-PCB NDCA-WDL6-PCB NDCA-WDB6-PCB             
109701      RODC-WDK7-PCB                                                       
109801      SDCA-ARTS-PCB SDCA-WDB6-PCB SDCA-WDK9-PCB SDCA-WDR6-PCB             
109901      SDCA-WDK6-PCB SDCA-WDQ4B-PCB SDCA-WDQ2-PCB SDCA-WDQ4-PCB            
110001      SDCA-WDB6-2-PCB SDCA-WDK6-2-PCB SDCA-WDK7-2-PCB                     
110101      SDCA-WDK7-3-PCB                                                     
110201      CDCA-ARTM-PCB CDCA-INLB-PCB CDCA-WDB2-PCB CDCA-WDC1-PCB             
110301      RANS-XXKM-PCB RANS-ARTM-PCB RANS-ARTS-PCB                           
110701      RELS-ORDP-PCB RELS-FILA-PCB RELS-ARTM-PCB                           
111201      TIME-4437-PCB                                                       
111301      AVSR-ORQI-PCB AVSR-GMTB-PCB AVSR-GMTC-PCB                           
111401      AVSR-WDB2-PCB AVSR-WDB6-PCB                                         
111601      KVAN-WDB2-PCB                                                       
111701      KVAN-WDC1-PCB                                                       
111801      XDCA-USEA-PCB                                                       
111901        XDCA-WDB6-PCB XDCA-WDK6-PCB XDCA-WDK7-PCB                         
112001        XDCA-WDK9-PCB XDCA-WDL6-PCB XDCA-WDQ4B-PCB                        
112101        XDCA-WDQ2-PCB XDCA-WDQ4-PCB XDCA-WDR6-PCB                         
112201        XDCA-WDB6-2-PCB XDCA-WDK6-2-PCB XDCA-WDK7-2-PCB                   
112301        XDCA-WDK7-3-PCB                                                   
112302        AVSO-WDE6-PCB AVSO-ORQA-PCB                                       
112303        AVSO-WDQ2-PCB                                                     
112304        AVSO-GMTB-PCB AVSO-XXKA-PCB                                       
112305        AVSO-4437-PCB AVSO-XXKE-PCB                                       
112306        AVSO-XXKF-PCB AVSO-XXKG-PCB                                       
112307        AVSO-XXKH-PCB AVSO-XXKI-PCB                                       
112308        AVSO-XXKP-PCB AVSO-WDB2-PCB                                       
112309        AVSO-WDB6-PCB                                                     
112310        AVSO-WDP7-PCB TRAN-XXKB-PCB                                       
112320        ORDN-ORQL-PCB ORDN-PROC-PCB                                       
112330        ORDN-ORQI-PCB ORDN-WDQ3-PCB.                                      
112401     EJECT                                                                
112501                                                                          
112601     PERFORM IMS-GET-MSG                                                  
112701                                                                          
112801     IF SEGMENT-FINNS                                                     
112901        PERFORM IMS-GN-MSG                                                
113001     END-IF                                                               
113101                                                                          
113201     IF SEGMENT-FINNS                                                     
113301        PERFORM A-INIT                                                    
113401                                                                          
113501        PERFORM B-VALIDATE-INPUTS                                         
113502                                                                          
113504        IF ALLT-OK                                                        
113505           PERFORM C-ORDERLINES-CHECK-ORFK                                
113701        END-IF                                                            
113702                                                                          
113703        IF ALLT-OK                                                        
113704           PERFORM E-HANDLE-ORDERLINES                                    
113706           IF MID-FLSLUT = 'J'                                            
113707              IF DIST79-DEALER-PRICE AND WS-IDPRQUES NOT = +0             
113708                 PERFORM G-SKICKA-PRISFRAGA                               
113709              END-IF                                                      
113710              PERFORM F-BIPACKNING-ORDERAVSLUT                            
113713           ELSE                                                           
113714              IF DIST79-DEALER-PRICE AND WS-IDPRQUES NOT = +0             
113715                 PERFORM G-SKICKA-PRISFRAGA                               
113716              END-IF                                                      
113717           END-IF                                                         
113720        END-IF                                                            
113801        PERFORM Z-FINIT                                                   
113901     END-IF                                                               
114001                                                                          
114101     MOVE +0                          TO RETURN-CODE                      
114201     GOBACK                                                               
114301     .                                                                    
114401     EJECT                                                                
114501 A-INIT SECTION.                                                          
114601                                                                          
114701     MOVE JA                     TO ALLT-SW                               
114801                                                                          
114901     MOVE MSG-INDATA-MINUS-1-TRANSKOD                                     
115001                                 TO MID-AREA                              
115101     MOVE SPACE                  TO MSG-KOM-IDMFSMED                      
115201                                                                          
115301     ACCEPT DAGENS-DATUM         FROM DATE                                
115401                                                                          
115501     PERFORM AA-NOLLA-WOPS-TABELL                                         
115502                                                                          
115701     MOVE SPACE                  TO 2191-MID-W2I19101                     
115702     MOVE MID-IDSYSTEM           TO WS-IDSYSTEM                           
115801     .                                                                    
115901     EJECT                                                                
116001                                                                          
116101 AA-NOLLA-WOPS-TABELL SECTION.                                            
116201                                                                          
116301     MOVE +1                     TO WS-INDEX-WOPS                         
116401     PERFORM UNTIL WS-INDEX-WOPS > WS-INDEX-WOPS-MAX                      
116501        MOVE +0                  TO AVSR-ADLAGOMR(WS-INDEX-WOPS)          
116601        MOVE SPACE               TO AVSR-IDLEVNR(WS-INDEX-WOPS)           
116701        MOVE SPACE               TO AVSR-IDDC(WS-INDEX-WOPS)              
116801        MOVE +0                  TO AVSR-KDSPEEMB(WS-INDEX-WOPS)          
116901        MOVE +0                  TO AVSR-KVANNANT(WS-INDEX-WOPS)          
117001        MOVE +0                  TO AVSR-KVBEART-Q(WS-INDEX-WOPS)         
117101        MOVE +0                  TO AVSR-PRARTNTO(WS-INDEX-WOPS)          
117201        MOVE +0                  TO AVSR-PRAVCOST(WS-INDEX-WOPS)          
117301        INITIALIZE         AVSR-DEAL-PR-LINE(WS-INDEX-WOPS)               
117401        MOVE +0                  TO AVSR-VKART(WS-INDEX-WOPS)             
117501        MOVE +0                  TO AVSR-VLARTNTO(WS-INDEX-WOPS)          
117601        MOVE SPACE               TO AVSR-KDORDSTA(WS-INDEX-WOPS)          
117701        MOVE +0                  TO AVSR-KDVIA   (WS-INDEX-WOPS)          
117801        MOVE +0                TO AVSR-KVDAGAR-DIFF(WS-INDEX-WOPS)        
117901        MOVE +0                TO AVSR-TISKEPPN-DDC(WS-INDEX-WOPS)        
118001        MOVE +0                  TO AVSR-KDVSOP(WS-INDEX-WOPS)            
118101                                    AVSR-KDFARLIG(WS-INDEX-WOPS)          
118201        ADD +1                   TO WS-INDEX-WOPS                         
118301     END-PERFORM                                                          
118401                                                                          
118501     MOVE +1                     TO WS-INDEX-WOPS                         
118601     .                                                                    
118701     EJECT                                                                
118702                                                                          
118801 B-VALIDATE-INPUTS SECTION.                                               
118901                                                                          
119002     MOVE 'B-VALIDATE-INP'       TO   CURRENT-SECTION                     
119101                                                                          
119201     PERFORM BA-CHECK-DISTRICT-CUST                                       
119202                                                                          
119203     IF ALLT-OK                                                           
119301        PERFORM BB-VALIDATE-ORDERHEAD                                     
119302     END-IF                                                               
119303                                                                          
119304     IF ALLT-OK                                                           
119401        PERFORM BC-CHECK-ORDERLINE                                        
119403     END-IF                                                               
119601     .                                                                    
119701     EJECT                                                                
119801 BA-CHECK-DISTRICT-CUST SECTION.                                          
119901                                                                          
120002     MOVE 'BA-CHECK-DISTR'      TO   CURRENT-SECTION                      
120101                                                                          
120201     MOVE MID-IDDISTR           TO   W-IDDISTR-WDB2                       
120301                                     TEST-IDDISTR                         
120401                                     W-WDQ2C-IDDISTR                      
120501     MOVE MID-IDKUNDNR          TO   W-IDKUNDNR-WDB2                      
120601                                     W-WDQ2C-IDKUNDNR                     
120602                                                                          
120701     PERFORM IMS-GU-WDB201                                                
120702                                                                          
120801     IF SEGMENT-SAKNAS                                                    
120901        MOVE NEJ                  TO ALLT-SW                              
121001        MOVE ERR-FORM-IDDISTR-FEL TO MSG-KOM-IDMFSMED                     
121101        MOVE '4'                  TO MSG-KOM-KDSVAR                       
121201     ELSE                                                                 
121202        PERFORM BAA-VALIDATE-CUST                                         
121203        IF ALLT-OK                                                        
121204           PERFORM BAB-CHECK-REFILL                                       
121205        END-IF                                                            
121206        IF ALLT-OK                                                        
121207           PERFORM BAC-GET-LOCAL-TIME                                     
121208        END-IF                                                            
121301        IF WS-IDSYSTEM = WS-IDSYSTEM-LYNK                                 
121401           IF GMT-KDKUNDKAT NOT = '03'                                    
121601              MOVE NEJ                  TO ALLT-SW                        
121701              MOVE ERR-FORM-IDDISTR-FEL TO MSG-KOM-IDMFSMED               
121801              MOVE '4'                  TO MSG-KOM-KDSVAR                 
121901           END-IF                                                         
122001        END-IF                                                            
122101        IF WS-IDSYSTEM = WS-IDSYSTEM-POLE                                 
122201           IF GMT-KDKUNDKAT NOT = '02'                                    
122301              MOVE NEJ                  TO ALLT-SW                        
122401              MOVE ERR-FORM-IDDISTR-FEL TO MSG-KOM-IDMFSMED               
122501              MOVE '4'                  TO MSG-KOM-KDSVAR                 
122601           END-IF                                                         
122701        END-IF                                                            
122801     END-IF                                                               
122901     .                                                                    
123001     EJECT                                                                
123002 BAA-VALIDATE-CUST  SECTION.                                              
123003                                                                          
123004     MOVE 'BAA-VALIDATE- '       TO   CURRENT-SECTION                     
123005                                                                          
123006     MOVE DAGENS-DATUM           TO TMP1-YYMMDD                           
123007     MOVE GMT-TISTADAT           TO TMP2-YYMMDD                           
123008     MOVE GMT-TISTODAT           TO TMP3-YYMMDD                           
123009     PERFORM WY2000Q1                                                     
123010     IF (TMP1-YYMMDD < TMP3-YYMMDD OR TMP3-YYMMDD = +0)                   
123020     AND                                                                  
123030      ((TMP1-YYMMDD NOT < TMP2-YYMMDD) AND TMP2-YYMMDD > +0)              
123031        CONTINUE                                                          
123032     ELSE                                                                 
123034        MOVE NEJ                 TO ALLT-SW                               
123035        MOVE ERR-SAKNAS-KREG     TO MSG-KOM-IDMFSMED                      
123036        MOVE '4'                 TO MSG-KOM-KDSVAR                        
123037     END-IF                                                               
123038     .                                                                    
123039     EJECT                                                                
123040 BAB-CHECK-REFILL     SECTION.                                            
123041     MOVE 'BAB-CHECK-REFILL'     TO   CURRENT-SECTION                     
123043                                                                          
123044     IF DIST35-REFILL                                                     
123045     OR DIST35-REFILL-INOM-NDC                                            
123046     OR DIST35-NONVCC-REFILL                                              
123047         MOVE NEJ                TO ALLT-SW                               
123048         MOVE ERR-REFILL-BLOCKED TO MSG-KOM-IDMFSMED                      
123049         MOVE '4'                TO MSG-KOM-KDSVAR                        
123050     END-IF                                                               
123051     .                                                                    
123052     EJECT                                                                
123053 BAC-GET-LOCAL-TIME   SECTION.                                            
123054     MOVE 'BAC-GET-LOCAL-TI'     TO   CURRENT-SECTION                     
123055                                                                          
123056     MOVE ALL '+'                TO MSGI-WMSGINIT                         
123057     MOVE '013'                  TO MSGI-KDCALL                           
123058     MOVE 'WIDDC   '             TO MSGI-IDUSER                           
123059     MOVE GMT-IDDC-BULK(1)       TO MSGI-IDUSER(6:2)                      
123060     MOVE '4258'                 TO MSGI-IDTRANS                          
123061     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
123062                                                                          
123063     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
123064                                                                          
123065     MOVE MSGI-TILOKDAT          TO WS-LOCAL-DATE-AAMMDD                  
123066     .                                                                    
123070     EJECT                                                                
123080                                                                          
123439 BB-VALIDATE-ORDERHEAD      SECTION.                                      
123440                                                                          
123441     MOVE 'BB-VALIDATE-OR'       TO   CURRENT-SECTION                     
123442                                                                          
123501     IF MID-IDORDNR7 NUMERIC AND MID-IDORDNR7 > 0                         
123601        MOVE MID-IDORDNR7        TO WS-NUM-7                              
123701        MOVE WS-NUM-7            TO W-WDQ2C-IDKUNDRF                      
123801     ELSE                                                                 
123901        MOVE NEJ                  TO ALLT-SW                              
124001        MOVE ERR-FORM-IDORDNR-FEL TO MSG-KOM-IDMFSMED                     
124101        MOVE '4'                  TO MSG-KOM-KDSVAR                       
124201     END-IF                                                               
124203                                                                          
124301     IF ALLT-OK                                                           
124401        PERFORM IMS-GU-WDQ201-WDQ2C                                       
124501        IF SEGMENT-FINNS                                                  
124503           IF ALLT-OK                                                     
124504              PERFORM BBA-POPULATE-DC-CLEAR                               
124505           END-IF                                                         
124507           IF ALLT-OK                                                     
124508              PERFORM BBB-CHECK-ORDER-VALIDITY                            
124509           END-IF                                                         
124515           IF ALLT-OK                                                     
124516              PERFORM BBC-CHECK-PREPLANNED                                
124520           END-IF                                                         
124701        ELSE                                                              
124702           MOVE NEJ               TO ALLT-SW                              
124703           MOVE ERR-ORDER-MISSING TO MSG-KOM-IDMFSMED                     
124704           MOVE '4'               TO MSG-KOM-KDSVAR                       
124902        END-IF                                                            
125001     END-IF                                                               
125101     .                                                                    
125201     EJECT                                                                
125202                                                                          
125231 BBA-POPULATE-DC-CLEAR SECTION.                                           
125232                                                                          
125233     MOVE 'BBA-POPULATE-D'       TO   CURRENT-SECTION                     
125234                                                                          
125235     IF OHUV-KDORDKL > 1                                                  
125236                                                                          
125237        MOVE +1 TO WS-INDEX                                               
125238        PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                           
125239           MOVE GMT-IDDC-BULK(WS-INDEX)                                   
125240                                 TO  W-GMT-IDDC-CLEAR  (WS-INDEX)         
125241           ADD +1                TO WS-INDEX                              
125242        END-PERFORM                                                       
125243                                                                          
125244     ELSE                                                                 
125245       IF OHUV-KDORDKL = 1                                                
125246                                                                          
125247          MOVE +1 TO WS-INDEX                                             
125248          PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                         
125249             MOVE GMT-IDDC-DAY(WS-INDEX)                                  
125250                                 TO W-GMT-IDDC-CLEAR  (WS-INDEX)          
125254             ADD +1              TO WS-INDEX                              
125255          END-PERFORM                                                     
125256                                                                          
125257       ELSE                                                               
125258         IF OHUV-KDORDKL = 0                                              
125259                                                                          
125260            MOVE +1 TO WS-INDEX                                           
125261            PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                       
125262               MOVE GMT-IDDC-VOR(WS-INDEX)                                
125263                                 TO W-GMT-IDDC-CLEAR  (WS-INDEX)          
125264               ADD +1            TO WS-INDEX                              
125265            END-PERFORM                                                   
125266                                                                          
125267         END-IF                                                           
125268       END-IF                                                             
125269     END-IF                                                               
125270     .                                                                    
125271     EJECT                                                                
125277                                                                          
125278 BBB-CHECK-ORDER-VALIDITY   SECTION.                                      
125279                                                                          
125280     MOVE 'BBB-CHECK-ORDE'       TO   CURRENT-SECTION                     
125281                                                                          
125282     IF (OHUV-KDORDKL = 1 AND                                             
125283         GMT-FLORDTIL-KL1 = JA)                                           
125284     OR                                                                   
125285        (OHUV-KDORDKL = 2 AND                                             
125286         GMT-FLORDTIL-KL2 = JA)                                           
125287     OR                                                                   
125288        (OHUV-KDORDKL = 3 AND                                             
125289         GMT-FLORDTIL-KL3 = JA)                                           
125290     OR                                                                   
125291        (OHUV-KDORDKL = 4 AND                                             
125292         GMT-FLORDTIL-KL4 = JA)                                           
125293         MOVE NEJ                  TO ALLT-SW                             
125294         MOVE ERR-EJ-TILLAEGG      TO MSG-KOM-IDMFSMED                    
125295         MOVE '4'                  TO MSG-KOM-KDSVAR                      
125296     END-IF                                                               
125297                                                                          
125298     IF ALLT-OK                                                           
125299        IF OHUV-FLBORT = NEJ                                              
125300          IF OHUV-FLFORBI = JA         OR                                 
125301             OHUV-FLFORBI = SPEC-FORBI OR                                 
125302             OHUV-FLORDSPE = JA        OR                                 
125303             OHUV-KDTPOTYP > 0         OR OHUV-IDKAMPRF > 0  OR           
125304             OHUV-FLOVRLEV = JA        OR OHUV-KDFAKTYP = 'G'             
125306             MOVE NEJ              TO ALLT-SW                             
125307             MOVE ERR-INV-ORDERTYP TO MSG-KOM-IDMFSMED                    
125308             MOVE '4'              TO MSG-KOM-KDSVAR                      
125309          END-IF                                                          
125310        ELSE                                                              
125311           MOVE NEJ                TO ALLT-SW                             
125312           MOVE ERR-ORDER-ANNULL   TO MSG-KOM-IDMFSMED                    
125313           MOVE '4'                TO MSG-KOM-KDSVAR                      
125314        END-IF                                                            
125315     END-IF                                                               
125316                                                                          
125317     IF ALLT-OK                                                           
125318        MOVE NEJ                   TO SW-SOFTWARE-ORDER                   
125319        PERFORM IMS-GNP-WDQ2-WDQ211-FIRST                                 
125320                                                                          
125321        PERFORM UNTIL SEGMENT-SAKNAS OR                                   
125322                      SOFTWARE-ORDER                                      
125323          IF DIRL-IDDC = '11' AND DIRL-IDLEVNR = '1441'                   
125324              MOVE JA              TO SW-SOFTWARE-ORDER                   
125325          END-IF                                                          
125326          PERFORM IMS-GNP-WDQ2-WDQ211                                     
125327        END-PERFORM                                                       
125328                                                                          
125329        IF SOFTWARE-ORDER                                                 
125330           MOVE NEJ                TO ALLT-SW                             
125331           MOVE ERR-INV-ORDERTYP   TO MSG-KOM-IDMFSMED                    
125332           MOVE '4'                TO MSG-KOM-KDSVAR                      
125333        END-IF                                                            
125334     END-IF                                                               
125335                                                                          
125336     IF ALLT-OK                                                           
125337        PERFORM IMS-GNP-WDQ212                                            
125338        IF SEGMENT-FINNS                                                  
125339           MOVE ARB-TIRFS        TO WS-TIRFS                              
125341           MOVE ARB-KDORDSTA-O   TO WS-KDORDSTA-O                         
125342           MOVE JA               TO SW-KDORDSTA-O-ALL-SPACE-FLAG          
125343           PERFORM UNTIL SEGMENT-SAKNAS                                   
125344              IF ARB-KDORDSTA-O NOT = SPACE                               
125345                 MOVE NEJ        TO SW-KDORDSTA-O-ALL-SPACE-FLAG          
125346              END-IF                                                      
125347              IF ARB-KDORDSTA  NOT = SPACE                                
125348                IF (ARB-KDORDSTA     = 'E' OR 'B' OR 'C' OR 'R')          
125349                AND (ARB-KDORDSTA-O  = ' ' OR 'B' OR 'C' OR 'R')          
125350                  CONTINUE                                                
125351                ELSE                                                      
125352                  MOVE NEJ                   TO ALLT-SW                   
125353                  MOVE ERR-ORDER-NT-R-STATUS TO MSG-KOM-IDMFSMED          
125354                  MOVE '4'                   TO MSG-KOM-KDSVAR            
125355                END-IF                                                    
125356              END-IF                                                      
125357              PERFORM IMS-GNP-WDQ212                                      
125358           END-PERFORM                                                    
125359        ELSE                                                              
125360          MOVE NEJ                  TO ALLT-SW                            
125361          MOVE ERR-EJ-ARBETSTABELL  TO MSG-KOM-IDMFSMED                   
125362          MOVE '4'                  TO MSG-KOM-KDSVAR                     
125363        END-IF                                                            
125364     END-IF                                                               
125366     .                                                                    
125367     EJECT                                                                
125399*****************************************************************         
125400*CHECK FOR PREPLANNED AND RFS FALLS OUTSIDE BREAKDAYS                     
125401*****************************************************************         
125402 BBC-CHECK-PREPLANNED SECTION.                                            
125403                                                                          
125404     MOVE 'BBC-CHECK-PREP'       TO   CURRENT-SECTION                     
125405                                                                          
125406     MOVE NEJ TO PREPLANED-SW                                             
125407                                                                          
125408     IF OHUV-TIREPDAT > ZERO                                              
125409         MOVE JA                 TO PREPLANED-SW                          
125410     END-IF                                                               
125411                                                                          
125412     IF PREPLANED                                                         
125413        MOVE WC-CDC-SE           TO WORK-IDDC                             
125414        MOVE +002                TO WORK-KDCALL                           
125415        MOVE GMT-KVDAGAR-CDC     TO WORK-KVWORKD                          
125416        MOVE WS-LOCAL-DATE-AAMMDD                                         
125417                                 TO WORK-TIAAMMDD-FOM                     
125418        CALL WORKDAY             USING WORK-KDCALL                        
125419                                       WORK-DATE-AREA                     
125420                                       WORK-KDSVAR                        
125421                                                                          
125424        IF WORK-KDSVAR-FEL                                                
125425           MOVE 'SECT BBC-,DATUM SAKNAS I WORKDAY'                        
125426                                 TO    FELTEXT                            
125427           CALL ABEND            USING RKOD-ABEND-MED-DUMP                
125428        ELSE                                                              
125429           IF WS-TIRFS-DAT  <= WORK-TIAAMMDD-NEXT-WORKDAY                 
125431              MOVE NEJ           TO ALLT-SW                               
125432              MOVE ERR-RFS-NEAR  TO MSG-KOM-IDMFSMED                      
125433              MOVE '4'           TO MSG-KOM-KDSVAR                        
125434           END-IF                                                         
125435        END-IF                                                            
125436     END-IF                                                               
125440     .                                                                    
125500     EJECT                                                                
125600*****************************************************************         
125610*VALIDATION OF ORDERLINES IN ADD/UPD REQ                                  
125630*****************************************************************         
125670 BC-CHECK-ORDERLINE         SECTION.                                      
125671                                                                          
125672     MOVE 'BC-CHECK-ORDER'       TO   CURRENT-SECTION                     
125680                                                                          
125701     MOVE 1                      TO MID-PART-IX                           
125702     INITIALIZE WS-UPD-REQUEST WS-ADD-REQUEST                             
125703     MOVE +1                     TO UPD-INDX                              
125704                                    ADD-INDX                              
125801     PERFORM UNTIL NOT ALLT-OK   OR                                       
125803                   MID-PART-IX > WS-INDEX-ORFK-MAX                        
125805        IF MID-IDARTNR(MID-PART-IX) > SPACES                              
125806           PERFORM BCA-VALIDATE-ORDERLINES                                
125807        END-IF                                                            
138801        ADD +1 TO MID-PART-IX                                             
138901     END-PERFORM                                                          
138902     IF ALLT-OK                                                           
138903        PERFORM BCB-CHECK-SAME-PART-ADD-UPD                               
138904     END-IF                                                               
139001     .                                                                    
140001     EJECT                                                                
140002*****************************************************************         
140003*CHECK THE ACTION,PARTNUMBER,QUANTITY IN THE REQUEST                      
150001*****************************************************************         
150002 BCA-VALIDATE-ORDERLINES  SECTION.                                        
150003                                                                          
150004     MOVE 'BCA-VALIDATE-O'       TO   CURRENT-SECTION                     
150005                                                                          
150006     IF MID-KDBEHX(MID-PART-IX) = 'A' OR 'U'                              
150007        IF MID-KDBEHX(MID-PART-IX) = 'A'                                  
150008           SET ADD-ORDERLINE  TO TRUE                                     
150009        ELSE                                                              
150010           SET UPD-ORDERLINE  TO TRUE                                     
150011           SET UPD-ACTION     TO TRUE                                     
150012        END-IF                                                            
150013     ELSE                                                                 
150014        MOVE NEJ                TO ALLT-SW                                
150015        MOVE ERR-INVALID-ACTION TO MSG-KOM-IDMFSMED                       
150016        MOVE '4'                TO MSG-KOM-KDSVAR                         
150017     END-IF                                                               
150018                                                                          
150049     IF ALLT-OK                                                           
150050        MOVE MID-IDARTNR(MID-PART-IX) TO W-IDARTNR                        
150051        PERFORM IMS-GU-WDK601                                             
150052        IF SEGMENT-SAKNAS                                                 
150053           MOVE NEJ                  TO ALLT-SW                           
150054           MOVE ERR-INVALID-IDARTNR  TO MSG-KOM-IDMFSMED                  
150055           MOVE '4'                  TO MSG-KOM-KDSVAR                    
150056        ELSE                                                              
150057           MOVE ART-REKSIFFR     TO MID-REKSIFFR(MID-PART-IX)             
150058           IF ART-KDSORT = 'SW'                                           
150059             MOVE NEJ                TO ALLT-SW                           
150060             MOVE ERR-NO-SW-PARTS    TO MSG-KOM-IDMFSMED                  
150061             MOVE '4'                TO MSG-KOM-KDSVAR                    
150062           END-IF                                                         
150063           IF ALLT-OK                                                     
150064             PERFORM IMS-GNP-WDK611                                       
150065             IF SEGMENT-SAKNAS                                            
150066                MOVE NEJ             TO ALLT-SW                           
150067                MOVE ERR-STD-PRICE-MISSING                                
150068                                     TO MSG-KOM-IDMFSMED                  
150069                MOVE '4'             TO MSG-KOM-KDSVAR                    
150070             ELSE                                                         
150071                IF CLAG-REDIRLEV > 0                                      
150072                  MOVE NEJ                  TO ALLT-SW                    
150074                  MOVE ERR-DDGS-PART-BLOCKED                              
150075                                           TO MSG-KOM-IDMFSMED            
150076                  MOVE '4'                  TO MSG-KOM-KDSVAR             
150077                END-IF                                                    
150078                IF ALLT-OK                                                
150079                   IF CLAG-KDERS >= 01 AND UPD-ORDERLINE                  
150080                                                                          
150081                      MOVE NEJ                 TO ALLT-SW                 
150082                      MOVE ERR-SS-PART-ADD-INSTEAD                        
150083                                               TO MSG-KOM-IDMFSMED        
150084                      MOVE '4'                 TO MSG-KOM-KDSVAR          
150085                   END-IF                                                 
150086                END-IF                                                    
150087             END-IF                                                       
150088           END-IF                                                         
150089        END-IF                                                            
150090     END-IF                                                               
150094                                                                          
150095     IF ALLT-OK                                                           
150096       IF MID-KVBEART(MID-PART-IX) NOT NUMERIC OR                         
150097          MID-KVBEART(MID-PART-IX) = ZERO                                 
150098          MOVE NEJ               TO ALLT-SW                               
150099          MOVE ERR-INV-QUANTITY  TO MSG-KOM-IDMFSMED                      
150100          MOVE '4'               TO MSG-KOM-KDSVAR                        
150101       END-IF                                                             
150102     END-IF                                                               
150103                                                                          
150104     IF ALLT-OK AND UPD-INDX > 1 AND UPD-ORDERLINE                        
150105       PERFORM BCAA-CHECK-UPD-REQ-DUPLICATES                              
150106     END-IF                                                               
150107                                                                          
150108     IF ALLT-OK                                                           
150109       PERFORM BCAB-CHECK-WDQ4                                            
150110     END-IF                                                               
150111     .                                                                    
150112     EJECT                                                                
150113*****************************************************************         
150114*UPD ORDERLINE VALIDATION:USER CANNOT SEND SAME P/N IN MORE THAN          
150115*1 UPDATE REQUEST                                                         
150116*****************************************************************         
150117 BCAA-CHECK-UPD-REQ-DUPLICATES SECTION.                                   
150118                                                                          
150119     MOVE 'BCAA-CHECK-UPD-'       TO   CURRENT-SECTION                    
150120                                                                          
150121     COMPUTE SEARCH-INDX   = UPD-INDX - 1                                 
150124     PERFORM UNTIL DUP-FND OR SEARCH-INDX < 1                             
150125        IF W-IDARTNR       = WS-UPD-IDARTNR(SEARCH-INDX)                  
150126           MOVE JA               TO DUP-FND-SW                            
150127        END-IF                                                            
150128        SUBTRACT 1               FROM SEARCH-INDX                         
150129     END-PERFORM                                                          
150130                                                                          
150131     MOVE 0                      TO SEARCH-INDX                           
150132                                                                          
150133     IF DUP-FND                                                           
150135        MOVE NEJ                 TO ALLT-SW                               
150136        MOVE ERR-DUP-UPD-REQ     TO MSG-KOM-IDMFSMED                      
150137        MOVE '4'                 TO MSG-KOM-KDSVAR                        
150138     END-IF                                                               
150140     .                                                                    
150200     EJECT                                                                
576133                                                                          
576134*****************************************************************         
576135*WITH THE INPUT P/N IN ADD/UPD REQUEST, CHECK AND VALIDATE WITH           
576136*EXISTING ORDERLINES IN Q4                                                
576137*****************************************************************         
576138 BCAB-CHECK-WDQ4 SECTION.                                                 
576139                                                                          
576140     MOVE 'BCAB-CHECK-WDQ4'      TO   CURRENT-SECTION                     
576141                                                                          
576142     MOVE OHUV-IDORDER           TO W-Q4SEQA-IDORDER-MIN                  
576143                                    W-Q4SEQA-IDORDER-MAX                  
576144                                                                          
576145     MOVE W-IDARTNR              TO W-Q4SEQA-IDARTNR-MIN                  
576146                                    W-Q4SEQA-IDARTNR-MAX                  
576148     PERFORM IMS-GU-WDQ4A                                                 
576149     IF ADD-ORDERLINE                                                     
576150        ADD W-IDARTNR         TO WS-ADD-IDARTNR(ADD-INDX)                 
576151        ADD +1                TO ADD-INDX                                 
576152     ELSE                                                                 
576153        IF SEGMENT-FINNS                                                  
576154           MOVE ZEROES                 TO W-IDARTNR-CNT                   
576155           PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                     
576156              ADD +1                   TO W-IDARTNR-CNT                   
576157              MOVE SEQA-IDORDER        TO WS-UPD-IDORDER(UPD-INDX)        
576158              MOVE SEQA-IDDC           TO WS-UPD-IDDC   (UPD-INDX)        
576159              MOVE SEQA-ADLAGOMR       TO                                 
576160                                         WS-UPD-ADLAGOMR(UPD-INDX)        
576161              MOVE SEQA-ADGANG         TO WS-UPD-ADGANG (UPD-INDX)        
576162              MOVE SEQA-ADPLATS        TO WS-UPD-ADPLATS(UPD-INDX)        
576163              MOVE SEQA-IDARTNR        TO WS-UPD-IDARTNR(UPD-INDX)        
576164              MOVE SEQA-IDLOPNR        TO WS-UPD-IDLOPNR(UPD-INDX)        
576165              IF W-IDARTNR-CNT > 1                                        
576166                                                                          
576167                 MOVE NEJ              TO ALLT-SW                         
576168                 MOVE ERR-SEND-ADD-REQ TO MSG-KOM-IDMFSMED                
576170                 MOVE '4'              TO MSG-KOM-KDSVAR                  
576171              END-IF                                                      
576172              ADD +1                   TO UPD-INDX                        
576173              PERFORM IMS-GN-WDQ4A1                                       
576174           END-PERFORM                                                    
576175        ELSE                                                              
576176           IF UPD-ORDERLINE                                               
576177              MOVE NEJ                    TO ALLT-SW                      
576178              MOVE ERR-ADD-PART-FIRST     TO MSG-KOM-IDMFSMED             
576179              MOVE '4'                    TO MSG-KOM-KDSVAR               
576180           END-IF                                                         
576181        END-IF                                                            
576182     END-IF                                                               
576183                                                                          
576218     .                                                                    
576219     EJECT                                                                
576220*****************************************************************         
576221*UPD/ADD VALIDATION:USER CANNOT SEND SAME P/N IN ADD AND UPD REQ.         
576222*POSSIBLE TO SEND SAME P/N MANY TIMES IN ADD REQ.                         
576230*****************************************************************         
576300 BCB-CHECK-SAME-PART-ADD-UPD SECTION.                                     
576310                                                                          
576320     MOVE 'BCB-CHECK-SAME'       TO   CURRENT-SECTION                     
576330                                                                          
576340     MOVE +1                     TO ADD-INDX                              
576350                                                                          
576360     PERFORM UNTIL ADD-INDX > 13 OR WS-ADD-IDARTNR(ADD-INDX) = 0          
576370                                 OR DUP-FND                               
576371       MOVE WS-ADD-IDARTNR(ADD-INDX) TO W-IDARTNR                         
576372       MOVE +1                     TO UPD-INDX                            
576373       PERFORM UNTIL UPD-INDX > 13 OR WS-UPD-IDARTNR(UPD-INDX) = 0        
576374                                   OR DUP-FND                             
576376         IF W-IDARTNR = WS-UPD-IDARTNR(UPD-INDX)                          
576377            MOVE JA              TO DUP-FND-SW                            
576378         END-IF                                                           
576379         ADD +1                  TO UPD-INDX                              
576380       END-PERFORM                                                        
576381       ADD +1                    TO ADD-INDX                              
576382     END-PERFORM                                                          
576383                                                                          
576384     IF DUP-FND                                                           
576385*RC TESTED                                                                
576386        MOVE NEJ                 TO ALLT-SW                               
576387        MOVE ERR-DUP-ADD-UPD-REQ TO MSG-KOM-IDMFSMED                      
576389        MOVE '4'                 TO MSG-KOM-KDSVAR                        
576390     END-IF                                                               
576391     .                                                                    
576392     EJECT                                                                
576393 C-ORDERLINES-CHECK-ORFK SECTION.                                         
576394                                                                          
576395     MOVE 'C-ORDERLINES-C'       TO   CURRENT-SECTION                     
576396                                                                          
576397     MOVE MID-IDSYSTEM           TO ORFK-IDSYSTEM                         
576398     MOVE OHUV-IDDISTR           TO ORFK-IDDISTR                          
576399     MOVE OHUV-IDKUNDNR          TO ORFK-IDKUNDNR                         
576400     MOVE OHUV-IDUSER            TO ORFK-IDUSER                           
576401     MOVE OHUV-KDFAKTYP          TO ORFK-KDFAKTYP                         
576402     MOVE OHUV-KDORDKL           TO ORFK-KDORDKL                          
576403     MOVE OHUV-KDTPOTYP          TO ORFK-KDTPOTYP                         
576404     MOVE OHUV-FLFORBI           TO ORFK-FLFORBI                          
576405     MOVE OHUV-FLORDSPE          TO ORFK-FLORDSPE                         
576406     MOVE OHUV-FLOVRLEV          TO ORFK-FLOVRLEV                         
576407     MOVE OHUV-FLEMBORD          TO ORFK-FLEMBORD                         
576408     MOVE OHUV-IDFTG             TO ORFK-IDFTG                            
576409                                                                          
576410     MOVE +1                     TO WS-INDEX-ORFK                         
576411     PERFORM UNTIL WS-INDEX-ORFK > WS-INDEX-ORFK-MAX                      
576412        IF MID-IDARTNR (WS-INDEX-ORFK) = SPACE                            
576413           MOVE ALL '+'          TO ORFK-IDARTNR-IN(WS-INDEX-ORFK)        
576414                                    MID-IDARTNR (WS-INDEX-ORFK)           
576415        ELSE                                                              
576416           MOVE MID-IDARTNR(WS-INDEX-ORFK)                                
576417                                 TO WS-IDARTNR-1-9                        
576418           MOVE MID-REKSIFFR(WS-INDEX-ORFK)                               
576419                                 TO WS-IDARTNR-11                         
576420           MOVE WS-IDARTNR       TO ORFK-IDARTNR-IN(WS-INDEX-ORFK)        
576421           MOVE OHUV-FLRESTN     TO ORFK-FLRESTN(WS-INDEX-ORFK)           
576422           MOVE ZERO             TO ORFK-IDKONTO(WS-INDEX-ORFK)           
576423           MOVE SPACE            TO ORFK-IDKST(WS-INDEX-ORFK)             
576424           MOVE OHUV-KDVRINFO    TO ORFK-KDVRINFO(WS-INDEX-ORFK)          
576425           MOVE ALL '+'          TO ORFK-KDKVBRYT(WS-INDEX-ORFK)          
576426           MOVE NEJ              TO ORFK-FLINVEST(WS-INDEX-ORFK)          
576427           MOVE MID-KVBEART(WS-INDEX-ORFK)                                
576428                                 TO ORFK-KVBEART(WS-INDEX-ORFK)           
576429           MOVE ALL '+'          TO ORFK-PRARTNTO(WS-INDEX-ORFK)          
576430           MOVE ALL '+'          TO ORFK-TITPO-RAD(WS-INDEX-ORFK)         
576431                                                                          
576432           MOVE JA               TO ORFK-FLSLATT (WS-INDEX-ORFK)          
576433           MOVE +0               TO                                       
576434                              ORFK-PRARTNTO-LOCPREL(WS-INDEX-ORFK)        
576435           MOVE MID-PRARTNTO-LOC(WS-INDEX-ORFK)                           
576436                               TO ORFK-PRARTNTO-LOC(WS-INDEX-ORFK)        
576437           IF MID-PRARTNTO-LOC(WS-INDEX-ORFK) = SPACE                     
576438              MOVE ALL '+'     TO MID-PRARTNTO-LOC(WS-INDEX-ORFK)         
576439           END-IF                                                         
576440           MOVE MID-PRARTNTO-LOC(WS-INDEX-ORFK)                           
576441                               TO ORFK-PRARTNTO-LOC(WS-INDEX-ORFK)        
576442           MOVE ALL '+'        TO ORFK-PRARTBTO-LOC(WS-INDEX-ORFK)        
576443        END-IF                                                            
576444        ADD +1                 TO WS-INDEX-ORFK                           
576445     END-PERFORM                                                          
576446                                                                          
576447     PERFORM UNTIL WS-INDEX-ORFK > WS-INDEX-ORFK-TOT                      
576448        MOVE ALL '+'           TO ORFK-IDARTNR-IN(WS-INDEX-ORFK)          
576449        ADD +1                 TO WS-INDEX-ORFK                           
576450     END-PERFORM                                                          
576451                                                                          
576452     CALL W411ORFK             USING ORFK-W411ORFK                        
576453                                     AREG-WDK6-PCB                        
576454                                     AREG-WDK7-PCB                        
576455                                                                          
576456     MOVE +1                   TO WS-INDEX-ORFK                           
576457     PERFORM UNTIL WS-INDEX-ORFK > WS-INDEX-ORFK-MAX                      
576458        PERFORM CA-HANDLE-ORFK-ERRORS                                     
576459        ADD +1                 TO WS-INDEX-ORFK                           
576460     END-PERFORM                                                          
576461     .                                                                    
576462     EJECT                                                                
576463                                                                          
576464 CA-HANDLE-ORFK-ERRORS   SECTION.                                         
576465                                                                          
576466     MOVE 'CA-HANDLE-ORFK'       TO   CURRENT-SECTION                     
576467                                                                          
576468     IF ORFK-FLINVEST-OK(WS-INDEX-ORFK) = NEJ                             
576469        MOVE 58                  TO ORFK-KDORDBEK(WS-INDEX-ORFK)          
576470     END-IF                                                               
576471                                                                          
576472     IF ORFK-FLRESTN-OK(WS-INDEX-ORFK) = NEJ                              
576473        MOVE 58                  TO ORFK-KDORDBEK(WS-INDEX-ORFK)          
576474     END-IF                                                               
576475                                                                          
576476     IF ORFK-IDARTNR-OK(WS-INDEX-ORFK) = NEJ                              
576477        IF ORFK-IDARTNR-IN(WS-INDEX-ORFK) NOT NUMERIC                     
576478           MOVE ZERO             TO ORFK-IDARTNR(WS-INDEX-ORFK)           
576479        END-IF                                                            
576480        IF ORFK-KDORDBEK (WS-INDEX-ORFK) = ZERO                           
576481           MOVE 58               TO ORFK-KDORDBEK(WS-INDEX-ORFK)          
576482        END-IF                                                            
576483     END-IF                                                               
576484                                                                          
576485     IF ORFK-IDKONTO-OK(WS-INDEX-ORFK) = NEJ                              
576486       MOVE ZERO                 TO ORFK-IDKONTO(WS-INDEX-ORFK)           
576487     END-IF                                                               
576488     IF ORFK-IDKST-OK(WS-INDEX-ORFK) = NEJ                                
576489       MOVE SPACE               TO ORFK-IDKST(WS-INDEX-ORFK)              
576490     END-IF                                                               
576491                                                                          
576492     IF ORFK-KDKVBRYT-OK(WS-INDEX-ORFK) = NEJ                             
576493        IF ORFK-KDKVBRYT(WS-INDEX-ORFK) NOT NUMERIC                       
576494           MOVE ZERO             TO ORFK-KDKVBRYT(WS-INDEX-ORFK)          
576495        END-IF                                                            
576496        MOVE 58                  TO ORFK-KDORDBEK(WS-INDEX-ORFK)          
576497     END-IF                                                               
576498                                                                          
576499     IF ORFK-KDVRINFO-OK(WS-INDEX-ORFK) = NEJ                             
576500        IF ORFK-KDVRINFO(WS-INDEX-ORFK) NOT NUMERIC                       
576501           MOVE ZERO             TO ORFK-KDVRINFO(WS-INDEX-ORFK)          
576502        END-IF                                                            
576503        MOVE 58                  TO ORFK-KDORDBEK(WS-INDEX-ORFK)          
576504     END-IF                                                               
576505                                                                          
576506     IF ORFK-KVBEART-OK(WS-INDEX-ORFK) = NEJ                              
576507        IF ORFK-KVBEART(WS-INDEX-ORFK) NOT NUMERIC                        
576508           MOVE ZERO             TO ORFK-KVBEART(WS-INDEX-ORFK)           
576509        END-IF                                                            
576510        MOVE 58                  TO ORFK-KDORDBEK(WS-INDEX-ORFK)          
576511     END-IF                                                               
576512     IF ORFK-PRARTNTO-OK(WS-INDEX-ORFK) = NEJ                             
576513        IF ORFK-PRARTNTO(WS-INDEX-ORFK) NOT NUMERIC                       
576514           MOVE ZERO          TO ORFK-PRARTNTO-UT(WS-INDEX-ORFK)          
576515        END-IF                                                            
576516        MOVE 58                  TO ORFK-KDORDBEK(WS-INDEX-ORFK)          
576517     END-IF                                                               
576518                                                                          
576519     IF ORFK-PRARTNTO-LOC-OK(WS-INDEX-ORFK) = NEJ                         
576520        IF ORFK-PRARTNTO-LOC(WS-INDEX-ORFK) NOT NUMERIC                   
576521           MOVE ZERO        TO ORFK-PRARTNTO-LOC-UT(WS-INDEX-ORFK)        
576522        END-IF                                                            
576523        MOVE 58                  TO ORFK-KDORDBEK(WS-INDEX-ORFK)          
576524     END-IF                                                               
576525                                                                          
576526     IF ORFK-PRARTNTO-LOCPREL-OK(WS-INDEX-ORFK) = NEJ                     
576527        IF ORFK-PRARTNTO-LOCPREL(WS-INDEX-ORFK) NOT NUMERIC               
576528           MOVE ZERO   TO ORFK-PRARTNTO-LOCPREL-UT(WS-INDEX-ORFK)         
576529        END-IF                                                            
576530        MOVE 58                  TO ORFK-KDORDBEK(WS-INDEX-ORFK)          
576531     END-IF                                                               
576532     IF ORFK-PRARTBTO-LOC-OK(WS-INDEX-ORFK) = NEJ                         
576533        IF ORFK-PRARTBTO-LOC(WS-INDEX-ORFK) NOT NUMERIC                   
576534           MOVE ZERO        TO ORFK-PRARTBTO-LOC-UT(WS-INDEX-ORFK)        
576535        END-IF                                                            
576536        MOVE 58                  TO ORFK-KDORDBEK(WS-INDEX-ORFK)          
576537     END-IF                                                               
576538     IF ORFK-TITPO-OK(WS-INDEX-ORFK) = NEJ                                
576539        IF ORFK-TITPO-RAD(WS-INDEX-ORFK) NOT NUMERIC                      
576540           MOVE ZERO             TO ORFK-TITPO-RAD(WS-INDEX-ORFK)         
576541        END-IF                                                            
576542        MOVE 58                  TO ORFK-KDORDBEK(WS-INDEX-ORFK)          
576543     END-IF                                                               
576544                                                                          
576545     IF ORFK-FLSLATT-OK(WS-INDEX-ORFK) = NEJ                              
576546        MOVE 58                  TO ORFK-KDORDBEK(WS-INDEX-ORFK)          
576547     END-IF                                                               
576548                                                                          
576549     IF ORFK-KDORDBEK(WS-INDEX-ORFK) = 56                                 
576550       CONTINUE                                                           
576551     ELSE                                                                 
576552       IF ORFK-KDORDBEK(WS-INDEX-ORFK) > ZERO                             
576553          MOVE SPACE             TO ORFK-IDLEVNR(WS-INDEX-ORFK)           
576554          MOVE ZERO              TO ORFK-KDFARLIG(WS-INDEX-ORFK)          
576555                                    ORFK-KDPRODSL(WS-INDEX-ORFK)          
576556                                    ORFK-REKSIFFR(WS-INDEX-ORFK)          
576557                                    ORFK-VKART(WS-INDEX-ORFK)             
576558                                    ORFK-VLARTNTO(WS-INDEX-ORFK)          
576559                                    ORFK-KDERS(WS-INDEX-ORFK)             
576560                                    ORFK-TIDISPIN(WS-INDEX-ORFK)          
576561                                    ORFK-KVQPACK-0(WS-INDEX-ORFK)         
576562                                    ORFK-KVQPACK-1(WS-INDEX-ORFK)         
576563       END-IF                                                             
576564     END-IF                                                               
576565     .                                                                    
576566     EJECT                                                                
576567                                                                          
576568 E-HANDLE-ORDERLINES     SECTION.                                         
576569                                                                          
576570     MOVE 'E-HANDLE-ORDER'       TO   CURRENT-SECTION                     
576571                                                                          
576572     PERFORM EA-PREPARE-ORDERLINES                                        
576573                                                                          
576574     MOVE +1 TO WS-INDEX-ORFK                                             
576575     MOVE NEJ                    TO TILLK-SW                              
576576                                    OBKR-SW                               
576577                                    LDC-ARTIKELBYTE-SW                    
576578                                    CDC-MOVE-SW                           
576580     MOVE +0                     TO WS-IDPRQUES                           
576581                                                                          
576582     PERFORM UNTIL WS-INDEX-ORFK > WS-INDEX-ORFK-MAX                      
576583        IF MID-IDARTNR(WS-INDEX-ORFK) NOT = ALL '+'                       
576584           PERFORM S02-INITIALIZE-TILLK-TAB                               
576585           MOVE ORFK-W411AREG-001(WS-INDEX-ORFK)                          
576586                                 TO AREG-W411AREG-001                     
576587                                                                          
576588           PERFORM EC-HANDLE-LINE                                         
576589           PERFORM S40-HAMTA-WDB6-INFO                                    
576590           IF DCS-NDC-NA                                                  
576591              PERFORM S22-DATA-TILL-DEL-NOTE                              
576592           END-IF                                                         
576593           MOVE JA               TO TILLK-SW                              
576594           MOVE NEJ              TO CDC-MOVE-SW                           
576595           MOVE +1               TO WS-INDEX-TILLK                        
576596           PERFORM UNTIL WS-INDEX-TILLK > WS-INDEX-TILLK-MAX OR           
576597              TILK-IDARTNR(WS-INDEX-TILLK) = ZERO                         
576598              IF TILK-FLTILLK-X(WS-INDEX-TILLK) = JA                      
576599                 PERFORM ED-LAES-TILLK-DATA                               
576600                 PERFORM EC-HANDLE-LINE                                   
576601                 IF DCS-NDC-NA                                            
576602                    PERFORM S22-DATA-TILL-DEL-NOTE                        
576603                 END-IF                                                   
576604              END-IF                                                      
576605              ADD +1             TO WS-INDEX-TILLK                        
576606           END-PERFORM                                                    
576607        END-IF                                                            
576608           MOVE NEJ              TO TILLK-SW                              
576609                                    OBKR-SW                               
576610                                    LDC-ARTIKELBYTE-SW                    
576611                                    CDC-MOVE-SW                           
576612           ADD +1 TO WS-INDEX-ORFK                                        
576613     END-PERFORM                                                          
576614                                                                          
576615     IF DIST79-DEALER-PRICE AND WS-IDPRQUES NOT = +0                      
576616       MOVE WS-IDPRQUES          TO PRNO-IDPRQUES-IN                      
576617       MOVE +3                   TO PRNO-KDCALL                           
576618                                                                          
576619       CALL W335PRNO USING PRNO-W335PRNO                                  
576620                           PRNO-3107-PCB                                  
576621     END-IF                                                               
576622                                                                          
576623     IF AVSR-IDDC(1) NOT = SPACE                                          
576625        CALL W413AVSR USING AVSR-W413AVSR AVSR-LIST-PCB                   
576626          AVSR-ORQI-PCB AVSR-GMTB-PCB AVSR-GMTC-PCB                       
576627          AVSR-WDB2-PCB AVSR-WDB6-PCB                                     
576628          TRAN-XXKB-PCB                                                   
576629                                                                          
576630        PERFORM AA-NOLLA-WOPS-TABELL                                      
576631     END-IF                                                               
576632     MOVE JA                     TO ALLT-SW                               
576633     .                                                                    
576634     EJECT                                                                
576635                                                                          
576636*****************************************************************         
576637*UPD WDQ2 AND DEL FROM WDQ4                                               
576638*****************************************************************         
576639 EA-PREPARE-ORDERLINES   SECTION.                                         
576641     MOVE 'EA-PREPARE-ORD'       TO   CURRENT-SECTION                     
576642                                                                          
576644     IF OHUV-FLKLAR = JA                                                  
576646       PERFORM EAA-UPD-TEMP-KDORDSTA                                      
576647     END-IF                                                               
576648                                                                          
576649     IF UPD-ACTION                                                        
576651        MOVE +1                  TO AVSR-INDX                             
576652        MOVE NEJ                 TO AVSR-SW                               
576653        MOVE +1                  TO UPD-INDX                              
576654        MOVE SPACE               TO 2109-MID2-W2I10902                    
576655        MOVE +1                  TO 2109-INDX                             
576656        PERFORM UNTIL UPD-INDX > 13 OR                                    
576657                      WS-UPD-IDARTNR(UPD-INDX) = 0                        
576659           PERFORM EAB-REMOVE-EXISTING-Q4                                 
576660           ADD +1                TO UPD-INDX                              
576661        END-PERFORM                                                       
576662                                                                          
576663        IF 2109-MID2-KVANTART > ZERO                                      
576664          PERFORM K-STARTA-2109                                           
576665        END-IF                                                            
576666        PERFORM EAC-CALL-AVSR-AVSO                                        
576667        INITIALIZE AVSR-W413AVSR                                          
576668     END-IF                                                               
576669     .                                                                    
576670     EJECT                                                                
576671                                                                          
576672*****************************************************************         
576673*FIRST TIME UPDATE THE KDORDSTA AND KDORDSTA-0                            
576674*****************************************************************         
576675 EAA-UPD-TEMP-KDORDSTA    SECTION.                                        
576676     MOVE 'EAA-UPD-TEMP-KD'      TO   CURRENT-SECTION                     
576677                                                                          
576678     PERFORM IMS-GHU-WDQ2-WDQ201                                          
576679                                                                          
576680     MOVE NEJ                    TO OHUV-FLKLAR                           
576682                                                                          
576683     PERFORM IMS-REPL-WDQ201                                              
576684                                                                          
576685     IF SW-KDORDSTA-O-ALL-SPACE-FLAG = NEJ                                
576686        CONTINUE                                                          
576687     ELSE                                                                 
576688        PERFORM IMS-GHNP-WDQ212                                           
576689                                                                          
576690        PERFORM UNTIL SEGMENT-SAKNAS                                      
576691                                                                          
576692           MOVE ARB-KDORDSTA     TO ARB-KDORDSTA-O                        
576693           MOVE 'E '             TO ARB-KDORDSTA                          
576694           PERFORM IMS-REPL-WDQ212                                        
576695                                                                          
576696           PERFORM IMS-GHNP-WDQ212                                        
576697        END-PERFORM                                                       
576698                                                                          
576699     END-IF                                                               
576700*    TO RESTORE ARB-IO-AREA WITH PRIMARY DC                               
576701     MOVE OHUV-IDDC-PRIM    TO W-IDDC-WDQ212                              
576702     PERFORM IMS-GNP-WDQ212-FIRST                                         
576703     .                                                                    
576704     EJECT                                                                
576705                                                                          
576706*****************************************************************         
576707*FOR UPD REQUEST,REMOVE THE EXISTING ORDERLINE IN Q4 IN THIS SEC.         
576708*FROM EC-, ADD THE LINE WITH NEW QTY.                                     
576709*****************************************************************         
576710 EAB-REMOVE-EXISTING-Q4   SECTION.                                        
576712     MOVE 'EAB-DELETE-EXIS'         TO   CURRENT-SECTION                  
576713                                                                          
576714     MOVE WS-UPD-IDORDER(UPD-INDX)  TO  W-Q401-IDORDER                    
576715     MOVE WS-UPD-IDDC   (UPD-INDX)  TO W-Q401-IDDC                        
576716     MOVE WS-UPD-ADLAGOMR(UPD-INDX) TO W-Q401-ADLAGOMR                    
576717     MOVE WS-UPD-ADGANG (UPD-INDX)  TO W-Q401-ADGANG                      
576718     MOVE WS-UPD-ADPLATS(UPD-INDX)  TO W-Q401-ADPLATS                     
576719     MOVE WS-UPD-IDARTNR(UPD-INDX)  TO W-Q401-IDARTNR                     
576720     MOVE WS-UPD-IDLOPNR(UPD-INDX)  TO W-Q401-IDLOPNR                     
576721                                                                          
576722     PERFORM IMS-GHU-WDQ401                                               
576723                                                                          
576724     IF SEGMENT-FINNS                                                     
576726         MOVE ORAD-KVBEART-Q        TO WS-ORAD-KVBEART-Q                  
576727         MOVE ORAD-KVPREAVB         TO WS-ORAD-KVPREAVB                   
576728         MOVE ORAD-KVPRERO          TO WS-ORAD-KVPRERO                    
576729         MOVE ORAD-IDARTNR          TO W-IDARTNR                          
576730         IF ORAD-IDDC NOT = W-IDDC-B6                                     
576731           MOVE ORAD-IDDC           TO W-IDDC-B6                          
576732           PERFORM IMS-GU-WDB601                                          
576733         END-IF                                                           
576734         IF DCS-CDC                                                       
576735            PERFORM S03-RESTORE-STOCKS-CDC                                
576736         ELSE                                                             
576737            PERFORM S05-RESTORE-STOCKS-XDC                                
576738         END-IF                                                           
576739         PERFORM S06-CREATE-AVSR-FIELDS                                   
576740         PERFORM S07-CREATE-2109-TRANS                                    
576741         PERFORM S08-DELETE-PRICE-Q-LINE                                  
576742         PERFORM IMS-DLET-WDQ401                                          
576743         ADD +1                     TO AVSR-INDX                          
576744     END-IF                                                               
576745     .                                                                    
576746     EJECT                                                                
576747                                                                          
576748 EAC-CALL-AVSR-AVSO      SECTION.                                         
576750     MOVE 'EAC-CALL-AVSR-'   TO   CURRENT-SECTION                         
576751                                                                          
576752     MOVE W-WDQ2C-IDDISTR    TO   AVSO-IDDISTR                            
576753     MOVE W-WDQ2C-IDKUNDNR   TO   AVSO-IDKUNDNR                           
576754     MOVE W-WDQ2C-IDKUNDRF   TO   AVSO-IDKUNDRF                           
576755     MOVE OHUV-IDORDER       TO   AVSO-IDORDER                            
576756     MOVE SPACE              TO   AVSO-IDDC                               
576757     MOVE ZERO               TO   AVSO-TIRFS                              
576758     MOVE ZERO               TO   AVSO-TIAAMMDD                           
576759     MOVE ZERO               TO   AVSO-TIHHMM                             
576760     MOVE W-IDTRANS          TO   AVSO-IDTRANS                            
576761                                                                          
576762     CALL W413AVSR USING AVSR-W413AVSR AVSR-LIST-PCB                      
576763     AVSR-ORQI-PCB AVSR-GMTB-PCB AVSR-GMTC-PCB                            
576764     AVSR-WDB2-PCB AVSR-WDB6-PCB TRAN-XXKB-PCB                            
576765                                                                          
576766     CALL W413AVSO USING AVSO-W413AVSO                                    
576767     AVSO-WDE6-PCB AVSO-ORQA-PCB                                          
576768     AVSO-WDQ2-PCB AVSO-GMTB-PCB                                          
576769     AVSO-XXKA-PCB AVSO-4437-PCB AVSO-XXKE-PCB                            
576770     AVSO-XXKF-PCB AVSO-XXKG-PCB AVSO-XXKH-PCB                            
576771     AVSO-XXKI-PCB AVSO-XXKP-PCB AVSO-WDB2-PCB AVSO-WDB6-PCB              
576772     AVSO-WDP7-PCB TRAN-XXKB-PCB                                          
576773     ORDN-ORQL-PCB ORDN-PROC-PCB ORDN-ORQI-PCB ORDN-WDQ3-PCB              
576774     .                                                                    
576775     EJECT                                                                
576776                                                                          
576777 EC-HANDLE-LINE SECTION.                                                  
576779     MOVE 'EC-HANDLE-LINE'       TO   CURRENT-SECTION                     
576780                                                                          
576781     PERFORM ECA-INTIALIZE-ALL-OCC                                        
576782     PERFORM ECB-BUILD-ORDERLINES-Q4                                      
576783     PERFORM ECC-GET-NEW-PART-DETAILS                                     
576784     PERFORM ECF-KOMPLETTERA-KVANT-BRYTES                                 
576785                                                                          
576786     IF NOT TILLKOMMANDE-RAD                                              
576787        PERFORM ECK-KOMPLETTERA-ERSATTNING                                
576788     END-IF                                                               
576789                                                                          
576790     PERFORM ECL-KOMPLETTERA-SPARRAR                                      
576791                                                                          
576792     IF MID-IDSYSTEM(1:3) NOT = 'ECO'                                     
576793       PERFORM ECJ-KOMPLETTERA-PRIS                                       
576794     END-IF                                                               
576795                                                                          
576796     PERFORM ECT-KOMPLETTERA-RELEASESPARR                                 
576797                                                                          
576798     IF (KOLLA-ERS AND PREPLANED-SW = NEJ)                                
576799     AND (DCS-CDC OR DCS-SDC)                                             
576800        PERFORM ECZ-CHECK-KDERS-IN-DC                                     
576801     END-IF                                                               
576802     PERFORM ECW-PREL-AVBOKNING-SDC1                                      
576803     PERFORM ECH-PREL-AVBOKNING-SDC2                                      
576804     PERFORM ECX-PREL-AVBOKNING-SDC3                                      
576805     PERFORM ECG-PREL-AVBOKNING-XDC                                       
576806     PERFORM ECT-KOMPLETTERA-RANSONERING                                  
576807     PERFORM ECR-PREL-AVBOKNING-CDC                                       
576808                                                                          
576809     IF NOT LDC-ARTIKELBYTE                                               
576810        IF SKRIV-OBKR                                                     
576811          PERFORM ECS-SKRIV-OBKR-OCH-VOR-RAD                              
576812          IF (OHUV-KDORDKL > +0 AND                                       
576813            (ORAD-KVPREAVB > +0 OR ORAD-KVPRERO > +0))                    
576814                     OR                                                   
576815            (OHUV-KDORDKL = +0 AND ORAD-KVPREAVB > +0)                    
576816                                                                          
576817            IF CDCA-KDORDBEK-UT = 92                                      
576818              COMPUTE ORAD-KVBEART = ORAD-KVBEART + ORAD-KVPRERO          
576819              MOVE +0             TO ORAD-KVPRERO                         
576820            END-IF                                                        
576821            PERFORM S09-KONTROLLERA-ENHETSLAST                            
576822            PERFORM S10-BERAKNA-WOPS-SKRIV-ORAD                           
576823          END-IF                                                          
576824        ELSE                                                              
576826           PERFORM S09-KONTROLLERA-ENHETSLAST                             
576827           PERFORM S10-BERAKNA-WOPS-SKRIV-ORAD                            
576828        END-IF                                                            
576829     ELSE                                                                 
576830        IF SKRIV-OBKR                                                     
576831          PERFORM ECS-SKRIV-OBKR-OCH-VOR-RAD                              
576832        END-IF                                                            
576833     END-IF                                                               
576835     .                                                                    
576836     EJECT                                                                
576837                                                                          
576838*INITIALZE ALL CONFIRMATION CODES FROM SUB MODULES                        
576839 ECA-INTIALIZE-ALL-OCC SECTION.                                           
576841     MOVE 'ECA-INITIALIZE'     TO   CURRENT-SECTION                       
576842                                                                          
576843     MOVE ZERO                 TO KVAN-KDORDBEK-UT                        
576844                                  KERS-KDERS                              
576845     IF NOT TILLKOMMANDE-RAD                                              
576846        MOVE ZERO              TO KERS-KDORDBEK                           
576847     ELSE                                                                 
576848        MOVE JA                TO OBKR-SW                                 
576849     END-IF                                                               
576850     MOVE ZERO                 TO RELS-KDORDBEK                           
576851                                  STOR-KDORDBEK                           
576852                                  XDCA-KDORDBEK                           
576853                                  SDCA-KDORDBEK                           
576854                                  SDCA-KDORDBEK-FIRST-SDC                 
576855                                  SDCA-KDORDBEK-SECOND-SDC                
576856                                  CDCA-KDORDBEK-UT                        
576857                                  SPAR-KDORDBEK                           
576858                                                                          
576859     MOVE JA                   TO ALLT-SW                                 
576860     MOVE NEJ                  TO KOLLA-ERS-SW                            
576861     .                                                                    
576862     EJECT                                                                
576863*POPULATE Q4 VALUES                                                       
576864 ECB-BUILD-ORDERLINES-Q4 SECTION.                                         
576865                                                                          
576866     MOVE 'ECB-BUILD-ORDE'     TO   CURRENT-SECTION                       
576867                                                                          
576868     MOVE OHUV-IDORDER         TO ORAD-IDORDER                            
576869     IF OHUV-IDDC-TVS = SPACE                                             
576870       MOVE OHUV-IDDC-PRIM     TO ORAD-IDDC                               
576871     ELSE                                                                 
576872       MOVE OHUV-IDDC-TVS      TO ORAD-IDDC                               
576873     END-IF                                                               
576874     MOVE ORAD-IDDC            TO WS-IDDC-SEEK                            
576875     MOVE AREG-ADLAGOMR        TO ORAD-ADLAGOMR                           
576876     MOVE AREG-ADGANG          TO ORAD-ADGANG                             
576877     MOVE AREG-ADPLATS         TO ORAD-ADPLATS                            
576878     IF TILLKOMMANDE-RAD                                                  
576879        MOVE AREG-IDARTNR      TO ORAD-IDARTNR                            
576880     ELSE                                                                 
576881        MOVE ORFK-IDARTNR(WS-INDEX-ORFK)                                  
576882                               TO ORAD-IDARTNR                            
576883     END-IF                                                               
576884     MOVE +1                   TO ORAD-IDLOPNR                            
576885     MOVE MID-BERADREF(WS-INDEX-ORFK)                                     
576886                               TO ORAD-BERADREF                           
576887*    MOVE OHUV-BEKUNDRF        TO ORAD-BEVOLREF                           
576888     MOVE SPACE                TO ORAD-BEVOLREF                           
576889     MOVE SPACE                TO ORAD-FLAKPLOC                           
576890                                                                          
576891     MOVE NEJ                  TO ORAD-FLINVEST                           
576892     MOVE JA                   TO ORAD-FLOBTRAN                           
576893     IF TILLKOMMANDE-RAD                                                  
576894        IF DIST79-DEALER-PRICE                                            
576895          MOVE NEJ             TO ORAD-FLPRTILL                           
576896        ELSE                                                              
576897         IF TILK-PRARTNTO(WS-INDEX-TILLK) > +0                            
576898           MOVE TILK-FLPRTILL(WS-INDEX-TILLK)                             
576899                               TO ORAD-FLPRTILL                           
576900         ELSE                                                             
576901           MOVE NEJ            TO ORAD-FLPRTILL                           
576902         END-IF                                                           
576903        END-IF                                                            
576904     ELSE                                                                 
576905        MOVE NEJ               TO ORAD-FLPRTILL                           
576906     END-IF                                                               
576907     MOVE OHUV-FLRESTN         TO ORAD-FLRESTN                            
576908     MOVE 'N'                  TO ORAD-FLSDCLEV                           
576909     IF TILLKOMMANDE-RAD                                                  
576910        MOVE JA                TO ORAD-FLTILLK                            
576911     ELSE                                                                 
576912        MOVE NEJ               TO ORAD-FLTILLK                            
576913     END-IF                                                               
576914     MOVE WC-CDC-SE            TO ORAD-IDDC-RO                            
576915     MOVE OHUV-IDDISTR         TO ORAD-IDDISTR                            
576916     MOVE OHUV-IDKUNDNR        TO ORAD-IDKUNDNR                           
576917     MOVE W-WDQ2C-IDKUNDRF     TO ORAD-IDKUNDRF                           
576918     MOVE OHUV-IDKAMPRF        TO ORAD-IDKAMPRF                           
576919     MOVE SPACE                TO ORAD-IDLEVNR                            
576920     MOVE +0                   TO ORAD-IDLOPNR-RO                         
576921     MOVE '0000000   '         TO ORAD-IDKUNDRF-RO                        
576922     MOVE ZERO                 TO ORAD-IDSPECEMB                          
576923     MOVE MID-IDSYSTEM         TO ORAD-IDSYSTEM                           
576924     MOVE AREG-KDARTURS        TO ORAD-KDARTURS                           
576925                                                                          
576926     MOVE +1                   TO ORAD-KDDSP                              
576927     MOVE +0                   TO ORAD-KDVRINFO                           
576932*WE WILL NOT HAVE VIPS OR VDI INPUT?!                                     
576933     MOVE AREG-KDFARLIG        TO ORAD-KDFARLIG                           
576934     MOVE +0                   TO ORAD-KDKVBRYT                           
576935     MOVE OHUV-KDORDING        TO ORAD-KDORDING                           
576936     MOVE OHUV-KDORDKL         TO ORAD-KDORDKL                            
576937     MOVE AREG-KDPRODSL        TO ORAD-KDPRODSL                           
576938                                  TEST-KDPRODSL                           
576939     IF ORAD-KDORDING = +3                                                
576940       MOVE SPACE              TO ORAD-KDOI                               
576941     ELSE                                                                 
576942       IF KDPRODSL-VOLVO-EMB OR ORAD-KDORDING = +1                        
576943         MOVE 'CD'             TO ORAD-KDOI                               
576944       ELSE                                                               
576945         MOVE 'DT'             TO ORAD-KDOI                               
576946       END-IF                                                             
576947     END-IF                                                               
576948     MOVE SPACE                TO ORAD-CLEARGROUP                         
576949     IF TILLKOMMANDE-RAD                                                  
576950       IF DIST79-DEALER-PRICE                                             
576951           MOVE SPACE          TO ORAD-KDPRTYP                            
576952       ELSE                                                               
576953        IF TILK-PRARTNTO(WS-INDEX-TILLK) > +0                             
576954           MOVE TILK-KDPRTYP(WS-INDEX-TILLK)                              
576955                               TO ORAD-KDPRTYP                            
576956        ELSE                                                              
576957           MOVE SPACE          TO ORAD-KDPRTYP                            
576958        END-IF                                                            
576959       END-IF                                                             
576960     ELSE                                                                 
576961        MOVE SPACE             TO ORAD-KDPRTYP                            
576962     END-IF                                                               
576963     MOVE AREG-KDSPEEMB        TO ORAD-KDSPEEMB                           
576964     MOVE OHUV-KDTPOTYP        TO ORAD-KDTPOTYP                           
576965     MOVE JA                   TO ORAD-FLORDING                           
576966     IF TILLKOMMANDE-RAD                                                  
576967        MOVE TILK-KVBEART(WS-INDEX-TILLK)                                 
576968                               TO ORAD-KVBEART                            
576969     ELSE                                                                 
576970        MOVE ORFK-KVBEART(WS-INDEX-ORFK) TO WS-ALFA-6                     
576971        MOVE WS-NUM-6          TO ORAD-KVBEART                            
576972     END-IF                                                               
576973     MOVE +0                   TO ORAD-KVBEART-Q                          
576974     MOVE +0                   TO ORAD-KVPREAVB                           
576975     MOVE +0                   TO ORAD-KVPRERO                            
576976     MOVE +0                   TO ORAD-KVOKS-PREL                         
576977                                                                          
576978     MOVE +0                   TO ORAD-IDPRQUES                           
576979     MOVE +0                   TO ORAD-PRARTBTO-LOC                       
576980     MOVE +0                   TO ORAD-RERAB                              
576981     MOVE MID-KDVALISO(WS-INDEX-ORFK)   TO ORAD-KDVALISO                  
576982*    *GLOBAL EXPORT. MAY NOT BE SPACE.                                    
576983     IF ORAD-KDVALISO = SPACE                                             
576984         MOVE WS-KDVALISO-NA             TO ORAD-KDVALISO                 
576985     END-IF                                                               
576986                                                                          
576987     MOVE SPACE                TO ORAD-KDVAT                              
576988     MOVE SPACE                TO ORAD-KDRAB                              
576989     MOVE SPACE                TO ORAD-BEART-VIPS                         
576990     IF TILLKOMMANDE-RAD                                                  
576991       IF DIST79-DEALER-PRICE                                             
576992           MOVE +0             TO ORAD-PRARTNTO                           
576993           INITIALIZE          ORAD-DEAL-PR-LINE                          
576994       ELSE                                                               
576995        IF TILK-PRARTNTO(WS-INDEX-TILLK) > +0                             
576996           MOVE TILK-PRARTNTO(WS-INDEX-TILLK)                             
576997                               TO ORAD-PRARTNTO                           
576998           MOVE ZERO           TO ORAD-PRARTNTO-LOC                       
576999        ELSE                                                              
577000           MOVE +0             TO ORAD-PRARTNTO                           
577001           INITIALIZE          ORAD-DEAL-PR-LINE                          
577002        END-IF                                                            
577003       END-IF                                                             
577004     ELSE                                                                 
577005       IF MID-PRARTNTO-LOC(WS-INDEX-ORFK) = ALL '+'                       
577006          MOVE +0              TO ORAD-PRARTNTO-LOC                       
577007       ELSE                                                               
577008          MOVE ORFK-PRARTNTO-LOC-UT(WS-INDEX-ORFK)                        
577009                               TO ORAD-PRARTNTO-LOC                       
577010       END-IF                                                             
577011       MOVE +0                 TO ORAD-PRARTNTO-LOCPREL                   
577012       MOVE +0                 TO ORAD-PRARTNTO                           
577013       MOVE +0                 TO ORAD-PRBPRIS                            
577014     END-IF                                                               
577015     IF ORFK-KDORDBEK (WS-INDEX-ORFK) = 59                                
577016        MOVE +0                TO ORAD-REKSIFFR                           
577017     ELSE                                                                 
577018        MOVE AREG-REKSIFFR     TO ORAD-REKSIFFR                           
577019     END-IF                                                               
577020     MOVE +0                   TO ORAD-RERF-RAD                           
577021     MOVE +0                   TO ORAD-KVSLATT                            
577022                                                                          
577023     IF TILLKOMMANDE-RAD                                                  
577024       IF DIST79-DEALER-PRICE                                             
577025         MOVE +0               TO ORAD-TIPRIS                             
577026       ELSE                                                               
577027        IF TILK-PRARTNTO(WS-INDEX-TILLK) > +0                             
577028           MOVE TILK-TIPRIS(WS-INDEX-TILLK)                               
577029                               TO ORAD-TIPRIS                             
577030        ELSE                                                              
577031           MOVE +0             TO ORAD-TIPRIS                             
577032        END-IF                                                            
577033       END-IF                                                             
577034     ELSE                                                                 
577035        MOVE +0                TO ORAD-TIPRIS                             
577036     END-IF                                                               
577037     MOVE MSGI-TILOKDAT        TO ORAD-TIREGDAT                           
577038     MOVE MSGI-TILOKTID        TO WS-TIHHMM                               
577039     MOVE WS-TIHHMMSS          TO ORAD-TIREGTID                           
577040     MOVE +0                   TO ORAD-TIRODAT                            
577041     MOVE OHUV-TITPO           TO ORAD-TITPO                              
577042     MOVE AREG-VKART           TO ORAD-VKART                              
577043     MOVE AREG-VKART-NTO       TO ORAD-VKART-NTO                          
577044     MOVE AREG-VLARTNTO        TO ORAD-VLARTNTO                           
577045     MOVE SPACE                TO ORAD-IDBIL                              
577046                                  ORAD-IDKLIENT                           
577047                                  ORAD-IDARBREF                           
577048                                  ORAD-IDVIN                              
577049     IF (ORAD-KDORDKL = 1 OR                                              
577050         ORAD-KDORDKL = 3) AND                                            
577051        GMT-FLLDCKND = JA                                                 
577052        MOVE ORAD-BERADREF     TO ORAD-IDKUNDRF-WIP                       
577053     ELSE                                                                 
577054        MOVE SPACE             TO ORAD-IDKUNDRF-WIP                       
577055     END-IF                                                               
577056     MOVE +0                   TO ORAD-PRAVCOST                           
577057     .                                                                    
577058     EJECT                                                                
577059                                                                          
577060 ECC-GET-NEW-PART-DETAILS SECTION.                                        
577062     MOVE 'ECC-GET-NEW-PA'     TO   CURRENT-SECTION                       
577063                                                                          
577064     IF ALLT-OK                                                           
577065        MOVE ORAD-IDARTNR      TO W-IDARTNR                               
577066        PERFORM IMS-GHU-WDK901                                            
577067                                                                          
577068        IF SEGMENT-SAKNAS                                                 
577069           MOVE W-IDARTNR      TO ARTM-IDARTNR-IN                         
577070           CALL W411ARTM USING ARTM-W411ARTM ARTM-ARTM-PCB                
577071        END-IF                                                            
577072                                                                          
577073     END-IF                                                               
577074     .                                                                    
577075     EJECT                                                                
577076                                                                          
577077 ECF-KOMPLETTERA-KVANT-BRYTES SECTION.                                    
577079     MOVE 'ECF-KOMPLETTER'        TO   CURRENT-SECTION                    
577080                                                                          
577081     IF ALLT-OK                                                           
577082                                                                          
577085        IF ORAD-IDBIL = SPACE                                             
577086          MOVE OHUV-FLORDSPE      TO KVAN-FLORDSPE-IN                     
577087        ELSE                                                              
577088          MOVE JA                 TO KVAN-FLORDSPE-IN                     
577089        END-IF                                                            
577090        IF ORAD-IDSYSTEM = 'LDC ' OR 'TACD'                               
577091            MOVE JA               TO KVAN-FLORDSPE-IN                     
577092        END-IF                                                            
577093        MOVE ORAD-KDKVBRYT        TO KVAN-KDKVBRYT-IN                     
577094        MOVE ORAD-IDSYSTEM        TO KVAN-IDSYSTEM-IN                     
577095        MOVE ORAD-KVBEART         TO KVAN-KVBEART-IN                      
577096        MOVE AREG-KVQPACK-0       TO KVAN-KVQPACK-0-IN                    
577098        MOVE AREG-KVQPACK-1       TO KVAN-KVQPACK-1-IN                    
577099        MOVE AREG-KDPRODSL        TO KVAN-KDPRODSL-IN                     
577100        MOVE AREG-KDSORT          TO KVAN-KDSORT-IN                       
577101        MOVE AREG-IDFKNGRP        TO KVAN-IDFKNGRP-IN                     
577102        MOVE OHUV-KDORDKL         TO KVAN-KDORDKL-IN                      
577103        MOVE OHUV-FLFORBI         TO KVAN-FLFORBI-IN                      
577104        MOVE OHUV-IDKAMPRF        TO KVAN-IDKAMPRF-IN                     
577105        MOVE OHUV-FLOVRLEV        TO KVAN-FLOVRLEV-IN                     
577106        MOVE OHUV-FLEMBORD        TO KVAN-FLEMBORD-IN                     
577107        MOVE ORAD-IDDC            TO KVAN-IDDC-IN                         
577108        MOVE ORAD-IDDISTR         TO KVAN-IDDISTR-IN                      
577109        MOVE ORAD-IDKUNDNR        TO KVAN-IDKUNDNR-IN                     
577110        MOVE ORAD-BERADREF        TO KVAN-BERADREF-IN                     
577111        MOVE ORAD-IDARTNR         TO KVAN-IDARTNR-IN                      
577112                                                                          
577113        CALL W411KVAN             USING KVAN-W411KVAN                     
577114                                        KVAN-WDB2-PCB                     
577115                                        KVAN-WDC1-PCB                     
577116                                                                          
577117        MOVE KVAN-KDKVBRYT-UT     TO ORAD-KDKVBRYT                        
577118        MOVE KVAN-KVBEART-Q-UT    TO ORAD-KVBEART-Q                       
577119                                                                          
577120        IF KVAN-KDORDBEK-UT > +0                                          
577121           MOVE JA                TO OBKR-SW                              
577122        END-IF                                                            
577124     END-IF                                                               
577125     .                                                                    
577126     EJECT                                                                
577127                                                                          
577128 ECK-KOMPLETTERA-ERSATTNING  SECTION.                                     
577130     MOVE 'ECK-KOMPLETTER'        TO   CURRENT-SECTION                    
577131                                                                          
577132     IF ALLT-OK                                                           
577133        MOVE ORAD-IDARTNR         TO KERS-IDARTNR                         
577134        MOVE ORAD-IDDC            TO KERS-IDDC                            
577135        MOVE OHUV-FLPRERS         TO KERS-FLPRERS                         
577136        MOVE ORAD-FLPRTILL        TO KERS-FLPRTILL                        
577137        MOVE OHUV-FLFORBI         TO KERS-FLFORBI                         
577138        MOVE ORAD-IDKAMPRF        TO KERS-IDKAMPRF                        
577139        MOVE AREG-KDERS           TO KERS-KDERS                           
577140        MOVE AREG-KDERS-UTG       TO KERS-KDERS-UTG                       
577141        MOVE ORAD-KDPRTYP         TO KERS-KDPRTYP                         
577142        MOVE ORAD-KDTPOTYP        TO KERS-KDTPOTYP                        
577143        MOVE AREG-KDUART          TO KERS-KDUART                          
577144        MOVE ORAD-KVBEART         TO KERS-KVBEART                         
577145        MOVE ORAD-PRARTNTO        TO KERS-PRARTNTO                        
577146        MOVE ORAD-DEAL-PR-LINE    TO KERS-DEAL-PR-LINE                    
577147        MOVE ORAD-TIPRIS          TO KERS-TIPRIS                          
577148        MOVE OHUV-FLORDSPE        TO KERS-FLORDSPE                        
577149        MOVE OHUV-FLOVRLEV        TO KERS-FLOVRLEV                        
577150        CALL W411KERS USING KERS-W411KERS TILK-W411TILK                   
577151                            KERS-ARTC-PCB KERS-ERSA-PCB                   
577152                            SDCA-ARTS-PCB CDCA-ARTM-PCB                   
577153                                                                          
577154        IF KERS-KDORDBEK > ZERO   AND                                     
577155           ((KERS-KDERS-UTG > +20 AND KERS-KDERS = +0)  OR                
577156             KERS-KDERS > 10 )                                            
577157           MOVE JA                TO OBKR-SW                              
577158           MOVE NEJ               TO ALLT-SW                              
577159        END-IF                                                            
577160                                                                          
577161        PERFORM S40-HAMTA-WDB6-INFO                                       
577162        IF KERS-KDORDBEK > ZERO                                           
577163         IF DCS-SDC OR DCS-CDC                                            
577164           IF PREPLANED-SW = JA                                           
577165             IF AREG-KDERS = 11 OR 12 OR 14 OR 15 OR 17 OR 18 OR          
577166                             19 OR                                        
577167                             21 OR 22 OR 24 OR 25 OR 27 OR 28 OR          
577168                             29                                           
577169                  MOVE JA         TO KOLLA-ERS-SW                         
577170             END-IF                                                       
577171           ELSE                                                           
577172             IF AREG-KDERS = 11 OR 12 OR 17 OR                            
577173                             21 OR 22 OR 27                               
577174                MOVE JA           TO KOLLA-ERS-SW                         
577175             ELSE                                                         
577176                IF AREG-KDERS = 14 OR 15 OR 18 OR 19 OR                   
577177                                24 OR 25 OR 28 OR 29                      
577178                   MOVE JA        TO ALLT-SW                              
577179                 END-IF                                                   
577180               END-IF                                                     
577181           END-IF                                                         
577182         END-IF                                                           
577183                                                                          
577184         IF DCS-NDC                                                       
577185           IF AREG-KDERS > 18                                             
577186             MOVE JA              TO KOLLA-ERS-SW                         
577187           ELSE                                                           
577188              IF KERS-KDERS-UTG > +20                                     
577189**FIX---TESTA DDETTA FÖR ATT UNDVIKA ABEND I SPÄRR (GE K611)              
577190**      PGA UTGÅNGEN ART BESTÄLLT FRÅN NDC                                
577191                CONTINUE                                                  
577192              ELSE                                                        
577193                MOVE ZERO         TO KERS-KDORDBEK                        
577194                PERFORM S02-INITIALIZE-TILLK-TAB                          
577195                MOVE JA           TO ALLT-SW                              
577196              END-IF                                                      
577197            END-IF                                                        
577198          END-IF                                                          
577199        END-IF                                                            
577200        IF KERS-KDORDBEK > +0 AND KVAN-KDORDBEK-UT > +0                   
577201           IF KERS-KDERS = 01 AND (AREG-KDSORT = 'L' OR 'M')              
577202***        CHECK FOR 'KERS-KDERS = 01' AND KDSROT IS 'L' OR 'M' SO        
577203***        THAT QUANTITY ADAPTION DO HAPPEN EVEN WHEN THE                 
577204***        SUPERSESSION CODE IS EQUAL TO 1 AND HENCE Q1 QUANTITY          
577205***        IS NOT BROKEN FURTHER. MIN QTY ORDERED WILL BECOME Q1          
577206              CONTINUE                                                    
577207           ELSE                                                           
577208              MOVE ZERO           TO KVAN-KDORDBEK-UT                     
577209              MOVE ORAD-KVBEART   TO ORAD-KVBEART-Q                       
577210           END-IF                                                         
577211        END-IF                                                            
577212                                                                          
577213     ELSE                                                                 
577214         MOVE +0                  TO KERS-KDERS                           
577215     END-IF                                                               
577216     .                                                                    
577217     EJECT                                                                
577218                                                                          
577219 ECL-KOMPLETTERA-SPARRAR     SECTION.                                     
577221     MOVE 'ECL-KOMPLETTER'        TO   CURRENT-SECTION                    
577222                                                                          
577223     IF ALLT-OK OR KOLLA-ERS                                              
577224                                                                          
577225     MOVE ORAD-BERADREF           TO SPAR-BERADREF                        
577226     IF ORAD-IDBIL = SPACE OR ORAD-IDSYSTEM = 'SOFT'                      
577227       IF ORAD-IDSYSTEM = 'VDI '                                          
577228         MOVE SPACE               TO SPAR-BEKUNDRF                        
577229       ELSE                                                               
577230          MOVE OHUV-BEKUNDRF      TO SPAR-BEKUNDRF                        
577231       END-IF                                                             
577232     ELSE                                                                 
577233       MOVE 'SOFTWARE'            TO SPAR-BEKUNDRF                        
577234     END-IF                                                               
577235     MOVE AREG-FLAVRART        TO SPAR-FLAVRART                           
577236     MOVE OHUV-FLFORBI         TO SPAR-FLFORBI                            
577237     MOVE AREG-FLLSRDEL        TO SPAR-FLLSRDEL                           
577238     MOVE AREG-FLRADREF        TO SPAR-FLRADREF                           
577239     MOVE ORAD-FLRESTN         TO SPAR-FLRESTN                            
577240     MOVE ORAD-IDARTNR         TO SPAR-IDARTNR                            
577241     MOVE AREG-FLIART          TO SPAR-FLIART                             
577242     MOVE AREG-FLMARKSP        TO SPAR-FLMARKSP                           
577243     MOVE ORAD-IDDISTR         TO SPAR-IDDISTR                            
577244     MOVE ORAD-IDKUNDNR        TO SPAR-IDKUNDNR                           
577245     MOVE ORAD-IDKUNDRF-RO     TO SPAR-IDKUNDRF-RO                        
577246     PERFORM S40-HAMTA-WDB6-INFO                                          
577247     IF DCS-DDC                                                           
577248       MOVE OHUV-IDDC-PRIM     TO SPAR-IDDC                               
577249     ELSE                                                                 
577250       MOVE ORAD-IDDC          TO SPAR-IDDC                               
577251     END-IF                                                               
577252     MOVE ORAD-IDSYSTEM        TO SPAR-IDSYSTEM                           
577253     MOVE AREG-KDERS-UTG       TO SPAR-KDERS-UTG                          
577254     MOVE AREG-KDERS           TO SPAR-KDERS                              
577255     MOVE OHUV-KDFAKTYP        TO SPAR-KDFAKTYP                           
577256     MOVE AREG-KDLEVSP         TO SPAR-KDLEVSP                            
577257     MOVE +1                   TO SPAR-KDORDBEH                           
577258     MOVE OHUV-KDORDKL         TO SPAR-KDORDKL                            
577259     MOVE AREG-KDPRODSL        TO SPAR-KDPRODSL                           
577260     MOVE AREG-KDSORT          TO SPAR-KDSORT                             
577261     MOVE ORAD-KDPRTYP         TO SPAR-KDPRTYP                            
577262     MOVE ORAD-KDTPOTYP        TO SPAR-KDTPOTYP                           
577263     MOVE AREG-KDUART          TO SPAR-KDUART                             
577264     MOVE AREG-PRARTSTD        TO SPAR-PRARTSTD                           
577265     MOVE AREG-TIFINLV         TO SPAR-TIFINLV                            
577266     MOVE ORAD-TIRODAT         TO SPAR-TIRODAT                            
577267     MOVE ORAD-TITPO           TO SPAR-TITPO                              
577268     MOVE ORAD-FLSDCLEV        TO SPAR-FLSDCLEV                           
577269     MOVE OHUV-TIREPDAT        TO SPAR-TIREPDAT                           
577270                                                                          
577271     IF ORAD-IDBIL = SPACE OR ORAD-IDSYSTEM = 'SOFT'                      
577272       MOVE OHUV-FLORDSPE      TO SPAR-FLORDSPE                           
577273     ELSE                                                                 
577274       MOVE JA                 TO SPAR-FLORDSPE                           
577275     END-IF                                                               
577276                                                                          
577277     MOVE OHUV-FLOVRLEV        TO SPAR-FLOVRLEV                           
577278     MOVE OHUV-FLEMBORD        TO SPAR-FLEMBORD                           
577279                                                                          
577280     CALL W411SPAR USING SPAR-W411SPAR SPAR-WDF8-PCB                      
577281                                       SPAR-WDF8A-PCB                     
577282                                       SPAR-WDK6-PCB                      
577283                                                                          
577284     IF SPAR-KDORDBEK > ZERO                                              
577285        MOVE JA             TO OBKR-SW                                    
577286        MOVE NEJ            TO ALLT-SW                                    
577287        IF SPAR-KDORDBEK = 51 OR 67 OR 58                                 
577288          MOVE NEJ            TO KOLLA-ERS-SW                             
577289          MOVE ZERO            TO KERS-KDORDBEK                           
577290          PERFORM S02-INITIALIZE-TILLK-TAB                                
577291        END-IF                                                            
577292        MOVE ZERO           TO XDCA-DAPUBL                                
577293*FOR CODE 67 THERE IS A SPECIAL CHECK IN W411XDCA                         
577294        IF  SPAR-KDORDBEK = 67     AND SPAR-FLPUBCDC = YES                
577295*         MOVE 99999999       TO NDCA-DAPUBL                              
577296          MOVE 99999999       TO XDCA-DAPUBL                              
577297        END-IF                                                            
577298        IF KVAN-KDORDBEK-UT > +0                                          
577299           MOVE ZERO        TO KVAN-KDORDBEK-UT                           
577300           MOVE ORAD-KVBEART TO ORAD-KVBEART-Q                            
577301        END-IF                                                            
577302*FOR KDERS 19/29,SPAR GIVES OCC54.BUT IF STOCKS ARE PRESENT IN A          
577303*DC,ORDERLINE WITH 19/29 CAN BE REGISTERED.                               
577304        IF SPAR-KDORDBEK = 54 AND PREPLANED-SW = NEJ                      
577305                              AND NOT DCS-DDC                             
577306          MOVE JA                TO ALLT-SW                               
577307        END-IF                                                            
577308     ELSE                                                                 
577309*       KONTROLL OM REPARATIONSDATUM ÄR FÖR TÄTT INPÅ                     
577310*       GÄLLER ENGLAND, LDC, ORDKL=3                                      
577311        MOVE ORAD-IDDISTR   TO TEST-IDDISTR                               
577312        IF  OHUV-KDORDKL = 3 AND OHUV-TIREPDAT > 0                        
577313        AND (OHUV-IDSYSTEM = 'LDC '                                       
577314         OR (OHUV-IDSYSTEM(1:3) = 'LYN' OR 'ECO' OR 'VOU' OR              
577315                                  'TAD' OR 'ACC' OR 'APA' OR              
577316                                  'APB' OR 'APC' OR 'APD' OR              
577317                                  'APE' OR 'APF' OR 'APG' OR              
577318                                  'APH' OR 'API' OR 'APJ' ))              
577319          AND DIST34-ENGLAND-LDC                                          
577320*       LÄS KUNDREG O KOLLA ENGLANDS-DISTRIKT                             
577321          IF GMT-FLLDCKND = JA                                            
577322            PERFORM S16-KOLLA-REPARATIONSDATUM                            
577323            IF W-REPSW = NEJ                                              
577324              MOVE 68               TO SPAR-KDORDBEK                      
577325              MOVE JA               TO OBKR-SW                            
577326              MOVE NEJ              TO ALLT-SW                            
577327              IF SPAR-KDORDBEK = 51 OR 67 OR 58 OR 68                     
577328                MOVE NEJ            TO KOLLA-ERS-SW                       
577329                MOVE ZERO            TO KERS-KDORDBEK                     
577330                PERFORM S02-INITIALIZE-TILLK-TAB                          
577331              END-IF                                                      
577332              IF KVAN-KDORDBEK-UT > +0                                    
577333                MOVE ZERO        TO KVAN-KDORDBEK-UT                      
577334                MOVE ORAD-KVBEART TO ORAD-KVBEART-Q                       
577335              END-IF                                                      
577336            END-IF                                                        
577337          END-IF                                                          
577338        END-IF                                                            
577339     END-IF                                                               
577355                                                                          
577356     END-IF                                                               
577357     .                                                                    
577358     EJECT                                                                
577359 ECJ-KOMPLETTERA-PRIS SECTION.                                            
577361     MOVE 'ECJ-KOMPLETTER'        TO   CURRENT-SECTION                    
577362                                                                          
577363     IF ALLT-OK OR KOLLA-ERS OR SPAR-FLPUBCDC = YES                       
577364                                                                          
577365     IF DIST79-DEALER-PRICE                                               
577367       IF ORAD-PRARTNTO-LOC = +0    OR                                    
577368          ORAD-PRARTBTO-LOC = +0    OR                                    
577369          ORAD-KDVALISO     = SPACE OR                                    
577370          ORAD-KDVALISO     = WS-KDVALISO-NA OR                           
577371          ORAD-KDVAT        = SPACE OR                                    
577372          ORAD-RERAB        = +0    OR                                    
577373          ORAD-KDRAB        = SPACE OR                                    
577374          ORAD-BEART-VIPS   = SPACE                                       
577376         IF WS-IDPRQUES                = +0                               
577377           MOVE WS-IDPRQUES           TO PRNO-IDPRQUES-IN                 
577378           MOVE +1                    TO PRNO-KDCALL                      
577379                                                                          
577380           CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                
577381                                                                          
577382           MOVE PRNO-IDPRQUES-UT      TO PRQU-IDPRQUES                    
577383                                         WS-IDPRQUES                      
577384           MOVE +1                    TO PRQU-KDCALL                      
577385         ELSE                                                             
577386           MOVE WS-IDPRQUES           TO PRNO-IDPRQUES-IN                 
577387           MOVE +2                    TO PRNO-KDCALL                      
577388                                                                          
577389           CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                
577390                                                                          
577391           MOVE PRNO-IDPRQUES-UT      TO PRQU-IDPRQUES                    
577392                                         WS-IDPRQUES                      
577393           MOVE +2                    TO PRQU-KDCALL                      
577394         END-IF                                                           
577395                                                                          
577396         MOVE W-WDQ2C-IDDISTR          TO PRQU-IDDISTR                    
577397         MOVE W-WDQ2C-IDKUNDNR         TO PRQU-IDKUNDNR                   
577398         MOVE W-WDQ2C-IDKUNDRF         TO PRQU-IDKUNDRF                   
577399         MOVE ORAD-IDORDER             TO PRQU-IDORDER                    
577400         MOVE ORAD-KDORDKL             TO PRQU-KDORDKL                    
577401         MOVE 'N'                      TO PRQU-KDPRSTA                    
577402         MOVE ORAD-IDARTNR             TO PRQU-IDARTNR                    
577403         MOVE ORAD-KVBEART-Q           TO PRQU-KVBEART-Q                  
577404         MOVE ORAD-PRARTNTO-LOC        TO PRQU-PRARTNTO-LOC               
577405         MOVE +0                       TO PRQU-PRARTNTO-LOCPREL           
577406         MOVE ORAD-IDSYSTEM            TO PRQU-IDSYSTEM                   
577407                                                                          
577408         IF ORAD-KDVALISO  = SPACE OR WS-KDVALISO-NA                      
577409           PERFORM ECJB-HAEMTA-KDVALISO                                   
577410         END-IF                                                           
577411                                                                          
577412         MOVE ORAD-KDVALISO            TO PRQU-KDVALISO                   
577413                                                                          
577414         CALL W335PRQU USING PRQU-W335PRQU PRQU-WDG2-PCB                  
577415                                           PRQU-WDC7-PCB                  
577416                                           PRQU-SJKO-WDK6-PCB             
577417                                                                          
577418         MOVE PRQU-IDPRQUES            TO  ORAD-IDPRQUES                  
577419                                           WS-IDPRQUES                    
577420         MOVE PRQU-FLPRTILL            TO  ORAD-FLPRTILL                  
577421                                                                          
577422         IF ORAD-PRARTNTO-LOC = +0                                        
577423            MOVE PRQU-PRARTNTO-LOCPREL TO  ORAD-PRARTNTO-LOCPREL          
577424         END-IF                                                           
577425                                                                          
577426       END-IF                                                             
577427       IF ORAD-IDSYSTEM = 'OREL'  OR  'OVR '                              
577428          IF ORAD-PRARTNTO-LOC NOT = +0                                   
577429            IF ORAD-KDPRTYP = SPACE                                       
577430              MOVE 'P'            TO ORAD-KDPRTYP                         
577431              MOVE ORAD-TIREGDAT  TO ORAD-TIPRIS                          
577432            END-IF                                                        
577433          END-IF                                                          
577434       END-IF                                                             
577435     ELSE                                                                 
577436*      *NOT DIST79-DEALER-PRICE                                           
577437                                                                          
577438       PERFORM S40-HAMTA-WDB6-INFO                                        
577439                                                                          
577440       IF (ORAD-KDTPOTYP = +0   AND                                       
577441          (AREG-KDUART NOT = 'M' AND 'P' AND 'S' AND 'L'))                
577442                                                                          
577453           IF DIST79-ECOM-PRICE                                           
577454             CONTINUE                                                     
577455           ELSE                                                           
577456             IF ORAD-PRARTNTO NOT = +0                                    
577457*              *FETCH ONLY KDVALISO FROM W335PRIS                         
577458               MOVE 2                TO PRIS-KDCALL                       
577459             ELSE                                                         
577460*            *FETCH PRARTNTO/PRAVCOST AND KDVALISO FRON W335PRIS          
577461               MOVE 1                TO PRIS-KDCALL                       
577462             END-IF                                                       
577463           END-IF                                                         
577465       ELSE                                                               
577466           MOVE 2                  TO PRIS-KDCALL                         
577467       END-IF                                                             
577468                                                                          
577469       MOVE IDPGM                TO PRIS-IDPGM                            
577470       MOVE ORAD-IDARTNR         TO PRIS-IDARTNR                          
577471       MOVE ORAD-IDDISTR         TO PRIS-IDDISTR                          
577472       MOVE ORAD-IDKUNDNR        TO PRIS-IDKUNDNR                         
577473       MOVE ORAD-IDDC            TO PRIS-IDDC                             
577474       MOVE OHUV-KDORDKL         TO PRIS-KDORDKL                          
577475       MOVE ORAD-KVBEART-Q       TO PRIS-KVBEART                          
577476       MOVE ORAD-FLINVEST        TO PRIS-FLINVEST                         
577477                                                                          
577478       CALL W335PRIS USING PRIS-W335PRIS PRIS-ARTC-PCB                    
577479                           PRIS-WDK7-PCB                                  
577480                           PRIS-GMTA-PCB PRIS-BETA-PCB                    
577481                           PRIS-GPRIA-PCB PRIS-GPRIB-PCB                  
577482                           PRIS-COST-WDK6-PCB                             
577483                           PRIS-COST-WDK7-PCB                             
577484                           PRIS-COST-WDF1-PCB                             
577485                           PRIS-COST-9305-PCB                             
577486                           PRIS-COST-WDK72-PCB                            
577487                           PRIS-COST-WDB6-PCB                             
577488                                                                          
577489       IF PRIS-KDSVAR = '2'                                               
577490         MOVE 'DIST/KUND SAKNAS NÄR W335PRIS LÄSER KUNDREG'               
577491                             TO FELTEXT                                   
577492         CALL ABEND USING RKOD-ABEND                                      
577493       END-IF                                                             
577496       IF PRIS-KDCALL = 2                                                 
577497         MOVE PRIS-KDVALISO  TO ORAD-KDVALISO                             
577498         IF ORAD-KDPRTYP = SPACE                                          
577499           MOVE 'P'           TO ORAD-KDPRTYP                             
577500           MOVE ORAD-TIREGDAT TO ORAD-TIPRIS                              
577501         END-IF                                                           
577502       ELSE                                                               
577509         IF DIST79-ECOM-PRICE                                             
577510           MOVE PRIS-PRARTNTO  TO ORAD-PRARTNTO-LOC                       
577511                                  ORAD-PRARTBTO-LOC                       
577512         ELSE                                                             
577513           MOVE PRIS-PRARTNTO  TO ORAD-PRARTNTO                           
577514         END-IF                                                           
577516         MOVE PRIS-FLPRTILL  TO ORAD-FLPRTILL                             
577517         MOVE PRIS-KDPRTYP   TO ORAD-KDPRTYP                              
577518         MOVE PRIS-PRBPRIS   TO ORAD-PRBPRIS                              
577519         MOVE ORAD-TIREGDAT  TO ORAD-TIPRIS                               
577520         MOVE PRIS-KDVALISO  TO ORAD-KDVALISO                             
577521         MOVE PRIS-PRAVCOST  TO ORAD-PRAVCOST                             
577522       END-IF                                                             
577523     END-IF                                                               
577524     END-IF                                                               
577526     .                                                                    
577527     EJECT                                                                
577528                                                                          
577529 ECJB-HAEMTA-KDVALISO      SECTION.                                       
577531      MOVE 'STA ECJB-HAEMTA'               TO   CURRENT-SECTION           
577532                                                                          
577533      MOVE GMT-IDPARTNR              TO W-WDB1-IDPARTNR                   
577534      MOVE GMT-IDFTG                 TO W-WDB1-IDFTG                      
577535                                                                          
577536      PERFORM IMS-GU-WDB101                                               
577537      MOVE BET-KDVALISO              TO ORAD-KDVALISO                     
577539      .                                                                   
577540      EJECT                                                               
577541                                                                          
577542 ECT-KOMPLETTERA-RELEASESPARR SECTION.                                    
577544      MOVE 'STA ECJB-HAEMTA'               TO   CURRENT-SECTION           
577545                                                                          
577546      MOVE +0                      TO RELS-KDORDBEK                       
577547                                                                          
577548       IF ALLT-OK AND W-KDORDBEK = 56                                     
577549                                                                          
577550         MOVE ORAD-IDDISTR         TO RELS-IDDISTR                        
577551         MOVE ORAD-IDKUNDNR        TO RELS-IDKUNDNR                       
577552         MOVE ORAD-IDKUNDRF        TO RELS-IDKUNDRF                       
577553         MOVE ORAD-IDARTNR         TO RELS-IDARTNR                        
577554         MOVE ORAD-BERADREF        TO RELS-BERADREF                       
577555         MOVE AREG-IDANSK          TO RELS-IDANSK                         
577556         MOVE OHUV-IDKONTO         TO RELS-IDKONTO                        
577557         MOVE OHUV-IDKST           TO RELS-IDKST                          
577558         MOVE OHUV-IDANALYS        TO RELS-IDANALYS                       
577559         MOVE ORAD-KDDSP           TO RELS-KDDSP                          
577560         MOVE OHUV-KDFAKTYP        TO RELS-KDFAKTYP                       
577561         MOVE ARB-KDFRAKT          TO RELS-KDFRAKT                        
577562         MOVE ORAD-KDKVBRYT        TO RELS-KDKVBRYT                       
577563         MOVE ORAD-KDORDING        TO RELS-KDORDING                       
577564         MOVE OHUV-KDORDKL         TO RELS-KDORDKL                        
577565         MOVE AREG-KDPRODSL        TO RELS-KDPRODSL                       
577566         MOVE ORAD-KDVRINFO        TO RELS-KDVRINFO                       
577567         MOVE ORAD-KVBEART-Q       TO RELS-KVBEART-Q                      
577568         MOVE AREG-REKSIFFR        TO RELS-REKSIFFR                       
577569         MOVE MSGI-TILOKDAT        TO RELS-TITPO                          
577570         MOVE ORAD-PRARTNTO        TO RELS-PRARTNTO                       
577571         MOVE ORAD-PRAVCOST        TO RELS-PRAVCOST                       
577572         MOVE ORAD-DEAL-PR-LINE    TO RELS-DEAL-PR-LINE                   
577573         MOVE ORAD-KDPRTYP         TO RELS-KDPRTYP                        
577574         MOVE ORAD-FLPRTILL        TO RELS-FLPRTILL                       
577575         EJECT                                                            
577576         MOVE ORAD-BEVOLREF        TO RELS-BEVOLREF                       
577577         MOVE ORAD-IDKAMPRF        TO RELS-IDKAMPRF                       
577578         MOVE ORAD-IDSYSTEM        TO RELS-IDSYSTEM                       
577579         MOVE ORAD-FLINVEST        TO RELS-FLINVEST                       
577580         MOVE OHUV-FLORDSPE        TO RELS-FLORDSPE                       
577581         MOVE OHUV-FLOVRLEV        TO RELS-FLOVRLEV                       
577582         MOVE OHUV-FLFORBI         TO RELS-FLFORBI                        
577583         MOVE ORAD-IDLEVNR         TO RELS-IDLEVNR                        
577584         MOVE AREG-KDUART          TO RELS-KDUART                         
577585         MOVE AREG-KVFRYSTI        TO RELS-KVFRYSTI                       
577586         MOVE +1                   TO RELS-KDORDBEH                       
577587         MOVE W-KDTPOTYP           TO RELS-KDTPOTYP                       
577588         MOVE OHUV-BEKUNDRF        TO RELS-BEKUNDRF                       
577589         MOVE ORAD-FLTILLK         TO RELS-FLTILLK                        
577590         MOVE AREG-TIDISPIN        TO RELS-TIDISPIN                       
577591         MOVE OHUV-BEVARREF        TO RELS-BEVARREF                       
577592         MOVE 0                    TO RELS-KVQPACK-1                      
577593         MOVE ORAD-KVBEART         TO RELS-KVBEART                        
577594         MOVE W-KDORDBEK           TO RELS-KDORDBEK                       
577595                                                                          
577596         MOVE SPACE                TO RELS-FLKLAR                         
577597         MOVE OHUV-KDORDTYP-LDC    TO RELS-KDORDTYP-LDC                   
577598         MOVE OHUV-TIREPDAT        TO RELS-TIREPDAT                       
577599         MOVE ORAD-IDKUNDRF-WIP    TO RELS-IDKUNDRF-WIP                   
577600                                                                          
577601         CALL W411RELS USING RELS-W411RELS RELS-ORDP-PCB                  
577602                             RELS-FILA-PCB RELS-ARTM-PCB 2109-PCB         
577603                                                                          
577604         PERFORM ECTA-ANDRA-WDC711                                        
577605         IF RELS-KDORDBEK > +0                                            
577606            MOVE JA                TO OBKR-SW                             
577607            MOVE NEJ               TO ALLT-SW                             
577608            MOVE WC-CDC-SE         TO ORAD-IDDC                           
577609            MOVE ORAD-IDDC         TO WS-IDDC-SEEK                        
577610         ELSE                                                             
577611            IF RELS-FLKLAR = JA                                           
577612               MOVE NEJ            TO ALLT-SW                             
577613               IF KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD            
577614                  MOVE ZERO        TO KERS-KDORDBEK                       
577615                  PERFORM S02-INITIALIZE-TILLK-TAB                        
577616               END-IF                                                     
577617            END-IF                                                        
577618         END-IF                                                           
577619                                                                          
577620         MOVE +0           TO W-KDORDBEK                                  
577621         MOVE SPACE        TO RELS-FLKLAR                                 
577622                                                                          
577623       END-IF                                                             
577624     .                                                                    
577625     EJECT                                                                
577626 ECTA-ANDRA-WDC711 SECTION.                                               
577627     MOVE 'ECTA-ANDRA-WDC71'               TO  CURRENT-SECTION            
577628                                                                          
577629     IF W-KDTPOTYP = 7 AND RELS-FLOVRLEV NOT = JA                         
577630       IF ORAD-IDDISTR = 0778 AND OHUV-KDORDKL < 3                        
577631         INITIALIZE PRQU-W335PRQU                                         
577632         MOVE ORAD-IDDISTR       TO PRQU-IDDISTR                          
577633         MOVE ORAD-IDKUNDNR      TO PRQU-IDKUNDNR                         
577634         MOVE W-WDQ2C-IDKUNDRF   TO PRQU-IDKUNDRF                         
577635         MOVE ORAD-IDPRQUES      TO PRQU-IDPRQUES                         
577636         MOVE 6                  TO PRQU-KDCALL                           
577637         CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                 
577638                                            PRQU-WDC7-PCB                 
577639                                            PRQU-SJKO-WDK6-PCB            
577640         MOVE 'N'                TO ORAD-FLPRTILL                         
577641       END-IF                                                             
577642     END-IF                                                               
577643     .                                                                    
577644     EJECT                                                                
577645                                                                          
577646 ECW-PREL-AVBOKNING-SDC1 SECTION.                                         
577648     MOVE 'ECW-PREL-AVBOKNI'               TO  CURRENT-SECTION            
577649                                                                          
577650     IF LDC-ARTIKELBYTE AND                                               
577651        AREG-REDIRLEV > 0                                                 
577652        MOVE ZERO TO KERS-KDORDBEK                                        
577653        MOVE ZERO TO HELP-KERS-KDORDBEK                                   
577654        MOVE NEJ  TO ALLT-SW                                              
577655     END-IF                                                               
577656                                                                          
577657     IF (ALLT-OK OR KOLLA-ERS) AND                                        
577658        OHUV-FLOVRLEV = NEJ AND                                           
577659         OHUV-FLORDSPE = NEJ                                              
577662        AND ORAD-IDBIL = SPACE                                            
577663                                                                          
577664       PERFORM S40-HAMTA-WDB6-INFO                                        
577665       IF  DCS-SDC                                                        
577666                                                                          
577667         MOVE DCS-IDDC             TO DC01-IDDC                           
577668         IF ((ORAD-IDSYSTEM = 'LDC ' OR 'TACD') OR                        
577669             (ORAD-IDSYSTEM(1:3) = 'LYN' OR 'ECO' OR 'VOU' OR             
577670                                   'TAD' OR 'ACC' OR 'APA' OR             
577671                                   'APB' OR 'APC' OR 'APD' OR             
577672                                   'APE' OR 'APF' OR 'APG' OR             
577673                                   'APH' OR 'API' OR 'APJ' ))             
577674         AND ARB-TIRFS > 0                                                
577675         AND (AREG-KDERS = 0 OR                                           
577676            ((ORAD-IDSYSTEM = 'LDC' OR                                    
577677             (ORAD-IDSYSTEM(1:3) = 'LYN' OR 'ECO' OR 'VOU' OR             
577678                                   'TAD' OR 'ACC' OR 'APA' OR             
577679                                   'APB' OR 'APC' OR 'APD' OR             
577680                                   'APE' OR 'APF' OR 'APG' OR             
577681                                   'APH' OR 'API' OR 'APJ' ))             
577682             AND (AREG-KDERS = 01 OR 11)))                                
577683         AND REPAIR-CLEARING                                              
577684           PERFORM S13-CALL-WORKDAY-LDC                                   
577685           IF WORK-TIAAMMDD-FOM <= DAGENS-DATUM                           
577686             MOVE NEJ              TO SDCA-FLORDSPE                       
577687           ELSE                                                           
577688             MOVE JA               TO SDCA-FLORDSPE                       
577689           END-IF                                                         
577690         ELSE                                                             
577691           MOVE NEJ                TO SDCA-FLORDSPE                       
577692         END-IF                                                           
577693                                                                          
577694         MOVE OHUV-IDDISTR         TO SDCA-IDDISTR                        
577695         MOVE OHUV-FLFORBI         TO SDCA-FLFORBI                        
577696         MOVE AREG-FLREFILL        TO SDCA-FLREFILL                       
577697         MOVE ORAD-IDARTNR         TO SDCA-IDARTNR                        
577698         MOVE ORAD-IDDC            TO SDCA-IDDC                           
577699         MOVE OHUV-IDDC-TVS        TO SDCA-IDDC-TVS                       
577700         MOVE ORAD-IDLEVNR         TO SDCA-IDLEVNR                        
577701         MOVE ORAD-IDSYSTEM        TO SDCA-IDSYSTEM                       
577702         MOVE ORAD-KDORDKL         TO SDCA-KDORDKL                        
577703         MOVE ORAD-KDORDING        TO SDCA-KDORDING                       
577704         MOVE AREG-KDPRODSL        TO SDCA-KDPRODSL                       
577705         MOVE AREG-KDSORT          TO SDCA-KDSORT                         
577706         MOVE ORAD-KVBEART-Q       TO SDCA-KVBEART-Q                      
577707         MOVE AREG-KVQPACK-1       TO SDCA-KVQPACK-1                      
577708         MOVE AREG-REDIRLEV        TO SDCA-REDIRLEV                       
577709         MOVE ORAD-FLSDCLEV        TO SDCA-FLSDCLEV                       
577710         MOVE OHUV-TIREPDAT        TO SDCA-TIREPDAT                       
577711         MOVE ZERO                 TO SDCA-KVOKS-PREL                     
577712         MOVE +1                   TO SDCA-KDCALL                         
577713         MOVE +1                   TO SDCA-IXDCCLEAR                      
577714                                                                          
577715         CALL W411SDCA USING SDCA-W411SDCA SDCA-ARTS-PCB                  
577716                                           SDCA-WDB6-PCB                  
577717                                           SDCA-WDK9-PCB                  
577718                                           SDCA-WDR6-PCB                  
577719                                           SDCA-WDK6-PCB                  
577720                                           SDCA-WDQ4B-PCB                 
577721                                           SDCA-WDQ2-PCB                  
577722                                           SDCA-WDQ4-PCB                  
577723                                           SDCA-WDB6-2-PCB                
577724                                           SDCA-WDK6-2-PCB                
577725                                           SDCA-WDK7-2-PCB                
577726                                           SDCA-WDK7-3-PCB                
577727                                                                          
577728         MOVE SDCA-KDORDBEK TO SDCA-KDORDBEK-FIRST-SDC                    
577729         MOVE ZERO          TO SDCA-KDORDBEK                              
577730         IF SDCA-KVOKS-PREL > ZERO                                        
577731            MOVE SDCA-KVOKS-PREL TO ORAD-KVOKS-PREL                       
577732         END-IF                                                           
577733                                                                          
577734         IF SDCA-KDORDBEK-FIRST-SDC > ZERO                                
577735           IF SDCA-FLSDCLEV = JA AND SDCA-KDORDBEK-FIRST-SDC = 80         
577736                MOVE JA        TO OBKR-SW                                 
577737                MOVE NEJ       TO ALLT-SW                                 
577738           ELSE                                                           
577739             IF KOLLA-ERS                                                 
577740               IF PREPLANED-SW = JA                                       
577741                 MOVE ZERO            TO SDCA-KDORDBEK-FIRST-SDC          
577742                 IF SPAR-KDORDBEK = ZERO                                  
577743                   MOVE W-GMT-IDDC-CLEAR(2) TO ORAD-IDDC                  
577744                   MOVE ORAD-IDDC          TO WS-IDDC-SEEK                
577745                 ELSE                                                     
577746                   MOVE NEJ           TO KOLLA-ERS-SW                     
577747                 END-IF                                                   
577748               ELSE                                                       
577749                 IF SPAR-KDORDBEK = ZERO                                  
577750                  IF ORAD-IDDC = W-TILLK-DC                               
577751                     MOVE JA            TO OBKR-SW                        
577752                     MOVE NEJ           TO ALLT-SW                        
577753                                           KOLLA-ERS-SW                   
577754                     MOVE ZERO      TO SDCA-KDORDBEK-FIRST-SDC            
577755                  ELSE                                                    
577756                   MOVE W-GMT-IDDC-CLEAR(2) TO ORAD-IDDC                  
577757                   MOVE ORAD-IDDC     TO DC01-IDDC                        
577758                                         WS-IDDC-SEEK                     
577759                   IF DC01-IDDC = WC-CDC-SE                               
577760                      MOVE JA              TO CDC-MOVE-SW                 
577761                   END-IF                                                 
577762                  END-IF                                                  
577763                 ELSE                                                     
577764                   MOVE NEJ           TO KOLLA-ERS-SW                     
577765                 END-IF                                                   
577766               END-IF                                                     
577767             ELSE                                                         
577768                 IF SDCA-KDORDBEK-FIRST-SDC = 15                          
577769                   IF LDC-ARTIKELBYTE                                     
577770           MOVE 'T'                TO LDC-ARTIKELBYTE-SW                  
577771           MOVE HELP-ORAD-WDQ401   TO ORAD-WDQ401                         
577772           MOVE HELP-OBKR-WDQ101   TO OBKR-WDQ101                         
577773           MOVE HELP-CDCA-W411CDCA TO CDCA-W411CDCA                       
577774           MOVE HELP-KERS-W411KERS TO KERS-W411KERS                       
577775           MOVE HELP-KVAN-W411KVAN TO KVAN-W411KVAN                       
577776           MOVE HELP-ORFK-W411ORFK TO ORFK-W411ORFK                       
577777           MOVE HELP-XDCA-W411XDCA TO XDCA-W411XDCA                       
577778           MOVE HELP-SDCA-W411SDCA TO SDCA-W411SDCA                       
577779           MOVE HELP-SPAR-W411SPAR TO SPAR-W411SPAR                       
577780           MOVE HELP-STOR-W411STOR TO STOR-W411STOR                       
577781           MOVE HELP-RELS-W411RELS TO RELS-W411RELS                       
577782           MOVE HELP-TILK-W411TILK TO TILK-W411TILK                       
577783           MOVE HELP-AREG-W411AREG TO AREG-W411AREG                       
577784           MOVE ZERO TO OBKR-IDARTNR-TILLK                                
577785                        OBKR-REKSIFFR-TILLK                               
577786           PERFORM ECWC-AVSLUTA-15-CLEARING                               
577787                   ELSE                                                   
577788                    PERFORM ECWA-KOLLA-ARTIKELBYTE-LDC                    
577789                    IF BYT-ARTIKEL                                        
577790                      PERFORM ECWB-LDC-ARTIKELBYTE                        
577791                    ELSE                                                  
577792                      PERFORM ECWC-AVSLUTA-15-CLEARING                    
577793                    END-IF                                                
577794                   END-IF                                                 
577795                 ELSE                                                     
577796                   MOVE JA          TO OBKR-SW                            
577797                   MOVE NEJ         TO ALLT-SW                            
577798                 END-IF                                                   
577799             END-IF                                                       
577800           END-IF                                                         
577801         ELSE                                                             
577802              MOVE SDCA-ADLAGOMR     TO ORAD-ADLAGOMR                     
577803           IF LDC-ARTIKELBYTE                                             
577804              MOVE 'B' TO LDC-ARTIKELBYTE-SW                              
577805           END-IF                                                         
577806           MOVE SDCA-ADGANG          TO ORAD-ADGANG                       
577807           MOVE SDCA-ADPLATS         TO ORAD-ADPLATS                      
577808           MOVE SDCA-KVBEART-Q       TO ORAD-KVPREAVB                     
577809           MOVE NEJ                  TO ALLT-SW                           
577810           IF KOLLA-ERS  OR                                               
577811             (KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD)               
577812              MOVE NEJ               TO KOLLA-ERS-SW                      
577813              MOVE ZERO              TO KERS-KDORDBEK                     
577814              PERFORM S02-INITIALIZE-TILLK-TAB                            
577815              MOVE ZERO              TO SPAR-KDORDBEK                     
577816           END-IF                                                         
577817           IF (KERS-KDERS = +19 OR +29) AND PREPLANED-SW = NEJ            
577818              MOVE ZERO           TO SPAR-KDORDBEK                        
577819           END-IF                                                         
577820         END-IF                                                           
577821         MOVE SDCA-KDOI              TO ORAD-KDOI                         
577822         MOVE SDCA-CLEARGROUP        TO ORAD-CLEARGROUP                   
577823       END-IF                                                             
577824     ELSE                                                                 
577825       IF LDC-ARTIKELBYTE                                                 
577826          MOVE NEJ                TO ALLT-SW                              
577827          MOVE 'T'                TO LDC-ARTIKELBYTE-SW                   
577828          MOVE HELP-ORAD-WDQ401   TO ORAD-WDQ401                          
577829          MOVE HELP-OBKR-WDQ101   TO OBKR-WDQ101                          
577830          MOVE HELP-CDCA-W411CDCA TO CDCA-W411CDCA                        
577831          MOVE HELP-KERS-W411KERS TO KERS-W411KERS                        
577832          MOVE HELP-KVAN-W411KVAN TO KVAN-W411KVAN                        
577833          MOVE HELP-ORFK-W411ORFK TO ORFK-W411ORFK                        
577834          MOVE HELP-XDCA-W411XDCA TO XDCA-W411XDCA                        
577835          MOVE HELP-SDCA-W411SDCA TO SDCA-W411SDCA                        
577836          MOVE HELP-SPAR-W411SPAR TO SPAR-W411SPAR                        
577837          MOVE HELP-STOR-W411STOR TO STOR-W411STOR                        
577838          MOVE HELP-RELS-W411RELS TO RELS-W411RELS                        
577839          MOVE HELP-TILK-W411TILK TO TILK-W411TILK                        
577840          MOVE HELP-AREG-W411AREG TO AREG-W411AREG                        
577841          PERFORM ECWC-AVSLUTA-15-CLEARING                                
577842       END-IF                                                             
577843     END-IF                                                               
577844     .                                                                    
577845     EJECT                                                                
577846 ECWA-KOLLA-ARTIKELBYTE-LDC SECTION.                                      
577847     MOVE 'ECWA-KOLLA-ARTIK'               TO  CURRENT-SECTION            
577848                                                                          
577849     MOVE JA TO SW-BYT-ARTIKEL                                            
577850                                                                          
577857     IF (OHUV-IDSYSTEM = 'LDC' OR                                         
577858        (OHUV-IDSYSTEM(1:3) = 'LYN' OR 'ECO' OR 'VOU' OR                  
577859                              'TAD' OR 'ACC' OR 'APA' OR                  
577860                              'APB' OR 'APC' OR 'APD' OR                  
577861                              'APE' OR 'APF' OR 'APG' OR                  
577862                              'APH' OR 'API' OR 'APJ' )) AND              
577863        OHUV-TIREPDAT NOT = ZERO AND                                      
577864        AREG-KDERS = 01 OR 11                                             
577865                                                                          
577866        MOVE 1  TO TILK-IX                                                
577867        PERFORM UNTIL TILK-IX > TILK-IX-MAX                               
577868                   OR TILK-IDARTNR-TILLK(TILK-IX) = ZERO                  
577869                   OR BYT-EJ-ARTIKEL                                      
577870                   OR TILK-FLFINLV(TILK-IX) = NEJ                         
577871           IF TILK-IX > 1                                                 
577872               MOVE NEJ TO SW-BYT-ARTIKEL                                 
577873           ELSE                                                           
577874              MOVE TILK-IDARTNR-TILLK(TILK-IX) TO W-IDARTNR-T             
577875              PERFORM IMS-GU-WDK611-TILK                                  
577876              IF SEGMENT-FINNS                                            
577877                 IF TILK-CLAG-REDIRLEV > ZERO                             
577878                 OR TILK-CLAG-KDERS    > ZERO                             
577879                    MOVE NEJ TO SW-BYT-ARTIKEL                            
577880                 END-IF                                                   
577881              ELSE                                                        
577882                 MOVE NEJ TO SW-BYT-ARTIKEL                               
577883              END-IF                                                      
577884                                                                          
577885              MOVE ORAD-IDDC TO W-IDDC-T                                  
577886              PERFORM IMS-GU-WDK711-TILK                                  
577887              IF SEGMENT-SAKNAS                                           
577888                 MOVE NEJ TO SW-BYT-ARTIKEL                               
577889              ELSE                                                        
577890                 IF SLAG-KVLS = ZERO                                      
577891                    MOVE NEJ TO SW-BYT-ARTIKEL                            
577892                 END-IF                                                   
577893              END-IF                                                      
577894           END-IF                                                         
577895                                                                          
577896           ADD 1 TO TILK-IX                                               
577897        END-PERFORM                                                       
577898        IF TILK-IX = 1                                                    
577899           MOVE NEJ TO SW-BYT-ARTIKEL                                     
577900        END-IF                                                            
577901     ELSE                                                                 
577902        MOVE NEJ TO SW-BYT-ARTIKEL                                        
577903     END-IF                                                               
577904     .                                                                    
577905     EJECT                                                                
577906 ECWB-LDC-ARTIKELBYTE SECTION.                                            
577907                                                                          
577908     MOVE JA TO LDC-ARTIKELBYTE-SW                                        
577909     MOVE NEJ TO ALLT-SW                                                  
577910     MOVE NEJ TO OBKR-SW                                                  
577911     MOVE ORAD-WDQ401   TO HELP-ORAD-WDQ401                               
577912     MOVE OBKR-WDQ101   TO HELP-OBKR-WDQ101                               
577913     MOVE CDCA-W411CDCA TO HELP-CDCA-W411CDCA                             
577914     MOVE KERS-W411KERS TO HELP-KERS-W411KERS                             
577915     MOVE KVAN-W411KVAN TO HELP-KVAN-W411KVAN                             
577916     MOVE ORFK-W411ORFK TO HELP-ORFK-W411ORFK                             
577917     MOVE XDCA-W411XDCA TO HELP-XDCA-W411XDCA                             
577918     MOVE SDCA-W411SDCA TO HELP-SDCA-W411SDCA                             
577919     MOVE SPAR-W411SPAR TO HELP-SPAR-W411SPAR                             
577920     MOVE STOR-W411STOR TO HELP-STOR-W411STOR                             
577921     MOVE RELS-W411RELS TO HELP-RELS-W411RELS                             
577922     MOVE TILK-W411TILK TO HELP-TILK-W411TILK                             
577923     MOVE AREG-W411AREG TO HELP-AREG-W411AREG                             
577924     .                                                                    
577925     EJECT                                                                
577926 ECWC-AVSLUTA-15-CLEARING SECTION.                                        
577927                                                                          
577928     MOVE W-GMT-IDDC-CLEAR(2) TO ORAD-IDDC                                
577929                                WS-IDDC-SEEK                              
577930     MOVE JA      TO OBKR-SW                                              
577931     .                                                                    
577932     EJECT                                                                
577933 ECH-PREL-AVBOKNING-SDC2 SECTION.                                         
577934                                                                          
577935     MOVE 'ECH-PREL-AVB-SD'                TO   CURRENT-SECTION           
577936     IF (ALLT-OK OR KOLLA-ERS) AND                                        
577937        OHUV-FLOVRLEV = NEJ AND                                           
577938        OHUV-FLORDSPE = NEJ                                               
577941        AND ORAD-IDBIL = SPACE                                            
577942                                                                          
577943       PERFORM S40-HAMTA-WDB6-INFO                                        
577944       IF  DCS-SDC                                                        
577945                                                                          
577946         MOVE DCS-IDDC             TO DC01-IDDC                           
577947         IF (ORAD-IDSYSTEM = 'LDC ' OR 'TACD')                            
577948         AND ARB-TIRFS > 0                                                
577949         AND AREG-KDERS = 0                                               
577950         AND REPAIR-CLEARING                                              
577951           PERFORM S13-CALL-WORKDAY-LDC                                   
577952           IF WORK-TIAAMMDD-FOM <= DAGENS-DATUM                           
577953             MOVE NEJ              TO SDCA-FLORDSPE                       
577954           ELSE                                                           
577955             MOVE JA               TO SDCA-FLORDSPE                       
577956           END-IF                                                         
577957         ELSE                                                             
577958           MOVE NEJ                TO SDCA-FLORDSPE                       
577959         END-IF                                                           
577960         MOVE OHUV-IDDISTR         TO SDCA-IDDISTR                        
577961         MOVE OHUV-FLFORBI         TO SDCA-FLFORBI                        
577962         MOVE AREG-FLREFILL        TO SDCA-FLREFILL                       
577963         MOVE ORAD-IDARTNR         TO SDCA-IDARTNR                        
577964         MOVE ORAD-IDDC            TO SDCA-IDDC                           
577965         MOVE OHUV-IDDC-TVS        TO SDCA-IDDC-TVS                       
577966         MOVE ORAD-IDLEVNR         TO SDCA-IDLEVNR                        
577967         MOVE ORAD-IDSYSTEM        TO SDCA-IDSYSTEM                       
577968         MOVE ORAD-KDORDKL         TO SDCA-KDORDKL                        
577969         MOVE ORAD-KDORDING        TO SDCA-KDORDING                       
577970         MOVE AREG-KDPRODSL        TO SDCA-KDPRODSL                       
577971         MOVE ORAD-KVBEART-Q       TO SDCA-KVBEART-Q                      
577972         MOVE AREG-REDIRLEV        TO SDCA-REDIRLEV                       
577973         MOVE ORAD-FLSDCLEV        TO SDCA-FLSDCLEV                       
577974         MOVE +1                   TO SDCA-KDCALL                         
577975         MOVE +2                   TO SDCA-IXDCCLEAR                      
577976                                                                          
577977         CALL W411SDCA USING SDCA-W411SDCA SDCA-ARTS-PCB                  
577978                                           SDCA-WDB6-PCB                  
577979                                           SDCA-WDK9-PCB                  
577980                                           SDCA-WDR6-PCB                  
577981                                           SDCA-WDK6-PCB                  
577982                                           SDCA-WDQ4B-PCB                 
577983                                           SDCA-WDQ2-PCB                  
577984                                           SDCA-WDQ4-PCB                  
577985                                           SDCA-WDB6-2-PCB                
577986                                           SDCA-WDK6-2-PCB                
577987                                           SDCA-WDK7-2-PCB                
577988                                           SDCA-WDK7-3-PCB                
577989                                                                          
577990         MOVE SDCA-KDORDBEK TO SDCA-KDORDBEK-SECOND-SDC                   
577991         MOVE ZERO          TO SDCA-KDORDBEK                              
577992                                                                          
577993         IF SDCA-KDORDBEK-SECOND-SDC > ZERO                               
577994           IF SDCA-FLSDCLEV = JA AND SDCA-KDORDBEK-SECOND-SDC = 80        
577995             MOVE JA        TO OBKR-SW                                    
577996             MOVE NEJ       TO ALLT-SW                                    
577997           ELSE                                                           
577998             IF KOLLA-ERS                                                 
577999               IF PREPLANED-SW = JA                                       
578000                 MOVE ZERO            TO SDCA-KDORDBEK-SECOND-SDC         
578001                 IF SPAR-KDORDBEK = ZERO                                  
578002                   MOVE W-GMT-IDDC-CLEAR(3) TO ORAD-IDDC                  
578003                   MOVE ORAD-IDDC          TO WS-IDDC-SEEK                
578004                 ELSE                                                     
578005                   MOVE NEJ           TO KOLLA-ERS-SW                     
578006                 END-IF                                                   
578007               ELSE                                                       
578008                 MOVE ZERO            TO SDCA-KDORDBEK-FIRST-SDC          
578009                 IF SPAR-KDORDBEK = ZERO                                  
578010                  IF ORAD-IDDC = W-TILLK-DC                               
578011                     MOVE JA            TO OBKR-SW                        
578012                     MOVE NEJ           TO ALLT-SW                        
578013                                           KOLLA-ERS-SW                   
578014                    MOVE ZERO         TO SDCA-KDORDBEK-SECOND-SDC         
578015                  ELSE                                                    
578016                   MOVE W-GMT-IDDC-CLEAR(3) TO ORAD-IDDC                  
578017                   MOVE ORAD-IDDC     TO DC01-IDDC                        
578018                                         WS-IDDC-SEEK                     
578019                   IF DC01-IDDC = WC-CDC-SE                               
578020                      MOVE JA              TO CDC-MOVE-SW                 
578021                   END-IF                                                 
578022                  END-IF                                                  
578023                 ELSE                                                     
578024                   MOVE NEJ           TO KOLLA-ERS-SW                     
578025                 END-IF                                                   
578026               END-IF                                                     
578027             ELSE                                                         
578028               IF SDCA-KDORDBEK-SECOND-SDC = 15                           
578029                  IF SDCA-KDORDBEK-FIRST-SDC = 15                         
578030                     MOVE ZERO TO SDCA-KDORDBEK-FIRST-SDC                 
578031                  END-IF                                                  
578032                  MOVE W-GMT-IDDC-CLEAR(3) TO ORAD-IDDC                   
578033                                             WS-IDDC-SEEK                 
578034                  MOVE JA          TO OBKR-SW                             
578035               ELSE                                                       
578036                  MOVE JA          TO OBKR-SW                             
578037                  MOVE NEJ         TO ALLT-SW                             
578038               END-IF                                                     
578039             END-IF                                                       
578040           END-IF                                                         
578041         ELSE                                                             
578042              MOVE SDCA-ADLAGOMR     TO ORAD-ADLAGOMR                     
578043           MOVE SDCA-ADGANG          TO ORAD-ADGANG                       
578044           MOVE SDCA-ADPLATS         TO ORAD-ADPLATS                      
578045           MOVE SDCA-KVBEART-Q       TO ORAD-KVPREAVB                     
578046           MOVE NEJ                  TO ALLT-SW                           
578047           IF KOLLA-ERS  OR                                               
578048             (KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD)               
578049              MOVE NEJ               TO KOLLA-ERS-SW                      
578050              MOVE ZERO              TO KERS-KDORDBEK                     
578051              PERFORM S02-INITIALIZE-TILLK-TAB                            
578052              MOVE ZERO              TO SPAR-KDORDBEK                     
578053           END-IF                                                         
578054           IF (KERS-KDERS = +19 OR +29) AND PREPLANED-SW = NEJ            
578055              MOVE ZERO           TO SPAR-KDORDBEK                        
578056           END-IF                                                         
578057         END-IF                                                           
578058         MOVE SDCA-KDOI              TO ORAD-KDOI                         
578059         MOVE SDCA-CLEARGROUP        TO ORAD-CLEARGROUP                   
578060       END-IF                                                             
578061                                                                          
578062     END-IF                                                               
578063     .                                                                    
578064     EJECT                                                                
578065 ECX-PREL-AVBOKNING-SDC3 SECTION.                                         
578067     MOVE 'ECX-PREL-AVB-SDC'           TO   CURRENT-SECTION               
578068                                                                          
578069     IF (ALLT-OK OR KOLLA-ERS) AND                                        
578070        OHUV-FLOVRLEV = NEJ AND                                           
578071        OHUV-FLORDSPE = NEJ                                               
578074        AND ORAD-IDBIL = SPACE                                            
578075                                                                          
578076       PERFORM S40-HAMTA-WDB6-INFO                                        
578077       IF  DCS-SDC                                                        
578078                                                                          
578079         MOVE NEJ                  TO SDCA-FLORDSPE                       
578080         MOVE OHUV-IDDISTR         TO SDCA-IDDISTR                        
578081         MOVE OHUV-FLFORBI         TO SDCA-FLFORBI                        
578082         MOVE AREG-FLREFILL        TO SDCA-FLREFILL                       
578083         MOVE ORAD-IDARTNR         TO SDCA-IDARTNR                        
578084         MOVE ORAD-IDDC            TO SDCA-IDDC                           
578085         MOVE OHUV-IDDC-TVS        TO SDCA-IDDC-TVS                       
578086         MOVE ORAD-IDLEVNR         TO SDCA-IDLEVNR                        
578087         MOVE ORAD-IDSYSTEM        TO SDCA-IDSYSTEM                       
578088         MOVE ORAD-KDORDKL         TO SDCA-KDORDKL                        
578089         MOVE ORAD-KDORDING        TO SDCA-KDORDING                       
578090         MOVE AREG-KDPRODSL        TO SDCA-KDPRODSL                       
578091         MOVE ORAD-KVBEART-Q       TO SDCA-KVBEART-Q                      
578092         MOVE AREG-REDIRLEV        TO SDCA-REDIRLEV                       
578093         MOVE ORAD-FLSDCLEV        TO SDCA-FLSDCLEV                       
578094         MOVE +1                   TO SDCA-KDCALL                         
578095         MOVE +3                   TO SDCA-IXDCCLEAR                      
578096                                                                          
578097         CALL W411SDCA USING SDCA-W411SDCA SDCA-ARTS-PCB                  
578098                                           SDCA-WDB6-PCB                  
578099                                           SDCA-WDK9-PCB                  
578100                                           SDCA-WDR6-PCB                  
578101                                           SDCA-WDK6-PCB                  
578102                                           SDCA-WDQ4B-PCB                 
578103                                           SDCA-WDQ2-PCB                  
578104                                           SDCA-WDQ4-PCB                  
578105                                           SDCA-WDB6-2-PCB                
578106                                           SDCA-WDK6-2-PCB                
578107                                           SDCA-WDK7-2-PCB                
578108                                           SDCA-WDK7-3-PCB                
578109                                                                          
578110         IF SDCA-KDORDBEK > ZERO                                          
578111           IF SDCA-FLSDCLEV = JA AND SDCA-KDORDBEK = 80                   
578112              MOVE JA        TO OBKR-SW                                   
578113              MOVE NEJ       TO ALLT-SW                                   
578114           ELSE                                                           
578115             IF KOLLA-ERS                                                 
578116               IF PREPLANED-SW = JA                                       
578117                  MOVE NEJ             TO KOLLA-ERS-SW                    
578118                  MOVE ZERO            TO SDCA-KDORDBEK                   
578119                  IF SPAR-KDORDBEK = ZERO                                 
578120                       MOVE WC-CDC-SE TO ORAD-IDDC                        
578121                    MOVE ORAD-IDDC     TO WS-IDDC-SEEK                    
578122                  END-IF                                                  
578123               ELSE                                                       
578124                  MOVE ZERO         TO SDCA-KDORDBEK-SECOND-SDC           
578125                  IF SPAR-KDORDBEK = ZERO                                 
578126                   IF ORAD-IDDC = W-TILLK-DC                              
578127                      MOVE JA            TO OBKR-SW                       
578128                      MOVE NEJ           TO ALLT-SW                       
578129                                            KOLLA-ERS-SW                  
578130                      MOVE ZERO            TO SDCA-KDORDBEK               
578131                   ELSE                                                   
578132                    MOVE WC-CDC-SE     TO ORAD-IDDC                       
578133                    MOVE ORAD-IDDC     TO WS-IDDC-SEEK                    
578134                    MOVE JA            TO CDC-MOVE-SW                     
578135                   END-IF                                                 
578136                  END-IF                                                  
578137               END-IF                                                     
578138             ELSE                                                         
578139               IF SDCA-KDORDBEK = 15                                      
578140                  IF SDCA-KDORDBEK-SECOND-SDC = 15                        
578141                     MOVE ZERO TO SDCA-KDORDBEK-SECOND-SDC                
578142                  END-IF                                                  
578143                     MOVE WC-CDC-SE TO ORAD-IDDC                          
578144                                        WS-IDDC-SEEK                      
578145                  MOVE JA          TO OBKR-SW                             
578146               ELSE                                                       
578147                  MOVE JA          TO OBKR-SW                             
578148                  MOVE NEJ         TO ALLT-SW                             
578149               END-IF                                                     
578150             END-IF                                                       
578151           END-IF                                                         
578152         ELSE                                                             
578153              MOVE SDCA-ADLAGOMR     TO ORAD-ADLAGOMR                     
578154           MOVE SDCA-ADGANG          TO ORAD-ADGANG                       
578155           MOVE SDCA-ADPLATS         TO ORAD-ADPLATS                      
578156           MOVE SDCA-KVBEART-Q       TO ORAD-KVPREAVB                     
578157           MOVE NEJ                  TO ALLT-SW                           
578158           IF KOLLA-ERS  OR                                               
578159             (KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD)               
578160              MOVE NEJ               TO KOLLA-ERS-SW                      
578161              MOVE ZERO              TO KERS-KDORDBEK                     
578162              PERFORM S02-INITIALIZE-TILLK-TAB                            
578163              MOVE ZERO              TO SPAR-KDORDBEK                     
578164           END-IF                                                         
578165           IF (KERS-KDERS = +19 OR +29) AND PREPLANED-SW = NEJ            
578166              MOVE ZERO           TO SPAR-KDORDBEK                        
578167           END-IF                                                         
578168         END-IF                                                           
578169                                                                          
578170         MOVE SDCA-KDOI                TO ORAD-KDOI                       
578171         MOVE SDCA-CLEARGROUP          TO ORAD-CLEARGROUP                 
578172       END-IF                                                             
578173                                                                          
578174     END-IF                                                               
578175     .                                                                    
578176     EJECT                                                                
578177 ECG-PREL-AVBOKNING-XDC SECTION.                                          
578178     MOVE 'ECG-PREL-AVBOKNIN'               TO   CURRENT-SECTION          
578179                                                                          
578180     IF (ALLT-OK OR KOLLA-ERS OR SPAR-FLPUBCDC = YES)                     
578181        AND OHUV-FLOVRLEV = NEJ                                           
578182                                                                          
578184       IF ORAD-IDBIL = SPACE                                              
578185         PERFORM S40-HAMTA-WDB6-INFO                                      
578186         IF DCS-NDC AND ORAD-IDLEVNR = SPACE                              
578187                                                                          
578188              MOVE JA TO ALLT-SW                                          
578189              PERFORM ECGX-PREL-AVBOKNING-XDC                             
578190                                                                          
578191           IF XDCA-KDORDBEK > ZERO                                        
578192             IF SPAR-FLPUBCDC = YES                                       
578193***** SPAR-FLPUBCDC=YES MEANS THERE IS A BLOCK FOR CDC PUBL.DATE          
578194***** IF XDCA-KDORDBEK = 15 MEANS LINE IS MOVED TO ANOTHER DC             
578195***** THEN DAPUBL IS TESTED OK IN W411XDCA                                
578196              IF (XDCA-KDORDBEK = 15 OR 92 OR 98 OR 99)                   
578197                 AND XDCA-DAPUBL > ZERO                                   
578198                   MOVE ZERO TO SPAR-KDORDBEK                             
578199              ELSE                                                        
578200                   MOVE ZERO TO XDCA-KDORDBEK                             
578201                   IF XDCA-DAPUBL = ZERO                                  
578202                      MOVE 0              TO XDCA-KVPREAVB                
578203                                             XDCA-KVPRERO                 
578204                   END-IF                                                 
578205              END-IF                                                      
578206             END-IF                                                       
578207             IF KOLLA-ERS                                                 
578208                IF XDCA-KVPREAVB > 0                                      
578209                  MOVE ZERO          TO KERS-KDORDBEK                     
578210                  PERFORM S02-INITIALIZE-TILLK-TAB                        
578211                  MOVE ZERO          TO SPAR-KDORDBEK                     
578212                ELSE                                                      
578213                  MOVE ZERO          TO XDCA-KDORDBEK                     
578214                END-IF                                                    
578215             ELSE                                                         
578216               IF XDCA-KDORDBEK = 15                                      
578217                  IF SDCA-KDORDBEK-FIRST-SDC = 15                         
578218                     MOVE ZERO       TO SDCA-KDORDBEK-FIRST-SDC           
578219                  END-IF                                                  
578220                  IF SDCA-KDORDBEK-SECOND-SDC = 15                        
578221                     MOVE ZERO       TO SDCA-KDORDBEK-SECOND-SDC          
578222                  END-IF                                                  
578223                  IF SDCA-KDORDBEK = 15                                   
578224                     MOVE ZERO       TO SDCA-KDORDBEK                     
578225                  END-IF                                                  
578226               END-IF                                                     
578227               IF KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD            
578228                   MOVE ZERO         TO XDCA-KDORDBEK                     
578229               END-IF                                                     
578230             END-IF                                                       
578231             MOVE JA                 TO OBKR-SW                           
578232           ELSE                                                           
578233             IF KOLLA-ERS  OR                                             
578234               (KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD)             
578235               IF XDCA-KVPREAVB > 0                                       
578236                 MOVE ZERO           TO KERS-KDORDBEK                     
578237                 PERFORM S02-INITIALIZE-TILLK-TAB                         
578238                 MOVE ZERO           TO SPAR-KDORDBEK                     
578239               ELSE                                                       
578240                 MOVE JA             TO OBKR-SW                           
578241               END-IF                                                     
578242             ELSE                                                         
578243               IF XDCA-KVPREAVB > 0 AND SPAR-KDORDBEK = '54'              
578244                  MOVE ZERO             TO SPAR-KDORDBEK                  
578245               END-IF                                                     
578246             END-IF                                                       
578247             IF SPAR-FLPUBCDC = YES                                       
578248***** SPAR-FLPUBCDC=YES MEANS THERE IS A BLOCK FOR PUBL.DATE ON CD        
578249***** THAT BLOCK SHALL BE REMOVED IF CHECK ON DAPUBL FOR NDC IS OK        
578250               IF XDCA-DAPUBL > 0                                         
578251                 MOVE 0  TO SPAR-KDORDBEK                                 
578252                 MOVE JA  TO ALLT-SW                                      
578253                 MOVE NEJ TO OBKR-SW                                      
578254               ELSE                                                       
578255                 MOVE 0              TO XDCA-KVPREAVB                     
578256                                        XDCA-KVPRERO                      
578257               END-IF                                                     
578258             END-IF                                                       
578259           END-IF                                                         
578260           MOVE XDCA-ADLAGOMR        TO ORAD-ADLAGOMR                     
578261           MOVE XDCA-ADGANG          TO ORAD-ADGANG                       
578262           MOVE XDCA-ADPLATS         TO ORAD-ADPLATS                      
578263           MOVE XDCA-IDDC-OUT        TO ORAD-IDDC                         
578264           MOVE XDCA-IDDC-RO         TO ORAD-IDDC-RO                      
578265           MOVE XDCA-KDARTURS        TO ORAD-KDARTURS                     
578266           MOVE XDCA-KDOI            TO ORAD-KDOI                         
578267           MOVE XDCA-CLEARGROUP      TO ORAD-CLEARGROUP                   
578268           MOVE XDCA-KVPREAVB        TO ORAD-KVPREAVB                     
578269           MOVE XDCA-KVPRERO         TO ORAD-KVPRERO                      
578270           MOVE XDCA-TIREGDAT-OUT    TO ORAD-TIREGDAT                     
578271           MOVE XDCA-TIREGTID-OUT    TO ORAD-TIREGTID                     
578272           MOVE XDCA-VKART-OUT       TO ORAD-VKART                        
578273           MOVE XDCA-VKART-NTO       TO ORAD-VKART-NTO                    
578274           MOVE XDCA-VLARTNTO        TO ORAD-VLARTNTO                     
578275           MOVE NEJ                  TO ALLT-SW                           
578276         END-IF                                                           
578277                                                                          
578278       END-IF                                                             
578279                                                                          
578280     END-IF                                                               
578281     .                                                                    
578282     EJECT                                                                
578283 ECGX-PREL-AVBOKNING-XDC SECTION.                                         
578284                                                                          
578285* XDCA-INPUT                                                              
578286      MOVE +1 TO WS-INDEX                                                 
578287      PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                             
578288        MOVE W-GMT-IDDC-CLEAR  (WS-INDEX)                                 
578289                                TO XDCA-IDDC-CLEAR-IN(WS-INDEX)           
578290        ADD +1 TO WS-INDEX                                                
578291      END-PERFORM                                                         
578292                                                                          
578293     MOVE OHUV-IDDC-TVS        TO XDCA-IDDC-TVS                           
578294     MOVE GMT-FLLDCKND         TO XDCA-FLLDCKND                           
578295     MOVE ORAD-IDARTNR         TO XDCA-IDARTNR                            
578296     MOVE ORAD-IDDC            TO XDCA-IDDC                               
578297     MOVE ORAD-IDLEVNR         TO XDCA-IDLEVNR                            
578298     MOVE AREG-KDERS           TO XDCA-KDERS                              
578299     MOVE AREG-KDSORT          TO XDCA-KDSORT                             
578300     MOVE ORAD-KVBEART-Q       TO XDCA-KVBEART-Q                          
578301     MOVE AREG-KVQPACK-1       TO XDCA-KVQPACK-1                          
578302     MOVE ORAD-TIREGDAT        TO XDCA-TIREGDAT                           
578303     MOVE ORAD-TIREGTID        TO XDCA-TIREGTID                           
578304     MOVE ORAD-VKART           TO XDCA-VKART                              
578305     MOVE +1                   TO XDCA-KDCALL                             
578306                                                                          
578307* XDCA-OUTPUT                                                             
578308     MOVE SPACE                TO XDCA-IDDC-OUT                           
578309                                  XDCA-IDDC-RO                            
578310                                  XDCA-KDARTURS                           
578311                                  XDCA-KDOI                               
578312                                  XDCA-CLEARGROUP                         
578313     MOVE ZERO                 TO XDCA-ADLAGOMR                           
578314                                  XDCA-ADGANG                             
578315                                  XDCA-ADPLATS                            
578316                                  XDCA-KDORDBEK                           
578317                                  XDCA-KVPREAVB                           
578318                                  XDCA-KVPRERO                            
578319                                  XDCA-TIREGDAT-OUT                       
578320                                  XDCA-TIREGTID-OUT                       
578321                                  XDCA-VKART-OUT                          
578322                                  XDCA-VKART-NTO                          
578323                                  XDCA-VLARTNTO                           
578325     MOVE ZERO                 TO XDCA-KVOKS-DAG                          
578326                                  XDCA-KVOKS-BULK                         
578327                                                                          
578328     IF XDCA-DAPUBL NOT = 99999999                                        
578329        MOVE ZERO              TO XDCA-DAPUBL                             
578330     END-IF                                                               
578331     CALL W411XDCA USING OHUV-WDQ201 XDCA-W411XDCA                        
578332          XDCA-USEA-PCB                                                   
578333          XDCA-WDB6-PCB XDCA-WDK6-PCB XDCA-WDK7-PCB                       
578334          XDCA-WDK9-PCB XDCA-WDL6-PCB XDCA-WDQ4B-PCB                      
578335          XDCA-WDQ2-PCB XDCA-WDQ4-PCB XDCA-WDR6-PCB                       
578336          XDCA-WDB6-2-PCB XDCA-WDK6-2-PCB XDCA-WDK7-2-PCB                 
578337          XDCA-WDK7-3-PCB                                                 
578338                                                                          
578339     .                                                                    
578340     EJECT                                                                
578341 ECT-KOMPLETTERA-RANSONERING SECTION.                                     
578342                                                                          
578343     MOVE 'ECT-RANS'                       TO   CURRENT-SECTION           
578344     IF ALLT-OK OR LDC-ARTIKELBYTE                                        
578345                OR (CDC-MOVE AND PREPLANED-SW = NEJ)                      
578346     MOVE ORAD-BERADREF        TO RANS-BERADREF                           
578347     MOVE OHUV-FLEMBORD        TO RANS-FLEMBORD                           
578348     MOVE OHUV-FLFORBI         TO RANS-FLFORBI                            
578349                                                                          
578350     IF ORAD-IDBIL = SPACE OR ORAD-IDSYSTEM = 'SOFT'                      
578351       MOVE OHUV-FLORDSPE      TO RANS-FLORDSPE                           
578352     ELSE                                                                 
578353       MOVE JA                 TO RANS-FLORDSPE                           
578354     END-IF                                                               
578355                                                                          
578356     MOVE OHUV-FLOVRLEV        TO RANS-FLOVRLEV                           
578357     MOVE ORAD-IDKAMPRF        TO RANS-IDKAMPRF                           
578358     MOVE ORAD-IDARTNR         TO RANS-IDARTNR                            
578359     MOVE ORAD-IDLEVNR         TO RANS-IDLEVNR                            
578360     MOVE OHUV-IDRFTAB         TO RANS-IDRFTAB                            
578361     MOVE ORAD-TIRODAT         TO RANS-TIRODAT                            
578362     MOVE OHUV-KDORDKL         TO RANS-KDORDKL                            
578363     MOVE +1                   TO RANS-KDORDBEH                           
578364     MOVE ORAD-KVBEART-Q       TO RANS-KVBEART-Q                          
578365     MOVE ORAD-KDTPOTYP        TO RANS-KDTPOTYP                           
578366     MOVE AREG-KDERS           TO RANS-KDERS                              
578367     MOVE AREG-KVLS            TO RANS-KVLS                               
578368     MOVE AREG-KVPB-SATS       TO RANS-KVPB-SATS                          
578369     MOVE AREG-KVPB-SEP        TO RANS-KVPB-SEP                           
578370     MOVE AREG-REDIRLEV        TO RANS-REDIRLEV                           
578371     MOVE AREG-KVRESS          TO RANS-KVRESS                             
578372     MOVE AREG-KVSPANT         TO RANS-KVSPANT                            
578373     MOVE AREG-KVUTRS          TO RANS-KVUTRS                             
578374     MOVE AREG-TIDISPIN        TO RANS-TIDISPIN                           
578375                                                                          
578376     MOVE AREG-KDPRODSL        TO TEST-KDPRODSL                           
578377     IF KDPRODSL-BIMA                                                     
578378         MOVE 1                TO ORAD-RERF-RAD                           
578379                                  RANS-RERF-RAD-UT                        
578380         MOVE ZERO             TO RANS-SUTPO-PB-UT                        
578381                                  RANS-SUTPO-EJPB-UT                      
578382                                  RANS-RERF-ART-UT                        
578383     ELSE                                                                 
578384     CALL W411RANS USING RANS-W411RANS RANS-XXKM-PCB RANS-ARTM-PCB        
578385                                       RANS-ARTS-PCB                      
578386                                                                          
578387       MOVE RANS-RERF-RAD-UT     TO ORAD-RERF-RAD                         
578388                                                                          
578389     END-IF                                                               
578390     END-IF                                                               
578391     .                                                                    
578392     EJECT                                                                
578400                                                                          
578417 ECR-PREL-AVBOKNING-CDC SECTION.                                          
578418                                                                          
578419     MOVE 'ECR-PREL-AVBOKNING-C'   TO   CURRENT-SECTION                   
578420     IF ALLT-OK OR LDC-ARTIKEL-TILLBAKA                                   
578421                OR (CDC-MOVE AND PREPLANED-SW = NEJ)                      
578422                                                                          
578423     MOVE TILK-FLFINLV(WS-INDEX-TILLK)                                    
578424                               TO CDCA-FLFINLV-IN                         
578425     MOVE OHUV-FLFORBI         TO CDCA-FLFORBI-IN                         
578426     MOVE OHUV-FLPRELRO        TO CDCA-FLPRELRO-IN                        
578427     MOVE ORAD-FLRESTN         TO CDCA-FLRESTN-IN                         
578428     MOVE ORFK-FLSLATT (WS-INDEX-ORFK)                                    
578429                               TO CDCA-FLSLATT-IN                         
578430     MOVE ORAD-IDARTNR         TO CDCA-IDARTNR-IN                         
578431     MOVE OHUV-IDDISTR         TO CDCA-IDDISTR-IN                         
578432     MOVE ORAD-IDDC            TO CDCA-IDDC-IN                            
578433     MOVE ORAD-IDKAMPRF        TO CDCA-IDKAMPRF-IN                        
578434     MOVE ORAD-IDKUNDRF-RO     TO CDCA-IDKUNDRF-RO-IN                     
578435     MOVE ORAD-IDSYSTEM        TO CDCA-IDSYSTEM-IN                        
578436     MOVE ORAD-IDLEVNR         TO CDCA-IDLEVNR-IN                         
578437     MOVE AREG-KDERS           TO CDCA-KDERS-IN                           
578438     MOVE ORAD-KDKVBRYT        TO CDCA-KDKVBRYT-IN                        
578439     MOVE ORAD-KDORDKL         TO CDCA-KDORDKL-IN                         
578440     MOVE ORAD-KDPRODSL        TO CDCA-KDPRODSL-IN                        
578441     MOVE AREG-KDSORT          TO CDCA-KDSORT-IN                          
578442     MOVE SPACE                TO CDCA-KDPROTYP-IN                        
578443     MOVE ORAD-KDTPOTYP        TO CDCA-KDTPOTYP-IN                        
578444     MOVE AREG-KDUART          TO CDCA-KDUART-IN                          
578445     MOVE ORAD-KVBEART         TO CDCA-KVBEART-IN                         
578446     MOVE ORAD-KVBEART-Q       TO CDCA-KVBEART-Q-IN                       
578454     MOVE AREG-KVQPACK-0       TO CDCA-KVQPACK-0-IN                       
578455     MOVE AREG-KVQPACK-1       TO CDCA-KVQPACK-1-IN                       
578456     MOVE AREG-IDFKNGRP        TO CDCA-IDFKNGRP-IN                        
578457                                                                          
578458     PERFORM S40-HAMTA-WDB6-INFO                                          
578459                                                                          
578460     MOVE RANS-RERF-RAD-UT   TO CDCA-RERF-RAD-IN                          
578461     MOVE RANS-RERF-ART-UT   TO CDCA-RERF-ART-IN                          
578462     MOVE OHUV-RESLATT         TO CDCA-RESLATT-IN                         
578463                                                                          
578465     IF ORAD-IDBIL = SPACE                                                
578466       MOVE OHUV-FLORDSPE      TO CDCA-FLORDSPE-IN                        
578467     ELSE                                                                 
578468       MOVE JA                 TO CDCA-FLORDSPE-IN                        
578469     END-IF                                                               
578470                                                                          
578471     MOVE OHUV-FLOVRLEV        TO CDCA-FLOVRLEV-IN                        
578472     MOVE AREG-KVAKS-CDC       TO CDCA-KVAKS-CDC-IN                       
578473     MOVE AREG-KVAKS-PAV       TO CDCA-KVAKS-PAV-IN                       
578474     MOVE AREG-KVLS            TO CDCA-KVLS-IN                            
578475     MOVE AREG-KVRESS          TO CDCA-KVRESS-IN                          
578476     MOVE AREG-KVSPANT         TO CDCA-KVSPANT-IN                         
578477     MOVE AREG-KVUTRS          TO CDCA-KVUTRS-IN                          
578478     MOVE AREG-KVSPARR-KVAL    TO CDCA-KVSPARR-KVAL-IN                    
578479     MOVE ORAD-IDKUNDNR        TO CDCA-IDKUNDNR-IN                        
578480     MOVE ORAD-BERADREF        TO CDCA-BERADREF-IN                        
578481     MOVE JA                   TO CDC-CALL-SW                             
578482                                                                          
578483     IF KOLLA-ERS                                                         
578484        MOVE +2                   TO CDCA-KDCALL                          
578485        CALL W411CDCA USING CDCA-W411CDCA CDCA-ARTM-PCB                   
578486                                          CDCA-INLB-PCB                   
578487                                          CDCA-WDB2-PCB                   
578488                                          CDCA-WDC1-PCB                   
578489                                                                          
578492        IF (CDCA-KDORDBEK-UT NOT = 0) OR                                  
578493           (CDCA-KVPREAVB-UT <= 0)                                        
578494           MOVE NEJ            TO CDC-CALL-SW                             
578495           MOVE ZEROES        TO CDCA-KDORDBEK-UT                         
578496           IF (KOLLA-ERS AND KERS-KDORDBEK > ZERO)                        
578497           OR (KERS-KDORDBEK = 61 AND EJ-TILLKOMMANDE-RAD)                
578498           OR SPAR-KDORDBEK = 54                                          
578499              PERFORM S12-SPACE-SDCA-KDORDBEK                             
578500           END-IF                                                         
578501        END-IF                                                            
578502     END-IF                                                               
578503                                                                          
578504     IF CDC-CALL                                                          
578505        MOVE +1                   TO CDCA-KDCALL                          
578506        CALL W411CDCA USING CDCA-W411CDCA CDCA-ARTM-PCB                   
578507                                          CDCA-INLB-PCB                   
578508                                          CDCA-WDB2-PCB                   
578509                                          CDCA-WDC1-PCB                   
578510                                                                          
578511        MOVE CDCA-FLAKPLOC-UT     TO ORAD-FLAKPLOC                        
578512                                                                          
578513        IF OHUV-FLOVRLEV = JA OR OHUV-FLORDSPE = JA                       
578514          OR ORAD-IDBIL NOT = SPACE                                       
578515          CONTINUE                                                        
578516        ELSE                                                              
578517          IF (KERS-KDERS = 0) OR (KERS-KDERS > 10 AND                     
578518                               KERS-KDORDBEK = 0)                         
578519             CONTINUE                                                     
578520          ELSE                                                            
578521             IF KERS-KDERS > 0 AND < 10                                   
578522                IF CDCA-KVPREAVB-UT = +0 AND CDCA-KVPRERO-UT = +0         
578523                   MOVE CDCA-KVBEART-Q-UT TO CDCA-KVPRERO-UT              
578524                END-IF                                                    
578525                PERFORM S02-INITIALIZE-TILLK-TAB                          
578526                MOVE ZERO           TO KERS-KDORDBEK                      
578527                MOVE NEJ            TO TILLK-SW                           
578528             ELSE                                                         
578529*****FOR KDERS>10,DONT SWITCH IF A HAS STOCKS IN CDC                      
578530              IF PREPLANED-SW = NEJ                                       
578531                IF CDCA-KVPREAVB-UT > 0                                   
578532                  IF KOLLA-ERS    OR                                      
578533                    (KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD)        
578534                     MOVE NEJ               TO KOLLA-ERS-SW               
578535                     MOVE ZERO              TO KERS-KDORDBEK              
578536                     PERFORM S02-INITIALIZE-TILLK-TAB                     
578537                     MOVE ZERO              TO SPAR-KDORDBEK              
578538                  ELSE                                                    
578539                     IF KERS-KDERS = +19 OR +29                           
578540                        MOVE ZERO           TO SPAR-KDORDBEK              
578541                     END-IF                                               
578542                  END-IF                                                  
578543                ELSE                                                      
578544                  IF (CDCA-KVPREAVB-UT <= 0) AND                          
578545                     (CDCA-KDORDBEK-UT = 92 OR 99)                        
578546                     MOVE ZEROES        TO CDCA-KDORDBEK-UT               
578547                  END-IF                                                  
578548                  MOVE ZEROES           TO CDCA-KVPREAVB-UT               
578549                                           CDCA-KVPRERO-UT                
578550                  IF (KOLLA-ERS AND KERS-KDORDBEK > ZERO)                 
578551                  OR (KERS-KDORDBEK = 61 AND EJ-TILLKOMMANDE-RAD)         
578552                  OR SPAR-KDORDBEK = 54                                   
578553                     PERFORM S12-SPACE-SDCA-KDORDBEK                      
578554                  END-IF                                                  
578555                END-IF                                                    
578556              END-IF                                                      
578557             END-IF                                                       
578558          END-IF                                                          
578559                                                                          
578560          MOVE CDCA-KVPREAVB-UT     TO ORAD-KVPREAVB                      
578561          MOVE CDCA-KVPRERO-UT      TO ORAD-KVPRERO                       
578562          MOVE CDCA-KVSLATT-UT      TO ORAD-KVSLATT                       
578563          MOVE CDCA-RERF-RAD-UT     TO ORAD-RERF-RAD                      
578564          IF ORAD-IDLEVNR NOT = SPACE                                     
578565             CONTINUE                                                     
578566          ELSE                                                            
578567             MOVE CDCA-KVBEART-UT   TO ORAD-KVBEART                       
578568             MOVE CDCA-KVBEART-Q-UT TO ORAD-KVBEART-Q                     
578569          END-IF                                                          
578570        END-IF                                                            
578571                                                                          
578572        IF CDCA-KDORDBEK-UT > ZERO                                        
578573           MOVE JA                TO OBKR-SW                              
578574        END-IF                                                            
578575        MOVE AREG-ADLAGOMR     TO ORAD-ADLAGOMR                           
578576        MOVE AREG-ADGANG       TO ORAD-ADGANG                             
578577        MOVE AREG-ADPLATS      TO ORAD-ADPLATS                            
578578                                                                          
578579        END-IF                                                            
578580     END-IF                                                               
578581     .                                                                    
578582     EJECT                                                                
578583 ECS-SKRIV-OBKR-OCH-VOR-RAD SECTION.                                      
578584                                                                          
578585     MOVE 'ECS-SKRIV-OBKR-OCH'        TO   CURRENT-SECTION                
578586*--- EFTERSOM KVPREAVB OCH KVPRERO ENDAST SKALL SKRIVAS PÅ                
578587*    DEN SISTA ORDERBEKRÄFTELSERADEN 'SLÄPAR' ISRT AV RADEN               
578588*    OBKR-SW SÄTTS = JA NÄR EN ORDERBEKRÄFTELSE RAD SKALL                 
578589*    SKRIVAS.  DENNA TESTAR VI PÅ SIST I SECTIONEN FÖR ATT                
578590*    FÅ MED DEN SISTA ORDERBEKRÄFTELSEN MED KVPREAVB OCH KVPRERO          
578591*---                                                                      
578592     PERFORM ECSA-REDIGERA-OBKR-RAD                                       
578593                                                                          
578594     IF TILLKOMMANDE-RAD                                                  
578595        IF KERS-KDORDBEK = 41                                             
578596           MOVE KERS-KDORDBEK     TO OBKR-KDORDBEK                        
578597           MOVE '4258KER1'        TO OBKR-IDPGM                           
578598           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
578599                                  TO OBKR-KVBEART-TILLK                   
578600           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
578601              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
578602             MOVE +0              TO OBKR-DIERS-KVOT                      
578603           ELSE                                                           
578604             COMPUTE OBKR-DIERS-KVOT =                                    
578605                                  TILK-DIERS-TILLK(WS-INDEX-TILLK)        
578606                                  / TILK-DIERS-ERS(WS-INDEX-TILLK)        
578607           END-IF                                                         
578608           MOVE 'S'               TO OBKR-SW                              
578609        END-IF                                                            
578610     END-IF                                                               
578611                                                                          
578612     IF ORFK-KDORDBEK(WS-INDEX-ORFK) > 0                                  
578613       AND ORFK-KDORDBEK(WS-INDEX-ORFK) NOT = 56                          
578614*----(KOD  58, 59, 98)                                                    
578615        IF OBKR-SKRIVEN                                                   
578616           PERFORM IMS-ISRT-WDQ101                                        
578617           ADD +1              TO OBKR-IDSEKVNR                           
578618        END-IF                                                            
578619                                                                          
578620        MOVE ORFK-KDORDBEK(WS-INDEX-ORFK)                                 
578621                               TO OBKR-KDORDBEK                           
578622        MOVE '4258ORFK'        TO OBKR-IDPGM                              
578623        MOVE 'S'               TO OBKR-SW                                 
578624     END-IF                                                               
578625     EJECT                                                                
578626                                                                          
578627     IF KVAN-KDORDBEK-UT > +0                                             
578628*----(KOD 43, 44)                                                         
578629        IF OBKR-SKRIVEN                                                   
578630           PERFORM IMS-ISRT-WDQ101                                        
578631           ADD +1              TO OBKR-IDSEKVNR                           
578632        END-IF                                                            
578633        IF TILLKOMMANDE-RAD                                               
578634           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
578635                               TO OBKR-KVBEART-TILLK                      
578636           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
578637              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
578638             MOVE +0              TO OBKR-DIERS-KVOT                      
578639           ELSE                                                           
578640             COMPUTE OBKR-DIERS-KVOT =                                    
578641                                  TILK-DIERS-TILLK(WS-INDEX-TILLK)        
578642                                  / TILK-DIERS-ERS(WS-INDEX-TILLK)        
578643           END-IF                                                         
578644        END-IF                                                            
578645        MOVE KVAN-KDORDBEK-UT  TO OBKR-KDORDBEK                           
578646        MOVE '4258KVAN'        TO OBKR-IDPGM                              
578647        MOVE KVAN-KVBEART-IN   TO OBKR-KVBEART                            
578648        MOVE KVAN-KVBEART-Q-UT TO OBKR-KVBEART-Q                          
578649        MOVE 'S'               TO OBKR-SW                                 
578650     END-IF                                                               
578651     EJECT                                                                
578652                                                                          
578653     IF KERS-KDORDBEK > +0                                                
578654*----(KOD 41, 61)                                                         
578655                                                                          
578656        IF KERS-KDORDBEK = 61                                             
578657*----FÖR KOD 41 OCH 42 SKER ORDERBEKRÄFTELSE ENDAST PÅ TILL-              
578658*----KOMMANDE ARTIKLAR EFTER DET ATT DESSA HAR GENOMGÅTT                  
578659*----RADBEHANDLINGEN                                                      
578660           IF OBKR-SKRIVEN                                                
578661              PERFORM IMS-ISRT-WDQ101                                     
578662              ADD +1           TO OBKR-IDSEKVNR                           
578663           END-IF                                                         
578664           MOVE KERS-KDORDBEK  TO OBKR-KDORDBEK                           
578665           MOVE '4258KER2'     TO OBKR-IDPGM                              
578666           MOVE 'S'            TO OBKR-SW                                 
578667           PERFORM S20-OBKR-FRAN-TILLK-TAB                                
578668           PERFORM S02-INITIALIZE-TILLK-TAB                               
578669        ELSE                                                              
578670           IF NOT TILLKOMMANDE-RAD                                        
578671              IF OBKR-SKRIVEN                                             
578672                 PERFORM IMS-ISRT-WDQ101                                  
578673                 ADD +1        TO OBKR-IDSEKVNR                           
578674              END-IF                                                      
578675              MOVE 'S'            TO OBKR-SW                              
578676              MOVE KERS-KDORDBEK  TO OBKR-KDORDBEK                        
578677              MOVE '4258KER3'     TO OBKR-IDPGM                           
578678           END-IF                                                         
578679        END-IF                                                            
578680     END-IF                                                               
578681     EJECT                                                                
578682                                                                          
578683     IF SPAR-KDORDBEK > ZERO                                              
578684*----(KOD 51, 52, 53, 54, 55, 57, 58, 66, 67, 68, 80, 90)                 
578685        IF OBKR-SKRIVEN                                                   
578686           PERFORM IMS-ISRT-WDQ101                                        
578687           ADD +1              TO OBKR-IDSEKVNR                           
578688        END-IF                                                            
578689        IF TILLKOMMANDE-RAD                                               
578690           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
578691                               TO OBKR-KVBEART-TILLK                      
578692           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
578693              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
578694             MOVE +0              TO OBKR-DIERS-KVOT                      
578695           ELSE                                                           
578696             COMPUTE OBKR-DIERS-KVOT =                                    
578697                                  TILK-DIERS-TILLK(WS-INDEX-TILLK)        
578698                                  / TILK-DIERS-ERS(WS-INDEX-TILLK)        
578699           END-IF                                                         
578700        END-IF                                                            
578701        MOVE SPAR-KDORDBEK                                                
578702                               TO OBKR-KDORDBEK                           
578703        MOVE '4258SPAR'        TO OBKR-IDPGM                              
578704        MOVE 'S'               TO OBKR-SW                                 
578705     END-IF                                                               
578706     EJECT                                                                
578707                                                                          
578805     IF RELS-KDORDBEK > 0                                                 
578806*----(KOD 56)                                                             
578807          IF OBKR-SKRIVEN                                                 
578808             PERFORM IMS-ISRT-WDQ101                                      
578809             ADD +1              TO OBKR-IDSEKVNR                         
578810          END-IF                                                          
578811          MOVE RELS-KDORDBEK     TO OBKR-KDORDBEK                         
578812          MOVE '4258ORFK'        TO OBKR-IDPGM                            
578813          MOVE 'S'               TO OBKR-SW                               
578814       END-IF                                                             
578815    EJECT                                                                 
578842                                                                          
578843     IF XDCA-KDORDBEK > ZERO                                              
578845*----(KOD 15, 53, 55, 80, 92, 98, 99)                                     
578846        IF OBKR-SKRIVEN                                                   
578847           PERFORM IMS-ISRT-WDQ101                                        
578848           ADD +1              TO OBKR-IDSEKVNR                           
578849        END-IF                                                            
578850        IF TILLKOMMANDE-RAD                                               
578851           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
578852                               TO OBKR-KVBEART-TILLK                      
578853           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
578854              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
578855             MOVE +0              TO OBKR-DIERS-KVOT                      
578856           ELSE                                                           
578857             COMPUTE OBKR-DIERS-KVOT =                                    
578858                                  TILK-DIERS-TILLK(WS-INDEX-TILLK)        
578859                                  / TILK-DIERS-ERS(WS-INDEX-TILLK)        
578860           END-IF                                                         
578861        END-IF                                                            
578862                                                                          
578863        IF XDCA-KDORDBEK NOT = 15                                         
578864          IF OHUV-IDDC-TVS = SPACE                                        
578865            IF OHUV-IDDC-PRIM   NOT = XDCA-IDDC-OUT                       
578866              MOVE 15          TO OBKR-KDORDBEK                           
578867              MOVE IDPGM       TO OBKR-IDPGM                              
578868              MOVE 'S'         TO OBKR-SW                                 
578869                                                                          
578870* FÖR ATT SLIPPA DUBBLA 15-BEKRÄFTELSER NOLLAR VI EV. SDC                 
578871              IF SDCA-KDORDBEK-FIRST-SDC = 15                             
578872                 MOVE ZERO     TO SDCA-KDORDBEK-FIRST-SDC                 
578873              END-IF                                                      
578874              IF SDCA-KDORDBEK-SECOND-SDC = 15                            
578875                 MOVE ZERO     TO SDCA-KDORDBEK-SECOND-SDC                
578876              END-IF                                                      
578877              IF SDCA-KDORDBEK = 15                                       
578878                 MOVE ZERO     TO SDCA-KDORDBEK                           
578879              END-IF                                                      
578880            END-IF                                                        
578881            IF OBKR-SKRIVEN                                               
578882              PERFORM IMS-ISRT-WDQ101                                     
578883              ADD +1           TO OBKR-IDSEKVNR                           
578884            END-IF                                                        
578885          END-IF                                                          
578886        END-IF                                                            
578887        IF XDCA-KDORDBEK = 80                                             
578888           MOVE ORAD-KVBEART-Q TO OBKR-KVANNANT                           
578889        END-IF                                                            
578890        MOVE XDCA-KDORDBEK     TO OBKR-KDORDBEK                           
578891        MOVE '4258XDCA'        TO OBKR-IDPGM                              
578892        MOVE 'S'               TO OBKR-SW                                 
578893     END-IF                                                               
578894     EJECT                                                                
578895                                                                          
578896     IF SDCA-KDORDBEK-SECOND-SDC > ZERO                                   
578898*----(KOD 15, 53, 80, 92)                                                 
578899        IF OBKR-SKRIVEN                                                   
578900           PERFORM IMS-ISRT-WDQ101                                        
578901           ADD +1              TO OBKR-IDSEKVNR                           
578902        END-IF                                                            
578903        IF TILLKOMMANDE-RAD                                               
578904           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
578905                               TO OBKR-KVBEART-TILLK                      
578906           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
578907              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
578908             MOVE +0              TO OBKR-DIERS-KVOT                      
578909           ELSE                                                           
578910             COMPUTE OBKR-DIERS-KVOT =                                    
578911                                  TILK-DIERS-TILLK(WS-INDEX-TILLK)        
578912                                  / TILK-DIERS-ERS(WS-INDEX-TILLK)        
578913           END-IF                                                         
578914        END-IF                                                            
578915        IF SDCA-KDORDBEK-SECOND-SDC = 80                                  
578916           MOVE ORAD-KVBEART-Q TO OBKR-KVANNANT                           
578917        END-IF                                                            
578918        IF SDCA-KDORDBEK-SECOND-SDC = 80 OR 92                            
578919           MOVE 0              TO OBKR-IDARTNR-TILLK                      
578920        END-IF                                                            
578921        MOVE SDCA-KDORDBEK-SECOND-SDC TO OBKR-KDORDBEK                    
578922        MOVE '4258SDCA'              TO OBKR-IDPGM                        
578923        MOVE 'S'                     TO OBKR-SW                           
578924     END-IF                                                               
578925     EJECT                                                                
578926                                                                          
578927     IF SDCA-KDORDBEK-FIRST-SDC > ZERO                                    
578929     OR LDC-ARTIKEL-BYTT                                                  
578930*----(KOD 15, 53, 80, 92)                                                 
578931        IF OBKR-SKRIVEN                                                   
578932           IF LDC-ARTIKEL-BYTT                                            
578933              MOVE 'N'  TO OBKR-FLTILLK                                   
578934              MOVE ZERO TO OBKR-IDARTNR-TILLK                             
578935           ELSE                                                           
578936           PERFORM IMS-ISRT-WDQ101                                        
578937           ADD +1              TO OBKR-IDSEKVNR                           
578938           END-IF                                                         
578939        END-IF                                                            
578941        IF SDCA-KDORDBEK-FIRST-SDC >= ZERO                                
578942        OR LDC-ARTIKEL-TILLBAKA                                           
578943           IF TILLKOMMANDE-RAD                                            
578944              MOVE TILK-KVBEART(WS-INDEX-TILLK)                           
578945                                  TO OBKR-KVBEART-TILLK                   
578946              IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                  
578947                 TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                       
578948                MOVE +0              TO OBKR-DIERS-KVOT                   
578949              ELSE                                                        
578950                COMPUTE OBKR-DIERS-KVOT =                                 
578951                                  TILK-DIERS-TILLK(WS-INDEX-TILLK)        
578952                                  / TILK-DIERS-ERS(WS-INDEX-TILLK)        
578953              END-IF                                                      
578954           END-IF                                                         
578955           IF SDCA-KDORDBEK-FIRST-SDC = 80                                
578956              MOVE ORAD-KVBEART-Q TO OBKR-KVANNANT                        
578957           END-IF                                                         
578958           IF SDCA-KDORDBEK-FIRST-SDC = 80 OR 92                          
578959              MOVE 0              TO OBKR-IDARTNR-TILLK                   
578960           END-IF                                                         
578961           MOVE SDCA-KDORDBEK-FIRST-SDC TO OBKR-KDORDBEK                  
578962           MOVE '4258SDCA'           TO OBKR-IDPGM                        
578963           MOVE 'S'                  TO OBKR-SW                           
578964        END-IF                                                            
578965     END-IF                                                               
578966     EJECT                                                                
578967                                                                          
578968     IF SDCA-KDORDBEK > ZERO                                              
578970*----(KOD 15, 53, 80, 92)                                                 
578971        IF OBKR-SKRIVEN                                                   
578972           PERFORM IMS-ISRT-WDQ101                                        
578973           ADD +1              TO OBKR-IDSEKVNR                           
578974        END-IF                                                            
578975        IF TILLKOMMANDE-RAD                                               
578976           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
578977                               TO OBKR-KVBEART-TILLK                      
578978           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
578979              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
578980             MOVE +0              TO OBKR-DIERS-KVOT                      
578981           ELSE                                                           
578982             COMPUTE OBKR-DIERS-KVOT =                                    
578983                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
578984                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
578985           END-IF                                                         
578986        END-IF                                                            
578987        IF SDCA-KDORDBEK = 80                                             
578988           MOVE ORAD-KVBEART-Q TO OBKR-KVANNANT                           
578989        END-IF                                                            
578990        IF SDCA-KDORDBEK = 80 OR 92                                       
578991           MOVE 0              TO OBKR-IDARTNR-TILLK                      
578992        END-IF                                                            
578993        MOVE SDCA-KDORDBEK     TO OBKR-KDORDBEK                           
578994        MOVE '4258SDCA'        TO OBKR-IDPGM                              
578995        MOVE 'S'               TO OBKR-SW                                 
578996     END-IF                                                               
578997                                                                          
578998     EJECT                                                                
579017     IF CDCA-KDORDBEK-UT > +0                                             
579019     AND NOT LDC-ARTIKEL-TILLBAKA                                         
579020*----(KOD 80, 92, 99)                                                     
579021        IF OBKR-SKRIVEN                                                   
579022           PERFORM IMS-ISRT-WDQ101                                        
579023           ADD +1              TO OBKR-IDSEKVNR                           
579024        END-IF                                                            
579025        IF TILLKOMMANDE-RAD                                               
579026           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
579027                               TO OBKR-KVBEART-TILLK                      
579028           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
579029              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
579030             MOVE +0              TO OBKR-DIERS-KVOT                      
579031           ELSE                                                           
579032             COMPUTE OBKR-DIERS-KVOT =                                    
579033                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
579034                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
579035           END-IF                                                         
579036        END-IF                                                            
579037        IF CDCA-KDORDBEK-UT = +80                                         
579038           MOVE CDCA-KVANNANT-UT  TO OBKR-KVANNANT                        
579039        END-IF                                                            
579040        MOVE CDCA-KDORDBEK-UT  TO OBKR-KDORDBEK                           
579041        MOVE '4258CDCA'        TO OBKR-IDPGM                              
579042        MOVE 'S'               TO OBKR-SW                                 
579043     END-IF                                                               
579044*                                                                         
579045* PÅ SISTA RADEN FÖR KUNDENS NORMALA CL LÄGGS DE AVBOKADE ANTALEN!        
579046     IF OBKR-SKRIVEN                                                      
579047     AND NOT LDC-ARTIKELBYTE                                              
579048        IF OBKR-KDORDBEK = 61 AND                                         
579049         (OBKR-KVPREAVB = 0 AND OBKR-KVPRERO = 0)                         
579050           CONTINUE                                                       
579051        ELSE                                                              
579052           MOVE ORAD-KVPREAVB     TO OBKR-KVPREAVB                        
579053           MOVE ORAD-KVPRERO      TO OBKR-KVPRERO                         
579054        END-IF                                                            
579055******** TA BORT FRÅGAN FÖR ERSATT ARTIKEL                                
579056        IF KERS-KDORDBEK > 0 AND NOT TILLKOMMANDE-RAD                     
579057           PERFORM S23-DELETE-PRICE-Q-LINE                                
579058           INITIALIZE OBKR-DEAL-PR-LINE                                   
579059           MOVE 'N/A'          TO OBKR-KDVALISO                           
579060        END-IF                                                            
579061*************TL 030514                                                    
579062         IF LDC-ARTIKEL-TILLBAKA                                          
579063            MOVE NEJ          TO OBKR-FLTILLK                             
579064            MOVE ZERO         TO OBKR-IDARTNR-TILLK                       
579065                                 OBKR-REKSIFFR-TILLK                      
579066         ELSE                                                             
579067            IF LDC-ARTIKEL-BYTT                                           
579068               MOVE JA            TO OBKR-FLTILLK                         
579069               MOVE ORAD-IDARTNR  TO OBKR-IDARTNR-TILLK                   
579070               MOVE ORAD-REKSIFFR TO OBKR-REKSIFFR-TILLK                  
579071            END-IF                                                        
579072         END-IF                                                           
579075         PERFORM IMS-ISRT-WDQ101                                          
579076         ADD +1                TO OBKR-IDSEKVNR                           
579078     END-IF                                                               
579079                                                                          
579080     IF OHUV-KDORDKL = +0    AND                                          
579081         (OBKR-KDORDBEK = 51 OR 52 OR 53 OR 54 OR 55 OR 57 OR             
579082                          67 OR 92 OR 98)                                 
579083        PERFORM S11-SKRIV-VOR-RAD                                         
579084     END-IF                                                               
579085     .                                                                    
579090     EJECT                                                                
579091 ECSA-REDIGERA-OBKR-RAD SECTION.                                          
579092                                                                          
579093     MOVE 'STA ECSA-RED-OBKR   '           TO   CURRENT-SECTION           
579094     MOVE ORAD-IDORDER         TO OBKR-IDORDER                            
579095     MOVE ORFK-IDARTNR(WS-INDEX-ORFK)                                     
579096                               TO OBKR-IDARTNR                            
579097     MOVE ZERO                 TO OBKR-KDERS                              
579098     IF NOT TILLKOMMANDE-RAD                                              
579099     OR OBKR-IDLOPNR NOT NUMERIC                                          
579100        MOVE OBKR-IDORDER      TO W-IDORDER-Q1-MIN                        
579101                                  W-IDORDER-Q1-MAX                        
579102        MOVE OBKR-IDARTNR      TO W-IDARTNR-Q1-MIN                        
579103                                  W-IDARTNR-Q1-MAX                        
579104        MOVE +1                TO W-IDLOPNR-Q1-MIN                        
579105                                  W-IDLOPNR-Q1-MAX                        
579106                                  W-IDSEKVNR-Q1-MIN                       
579107                                  W-IDSEKVNR-Q1-MAX                       
579108        PERFORM IMS-GU-WDQ101                                             
579109        PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                        
579110           ADD +1              TO W-IDLOPNR-Q1-MIN                        
579111                                  W-IDLOPNR-Q1-MAX                        
579112           PERFORM IMS-GN-WDQ101                                          
579113        END-PERFORM                                                       
579114        MOVE W-IDLOPNR-Q1-MIN  TO OBKR-IDLOPNR                            
579115        MOVE +1                TO OBKR-IDSEKVNR                           
579116     END-IF                                                               
579117     MOVE ORAD-IDDC            TO OBKR-IDDC                               
579118     MOVE +0                   TO OBKR-KDORDBEK                           
579119     MOVE SPACE                TO OBKR-BEERS                              
579120     MOVE SPACE                TO OBKR-IDBIL                              
579121     MOVE OHUV-BEKUNDRF        TO OBKR-BEKUNDRF                           
579122     MOVE ORAD-BERADREF        TO OBKR-BERADREF                           
579123     MOVE ORAD-BEVOLREF        TO OBKR-BEVOLREF                           
579124     MOVE ORAD-IDKAMPRF        TO OBKR-IDKAMPRF                           
579125     MOVE +0                   TO OBKR-DIERS-KVOT                         
579126     MOVE ORAD-FLAKPLOC        TO OBKR-FLAKPLOC                           
579127     MOVE ORAD-FLINVEST        TO OBKR-FLINVEST                           
579128     MOVE JA                   TO OBKR-FLOBOK                             
579129     MOVE ORAD-FLOBTRAN        TO OBKR-FLOBTRAN                           
579130     MOVE NEJ                  TO OBKR-FLOBPRT                            
579131     MOVE ORAD-FLPRTILL        TO OBKR-FLPRTILL                           
579132     MOVE ORAD-FLRESTN         TO OBKR-FLRESTN                            
579133     MOVE ORFK-FLSLATT (WS-INDEX-ORFK)                                    
579134                               TO OBKR-FLSLATT                            
579135     MOVE ORAD-FLTILLK         TO OBKR-FLTILLK                            
579136     IF ORFK-KDORDBEK (WS-INDEX-ORFK) = 59                                
579137        MOVE ORAD-REKSIFFR     TO OBKR-REKSIFFR                           
579138     ELSE                                                                 
579139        MOVE ORFK-REKSIFFR(WS-INDEX-ORFK)                                 
579140                               TO OBKR-REKSIFFR                           
579141     END-IF                                                               
579142     IF TILLKOMMANDE-RAD                                                  
579143        MOVE TILK-IDARTNR-TILLK(WS-INDEX-TILLK)                           
579144                               TO OBKR-IDARTNR-TILLK                      
579145        MOVE TILK-REKSIFFR-TILLK(WS-INDEX-TILLK)                          
579146                               TO OBKR-REKSIFFR-TILLK                     
579147     ELSE                                                                 
579148        MOVE +0                TO OBKR-IDARTNR-TILLK                      
579149        MOVE +0                TO OBKR-REKSIFFR-TILLK                     
579150     END-IF                                                               
579151     MOVE ORAD-IDDC-RO         TO OBKR-IDDC-RO                            
579152     MOVE ORAD-IDGMTREF        TO OBKR-IDGMTREF                           
579153     MOVE ORAD-IDKUNDRF-RO     TO OBKR-IDKUNDRF-RO                        
579154     MOVE ORAD-IDLEVNR         TO OBKR-IDLEVNR                            
579155     MOVE ORAD-IDLOPNR-RO      TO OBKR-IDLOPNR-RO                         
579156     MOVE ORAD-IDSYSTEM        TO OBKR-IDSYSTEM                           
579157     MOVE ORAD-KDDSP           TO OBKR-KDDSP                              
579158     IF NOT TILLKOMMANDE-RAD                                              
579159     OR LDC-ARTIKEL-BYTT                                                  
579160        MOVE AREG-KDERS        TO OBKR-KDERS                              
579161     END-IF                                                               
579162     MOVE ORAD-KDKVBRYT        TO OBKR-KDKVBRYT                           
579163     MOVE ORAD-KDOI            TO OBKR-KDOI                               
579164     MOVE ORAD-CLEARGROUP      TO OBKR-CLEARGROUP                         
579165     MOVE ORAD-KDPRTYP         TO OBKR-KDPRTYP                            
579166     MOVE ORAD-KDTPOTYP        TO OBKR-KDTPOTYP                           
579167     MOVE ORAD-KDVRINFO        TO OBKR-KDVRINFO                           
579168     MOVE +0                   TO OBKR-KVANNANT                           
579169     MOVE +0                   TO OBKR-KVAVBART                           
579170     MOVE ORAD-KVBEART         TO OBKR-KVBEART                            
579171     MOVE ORAD-KVBEART-Q       TO OBKR-KVBEART-Q                          
579172     MOVE +0                   TO OBKR-KVBEART-TILLK                      
579173     MOVE +0                   TO OBKR-KVPREAVB                           
579174     MOVE +0                   TO OBKR-KVPRERO                            
579175     IF KVAN-KVQPACK-UT NUMERIC                                           
579176        MOVE KVAN-KVQPACK-UT   TO OBKR-KVQPACK                            
579177     ELSE                                                                 
579178        MOVE ZERO              TO OBKR-KVQPACK                            
579179     END-IF                                                               
579180     MOVE +0                   TO OBKR-KVRO                               
579181     MOVE ORAD-KVSLATT         TO OBKR-KVSLATT                            
579182     MOVE ORAD-PRARTNTO        TO OBKR-PRARTNTO                           
579183     MOVE ORAD-DEAL-PR-LINE    TO OBKR-DEAL-PR-LINE                       
579184     MOVE ORAD-PRBPRIS         TO OBKR-PRBPRIS                            
579185     MOVE ORAD-RERF-RAD        TO OBKR-RERF-RAD                           
579186     MOVE AREG-TIDISPIN        TO OBKR-TIDISPIN                           
579187     MOVE OHUV-TIREGDAT        TO OBKR-TIORDREG                           
579188     MOVE ORAD-TIPRIS          TO OBKR-TIPRIS                             
579189     MOVE ORAD-TIREGDAT        TO OBKR-TIREGDAT                           
579190     MOVE ORAD-TIREGTID        TO OBKR-TIREGTID                           
579191     MOVE +0                   TO OBKR-TIRODAT                            
579192     MOVE ORAD-TIREGDAT        TO WS-AAMMDD-9KOMPL                        
579193     IF WS-AAMMDD-9KOMPL(1:2) < 50                                        
579194       MOVE 20                 TO WS-SEKEL-9KOMPL                         
579195     ELSE                                                                 
579196       MOVE 19                 TO WS-SEKEL-9KOMPL                         
579197     END-IF                                                               
579198     COMPUTE OBKR-TITIREGD-9KOMPL = 999999999 - WS-TITIREGD-9KOMPL        
579199     MOVE ORAD-TITPO           TO OBKR-TITPO                              
579200     MOVE OHUV-TIREGDAT        TO WS-AAMMDD-9KOMPL                        
579201     IF WS-AAMMDD-9KOMPL(1:2) < 50                                        
579202       MOVE 20                 TO WS-SEKEL-9KOMPL                         
579203     ELSE                                                                 
579204       MOVE 19                 TO WS-SEKEL-9KOMPL                         
579205     END-IF                                                               
579206     COMPUTE OBKR-TITIORDD-9KOMPL = 999999999 - WS-TITIREGD-9KOMPL        
579207     MOVE ARB-KDFRAKT          TO OBKR-KDFRAKT                            
579208     MOVE OHUV-KDORDKL         TO OBKR-KDORDKL                            
579209                                                                          
579210     MOVE ORAD-IDBIL           TO OBKR-IDBIL                              
579211     MOVE OHUV-KDORDTYP-LDC    TO OBKR-KDORDTYP-LDC                       
579212     MOVE OHUV-TIREPDAT        TO OBKR-TIREPDAT                           
579213     MOVE ORAD-IDKUNDRF-WIP    TO OBKR-IDKUNDRF-WIP                       
579214     MOVE ZERO                 TO OBKR-TIDLEVDAT                          
579215     MOVE ORAD-PRAVCOST        TO OBKR-PRAVCOST                           
579216     MOVE ORAD-KDVALISO        TO OBKR-KDVALISO                           
579217     .                                                                    
579218     EJECT                                                                
579397                                                                          
579398 ECZ-CHECK-KDERS-IN-DC SECTION.                                           
579399                                                                          
579400     MOVE WS-INDEX-TILLK    TO WS-SAVE-INDEX                              
579401     MOVE +1                TO WS-INDEX-TILLK                             
579402                               IDDC-IX                                    
579403     MOVE NEJ               TO BAL-DC-FND-SW                              
579404                               TILLK-BAL-DC-FND-SW                        
579405                               KDERS-CHAIN-SW                             
579406     MOVE AREG-W411AREG-001 TO ORFK-W411AREG-001(WS-INDEX-ORFK)           
579407                                                                          
579408     PERFORM UNTIL WS-INDEX-TILLK > WS-INDEX-TILLK-MAX OR                 
579409                   TILK-IDARTNR(WS-INDEX-TILLK) = ZERO OR                 
579410                   BAL-DC-FND                          OR                 
579411                   TILLK-BAL-DC-FND                    OR                 
579412                   KDERS-CHAIN                                            
579413        PERFORM ECZD-CHECK-KDERS-CHAIN                                    
579414        IF KDERS-CHAIN-SW = NEJ                                           
579415           IF TILK-IDARTNR(WS-INDEX-TILLK) > ZERO AND                     
579416              TILK-IDARTNR-TILLK(WS-INDEX-TILLK) > ZERO AND               
579417              TILK-FLTILLK-X(WS-INDEX-TILLK) = JA                         
579418               PERFORM UNTIL W-GMT-IDDC-CLEAR(IDDC-IX) = SPACES           
579419                     OR IDDC-IX > IX-DCCLEAR-MAX                          
579420                     OR BAL-DC-FND                                        
579421                     OR TILLK-BAL-DC-FND                                  
579422                  MOVE TILK-IDARTNR(WS-INDEX-TILLK)                       
579423                                           TO W-IDARTNR-SDCA              
579424                  PERFORM ECZB-CALL-SDCA                                  
579425                  IF SDCA-KDORDBEK > 0                                    
579426                     PERFORM ECZA-GET-TILLK-DATA                          
579427                     MOVE TILK-IDARTNR-TILLK(WS-INDEX-TILLK)              
579428                                           TO W-IDARTNR-SDCA              
579429                     PERFORM ECZB-CALL-SDCA                               
579430                     IF SDCA-KDORDBEK = 0                                 
579431                       MOVE JA             TO TILLK-BAL-DC-FND-SW         
579432                       MOVE W-GMT-IDDC-CLEAR(IDDC-IX)                     
579433                                           TO W-TILLK-DC                  
579434                     END-IF                                               
579435                  ELSE                                                    
579436                     MOVE JA                TO BAL-DC-FND-SW              
579437                  END-IF                                                  
579438                  ADD +1       TO IDDC-IX                                 
579439               END-PERFORM                                                
579440           END-IF                                                         
579441        END-IF                                                            
579442        ADD +1              TO WS-INDEX-TILLK                             
579443     END-PERFORM                                                          
579444                                                                          
579445     MOVE WS-SAVE-INDEX     TO WS-INDEX-TILLK                             
579446     MOVE ORFK-W411AREG-001(WS-INDEX-ORFK) TO AREG-W411AREG-001           
579447     MOVE ZEROES            TO SDCA-KDORDBEK                              
579448                                                                          
579449     IF KDERS-CHAIN-SW = NEJ                                              
579450        IF BAL-DC-FND-SW = NEJ AND TILLK-BAL-DC-FND-SW = NEJ              
579451           MOVE '11'           TO W-TILLK-DC                              
579452        END-IF                                                            
579453     END-IF                                                               
579454     .                                                                    
579455     EJECT                                                                
579456*****************************************************************         
579457*IF A(KDERS 22) SUPERSEEDED BY B(KDERS-25) AND IS SUPERSEEDED             
579458*BY C1(KDERS 00) AND C2(KDERS 00),C1,C2 WILL BE SKIPPED AND               
579459*A WILL BE CHECKED FOR STOCKS, IF NOT OCC61                               
579460*****************************************************************         
579461 ECZD-CHECK-KDERS-CHAIN SECTION.                                          
579462                                                                          
579463     IF TILK-IDARTNR(WS-INDEX-TILLK) > ZERO AND                           
579464        TILK-IDARTNR-TILLK(WS-INDEX-TILLK) > ZERO AND                     
579465        TILK-FLTILLK-X(WS-INDEX-TILLK) = NEJ AND                          
579466       (TILK-KDERS(WS-INDEX-TILLK) = 14 OR 15 OR 18 OR                    
579467                                     24 OR 25 OR 28)                      
579468          MOVE JA              TO KDERS-CHAIN-SW                          
579469     END-IF                                                               
579470     .                                                                    
579471     EJECT                                                                
579472 ECZA-GET-TILLK-DATA SECTION.                                             
579473                                                                          
579474     MOVE AREG-FLREFILL        TO W-FLREFILL-MAIN                         
579475     MOVE AREG-KDPRODSL        TO W-KDPRODSL-MAIN                         
579476     MOVE AREG-KDSORT          TO W-KDSORT-MAIN                           
579477     MOVE AREG-KVQPACK-1       TO W-KVQPACK-1-MAIN                        
579478     MOVE AREG-REDIRLEV        TO W-REDIRLEV-MAIN                         
579479                                                                          
579480     PERFORM ED-LAES-TILLK-DATA                                           
579481                                                                          
579482     MOVE AREG-FLREFILL        TO W-FLREFILL-REPL                         
579483     MOVE AREG-KDPRODSL        TO W-KDPRODSL-REPL                         
579484     MOVE AREG-KDSORT          TO W-KDSORT-REPL                           
579485     MOVE AREG-KVQPACK-1       TO W-KVQPACK-1-REPL                        
579486     MOVE AREG-REDIRLEV        TO W-REDIRLEV-REPL                         
579487                                                                          
579488     MOVE W-FLREFILL-MAIN      TO AREG-FLREFILL                           
579489     MOVE W-KDPRODSL-MAIN      TO AREG-KDPRODSL                           
579490     MOVE W-KDSORT-MAIN        TO AREG-KDSORT                             
579491     MOVE W-KVQPACK-1-MAIN     TO AREG-KVQPACK-1                          
579492     MOVE W-REDIRLEV-MAIN      TO AREG-REDIRLEV                           
579493     .                                                                    
579494     EJECT                                                                
579495 ECZB-CALL-SDCA   SECTION.                                                
579496                                                                          
579497     IF ORAD-IDARTNR = W-IDARTNR-SDCA                                     
579498        MOVE ORAD-IDARTNR         TO SDCA-IDARTNR                         
579499        MOVE AREG-FLREFILL        TO SDCA-FLREFILL                        
579500        MOVE AREG-KDPRODSL        TO SDCA-KDPRODSL                        
579501        MOVE AREG-KDSORT          TO SDCA-KDSORT                          
579502        MOVE AREG-KVQPACK-1       TO SDCA-KVQPACK-1                       
579503        MOVE AREG-REDIRLEV        TO SDCA-REDIRLEV                        
579504     ELSE                                                                 
579505        MOVE W-IDARTNR-SDCA       TO SDCA-IDARTNR                         
579506        MOVE W-FLREFILL-REPL      TO SDCA-FLREFILL                        
579507        MOVE W-KDPRODSL-REPL      TO SDCA-KDPRODSL                        
579508        MOVE W-KDSORT-REPL        TO SDCA-KDSORT                          
579509        MOVE W-KVQPACK-1-REPL     TO SDCA-KVQPACK-1                       
579510        MOVE W-REDIRLEV-REPL      TO SDCA-REDIRLEV                        
579511     END-IF                                                               
579512                                                                          
579513     MOVE OHUV-IDDISTR         TO SDCA-IDDISTR                            
579514     MOVE OHUV-FLFORBI         TO SDCA-FLFORBI                            
579515     MOVE NEJ                  TO SDCA-FLORDSPE                           
579516     MOVE W-GMT-IDDC-CLEAR(IDDC-IX)                                       
579517                               TO SDCA-IDDC                               
579518     MOVE OHUV-IDDC-TVS        TO SDCA-IDDC-TVS                           
579519     MOVE ORAD-IDLEVNR         TO SDCA-IDLEVNR                            
579520     MOVE ORAD-IDSYSTEM        TO SDCA-IDSYSTEM                           
579521     MOVE ORAD-KDORDKL         TO SDCA-KDORDKL                            
579522     MOVE ORAD-KDORDING        TO SDCA-KDORDING                           
579523     MOVE ORAD-KVBEART-Q       TO SDCA-KVBEART-Q                          
579524     MOVE ORAD-FLSDCLEV        TO SDCA-FLSDCLEV                           
579525     MOVE +0                   TO SDCA-TIREPDAT                           
579526     MOVE +0                   TO SDCA-KVOKS-PREL                         
579527     MOVE +2                   TO SDCA-KDCALL                             
579528     MOVE +1                   TO SDCA-IXDCCLEAR                          
579529                                                                          
579530     CALL W411SDCA USING SDCA-W411SDCA SDCA-ARTS-PCB                      
579531                                       SDCA-WDB6-PCB                      
579532                                       SDCA-WDK9-PCB                      
579533                                       SDCA-WDR6-PCB                      
579534                                       SDCA-WDK6-PCB                      
579535                                       SDCA-WDQ4B-PCB                     
579536                                       SDCA-WDQ2-PCB                      
579537                                       SDCA-WDQ4-PCB                      
579538                                       SDCA-WDB6-2-PCB                    
579539                                       SDCA-WDK6-2-PCB                    
579540                                       SDCA-WDK7-2-PCB                    
579541                                       SDCA-WDK7-3-PCB                    
579542     .                                                                    
579543     EJECT                                                                
579544 ED-LAES-TILLK-DATA SECTION.                                              
579545                                                                          
579546     MOVE 'STA ED-LAES-TILLK   ' TO  CURRENT-SECTION                      
579547     MOVE TILK-IDARTNR-TILLK(WS-INDEX-TILLK)                              
579548                               TO AREG-IDARTNR                            
579549                                                                          
579550     CALL W411AREG USING AREG-W411AREG                                    
579551                         AREG-WDK6-PCB                                    
579552                         AREG-WDK7-PCB                                    
579553     .                                                                    
579554     EJECT                                                                
579555 S12-SPACE-SDCA-KDORDBEK  SECTION.                                        
579556                                                                          
579557     IF SDCA-KDORDBEK-FIRST-SDC > 0                                       
579558         MOVE ZEROES  TO SDCA-KDORDBEK-FIRST-SDC                          
579559     ELSE                                                                 
579560        IF SDCA-KDORDBEK-SECOND-SDC > 0                                   
579561           MOVE ZEROES  TO SDCA-KDORDBEK-SECOND-SDC                       
579562        ELSE                                                              
579563           IF SDCA-KDORDBEK > 0                                           
579564              MOVE ZEROES  TO SDCA-KDORDBEK                               
579565           END-IF                                                         
579566        END-IF                                                            
579567     END-IF                                                               
579568     .                                                                    
579569                                                                          
579570     EJECT                                                                
579571 S02-INITIALIZE-TILLK-TAB SECTION.                                        
579572                                                                          
579573     MOVE 'S02-INITIALIZE'        TO   CURRENT-SECTION                    
579574                                                                          
579575     MOVE +1              TO WS-INDEX-TILLK                               
579576     PERFORM UNTIL WS-INDEX-TILLK > WS-INDEX-TILLK-MAX                    
579577        MOVE +0           TO TILK-IDARTNR(WS-INDEX-TILLK)                 
579578        MOVE +0           TO TILK-IDARTNR-TILLK(WS-INDEX-TILLK)           
579579        MOVE SPACE        TO TILK-BEERS(WS-INDEX-TILLK)                   
579580        MOVE +0           TO TILK-DIERS-ERS(WS-INDEX-TILLK)               
579581        MOVE +0           TO TILK-DIERS-TILLK(WS-INDEX-TILLK)             
579582        MOVE SPACE        TO TILK-FLFINLV(WS-INDEX-TILLK)                 
579583        MOVE SPACE        TO TILK-FLPRTILL(WS-INDEX-TILLK)                
579584        MOVE SPACE        TO TILK-FLTILLK-X(WS-INDEX-TILLK)               
579585        MOVE +0           TO TILK-KDERS(WS-INDEX-TILLK)                   
579586        MOVE SPACE        TO TILK-KDPRTYP(WS-INDEX-TILLK)                 
579587        MOVE +0           TO TILK-KVBEART(WS-INDEX-TILLK)                 
579588        MOVE +0           TO TILK-PRARTNTO(WS-INDEX-TILLK)                
579589        INITIALIZE           TILK-DEAL-PR-LINE(WS-INDEX-TILLK)            
579590        MOVE +0           TO TILK-TIPRIS(WS-INDEX-TILLK)                  
579591        MOVE +0           TO TILK-REKSIFFR-TILLK(WS-INDEX-TILLK)          
579592        ADD +1            TO WS-INDEX-TILLK                               
579593     END-PERFORM                                                          
579594     MOVE +1              TO WS-INDEX-TILLK                               
579595     .                                                                    
579596     EJECT                                                                
579597 S03-RESTORE-STOCKS-CDC SECTION.                                          
579598                                                                          
579599     MOVE 'S03-RESTORE-ST'        TO   CURRENT-SECTION                    
579600                                                                          
579610     PERFORM IMS-GHU-WDK901                                               
579620     IF SEGMENT-FINNS                                                     
579621        IF OHUV-KDORDKL = ZERO                                            
579626           COMPUTE ART-KVOKS-VOR    = ART-KVOKS-VOR     -                 
579627                                      WS-ORAD-KVBEART-Q                   
579629           COMPUTE ART-KVPREAVB-VOR = ART-KVPREAVB-VOR  -                 
579630                                      WS-ORAD-KVPREAVB                    
579631        END-IF                                                            
579632        IF OHUV-KDORDKL = +1                                              
579634            COMPUTE ART-KVOKS-DAG   = ART-KVOKS-DAG     -                 
579635                                      WS-ORAD-KVBEART-Q                   
579637          COMPUTE ART-KVPRERO-DAG   = ART-KVPRERO-DAG   -                 
579638                                      WS-ORAD-KVPRERO                     
579639          COMPUTE ART-KVPREAVB-DAG  = ART-KVPREAVB-DAG  -                 
579640                                      WS-ORAD-KVPREAVB                    
579641        END-IF                                                            
579642        IF OHUV-KDORDKL = +2 OR +3 OR +4                                  
579643            COMPUTE ART-KVOKS-BULK  = ART-KVOKS-BULK    -                 
579644                                      WS-ORAD-KVBEART-Q                   
579645          COMPUTE ART-KVPRERO-BULK  = ART-KVPRERO-BULK  -                 
579646                                      WS-ORAD-KVPRERO                     
579647          COMPUTE ART-KVPREAVB-BULK = ART-KVPREAVB-BULK -                 
579648                                      WS-ORAD-KVPREAVB                    
579649        END-IF                                                            
579650                                                                          
579651        PERFORM IMS-REPL-WDK901                                           
579652     END-IF                                                               
579653     .                                                                    
579654     EJECT                                                                
579655 S05-RESTORE-STOCKS-XDC SECTION.                                          
579656                                                                          
579657     MOVE 'S05-RESTORE-ST'        TO   CURRENT-SECTION                    
579660     MOVE ORAD-IDDC               TO W-IDDC                               
579661     PERFORM IMS-GHU-WDK711                                               
579662     IF SEGMENT-FINNS                                                     
579663        IF OHUV-KDORDKL = +0 OR +1                                        
579664          COMPUTE SLAG-KVOKS-DAG       = SLAG-KVOKS-DAG -                 
579665                                         WS-ORAD-KVBEART-Q                
579666        ELSE                                                              
579667          IF OHUV-KDORDKL = +2 OR +3 OR +4                                
579668            IF ORAD-KVOKS-PREL = ZERO                                     
579669               COMPUTE SLAG-KVOKS-BULK = SLAG-KVOKS-BULK -                
579670                                         WS-ORAD-KVBEART-Q                
579671            END-IF                                                        
579672          END-IF                                                          
579673        END-IF                                                            
579674        PERFORM IMS-REPL-WDK711                                           
579675     END-IF                                                               
579676     .                                                                    
579677     EJECT                                                                
579678 S06-CREATE-AVSR-FIELDS SECTION.                                          
579679                                                                          
579680     MOVE 'S06-CREATE-AVSR  '         TO   CURRENT-SECTION                
579681     MOVE JA                 TO AVSR-SW                                   
579682     MOVE 2                  TO   AVSR-KDCALL                             
579683     MOVE OHUV-IDORDER       TO   AVSR-IDORDER                            
579684     MOVE OHUV-KDORDKL       TO   AVSR-KDORDKL                            
579685     MOVE ZERO               TO   AVSR-KDFRAKT                            
579686     MOVE ZERO               TO   AVSR-KDROPACK                           
579687     MOVE MSGI-TILOKDAT      TO   AVSR-TIREGDAT                           
579688     MOVE MSGI-TILOKTID      TO   AVSR-TIHHMM                             
579689                                                                          
579690     MOVE ORAD-ADLAGOMR      TO   AVSR-ADLAGOMR(AVSR-INDX)                
579691     MOVE ORAD-IDLEVNR       TO   AVSR-IDLEVNR(AVSR-INDX)                 
579692     MOVE ORAD-IDDC          TO   AVSR-IDDC(AVSR-INDX)                    
579693     MOVE ORAD-KDSPEEMB      TO   AVSR-KDSPEEMB(AVSR-INDX)                
579694     MOVE WS-ORAD-KVBEART-Q  TO   AVSR-KVANNANT(AVSR-INDX)                
579695     MOVE ORAD-KVBEART-Q     TO   AVSR-KVBEART-Q(AVSR-INDX)               
579696     MOVE ORAD-PRARTNTO      TO   AVSR-PRARTNTO(AVSR-INDX)                
579697     MOVE ORAD-PRAVCOST      TO   AVSR-PRAVCOST(AVSR-INDX)                
579698     MOVE ORAD-VKART         TO   AVSR-VKART(AVSR-INDX)                   
579699     MOVE ORAD-VLARTNTO      TO   AVSR-VLARTNTO(AVSR-INDX)                
579700     MOVE SPACE              TO   AVSR-KDORDSTA(AVSR-INDX)                
579701     MOVE +0                 TO   AVSR-KDVIA   (AVSR-INDX)                
579702     MOVE +0                 TO   AVSR-KVDAGAR-DIFF(AVSR-INDX)            
579703     MOVE +0                 TO   AVSR-TISKEPPN-DDC(AVSR-INDX)            
579704     MOVE ORAD-DEAL-PR-LINE  TO   AVSR-DEAL-PR-LINE(AVSR-INDX)            
579705     .                                                                    
579706     EJECT                                                                
579707 S07-CREATE-2109-TRANS SECTION.                                           
579708                                                                          
579709     MOVE 'S07-CEATE-2109   '         TO   CURRENT-SECTION                
579710                                                                          
579711     IF ORAD-IDKUNDRF-RO = '0000000   ' OR                                
579712       ORAD-IDKUNDRF-RO = ORAD-IDKUNDRF                                   
579713                                                                          
579714       IF ORAD-KDOI NOT = SPACE                                           
579715         MOVE ORAD-IDARTNR       TO BYT03-IDARTNR                         
579716         IF NOT BYT03-OBJEKT                                              
579717                                                                          
579718            MOVE 2109-INDX       TO 2109-MID2-KVANTART                    
579719            MOVE ORAD-IDARTNR    TO 2109-MID2-IDARTNR (2109-INDX)         
579720            MOVE OHUV-IDDC-PRIM  TO 2109-MID2-IDDC    (2109-INDX)         
579721            MOVE '-'             TO 2109-MID2-KDTECKEN(2109-INDX)         
579722            MOVE ORAD-KDOI       TO 2109-MID2-KDOI    (2109-INDX)         
579723            MOVE ORAD-CLEARGROUP TO                                       
579724                                 2109-MID2-CLEARGROUP (2109-INDX)         
579725            MOVE WS-ORAD-KVBEART-Q TO 2109-MID2-KVOI  (2109-INDX)         
579726            MOVE ORAD-TIREGDAT   TO 2109-MID2-TIUPPDAT(2109-INDX)         
579727                                                                          
579728            ADD +1 TO 2109-INDX                                           
579729            IF 2109-INDX > MAX-2109-INDX                                  
579730              PERFORM K-STARTA-2109                                       
579731              MOVE ZERO TO 2109-MID2-KVANTART                             
579732            END-IF                                                        
579733         END-IF                                                           
579734       END-IF                                                             
579735                                                                          
579736     END-IF                                                               
579737     .                                                                    
579738     EJECT                                                                
579739 S08-DELETE-PRICE-Q-LINE SECTION.                                         
579740                                                                          
579741     MOVE 'S08-DELETE-PRICE '         TO   CURRENT-SECTION                
579742                                                                          
579743     MOVE ORAD-IDDISTR TO TEST-IDDISTR                                    
579744                                                                          
579745     IF DIST79-DEALER-PRICE                                               
579746       IF ORAD-IDPRQUES > ZERO                                            
579747         INITIALIZE PRQU-W335PRQU                                         
579748         MOVE OHUV-IDDISTR       TO PRQU-IDDISTR                          
579749         MOVE OHUV-IDKUNDNR      TO PRQU-IDKUNDNR                         
579750         MOVE OHUV-IDKUNDRF      TO PRQU-IDKUNDRF                         
579751         MOVE ORAD-IDPRQUES      TO PRQU-IDPRQUES                         
579752         MOVE 4                  TO PRQU-KDCALL                           
579753                                                                          
579754         CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                 
579755                                            PRQU-WDC7-PCB                 
579756                                            PRQU-SJKO-WDK6-PCB            
579757       END-IF                                                             
579758     END-IF                                                               
579759     .                                                                    
579760     EJECT                                                                
579761 S04-SKICKA-OPEN SECTION.                                                 
579762                                                                          
579763     MOVE 'OPEN'                     TO SEND-KDFUNC                       
579764     MOVE 'CARPARTS.PULS.PRQRY' TO SEND-ADDISPABS                         
579765     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-OPEN-AREA                 
579766                                                                          
579767     IF SEND-KDRC > 0                                                     
579768       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
579769       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
579770       DELIMITED BY SIZE INTO FELTEXT                                     
579771       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
579772     END-IF                                                               
579773     .                                                                    
579774     SKIP3                                                                
579775 S04-SKICKA-MEDDELANDE SECTION.                                           
579776                                                                          
579777     MOVE 'PUT'                      TO SEND-KDFUNC                       
579778     MOVE LENGTH OF SEND-AREA        TO SEND-KVDLEN                       
579779     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-KVDLEN SEND-AREA          
579780                                                                          
579781     IF SEND-KDRC > 0                                                     
579782       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
579783       STRING 'WZ01SEND GET ERROR RC=' KDRC-DISPLAY                       
579784       DELIMITED BY SIZE INTO FELTEXT                                     
579785       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
579786     END-IF                                                               
579787     .                                                                    
579788     SKIP3                                                                
579789 S04-SKICKA-CLOSE SECTION.                                                
579790                                                                          
579791     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
579792     CALL WZ01SEND USING SEND-CONTROL-AREA                                
579793                                                                          
579794     IF SEND-KDRC > 0                                                     
579795       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
579796       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
579797       DELIMITED BY SIZE INTO FELTEXT                                     
579798       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
579799     END-IF                                                               
579800     .                                                                    
579801     EJECT                                                                
579802                                                                          
579803 S09-KONTROLLERA-ENHETSLAST SECTION.                                      
579804                                                                          
579805     MOVE 'STA S09-ENHETSLAST  '           TO   CURRENT-SECTION           
579806     MOVE ZERO                 TO LAST-ADGANG-UT                          
579807     MOVE ZERO                 TO LAST-ADLAGOMR-UT                        
579808     MOVE ZERO                 TO LAST-KVANTAL-UT                         
579809     MOVE ZERO                 TO LAST-KVBEART-UT                         
579810                                                                          
579811     PERFORM S40-HAMTA-WDB6-INFO                                          
579812     IF DCS-CDC             AND                                           
579813        OHUV-FLFORBI = NEJ  AND OHUV-FLORDSPE = NEJ AND                   
579814        OHUV-FLOVRLEV = NEJ AND ORAD-IDLEVNR = SPACE AND                  
579815       (AREG-KVQPACK-3 > +0 OR AREG-KVQPACK-4 > +0)                       
579816                                                                          
579817        AND ORAD-IDBIL = SPACE                                            
579818                                                                          
579819        MOVE ORAD-ADLAGOMR        TO LAST-ADLAGOMR                        
579820        MOVE OHUV-FLFORBI         TO LAST-FLFORBI                         
579821        MOVE OHUV-FLOVRLEV        TO LAST-FLOVRLEV                        
579822        MOVE OHUV-FLORDSPE        TO LAST-FLORDSPE                        
579823        MOVE ORAD-IDLEVNR         TO LAST-IDLEVNR                         
579824        MOVE ORAD-IDDC            TO LAST-IDDC                            
579825        MOVE ARB-KDFDKRAV         TO LAST-KDFDKRAV                        
579826        MOVE ORAD-KVPREAVB        TO LAST-KVPREAVB                        
579827        MOVE AREG-KVQPACK-3       TO LAST-KVQPACK-3                       
579828        MOVE AREG-KVQPACK-4       TO LAST-KVQPACK-4                       
579829                                                                          
579830        CALL W411LAST USING LAST-W411LAST                                 
579831     END-IF                                                               
579832     .                                                                    
579833     EJECT                                                                
579834 S10-BERAKNA-WOPS-SKRIV-ORAD SECTION.                                     
579835                                                                          
579836     MOVE 'STA S10-WOPS        '           TO   CURRENT-SECTION           
579837                                                                          
579838     IF LAST-ADLAGOMR-UT = +0 AND                                         
579839        LAST-KVANTAL-UT  = +0 AND                                         
579840        LAST-KVBEART-UT  = +0                                             
579841*------------------------------------------------------------*            
579842*-----ALLT MOT VANLIGT LAGEROMRÅDE ( 'VANLIG' ORDERRAD)------*            
579843*------------------------------------------------------------*            
579844        PERFORM S10A-FIXA-LAGEROMR-PLATS                                  
579845        PERFORM S10B-REDIGERA-WOPS-AREA                                   
579846        PERFORM IMS-ISRT-WDQ401                                           
579848        PERFORM UNTIL SEGMENT-FINNS                                       
579849           ADD +1           TO ORAD-IDLOPNR                               
579850           PERFORM IMS-ISRT-WDQ401                                        
579851        END-PERFORM                                                       
579852     ELSE                                                                 
579853        IF LAST-KVANTAL-UT > +0 AND LAST-KVBEART-UT > +0                  
579854*------------------------------------------------------------*            
579855*--------- RADEN MOT VANLIGT LAGEROMRÅDE (KVBEART)-----------*            
579856*------------------------------------------------------------*            
579857                                                                          
579858           MOVE LAST-KVBEART-UT      TO ORAD-KVPREAVB                     
579859           COMPUTE ORAD-KVBEART-Q = ORAD-KVPREAVB +                       
579860                                    ORAD-KVPRERO                          
579861           MOVE ORAD-KVSLATT         TO SPAR-ORAD-KVSLATT                 
579862           PERFORM S10C-BERAEKNA-KVSLATT                                  
579863           PERFORM S10A-FIXA-LAGEROMR-PLATS                               
579864           MOVE ORAD-ADLAGOMR        TO WS-ADLAGOMR                       
579865           MOVE ORAD-ADGANG          TO WS-ADGANG                         
579866           MOVE ORAD-ADPLATS         TO WS-ADPLATS                        
579867           PERFORM S10B-REDIGERA-WOPS-AREA                                
579869           PERFORM IMS-ISRT-WDQ401                                        
579870           PERFORM UNTIL SEGMENT-FINNS                                    
579871              ADD +1        TO ORAD-IDLOPNR                               
579872              PERFORM IMS-ISRT-WDQ401                                     
579873           END-PERFORM                                                    
579874****************************************************************          
579875*                                                                         
579876*------------------------------------------------------------*            
579877*--------- RADEN MOT 'ENHETS' LAGEROMRÅDE (KVANTAL)----------*            
579878*------------------------------------------------------------*            
579879           MOVE WS-ADLAGOMR          TO ORAD-ADLAGOMR                     
579880           MOVE WS-ADGANG            TO ORAD-ADGANG                       
579881           MOVE WS-ADPLATS           TO ORAD-ADPLATS                      
579882                                                                          
579883           MOVE +0                   TO ORAD-KVBEART                      
579884           MOVE LAST-KVANTAL-UT      TO ORAD-KVBEART-Q                    
579885                                        ORAD-KVPREAVB                     
579886           MOVE LAST-ADLAGOMR-UT     TO ORAD-ADLAGOMR                     
579887           MOVE +0                   TO ORAD-KVPRERO                      
579888           MOVE 1.0000               TO ORAD-RERF-RAD                     
579889           COMPUTE ORAD-KVSLATT = SPAR-ORAD-KVSLATT - ORAD-KVSLATT        
579890           PERFORM S10A-FIXA-LAGEROMR-PLATS                               
579891           PERFORM S10B-REDIGERA-WOPS-AREA                                
579893           PERFORM IMS-ISRT-WDQ401                                        
579894           PERFORM UNTIL SEGMENT-FINNS                                    
579895              ADD +1        TO ORAD-IDLOPNR                               
579896              PERFORM IMS-ISRT-WDQ401                                     
579897           END-PERFORM                                                    
579898        ELSE                                                              
579899*------------------------------------------------------------*            
579900*-----ALLT MOT 'ENHETS' LAGEROMRÅDE -------------------------*            
579901*------------------------------------------------------------*            
579902           MOVE LAST-ADLAGOMR-UT TO ORAD-ADLAGOMR                         
579903           IF LAST-ADLAGOMR-UT = 62 OR 63 OR 64                           
579904             CONTINUE                                                     
579905           ELSE                                                           
579906             IF LAST-ADGANG-UT > ZERO                                     
579907               MOVE LAST-ADGANG-UT   TO ORAD-ADGANG                       
579908             END-IF                                                       
579909           END-IF                                                         
579910           MOVE 1.0000            TO ORAD-RERF-RAD                        
579911           PERFORM S10A-FIXA-LAGEROMR-PLATS                               
579912           PERFORM S10B-REDIGERA-WOPS-AREA                                
579914           PERFORM IMS-ISRT-WDQ401                                        
579915           PERFORM UNTIL SEGMENT-FINNS                                    
579916              ADD +1        TO ORAD-IDLOPNR                               
579917              PERFORM IMS-ISRT-WDQ401                                     
579918           END-PERFORM                                                    
579919        END-IF                                                            
579920     END-IF                                                               
579921     .                                                                    
579922     EJECT                                                                
579923 S10A-FIXA-LAGEROMR-PLATS SECTION.                                        
579924                                                                          
579925     MOVE 'STA S10A-LAGER      '    TO   CURRENT-SECTION                  
579926     MOVE ORAD-ADLAGOMR        TO ADRS-ADLAGOMR-IN                        
579927     MOVE ORAD-ADPLATS         TO ADRS-ADPLATS-IN                         
579928     MOVE OHUV-BEVARREF        TO ADRS-BEVARREF-IN                        
579929                                                                          
579930* TO HANDLE BEVARREF FROM EXCEL - ORDER UPLOAD                            
579931* TO MAKE IT POSSIBLE TO STEER ORDERS TO PARTICULAR PRC                   
579932* THIS DONE USING W413WHFA                                                
579937     MOVE OHUV-FLFORBI         TO ADRS-FLFORBI-IN                         
579939                                                                          
579940     MOVE OHUV-IDDISTR         TO ADRS-IDDISTR-IN                         
579941     MOVE 1                    TO ADRS-KDCALL-IN                          
579942     MOVE ORAD-IDDC            TO ADRS-IDDC-IN                            
579943     MOVE OHUV-KDORDKL         TO ADRS-KDORDKL-IN                         
579944     MOVE ORAD-KVBEART-Q       TO ADRS-KVBEART-Q-IN                       
579945     MOVE ORAD-VLARTNTO        TO ADRS-VLARTNTO-IN                        
579946                                                                          
579947     CALL W413ADRS USING ADRS-W413ADRS                                    
579948                                                                          
579949     MOVE ADRS-ADLAGOMR-UT   TO ORAD-ADLAGOMR                             
579950     MOVE ADRS-ADPLATS-UT    TO ORAD-ADPLATS                              
579951     .                                                                    
579952     EJECT                                                                
579953 S10B-REDIGERA-WOPS-AREA SECTION.                                         
579954                                                                          
579955     MOVE 'S10B-REDIGERA-WOPS- '    TO   CURRENT-SECTION                  
579956                                                                          
579960     IF ((OHUV-IDSYSTEM = 'LDC ' OR 'TACD' OR 'LDCB' OR 'LDCD') OR        
579961        (OHUV-TIREPDAT > 0 AND                                            
579962        (OHUV-IDSYSTEM(1:3) = 'LYN' OR 'ECO' OR 'VOU' OR                  
579963                              'TAD' OR 'ACC' OR 'APA' OR                  
579964                              'APB' OR 'APC' OR 'APD' OR                  
579965                              'APE' OR 'APF' OR 'APG' OR                  
579966                              'APH' OR 'API' OR 'APJ' )))                 
579967     AND (OHUV-KDORDKL  = +3 OR +2)                                       
579968       MOVE +9                 TO AVSR-KDCALL                             
579969     ELSE                                                                 
579970       MOVE +1                 TO AVSR-KDCALL                             
579971     END-IF                                                               
579972                                                                          
579973     MOVE OHUV-IDORDER         TO AVSR-IDORDER                            
579974     MOVE OHUV-KDORDKL         TO AVSR-KDORDKL                            
579975     MOVE ARB-KDFRAKT          TO AVSR-KDFRAKT                            
579976     MOVE ARB-KDROPACK         TO AVSR-KDROPACK                           
579977     MOVE MSGI-TILOKDAT        TO AVSR-TIREGDAT                           
579978     MOVE MSGI-TILOKTID        TO AVSR-TIHHMM                             
579979                                                                          
579980     MOVE ORAD-ADLAGOMR        TO AVSR-ADLAGOMR(WS-INDEX-WOPS)            
579983     IF OHUV-FLEMBORD = JA                                                
579984        MOVE SPACE             TO AVSR-IDLEVNR(WS-INDEX-WOPS)             
579985     ELSE                                                                 
579986        MOVE ORAD-IDLEVNR      TO AVSR-IDLEVNR(WS-INDEX-WOPS)             
579987     END-IF                                                               
579988     MOVE ORAD-IDDC            TO AVSR-IDDC(WS-INDEX-WOPS)                
579989*LDC URSÄKTA FIX-LÖSNINGEN FÖR RFS                                        
579990     IF (OHUV-IDSYSTEM = 'LDC ' OR 'TACD' OR 'LYNK'                       
579991                                OR 'ECOM' OR 'VOUI' OR 'TAD '             
579992                                OR 'ACC ' OR 'APA ' OR 'APB '             
579993                                OR 'APC ' OR 'APD ' OR 'APE '             
579994                                OR 'APF ' OR 'APG ' OR 'APH '             
579995                                OR 'API ' OR 'APJ '  )                    
579996        AND ORAD-IDDC = WC-CDC-SE                                         
579997        AND ARB-TIRFS > 0                                                 
579998       PERFORM S15-CALL-WORKDAY-LDC-CDC-RFS                               
579999       MOVE WORK-TIAAMMDD-FOM TO AVSR-TISKEPPN-DDC(WS-INDEX-WOPS)         
580000     END-IF                                                               
580001     MOVE ORAD-KDSPEEMB        TO AVSR-KDSPEEMB(WS-INDEX-WOPS)            
580002     MOVE +0                   TO AVSR-KVANNANT(WS-INDEX-WOPS)            
580003     MOVE ORAD-KVBEART-Q       TO AVSR-KVBEART-Q(WS-INDEX-WOPS)           
580004     MOVE ORAD-PRARTNTO        TO AVSR-PRARTNTO(WS-INDEX-WOPS)            
580005     MOVE ORAD-PRAVCOST        TO AVSR-PRAVCOST(WS-INDEX-WOPS)            
580006     MOVE ORAD-DEAL-PR-LINE    TO AVSR-DEAL-PR-LINE(WS-INDEX-WOPS)        
580007     MOVE ORAD-VKART           TO AVSR-VKART(WS-INDEX-WOPS)               
580008     MOVE ORAD-VLARTNTO        TO AVSR-VLARTNTO(WS-INDEX-WOPS)            
580009     MOVE AREG-KDVSOP          TO AVSR-KDVSOP(WS-INDEX-WOPS)              
580010     MOVE AREG-KDFARLIG        TO AVSR-KDFARLIG(WS-INDEX-WOPS)            
580011                                                                          
580012     ADD +1                    TO WS-INDEX-WOPS                           
580013                                                                          
580014     IF WS-INDEX-WOPS > WS-INDEX-WOPS-MAX                                 
580016        CALL W413AVSR USING AVSR-W413AVSR AVSR-LIST-PCB                   
580017          AVSR-ORQI-PCB AVSR-GMTB-PCB AVSR-GMTC-PCB                       
580018          AVSR-WDB2-PCB AVSR-WDB6-PCB                                     
580019          TRAN-XXKB-PCB                                                   
580020                                                                          
580021        PERFORM AA-NOLLA-WOPS-TABELL                                      
580022     END-IF                                                               
580023     .                                                                    
580024     EJECT                                                                
580025 S10C-BERAEKNA-KVSLATT SECTION.                                           
580026                                                                          
580027     MOVE 'S10C-BERAEKNA-KV'           TO   CURRENT-SECTION               
580028     IF ORAD-FLRESTN = JA AND OHUV-RESLATT > +0                           
580029                                                                          
580030        COMPUTE WS-RESLATT = OHUV-RESLATT / 100                           
580031                                                                          
580032        COMPUTE ORAD-KVSLATT ROUNDED =                                    
580033               (ORAD-KVPREAVB + ORAD-KVPRERO) * WS-RESLATT                
580034     END-IF                                                               
580035     .                                                                    
580036     EJECT                                                                
580037                                                                          
580038 S11-SKRIV-VOR-RAD SECTION.                                               
580039                                                                          
580040     MOVE 'STA S11-VOR         '           TO   CURRENT-SECTION           
580041     MOVE OBKR-IDDISTR         TO 4542-IDDISTR                            
580042     MOVE OBKR-IDDC            TO WS-IDDC-SEEK                            
580043                                                                          
580044     IF OBKR-IDARTNR-TILLK > +0                                           
580045       MOVE OBKR-IDARTNR-TILLK TO W-IDARTNR                               
580046                                  4542-IDARTNR                            
580047     ELSE                                                                 
580048        MOVE OBKR-IDARTNR      TO W-IDARTNR                               
580049                                  4542-IDARTNR                            
580050     END-IF                                                               
580051                                                                          
580052     MOVE AREG-IDANSK          TO 4542-IDANSK                             
580053     MOVE +1                   TO 4542-IDLOPNR                            
580054     MOVE OBKR-IDORDER         TO 4542-IDORDER                            
580055     MOVE OBKR-BERADREF        TO 4542-BERADREF                           
580056     MOVE OBKR-IDDC            TO W-IDDC                                  
580057                                  4542-IDDC                               
580058     MOVE OBKR-IDKUNDNR        TO 4542-IDKUNDNR                           
580059     MOVE OBKR-IDKUNDRF        TO 4542-IDKUNDRF                           
580060     MOVE OHUV-IDUSER          TO 4542-IDUSER                             
580061     MOVE OBKR-KDORDBEK        TO 4542-KDORDBEK                           
580062     MOVE OBKR-KDPRTYP         TO 4542-KDPRTYP                            
580063     MOVE ZERO                 TO 4542-KDVORATG                           
580064     IF 4542-KDORDBEK = 92 OR 98                                          
580065       COMPUTE 4542-KVBEART = OBKR-KVPREAVB + OBKR-KVPRERO                
580066       MOVE 4542-KVBEART         TO 4542-KVBEART-Q                        
580067       MOVE OBKR-KVPREAVB        TO 4542-KVPREAVB                         
580068     ELSE                                                                 
580069       MOVE OBKR-KVBEART         TO 4542-KVBEART-Q                        
580070       MOVE +0                   TO 4542-KVPREAVB                         
580071     END-IF                                                               
580072     EJECT                                                                
580073     MOVE OBKR-PRARTNTO        TO 4542-PRARTNTO                           
580074     MOVE OBKR-DEAL-PR-LINE    TO 4542-DEAL-PR-LINE                       
580075     MOVE SPACE                TO 4542-TEVORMRK                           
580076     MOVE MSGI-TILOKDAT        TO 4542-TIREGDAT                           
580077     MOVE MSGI-TILOKTID        TO WS-TIHHMM                               
580078     MOVE WS-TIHHMMSS          TO 4542-TIREGTID                           
580079     MOVE +0                   TO 4542-TIUPPDAT                           
580080     MOVE +0                   TO 4542-TIUPPTID                           
580081     MOVE OBKR-IDLEVNR         TO 4542-IDLEVNR                            
580082                                                                          
580083*---------------------------------------UPPLÄGG TILL NY VORKÖ             
580084*                                       SKER I W40293                     
580085     PERFORM S40-HAMTA-WDB6-INFO                                          
580086     IF  DCS-NDC                                                          
580087                                                                          
580088         PERFORM IMS-ISRT-WDGX4542                                        
580089         PERFORM UNTIL SEGMENT-FINNS                                      
580090                                                                          
580091            ADD +1             TO 4542-IDLOPNR                            
580092            PERFORM IMS-ISRT-WDGX4542                                     
580093         END-PERFORM                                                      
580094                                                                          
580095         PERFORM IMS-GHU-WDK711                                           
580096         IF  DCS-NDC-CN                                                   
580097         OR (DCS-NDC-NA AND DCS-USA)                                      
580099            IF SLAG-IDDC-REF = SPACE                                      
580100               PERFORM IMS-GU-WDK722                                      
580101               IF SEGMENT-FINNS AND XLAG-IDANSK > 0                       
580102                  MOVE XLAG-IDANSK TO WS-IDANSK                           
580103               ELSE                                                       
580104                  PERFORM IMS-GHU-WDK611                                  
580105                  MOVE XLAG-IDANSK TO WS-IDANSK                           
580106               END-IF                                                     
580108               PERFORM S11A-STARTA-W2T191X                                
580109            END-IF                                                        
580110         END-IF                                                           
580111     END-IF                                                               
580112                                                                          
580113     IF (4542-KDORDBEK NOT = 92 AND 98) AND                               
580114        4542-IDLEVNR = SPACE                                              
580115       IF DCS-CDC                                                         
580116         MOVE 4542-IDARTNR    TO W-IDARTNR                                
580117         PERFORM IMS-GHU-WDK901                                           
580118         COMPUTE ART-KVOKS-VOR =                                          
580119           ART-KVOKS-VOR + (4542-KVBEART-Q - 4542-KVPREAVB)               
580120         PERFORM IMS-REPL-WDK901                                          
580121       END-IF                                                             
580122     END-IF                                                               
580123     PERFORM S24-CHANGE-PRICE-Q-LINE                                      
580124     .                                                                    
580125     EJECT                                                                
580126                                                                          
580127 S11A-STARTA-W2T191X   SECTION.                                           
580128                                                                          
580129     MOVE 'S11A-STARTA-W2T191X '    TO   CURRENT-SECTION                  
580130                                                                          
580131     MOVE +1                    TO 2191-MID-KDCLAGER                      
580132     MOVE 4542-IDARTNR          TO 2191-MID-IDARTNR                       
580133     MOVE ZERO                  TO 2191-MID-TISENBEK-DAG                  
580134                                   2191-MID-TISENBEK-KL                   
580135     MOVE SPACE                 TO 2191-MID-IDKR                          
580136     MOVE WS-IDANSK             TO 2191-MID-IDANSK                        
580137     MOVE '500'                 TO 2191-MID-KDLARM                        
580138     MOVE 4542-IDDISTR          TO WS-IDDISTR-NUM4                        
580139     MOVE WS-IDDISTR-NUM4       TO 2191-MID-IDDISTR                       
580140     MOVE 4542-IDKUNDNR         TO WS-IDKUNDNR-NUM6                       
580141     MOVE WS-IDKUNDNR-NUM6      TO 2191-MID-IDKUNDNR                      
580142     MOVE 4542-IDKUNDRF         TO 2191-MID-IDKUNDRF                      
580143     MOVE 'J'                   TO 2191-MID-FLNYLARM                      
580144     MOVE SLAG-IDDC             TO 2191-MID-IDDC                          
580145     MOVE SLAG-IDLEVNR          TO 2191-MID-IDLEVNR                       
580146                                                                          
580147     COMPUTE MSG-KVLL = LENGTH OF 2191-MID-W2I19101 + 17                  
580148     MOVE 'W2T191X '            TO MSG-KDTRANS-1                          
580149     MOVE '4258'                TO MSG-IDTRANS-1                          
580150     MOVE '1'                   TO MSG-KDMFSFOR-1                         
580151     MOVE 2191-MID-W2I19101     TO MSG-INDATA-MINUS-1-TRANSKOD            
580152                                                                          
580153     PERFORM IMS-PURG-MSG-2191                                            
580154                                                                          
580155     MOVE SPACE                 TO 2191-MID-W2I19101                      
580156     .                                                                    
580157     EJECT                                                                
580158 S13-CALL-WORKDAY-LDC SECTION.                                            
580159                                                                          
580160     MOVE ORAD-IDDC             TO W-IDDC                                 
580161** RÄKNAR X+1 ARBETSDAGAR BAKÅT                                           
580162     MOVE WC-CDC-SE             TO WORK-IDDC                              
580163     MOVE +003                  TO WORK-KDCALL                            
580164     COMPUTE WORK-KVWORKD       = GMT-KVDAGAR-CDC + 1                     
580165     MOVE ARB-TIRFS             TO WS-TIRFS                               
580166     MOVE WS-TIRFS-DAT          TO WORK-TIAAMMDD-TOM                      
580167     CALL WORKDAY               USING WORK-KDCALL                         
580168                                      WORK-DATE-AREA                      
580169                                      WORK-KDSVAR                         
580170     IF WORK-KDSVAR-FEL                                                   
580171        MOVE 'SECT S13-, DATUM SAKNAS I WORKDAY'                          
580172                                TO    FELTEXT                             
580173        CALL ABEND              USING RKOD-ABEND                          
580174     END-IF                                                               
580175     .                                                                    
580176     EJECT                                                                
580177 S15-CALL-WORKDAY-LDC-CDC-RFS SECTION.                                    
580178     MOVE WC-CDC-SE             TO WORK-IDDC                              
580179     MOVE +003                  TO WORK-KDCALL                            
580180     MOVE +001                  TO WORK-KVWORKD                           
580181     MOVE ARB-TIRFS             TO WS-TIRFS                               
580182     MOVE WS-TIRFS-DAT          TO WORK-TIAAMMDD-TOM                      
580183     CALL WORKDAY               USING WORK-KDCALL                         
580184                                      WORK-DATE-AREA                      
580185                                      WORK-KDSVAR                         
580186     IF WORK-KDSVAR-FEL                                                   
580187        MOVE 'SECT S15-, DATUM SAKNAS I WORKDAY'                          
580188                                TO    FELTEXT                             
580189        CALL ABEND              USING RKOD-ABEND                          
580190     END-IF                                                               
580191     .                                                                    
580192     EJECT                                                                
580193 S16-KOLLA-REPARATIONSDATUM SECTION.                                      
580194                                                                          
580195     MOVE JA                TO W-REPSW                                    
580196**   MOVE WC-CDC-SE         TO WORK-IDDC                                  
580197     MOVE ORAD-IDDC         TO WORK-IDDC                                  
580198     IF ORAD-IDDC = 'SE' OR 'BE' OR 'DE' OR 'NO'                          
580199       MOVE WC-CDC-SE       TO WORK-IDDC                                  
580200     END-IF                                                               
580201     MOVE OHUV-TIREPDAT     TO WORK-TIAAMMDD-FOM                          
580202     MOVE +002              TO WORK-KDCALL                                
580203     MOVE +001              TO WORK-KVWORKD                               
580204     CALL WORKDAY           USING WORK-KDCALL                             
580205                                  WORK-DATE-AREA                          
580206                                  WORK-KDSVAR                             
580207     IF WORK-KDSVAR-FEL                                                   
580208        MOVE 'SECT S16-, DATUM SAKNAS I WORKDAY'                          
580209                            TO    FELTEXT                                 
580210        CALL ABEND          USING RKOD-ABEND                              
580211     ELSE                                                                 
580212        MOVE +003                   TO WORK-KDCALL                        
580213*       REPAIRDAY - ANTAL RFSDAY - DAGAR (WEDN-MOND)                      
580214        MOVE GMT-KVDAGAR-RFS-DEF   TO WORK-KVWORKD                        
580215        PERFORM                                                           
580216        VARYING RFS-IX FROM 1 BY 1                                        
580217          UNTIL RFS-IX > MAX-RFS-IX                                       
580218          IF GMT-IDDC-RFS (RFS-IX) = WORK-IDDC                            
580219            MOVE GMT-KVDAGAR-RFS (RFS-IX)                                 
580220                                   TO WORK-KVWORKD                        
580221          END-IF                                                          
580222        END-PERFORM                                                       
580223        ADD +2  TO WORK-KVWORKD                                           
580224        CALL WORKDAY           USING WORK-KDCALL                          
580225                                     WORK-DATE-AREA                       
580226                                     WORK-KDSVAR                          
580227        IF WORK-KDSVAR-FEL                                                
580228           MOVE 'SECT S16-2, DATUM SAKNAS I WORKDAY'                      
580229                               TO    FELTEXT                              
580230           CALL ABEND          USING RKOD-ABEND                           
580231        ELSE                                                              
580232           IF WORK-TIAAMMDD-FOM < DAGENS-DATUM                            
580233             MOVE NEJ            TO W-REPSW                               
580234           END-IF                                                         
580235        END-IF                                                            
580236     END-IF                                                               
580237     .                                                                    
580238     EJECT                                                                
580239                                                                          
580240 S20-OBKR-FRAN-TILLK-TAB SECTION.                                         
580241                                                                          
580242     MOVE 'STA S20-OBKR-TILLK  '           TO CURRENT-SECTION             
580243     MOVE +1                   TO WS-INDEX-TILLK                          
580244     PERFORM UNTIL WS-INDEX-TILLK > WS-INDEX-TILLK-MAX OR                 
580245                TILK-IDARTNR(WS-INDEX-TILLK) = ZERO                       
580246        IF TILK-FLTILLK-X(WS-INDEX-TILLK) = JA                            
580247           IF OBKR-SKRIVEN                                                
580248              PERFORM IMS-ISRT-WDQ101                                     
580249              ADD +1              TO OBKR-IDSEKVNR                        
580250           END-IF                                                         
580251           MOVE KERS-KDORDBEK  TO OBKR-KDORDBEK                           
580252           MOVE '4258KER4'     TO OBKR-IDPGM                              
580253           MOVE TILK-IDARTNR-TILLK(WS-INDEX-TILLK)                        
580254                               TO OBKR-IDARTNR-TILLK                      
580255           MOVE TILK-REKSIFFR-TILLK(WS-INDEX-TILLK)                       
580256                               TO OBKR-REKSIFFR-TILLK                     
580257           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
580258                               TO OBKR-KVBEART-TILLK                      
580259           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
580260              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
580261             MOVE +0              TO OBKR-DIERS-KVOT                      
580262           ELSE                                                           
580263             COMPUTE OBKR-DIERS-KVOT =                                    
580264                   TILK-DIERS-TILLK(WS-INDEX-TILLK) /                     
580265                   TILK-DIERS-ERS(WS-INDEX-TILLK)                         
580266           END-IF                                                         
580267           MOVE TILK-BEERS(WS-INDEX-TILLK)                                
580268                               TO OBKR-BEERS                              
580269           MOVE ZEROES         TO OBKR-KVPREAVB                           
580270                                  OBKR-KVPRERO                            
580271           MOVE 'S'            TO OBKR-SW                                 
580272        END-IF                                                            
580273        ADD +1                 TO WS-INDEX-TILLK                          
580274     END-PERFORM                                                          
580275     IF WS-INDEX-TILLK = +1                                               
580276        MOVE +0                TO OBKR-KDERS                              
580277     END-IF                                                               
580278     .                                                                    
580279     EJECT                                                                
580280 S22-DATA-TILL-DEL-NOTE SECTION.                                          
580281                                                                          
580282     MOVE 'STA S22-DEL-NOTE    '           TO   CURRENT-SECTION           
580283     MOVE OHUV-IDDISTR             TO TEST-IDDISTR                        
580284     IF DIST07-USA-RETAILER-DNOTE                                         
580285     OR DIST07-CAN-RETAILER                                               
580286     INITIALIZE DNOT-ORDER-INFO                                           
580287                                                                          
580288        MOVE IDPGM                    TO DNOT-IDPGM                       
580289        MOVE OHUV-IDORDER             TO DNOT-IDORDER                     
580290        MOVE ORAD-IDARTNR             TO DNOT-IDARTNR                     
580291        MOVE ORAD-IDDC                TO DNOT-IDDC                        
580292        MOVE OHUV-ADGMT-GATA          TO DNOT-ADGMT-GATA                  
580293        MOVE OHUV-ADGMT-PADR          TO DNOT-ADGMT-PADR                  
580294        MOVE OHUV-ADGMT-LAND          TO DNOT-ADGMT-LAND                  
580295        MOVE OHUV-BEGMT-RAD1          TO DNOT-BEGMT-RAD1                  
580296        MOVE OHUV-BEGMT-RAD2          TO DNOT-BEGMT-RAD2                  
580297        MOVE OHUV-BEKUNDRF            TO DNOT-BEKUNDRF                    
580298        MOVE ORAD-BERADREF            TO DNOT-BERADREF                    
580299        MOVE OHUV-IDGMTREF            TO DNOT-IDGMTREF                    
580300        MOVE OHUV-IDDC-PRIM           TO DNOT-IDDC-PRIM                   
580301        MOVE ORAD-IDKUNDRF-RO         TO DNOT-IDKUNDRF-RO                 
580302        MOVE ORAD-FLTILLK             TO DNOT-FLTILLK                     
580303        MOVE OHUV-KDORDKL             TO DNOT-KDORDKL                     
580304        MOVE ARB-KDFRAKT              TO DNOT-KDFRAKT                     
580305        MOVE ORAD-KVBEART             TO DNOT-KVBEART                     
580306        MOVE ORAD-KVBEART-Q           TO DNOT-KVBEART-Q                   
580307        MOVE ORAD-REKSIFFR            TO DNOT-REKSIFFR                    
580308        MOVE ORAD-TIREGDAT            TO DNOT-TIREGDAT                    
580309        MOVE ORAD-TIREGTID            TO DNOT-TIREGTID                    
580310*       IF MID-FLDIRLEV (WS-INDEX-ORFK ) = JA OR YES                      
580311*          MOVE JA                    TO DNOT-FLDIRLEV                    
580312*       ELSE                                                              
580313           MOVE NEJ                   TO DNOT-FLDIRLEV                    
580314*       END-IF                                                            
580315                                                                          
580316        IF MID-FLSLUT = JA                                                
580317           IF WS-INDEX-ORFK = WS-INDEX-ORFK-MAX                           
580318           OR MID-IDARTNR(WS-INDEX-ORFK + 1) = ALL '+'                    
580319              MOVE JA              TO DNOT-FL-ORAD-LAST                   
580320           END-IF                                                         
580321        END-IF                                                            
580322                                                                          
580323        CALL W411DNOT USING DNOT-W411DNOT                                 
580324                            DNOT-ORQP-PCB                                 
580325                            DNOT-ORQP2-PCB                                
580326                            DNOT-ORQP3-PCB                                
580327                            DNOT-4013-PCB                                 
580328                            DNOT-BENA-PCB                                 
580329     END-IF                                                               
580330     .                                                                    
580331     EJECT                                                                
580332 S23-DELETE-PRICE-Q-LINE  SECTION.                                        
580333                                                                          
580334     IF DIST79-DEALER-PRICE                                               
580335       IF ORAD-IDPRQUES > ZERO                                            
580336         INITIALIZE PRQU-W335PRQU                                         
580337         MOVE OHUV-IDDISTR       TO PRQU-IDDISTR                          
580338         MOVE OHUV-IDKUNDNR      TO PRQU-IDKUNDNR                         
580339         MOVE OHUV-IDKUNDRF      TO PRQU-IDKUNDRF                         
580340         MOVE ORAD-IDPRQUES      TO PRQU-IDPRQUES                         
580341         MOVE 4                  TO PRQU-KDCALL                           
580342         CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                 
580343                                            PRQU-WDC7-PCB                 
580344                                            PRQU-SJKO-WDK6-PCB            
580345       END-IF                                                             
580346     END-IF                                                               
580347     .                                                                    
580348     EJECT                                                                
580349 S24-CHANGE-PRICE-Q-LINE SECTION.                                         
580350                                                                          
580351     IF DIST79-DEALER-PRICE                                               
580352       IF ORAD-IDPRQUES > ZERO                                            
580353         INITIALIZE PRQU-W335PRQU                                         
580354         MOVE OHUV-IDDISTR       TO PRQU-IDDISTR                          
580355         MOVE OHUV-IDKUNDNR      TO PRQU-IDKUNDNR                         
580356         MOVE OHUV-IDKUNDRF      TO PRQU-IDKUNDRF                         
580357         MOVE ORAD-IDPRQUES      TO PRQU-IDPRQUES                         
580358         MOVE ORAD-KVBEART-Q     TO PRQU-KVBEART-Q                        
580359         MOVE 5                  TO PRQU-KDCALL                           
580360         CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                 
580361                                            PRQU-WDC7-PCB                 
580362                                            PRQU-SJKO-WDK6-PCB            
580363       END-IF                                                             
580364     END-IF                                                               
580365     .                                                                    
580366     EJECT                                                                
580367                                                                          
580368 S40-HAMTA-WDB6-INFO      SECTION.                                        
580369                                                                          
580370     IF WS-CLDC-IX > IX-DCCLEAR-MAX                                       
580371        CONTINUE                                                          
580372     ELSE                                                                 
580373        IF CLDC-IDDC (WS-CLDC-IX) NOT = WS-IDDC-SEEK                      
580374           MOVE 1 TO WS-CLDC-IX                                           
580375           PERFORM UNTIL WS-CLDC-IX > IX-DCCLEAR-MAX OR                   
580376                         CLDC-IDDC (WS-CLDC-IX) = WS-IDDC-SEEK OR         
580377                         CLDC-IDDC (WS-CLDC-IX) = SPACE                   
580378              ADD 1 TO WS-CLDC-IX                                         
580379           END-PERFORM                                                    
580380        END-IF                                                            
580381     END-IF                                                               
580382     IF WS-CLDC-IX > IX-DCCLEAR-MAX OR                                    
580383        CLDC-IDDC (WS-CLDC-IX) = SPACE                                    
580384        MOVE WS-IDDC-SEEK TO W-IDDC-B6                                    
580385        PERFORM IMS-GU-WDB601                                             
580386     ELSE                                                                 
580387        MOVE CLDC-WDB601 (WS-CLDC-IX)  TO DLI-IO-WDB601                   
580388     END-IF                                                               
580389     .                                                                    
580390     EJECT                                                                
580391                                                                          
580392 F-BIPACKNING-ORDERAVSLUT SECTION.                                        
580393                                                                          
580394     MOVE 'STA F-BIPACK        '      TO   CURRENT-SECTION                
580395*RC CHECK WHICH ONES TO REMOVE                                            
580396     IF OHUV-FLOVRLEV = JA OR OHUV-FLORDSPE = JA OR                       
580397                              OHUV-KDORDKL  = +0 OR                       
580398                              OHUV-IDKAMPRF > +0 OR                       
580399                              OHUV-KDTPOTYP > +0 OR                       
580400**LDC INGEN BIPACKNING ÄVEN FÖR RADER SOM HAMNAR DIREKT PÅ CDC            
580401                              OHUV-IDSYSTEM = 'LDC ' OR                   
580402                              OHUV-IDSYSTEM = 'TACD' OR                   
580403                              (AVSR-KDROPACK = ZERO OR SPACE)             
580404                                                                          
580405        PERFORM FA-STARTA-ORDERAVSLUT                                     
580406     ELSE                                                                 
580407        PERFORM FB-STARTA-BIPACKNING-4299                                 
580408     END-IF                                                               
580409     .                                                                    
580410     EJECT                                                                
580411                                                                          
580412 FA-STARTA-ORDERAVSLUT SECTION.                                           
580413                                                                          
580414     MOVE 'STA FA-STA-ORD-AVSLU'      TO   CURRENT-SECTION                
580415     MOVE W-WDQ2C-IDDISTR      TO 4298-MID-IDDISTR                        
580416     MOVE W-WDQ2C-IDKUNDNR     TO 4298-MID-IDKUNDNR                       
580417     MOVE W-WDQ2C-IDKUNDRF     TO 4298-MID-IDKUNDRF                       
580418     MOVE OHUV-IDORDER         TO 4298-MID-IDORDER                        
580419                                                                          
580420     COMPUTE MSG-KVLL = LENGTH OF 4298-MID-W4I29801 + 17                  
580421     MOVE 'W4T298X '           TO MSG-KDTRANS-1                           
580422     MOVE '4258'               TO MSG-IDTRANS-1                           
580423     MOVE SPACE                TO MSG-KDMFSFOR-1                          
580424                                                                          
580425     PERFORM IMS-INSERT-4298-MSG                                          
580426     .                                                                    
580427     EJECT                                                                
580428                                                                          
580429 FB-STARTA-BIPACKNING-4299 SECTION.                                       
580430                                                                          
580431     MOVE 'STA FB-4299         '           TO   CURRENT-SECTION           
580432     MOVE OHUV-BEKUNDRF        TO 4299-MID-BEKUNDRF                       
580433     MOVE OHUV-FLEMBORD        TO 4299-MID-FLEMBORD                       
580434     MOVE OHUV-FLFORBI         TO 4299-MID-FLFORBI                        
580435     MOVE OHUV-FLOVRLEV        TO 4299-MID-FLOVRLEV                       
580436     MOVE OHUV-FLPRELRO        TO 4299-MID-FLPRELRO                       
580437     MOVE W-WDQ2C-IDDISTR      TO 4299-MID-IDDISTR                        
580438     MOVE OHUV-IDKAMPRF        TO 4299-MID-IDKAMPRF                       
580439     MOVE OHUV-IDKONTO         TO 4299-MID-IDKONTO                        
580440     MOVE OHUV-IDKST           TO 4299-MID-IDKST                          
580441     MOVE OHUV-IDANALYS        TO 4299-MID-IDANALYS                       
580442     MOVE W-WDQ2C-IDKUNDNR     TO 4299-MID-IDKUNDNR                       
580443     MOVE W-WDQ2C-IDKUNDRF     TO 4299-MID-IDKUNDRF                       
580444     MOVE OHUV-IDRFTAB         TO 4299-MID-IDRFTAB                        
580445     MOVE OHUV-IDORDER         TO 4299-MID-IDORDER                        
580446     MOVE MID-IDSYSTEM         TO 4299-MID-IDSYSTEM                       
580447     MOVE OHUV-KDFAKTYP        TO 4299-MID-KDFAKTYP                       
580448     MOVE ARB-KDFDKRAV         TO 4299-MID-KDFDKRAV                       
580449     MOVE OHUV-KDORDING        TO 4299-MID-KDORDING                       
580450     MOVE OHUV-KDORDKL         TO 4299-MID-KDORDKL                        
580451     MOVE OHUV-KDTPOTYP        TO 4299-MID-KDTPOTYP                       
580452     MOVE OHUV-RESLATT         TO 4299-MID-RESLATT                        
580453     MOVE OHUV-TIREGDAT        TO 4299-MID-TIREGDAT                       
580454     MOVE OHUV-BEVARREF        TO 4299-MID-BEVARREF                       
580455     MOVE OHUV-IDBIPREF        TO 4299-MID-IDBIPREF                       
580456     MOVE ARB-KDROPACK         TO 4299-MID-KDROPACK                       
580457     MOVE ARB-KDFRAKT          TO 4299-MID-KDFRAKT                        
580458     IF OHUV-IDDC-TVS NOT = SPACE                                         
580459       MOVE OHUV-IDDC-TVS      TO 4299-MID-IDDC                           
580460     ELSE                                                                 
580461       MOVE SPACE              TO 4299-MID-IDDC                           
580462     END-IF                                                               
580463                                                                          
580464     COMPUTE MSG-KVLL = LENGTH OF 4299-MID-W4I29901 + 17                  
580465     MOVE 'W4T299X '           TO MSG-KDTRANS-1                           
580466     MOVE '4258'               TO MSG-IDTRANS-1                           
580467     MOVE SPACE                TO MSG-KDMFSFOR-1                          
580468                                                                          
580469     PERFORM IMS-INSERT-4299-MSG                                          
580470     .                                                                    
580471     EJECT                                                                
580472                                                                          
580473 G-SKICKA-PRISFRAGA SECTION.                                              
580474                                                                          
580475     MOVE 1                      TO 3039-REQU-IDMSGVER                    
580476     MOVE SPACE                  TO 3039-REQU-KDPGMACT                    
580477     MOVE 'W4025800'             TO 3039-REQU-IDUSER                      
580478                                                                          
580479     MOVE SPACE                  TO 3039-MID-IDBUNDLE                     
580480     MOVE ORAD-IDDISTR           TO 3039-MID-IDDISTR                      
580481     MOVE ORAD-IDKUNDNR          TO 3039-MID-IDKUNDNR                     
580482       MOVE ORAD-IDORDNR7        TO 3039-MID-IDBUNDLE                     
580483     MOVE ZERO                   TO 3039-MID-IDPRQUES                     
580484                                                                          
580485     PERFORM S04-SKICKA-OPEN                                              
580486     PERFORM S04-SKICKA-MEDDELANDE                                        
580487     PERFORM S04-SKICKA-CLOSE                                             
580488                                                                          
580489     .                                                                    
580490     EJECT                                                                
580491 K-STARTA-2109  SECTION.                                                  
580492                                                                          
580493     MOVE 'STA K-STARTA-2109    '    TO   CURRENT-SECTION                 
580494     COMPUTE 2109-KVLL = LENGTH OF 2109-MID2-W2I10902 + 17                
580495                                                                          
580496     PERFORM IMS-PURG-ALT-MSG-2109                                        
580497                                                                          
580498     MOVE SPACE              TO 2109-MID2-W2I10902                        
580499     MOVE +1                 TO 2109-INDX                                 
580500     .                                                                    
580501     EJECT                                                                
580502 Z-FINIT  SECTION.                                                        
580503                                                                          
580504*    SKRIV MED TILL MPP DISPATCHERN                                       
580505     IF MSG-KOM-IDMFSMED = SPACE                                          
580506        MOVE OK-BEHANDLAD      TO MSG-KOM-IDMFSMED                        
580507     END-IF                                                               
580508     PERFORM IMS-INSERT-DISP-MSG                                          
580510     .                                                                    
580600     EJECT                                                                
740101 IMS-GET-MSG SECTION.                                                     
740102                                                                          
740103     MOVE '  QC' TO GODK-STATUSKODER                                      
740104     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
740105     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
740106     PERFORM IMS-STATUSKONTROLL                                           
740107     .                                                                    
740108     SKIP2                                                                
740109 IMS-GN-MSG SECTION.                                                      
740110                                                                          
740120     MOVE '  '   TO GODK-STATUSKODER                                      
740130     CALL CBLTDLI USING GN MSG-PCB KOM-IO-AREA                            
740140     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
740150     PERFORM IMS-STATUSKONTROLL                                           
740160     .                                                                    
740170     SKIP2                                                                
740171 IMS-INSERT-DISP-MSG SECTION.                                             
740172                                                                          
740173     MOVE SPACE TO GODK-STATUSKODER                                       
740174     CALL CBLTDLI USING ISRT DISP-PCB KOM-IO-AREA                         
740175     MOVE DISP-STATUS-CODE TO STATUS-WS                                   
740176     PERFORM IMS-STATUSKONTROLL                                           
740177     .                                                                    
740178     EJECT                                                                
740179 IMS-INSERT-4298-MSG SECTION.                                             
740180                                                                          
740190     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
740200     MOVE SPACE TO GODK-STATUSKODER                                       
740210     CALL CBLTDLI USING ISRT 4298-PCB MSG-IO-AREA                         
740220     MOVE 4298-STATUS-CODE TO STATUS-WS                                   
740221     PERFORM IMS-STATUSKONTROLL                                           
740222     .                                                                    
740223     SKIP3                                                                
740224 IMS-INSERT-4299-MSG SECTION.                                             
740225                                                                          
740226     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
740227     MOVE SPACE TO GODK-STATUSKODER                                       
740228     CALL CBLTDLI USING ISRT 4299-PCB MSG-IO-AREA                         
740229     MOVE 4299-STATUS-CODE TO STATUS-WS                                   
740230     PERFORM IMS-STATUSKONTROLL                                           
740231     .                                                                    
740232     EJECT                                                                
740233                                                                          
740234 IMS-PURG-MSG-2191  SECTION.                                              
740235                                                                          
740236     MOVE SPACE TO GODK-STATUSKODER                                       
740237     CALL  CBLTDLI  USING PURG 2191-PCB MSG-IO-AREA                       
740238     MOVE 2191-STATUS-CODE TO STATUS-WS                                   
740239     PERFORM IMS-STATUSKONTROLL                                           
740240     .                                                                    
740241     EJECT                                                                
740242 IMS-PURG-ALT-MSG-2109 SECTION.                                           
740243     MOVE LOW-VALUE TO 2109-Z1 2109-Z2                                    
740244     MOVE SPACE TO GODK-STATUSKODER                                       
740245     CALL CBLTDLI USING PURG 2109-PCB W-PROG-TO-PROG-SW-2                 
740246     MOVE 2109-STATUS-CODE TO STATUS-WS                                   
740247     PERFORM IMS-STATUSKONTROLL                                           
740248     .                                                                    
740249     EJECT                                                                
740250 IMS-GU-WDB101 SECTION.                                                   
740251     MOVE 'IMS-GU-WDB101   ' TO CURRENT-IMS-SECTION                       
740252                                                                          
740253     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
740254          DELIMITED BY SIZE INTO SSA1                                     
740255     MOVE '  '                 TO GODK-STATUSKODER                        
740256     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-AREA-WDB101 SSA1               
740257     MOVE WDB1-STATUS-CODE     TO STATUS-WS                               
740258     PERFORM IMS-STATUSKONTROLL                                           
740259     .                                                                    
740260     EJECT                                                                
740261 IMS-GU-WDB201 SECTION.                                                   
740262     MOVE 'IMS-GU-WDB201   ' TO CURRENT-IMS-SECTION                       
740263                                                                          
740264     MOVE SPACE               TO ALL-SSA                                  
740265     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
740266          DELIMITED BY SIZE INTO SSA1                                     
740267     MOVE '  GE'              TO GODK-STATUSKODER                         
740268     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
740269     MOVE WDB2-STATUS-CODE    TO STATUS-WS                                
740270     PERFORM IMS-STATUSKONTROLL                                           
740271     .                                                                    
740272     EJECT                                                                
741301 IMS-GU-WDB601    SECTION.                                                
741401     MOVE 'IMS-GU-WDB601   ' TO CURRENT-IMS-SECTION                       
741501                                                                          
741601     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
741701          DELIMITED BY SIZE INTO SSA1                                     
741801     MOVE '    '             TO GODK-STATUSKODER                          
741901     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
742001     MOVE WDB6-STATUS-CODE   TO STATUS-WS                                 
742101     PERFORM IMS-STATUSKONTROLL                                           
742201     .                                                                    
742301     EJECT                                                                
742302 IMS-GU-WDK601  SECTION.                                                  
742303     MOVE 'IMS-GU-WDK601   ' TO CURRENT-IMS-SECTION                       
742304                                                                          
742305     MOVE SPACE                 TO ALL-SSA                                
742306     STRING 'WDK601  (IDARTNR = ' W-IDARTNR-X ')'                         
742307            DELIMITED BY SIZE INTO SSA1                                   
742308     MOVE '  GE'                TO GODK-STATUSKODER                       
742309     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
742310     MOVE WDK6-STATUS-CODE      TO STATUS-WS                              
742320     PERFORM IMS-STATUSKONTROLL                                           
742321     .                                                                    
742322     EJECT                                                                
742330 IMS-GNP-WDK611  SECTION.                                                 
742340     MOVE 'IMS-GNP-WDK611  ' TO CURRENT-IMS-SECTION                       
742350                                                                          
742360     MOVE SPACE                 TO ALL-SSA                                
742370     MOVE 'WDK611 '             TO SSA1                                   
742380     MOVE '  GE'                TO GODK-STATUSKODER                       
742390     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
742400     MOVE WDK6-STATUS-CODE      TO STATUS-WS                              
742500     PERFORM IMS-STATUSKONTROLL                                           
742600     .                                                                    
742700     EJECT                                                                
746101 IMS-GU-WDQ201-WDQ2C SECTION.                                             
746201     MOVE 'IMS-GU-WDQ201   ' TO CURRENT-IMS-SECTION                       
746401                                                                          
746501     STRING 'WDQ201  (WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                        
746601          DELIMITED BY SIZE INTO SSA1                                     
746701     MOVE '  GE'               TO GODK-STATUSKODER                        
746801     CALL CBLTDLI USING GU WDQ2-PCB DLI-IO-AREA-WDQ2 SSA1                 
746901     MOVE WDQ2-STATUS-CODE     TO STATUS-WS                               
747001     PERFORM IMS-STATUSKONTROLL                                           
747101     .                                                                    
747201     EJECT                                                                
747202 IMS-GNP-WDQ212  SECTION.                                                 
747203                                                                          
747204     MOVE 'IMS-GNP-WDQ212  ' TO CURRENT-IMS-SECTION                       
747205     MOVE 'WDQ212'   TO SSA1                                              
747206     MOVE '  GE' TO GODK-STATUSKODER                                      
747207     CALL CBLTDLI USING GNP WDQ2-PCB DLI-IO-AREA-ARB SSA1                 
747208     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
747209     PERFORM IMS-STATUSKONTROLL                                           
747210     .                                                                    
747211     EJECT                                                                
747212 IMS-GHU-WDQ2-WDQ201 SECTION.                                             
747213     MOVE 'IMS-GHU-WDQ2-WDQ' TO CURRENT-IMS-SECTION                       
747214                                                                          
747215     STRING 'WDQ201  (WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                        
747216          DELIMITED BY SIZE INTO SSA1                                     
747217     MOVE '  GE'               TO GODK-STATUSKODER                        
747218     CALL CBLTDLI USING GHU WDQ2-PCB DLI-IO-AREA-WDQ2 SSA1                
747219     MOVE WDQ2-STATUS-CODE     TO STATUS-WS                               
747240     PERFORM IMS-STATUSKONTROLL                                           
747250     .                                                                    
747260     EJECT                                                                
747270 IMS-GHNP-WDQ212  SECTION.                                                
747271     MOVE 'IMS-GHNP-WDQ212 ' TO CURRENT-IMS-SECTION                       
747272                                                                          
747280     MOVE 'WDQ212'   TO SSA1                                              
747290     MOVE '  GE' TO GODK-STATUSKODER                                      
747300     CALL CBLTDLI USING GHNP WDQ2-PCB DLI-IO-AREA-ARB SSA1                
747301     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
747303     PERFORM IMS-STATUSKONTROLL                                           
747304     .                                                                    
747305     EJECT                                                                
747306 IMS-GNP-WDQ212-FIRST   SECTION.                                          
747307     MOVE 'IMS-GNP-WDQ212 ' TO CURRENT-IMS-SECTION                        
747309                                                                          
747310     STRING 'WDQ212  *F(IDDC     =' W-IDDC-WDQ212-X ')'                   
747311          DELIMITED BY SIZE INTO SSA1                                     
747312     MOVE '  GE' TO GODK-STATUSKODER                                      
747313     CALL CBLTDLI USING GNP WDQ2-PCB DLI-IO-AREA-ARB SSA1                 
747314     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
747315     PERFORM IMS-STATUSKONTROLL                                           
747316     .                                                                    
747317     SKIP3                                                                
747318 IMS-REPL-WDQ201 SECTION.                                                 
747319     MOVE 'IMS-REPL-WDQ201' TO CURRENT-IMS-SECTION                        
747320                                                                          
747321     MOVE '    '               TO GODK-STATUSKODER                        
747322     CALL CBLTDLI USING REPL WDQ2-PCB DLI-IO-AREA-WDQ2                    
747323     MOVE WDQ2-STATUS-CODE     TO STATUS-WS                               
747325     PERFORM IMS-STATUSKONTROLL                                           
747326     .                                                                    
747327     EJECT                                                                
747328 IMS-REPL-WDQ212  SECTION.                                                
747329     MOVE 'IMS-REPL-WDQ212' TO CURRENT-IMS-SECTION                        
747331                                                                          
747332     MOVE '    '               TO GODK-STATUSKODER                        
747333     CALL CBLTDLI USING REPL WDQ2-PCB DLI-IO-AREA-ARB                     
747334     MOVE WDQ2-STATUS-CODE     TO STATUS-WS                               
747336     PERFORM IMS-STATUSKONTROLL                                           
747337     .                                                                    
747338     EJECT                                                                
747339 IMS-GNP-WDQ221 SECTION.                                                  
747340     MOVE 'IMS-GNP-WDQ221 ' TO CURRENT-IMS-SECTION                        
747342                                                                          
747343     STRING 'WDQ212  (IDDC     =' W-IDDC-WDQ221-X ')'                     
747344          DELIMITED BY SIZE INTO SSA1                                     
747345     MOVE 'WDQ221   '         TO SSA2                                     
747346     MOVE '  GE' TO GODK-STATUSKODER                                      
747347     CALL CBLTDLI USING GNP WDQ2-PCB DLI-IO-AREA-LOR SSA1 SSA2            
747348     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
747349     PERFORM IMS-STATUSKONTROLL                                           
747350     .                                                                    
747351     EJECT                                                                
747352 IMS-GU-WDQ4A SECTION.                                                    
747353     MOVE 'IMS-GU-WDQ4A' TO CURRENT-IMS-SECTION                           
747355                                                                          
747356     STRING 'WDQ4A1  (WDQ4A1KY>=' W-WDQ4A1KY-MIN-X                        
747357                    '&WDQ4A1KY<=' W-WDQ4A1KY-MAX-X ')'                    
747358          DELIMITED BY SIZE INTO SSA1                                     
747359     MOVE '  GBGE' TO GODK-STATUSKODER                                    
747360     CALL CBLTDLI USING GU WDQ4A-PCB DLI-IO-AREA-WDQ4A SSA1               
747361     MOVE WDQ4A-STATUS-CODE TO STATUS-WS                                  
747363     PERFORM IMS-STATUSKONTROLL                                           
747364     .                                                                    
747365     EJECT                                                                
747366 IMS-GN-WDQ4A1  SECTION.                                                  
747367     MOVE 'IMS-GN-WDQ4A1 ' TO CURRENT-IMS-SECTION                         
747368                                                                          
747369     STRING 'WDQ4A1  (WDQ4A1KY>=' W-WDQ4A1KY-MIN-X                        
747370                    '&WDQ4A1KY<=' W-WDQ4A1KY-MAX-X ')'                    
747371          DELIMITED BY SIZE INTO SSA1                                     
747372     MOVE '  GBGE' TO GODK-STATUSKODER                                    
747373     CALL CBLTDLI USING GN WDQ4A-PCB DLI-IO-AREA-WDQ4A SSA1               
747374     MOVE WDQ4A-STATUS-CODE TO STATUS-WS                                  
747376     PERFORM IMS-STATUSKONTROLL                                           
747377     .                                                                    
747378     EJECT                                                                
747379 IMS-GU-WDK611  SECTION.                                                  
747380     MOVE 'IMS-GU-WDK611  '  TO CURRENT-IMS-SECTION                       
747381                                                                          
747382     MOVE SPACE                 TO ALL-SSA                                
747383     STRING 'WDK601  (IDARTNR = ' W-IDARTNR-X ')'                         
747384            DELIMITED BY SIZE INTO SSA1                                   
747385     MOVE 'WDK611   ' TO SSA2                                             
747386     MOVE '  GE'                TO GODK-STATUSKODER                       
747387     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
747388     MOVE WDK6-STATUS-CODE      TO STATUS-WS                              
747389     PERFORM IMS-STATUSKONTROLL                                           
747390     .                                                                    
747391     EJECT                                                                
747392 IMS-GHU-WDK901 SECTION.                                                  
747393     MOVE 'IMS-GU-WDK901  '  TO CURRENT-IMS-SECTION                       
747394                                                                          
747395     MOVE SPACE                 TO ALL-SSA                                
747396     STRING 'WDK901  (IDARTNR  =' W-IDARTNR-X ')'                         
747397          DELIMITED BY SIZE INTO SSA1                                     
747398     MOVE '  GE'               TO GODK-STATUSKODER                        
747399     CALL CBLTDLI USING GHU WDK9-PCB DLI-IO-WDK901 SSA1                   
747400     MOVE WDK9-STATUS-CODE     TO STATUS-WS                               
747401     PERFORM IMS-STATUSKONTROLL                                           
747402     .                                                                    
747403     EJECT                                                                
747404 IMS-REPL-WDK901 SECTION.                                                 
747405     MOVE 'IMS-REPL-WDK901' TO CURRENT-IMS-SECTION                        
747406                                                                          
747407     MOVE '  ' TO GODK-STATUSKODER                                        
747408     CALL CBLTDLI USING REPL WDK9-PCB DLI-IO-WDK901                       
747409     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
747410     PERFORM IMS-STATUSKONTROLL                                           
747411     .                                                                    
747412     EJECT                                                                
747413 IMS-GHU-WDK611 SECTION.                                                  
747414     MOVE 'IMS-GHU-WDK611 ' TO CURRENT-IMS-SECTION                        
747415                                                                          
747416     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
747417          DELIMITED BY SIZE INTO SSA1                                     
747418     MOVE 'WDK611   '         TO SSA2                                     
747419     MOVE '    '              TO GODK-STATUSKODER                         
747420     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
747421     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
747422     PERFORM IMS-STATUSKONTROLL                                           
747423     .                                                                    
747424 IMS-GU-WDK611-TILK SECTION.                                              
747425     MOVE 'IMS-GU-WDK611-T' TO CURRENT-IMS-SECTION                        
747426                                                                          
747427     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-T-X ')'                       
747428          DELIMITED BY SIZE INTO SSA1                                     
747429     MOVE 'WDK611   '         TO SSA2                                     
747430     MOVE '    '              TO GODK-STATUSKODER                         
747431     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611-T SSA1 SSA2             
747432     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
747433     PERFORM IMS-STATUSKONTROLL                                           
747434     .                                                                    
747435     EJECT                                                                
747436 IMS-GU-WDK711-TILK SECTION.                                              
747437     MOVE 'IMS-GU-WDK711-T' TO CURRENT-IMS-SECTION                        
747438                                                                          
747439     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-T-X ')'                       
747440          DELIMITED BY SIZE INTO SSA1                                     
747441     STRING 'WDK711  (IDDC     =' W-IDDC-T-X ')'                          
747442          DELIMITED BY SIZE INTO SSA2                                     
747443     MOVE '  GE'               TO GODK-STATUSKODER                        
747444     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK7 SSA1 SSA2            
747445     MOVE WDK7-STATUS-CODE     TO STATUS-WS                               
747446     PERFORM IMS-STATUSKONTROLL                                           
747447     .                                                                    
747448     EJECT                                                                
747473 IMS-GU-WDQ101 SECTION.                                                   
747474     MOVE 'IMS-GU-WDQ101  ' TO CURRENT-IMS-SECTION                        
747475                                                                          
747476     STRING 'WDQ101  (WDQ101KY >' W-WDQ101KY-MIN-X                        
747477                    '&WDQ101KY <' W-WDQ101KY-MAX-X ')'                    
747478          DELIMITED BY SIZE INTO SSA1                                     
747479     MOVE '  GE'               TO GODK-STATUSKODER                        
747480     CALL CBLTDLI USING GU   WDQ1-PCB DLI-IO-AREA-WDQ1 SSA1               
747481     MOVE WDQ1-STATUS-CODE     TO STATUS-WS                               
747482     PERFORM IMS-STATUSKONTROLL                                           
747483     .                                                                    
747484     SKIP2                                                                
747485 IMS-GN-WDQ101      SECTION.                                              
747486     MOVE 'IMS-GN-WDQ101  ' TO CURRENT-IMS-SECTION                        
747487                                                                          
747488     STRING 'WDQ101  (WDQ101KY >' W-WDQ101KY-MIN-X                        
747489                    '&WDQ101KY <' W-WDQ101KY-MAX-X ')'                    
747490          DELIMITED BY SIZE INTO SSA1                                     
747491     MOVE '  GEGB'             TO GODK-STATUSKODER                        
747492     CALL CBLTDLI USING GN   WDQ1-PCB DLI-IO-AREA-WDQ1 SSA1               
747493     MOVE WDQ1-STATUS-CODE     TO STATUS-WS                               
747494     PERFORM IMS-STATUSKONTROLL                                           
747495     .                                                                    
747496     SKIP2                                                                
747497 IMS-ISRT-WDQ101 SECTION.                                                 
747498     MOVE 'IMS-ISRT-WDQ101' TO CURRENT-IMS-SECTION                        
747499                                                                          
747500     MOVE 'WDQ101   '          TO SSA1                                    
747501     MOVE '    '               TO GODK-STATUSKODER                        
747502     CALL CBLTDLI USING ISRT WDQ1-PCB DLI-IO-AREA-WDQ1 SSA1               
747503     MOVE WDQ1-STATUS-CODE     TO STATUS-WS                               
747504     PERFORM IMS-STATUSKONTROLL                                           
747505     .                                                                    
747506     EJECT                                                                
747507 IMS-ISRT-WDQ401 SECTION.                                                 
747508     MOVE 'IMS-ISRT-WDQ401' TO CURRENT-IMS-SECTION                        
747509                                                                          
747510     MOVE 'WDQ401   '          TO SSA1                                    
747511     MOVE '  II'               TO GODK-STATUSKODER                        
747512     CALL CBLTDLI USING ISRT WDQ4-PCB DLI-IO-AREA-WDQ4 SSA1               
747513     MOVE WDQ4-STATUS-CODE     TO STATUS-WS                               
747514     PERFORM IMS-STATUSKONTROLL                                           
747515     .                                                                    
747516     EJECT                                                                
747517 IMS-GHU-WDQ401 SECTION.                                                  
747518     MOVE 'IMS-GHU-WDQ401' TO CURRENT-IMS-SECTION                         
747519                                                                          
747520     STRING 'WDQ401  (WDQ401KY =' W-WDQ401KY-X ')'                        
747521        DELIMITED BY SIZE INTO SSA1                                       
747522     MOVE '  GE'               TO GODK-STATUSKODER                        
747523     CALL CBLTDLI USING GHU WDQ4-PCB DLI-IO-AREA-WDQ4 SSA1                
747524     MOVE WDQ4-STATUS-CODE     TO STATUS-WS                               
747526     PERFORM IMS-STATUSKONTROLL                                           
747527     .                                                                    
747528     EJECT                                                                
747529 IMS-DLET-WDQ401 SECTION.                                                 
747530     MOVE 'IMS-DLET-WDQ401'   TO CURRENT-IMS-SECTION                      
747531                                                                          
747532     MOVE '    '               TO GODK-STATUSKODER                        
747533     CALL CBLTDLI USING DLET WDQ4-PCB DLI-IO-AREA-WDQ4                    
747534     MOVE WDQ4-STATUS-CODE     TO STATUS-WS                               
747537     PERFORM IMS-STATUSKONTROLL                                           
747538     .                                                                    
747539     EJECT                                                                
747558 IMS-ISRT-WDGX4542 SECTION.                                               
747559     MOVE 'IMS-ISRT-WDGX45'    TO CURRENT-IMS-SECTION                     
747560                                                                          
747561     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-4541-X ')'                    
747562          DELIMITED BY SIZE INTO SSA1                                     
747563     MOVE 'WDGX4542 '          TO SSA2                                    
747564     MOVE '  II'               TO GODK-STATUSKODER                        
747565     CALL CBLTDLI USING ISRT 4541-PCB DLI-IO-AREA-4541 SSA1 SSA2          
747566     MOVE 4541-STATUS-CODE     TO STATUS-WS                               
747567     PERFORM IMS-STATUSKONTROLL                                           
747568     .                                                                    
747569     SKIP2                                                                
747570                                                                          
747571 IMS-GHU-WDK711 SECTION.                                                  
747572     MOVE 'IMS-GHU-WDK711'  TO CURRENT-IMS-SECTION                        
747573                                                                          
747574     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
747575          DELIMITED BY SIZE INTO SSA1                                     
747576     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
747577          DELIMITED BY SIZE INTO SSA2                                     
747578     MOVE '    '               TO GODK-STATUSKODER                        
747579     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-AREA-WDK7 SSA1 SSA2           
747580     MOVE WDK7-STATUS-CODE     TO STATUS-WS                               
747581     PERFORM IMS-STATUSKONTROLL                                           
747582     .                                                                    
747583     EJECT                                                                
747584 IMS-REPL-WDK711 SECTION.                                                 
747585     MOVE 'IMS-REPL-WDK71'  TO CURRENT-IMS-SECTION                        
747586                                                                          
747587     MOVE '  ' TO GODK-STATUSKODER                                        
747588     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-AREA-WDK7                    
747589     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
747590     PERFORM IMS-STATUSKONTROLL                                           
747591     .                                                                    
747592     EJECT                                                                
747593 IMS-GU-WDK722 SECTION.                                                   
747594                                                                          
747595     MOVE 'IMS-GU-WDK722 '  TO CURRENT-IMS-SECTION                        
747596                                                                          
747597     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
747598          DELIMITED BY SIZE INTO SSA1                                     
747599     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
747600          DELIMITED BY SIZE INTO SSA2                                     
747601     MOVE 'WDK722 '           TO SSA3                                     
747602     MOVE '  GE'               TO GODK-STATUSKODER                        
747603     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3          
747604     MOVE WDK7-STATUS-CODE     TO STATUS-WS                               
747605     PERFORM IMS-STATUSKONTROLL                                           
747606     .                                                                    
747607 IMS-GNP-WDQ2-WDQ211-FIRST SECTION.                                       
747608     MOVE 'IMS-GNP-WDQ2- '  TO CURRENT-IMS-SECTION                        
747609                                                                          
747610     MOVE 'WDQ211  *F'  TO SSA1                                           
747611     MOVE '  GE' TO GODK-STATUSKODER                                      
747612     CALL CBLTDLI USING GNP WDQ2-PCB DLI-IO-AREA-DIRL SSA1                
747613     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
747614     PERFORM IMS-STATUSKONTROLL                                           
747615     .                                                                    
747616     EJECT                                                                
747617 IMS-GNP-WDQ2-WDQ211 SECTION.                                             
747618     MOVE 'IMS-GNP-WDQ2-W'  TO CURRENT-IMS-SECTION                        
747619                                                                          
747620     MOVE 'WDQ211   ' TO SSA1                                             
747621     MOVE '  GE' TO GODK-STATUSKODER                                      
747622     CALL CBLTDLI USING GNP WDQ2-PCB DLI-IO-AREA-DIRL SSA1                
747623     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
747624     PERFORM IMS-STATUSKONTROLL                                           
747625     .                                                                    
747626     EJECT                                                                
747627 IMS-STATUSKONTROLL SECTION.                                              
747628                                                                          
747629     SET STATUS-IX TO 1                                                   
747630     SEARCH GODK-STATUS                                                   
747701       AT END CALL FELLOG                                                 
747801       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
747901     END-SEARCH                                                           
748001     .                                                                    
748101     EJECT                                                                
748201 DB2-STATUSKONTROLL  SECTION.                                             
748301                                                                          
748401     SET SQLCODE-IX TO 1                                                  
748501     SEARCH GODK-SQLCODE                                                  
748601       AT END                                                             
748701          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
748801          DELIMITED BY SIZE INTO FELTEXT                                  
748901          CALL ABEND USING RKOD-ABEND-DB2                                 
749001       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
749101     END-SEARCH                                                           
750001     .                                                                    
750002     EJECT                                                                
750003*    -COPY WY2000Q1                                                       
