000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W4069800.                                                
000400 AUTHOR.         LASSI OLGRENER.                                          
000500 DATE-WRITTEN.   95/09/16.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        SKAPAR DET BERÖMDA TRANSPORT DOKUMENTET 'B O L L A'              
001000*        FÖR ALL GODS SOM LÄMNAR LAGER I ITALIEN.                         
001100*                                                                         
001200*        FÖR LAGER SOM JOBBAR MED PULS CLASSIC STARTAS PROGRAMMET         
001300*        FRÅN BILD 4664. BOLLADOKUMENTET SKICKAS TILL D&P FÖR             
001400*        UTSKRIFT OCH LAGRING. 4698 STARTAR U- ELLER X- TRANS AV          
001500*        4675 BILDEN.                                                     
001600*                                                                         
001700*        FÖR LAGER SOM ANVÄNDER PULS-WEB STARTAS PROGRAMMET FRÅN          
001800*        WL0187. DATA TILL BOLLADOKUMENTET SKICKAS TILL D&P SOM           
001900*        VIA WL0187'S KONTROLLBÖNA,                                       
002000*        KONTROLLBÖNAND VISAR DOKUMENTET PÅ WEBBEN OCH                    
002100*        DOKUMENTET KAN SKRIVAS UT PÅ LOKAL PRINTER. NÄR                  
002200*        NÄR DOKUMENTET KOMMIT TILL WL0187'S KONTROLLBÖNA STARTAR         
002300*        DENNA WL0188.                                                    
002400*        PÅ DATABASEN WDR4 HÄNDELSE 4491-4492-4494, LAGRAS                
002500*        BOLLA INFORMATION PER LAGER.  VARJE LAGER HAR SIN EGEN           
002600*        NUMMERSERIE.                                                     
002700*                                                                         
002800*        PROGRAMMET LÄSER      WDB2                                       
002900*        PROGRAMMET LÄSER      WDB7                                       
003000*        PROGRAMMET LÄSER      WDB7A                                      
003100*        PROGRAMMET LÄSER      WDE4                                       
003200*        PROGRAMMET LÄSER      WDE6                                       
003300*        PROGRAMMET LÄSER      WDQ2                                       
003400*        PROGRAMMET LÄSER      WL4495 (WDR4)                              
003500*        PROGRAMMET UPPDATERAR WL4491 (WDR4)                              
003600*                                                                         
003700*    INDATA.                                                              
003800*        TRANSAKTION: W4T698X                                             
003900*        MID:         W4I69801                                            
004000     SKIP3                                                                
004100 ENVIRONMENT DIVISION.                                                    
004200     EJECT                                                                
004300 DATA DIVISION.                                                           
004400 WORKING-STORAGE SECTION.                                                 
004500                                                                          
004600*    -- CHECKED BY WY2000                                                 
004700 77  IDPGM                       PIC X(08)   VALUE 'W4069800'.            
004800 77  FELTEXTTEXT                 PIC X(08)   VALUE 'FELTEXT'.             
004900 77  FELTEXT                     PIC X(64)   VALUE SPACE.                 
005000 77  CURRENT-SECTION             PIC X(64)   VALUE SPACE.                 
005100                                                                          
005200 01  ERRTEXT.                                                             
005300     03 FILLER                   PIC X(8)    VALUE SPACE.                 
005400     03 ERRTEXT-STR              PIC X(72)   VALUE SPACE.                 
005500 77  KDRC-DISPLAY                PIC Z(5).                                
005600                                                                          
005700 77  JA                          PIC X       VALUE 'J'.                   
005800 77  YES                         PIC X       VALUE 'Y'.                   
005900 77  NEJ                         PIC X       VALUE 'N'.                   
006000 77  INDX-KVKOLLI                PIC 9(3)    VALUE ZERO.                  
006100 77  INDX                        PIC S9(9)   VALUE +0  COMP SYNC.         
006200 77  INDX-4492                   PIC S9(9)   VALUE +0  COMP SYNC.         
006300 77  MAX-INDX                    PIC S9(9)   VALUE +15 COMP SYNC.         
006400 77  MAX-INDX-4492               PIC S9(9)   VALUE +10 COMP SYNC.         
006500 77  RAD-INDX                    PIC S9(9)   VALUE +0  COMP SYNC.         
006600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   VALUE +33 COMP SYNC.         
006700 77  WS-IDDC                     PIC X(2)    VALUE SPACE.                 
006800 77  WS-4498-IDDISTR             PIC S9(5)   VALUE +0    COMP-3.          
006900 77  WS-4498-IDKUNDNR            PIC S9(7)   VALUE +0    COMP-3.          
007000 77  SPAR-IDDISTR                PIC S9(5)   VALUE +0    COMP-3.          
007100 77  SPAR-IDKUNDNR               PIC S9(7)   VALUE +0    COMP-3.          
007200 77  SPAR-IDKUNDRF               PIC X(10)   VALUE SPACE.                 
007300 77  W-KDMFSFOR                  PIC X(1)    VALUE SPACE.                 
007400 77  W-VKORDBTO                  PIC S9(6)V9    VALUE +0 COMP-3.          
007500 77  W-VLORDBTO                  PIC S9(4)V9(3) VALUE +0 COMP-3.          
007600 77  W-KVKOLLI                   PIC 9(3)    VALUE ZERO.                  
007700 77  WS-UPPD                     PIC 9(3)    VALUE ZERO.                  
007800 77  WS-RAD-RAKNARE              PIC 9(3)    VALUE ZERO.                  
007900 77  WS-SID-RAKNARE              PIC 9(3)    VALUE ZERO.                  
008000 77  WS-KDORDKL                  PIC 9(1)    VALUE ZERO.                  
008100 77  WS-IDTRPTNR                 PIC S9(3)   VALUE ZERO COMP-3.           
008200 01  W-IDDISTR-NUM               PIC  9(4)   VALUE ZERO.                  
008300 01  FILLER                      REDEFINES W-IDDISTR-NUM.                 
008400     03  W-IDDISTR-ALFA          PIC  X(4).                               
008500     EJECT                                                                
008600 01  WS-DAP-HDR.                                                          
008700     03 WS-DAP-IDDC                  PIC X(2)    VALUE SPACE.             
008800     03 WS-DAP-IDDISTR               PIC X(4)    VALUE SPACE.             
008900                                                                          
009000 01  WS-NUM5                     PIC 9(5).                                
009100 01  WS-REDUIN                   PIC X(30)   VALUE SPACE.                 
009200 01  WS-REDUUT                   PIC X(30)   VALUE SPACE.                 
009300                                                                          
009400                                                                          
009500 01  FILLER                      PIC X(16)   VALUE 'SWITCHAR    '.        
009600                                                                          
009700 77  TRAFF-SW                    PIC X       VALUE 'N'.                   
009800     88  TRAFF                               VALUE 'J'.                   
009900     88  NO-TRAFF                            VALUE 'N'.                   
010000                                                                          
010100 77  SW-FLTRANS                  PIC X(1)    VALUE 'N'.                   
010200     88  FL-XTRANS                           VALUE 'J'.                   
010300                                                                          
010400 77  FIRST-TIME-SW               PIC X(1)    VALUE 'J'.                   
010500     88  FIRST-TIME                          VALUE 'J'.                   
010600     88  NOT-FIRST-TIME                      VALUE 'N'.                   
010700                                                                          
010800*    --- STYRTECKEN PRINTER                                               
010900 01  WS-PAGESKIP                 PIC X      VALUE '1'.                    
011000 01  WS-SKIP1                    PIC X      VALUE ' '.                    
011100 01  WS-SKIP2                    PIC X      VALUE '0'.                    
011200 01  WS-SKIP3                    PIC X      VALUE '-'.                    
011300                                                                          
011400 01  SEND-RAD.                                                            
011500     03  STYRTECKEN-RAD          PIC X.                                   
011600     03  FILLER                  PIC X(81)  VALUE SPACE.                  
011700                                                                          
011800 01  SEND-AREA.                                                           
011900*    03  -COPY WZ01SEND                                                   
012000                                                                          
012100 01  HDR-AREA.                                                            
012200*    03  -COPY WZ01REQU                                                   
012300*    03  -COPY WZ04HDR                                                    
012400                                                                          
012500                                                                          
012600     EJECT                                                                
012700 01  TEST-IDDISTR                PIC 9(5)    COMP-3.                      
012800*01  FILLER       -COPY WWDIST09    -RED TEST-IDDISTR.                    
012900     EJECT                                                                
012910*    ----TABELL FÖR X-TRANS--------                                       
013000*01  FILLER  -COPY W476DIST.                                              
013200                                                                          
013300 01  FILLER                      PIC X(16)   VALUE 'WORK FIELDS '.        
013400 01  ANTAL-KOLLI-PER-TYP.                                                 
013500     03  W-KVKOLLITYP OCCURS 15  PIC 9(2).                                
013600                                                                          
013700 01  WS-RAD-INFO                 PIC X(60).                               
013800 01  FILLER REDEFINES WS-RAD-INFO.                                        
013900     03  WS-RAD-INFO-13          PIC X(30).                               
014000     03  WS-RAD-INFO-14          PIC X(30).                               
014100                                                                          
014200 01  WS-IDTRPBO.                                                          
014300     03  FILLER                  PIC X       VALUE 'H'.                   
014400     03  WS-IDTRPBON             PIC 9(7).                                
014500                                                                          
014600 01 WS-IT-POSTADRESS.                                                     
014700     03  WS-IT-POSTNR            PIC X(5).                                
014800     03  WS-IT-MELLAN            PIC X(1) VALUE SPACE.                    
014900     03  WS-IT-ORT               PIC X(25).                               
015000                                                                          
015100 01  DAGENS-DATUM-Y2K            PIC 9(8).                                
015200 01  FILLER REDEFINES DAGENS-DATUM-Y2K.                                   
015300     03  DAGENS-AAAA-Y2K         PIC 9(4).                                
015400     03  DAGENS-MM-Y2K           PIC 9(2).                                
015500     03  DAGENS-DD-Y2K           PIC 9(2).                                
015600 01  FILLER REDEFINES DAGENS-DATUM-Y2K.                                   
015700     03  FILLER                  PIC 9(2).                                
015800     03 DAGENS-DATUM             PIC 9(6).                                
015900     03 FILLER REDEFINES DAGENS-DATUM.                                    
016000         05 DAGENS-AA            PIC 9(2).                                
016100         05 DAGENS-MM            PIC 9(2).                                
016200         05 DAGENS-DD            PIC 9(2).                                
016300                                                                          
016400 01  DAGENS-TID.                                                          
016500     03  DAGENS-HH               PIC 9(2).                                
016600     03  DAGENS-MIN              PIC 9(2).                                
016700     03  FILLER                  PIC 9(4).                                
016800 01  WEB-TID.                                                             
016900     03  WEB-HH                  PIC X(2).                                
017000     03  WEB-PUNKT               PIC X VALUE '.'.                         
017100     03  WEB-MM                  PIC X(2).                                
017200     EJECT                                                                
017300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
017400 01  GENERELLA-SUBPROGRAM.                                                
017500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
017600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
017700     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
017800     03  W006PRR1                PIC X(8)    VALUE 'W006PRR1'.            
017900     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
018000     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
018100     03  W009REDU                PIC X(8)    VALUE 'W009REDU'.            
018200     SKIP3                                                                
018300 01  FILLER                      PIC X(16)   VALUE 'W006PRAR'.            
018400*01  -COPY W006PRAR                                                       
018500     EJECT                                                                
018600 01  FILLER                      PIC X(16)   VALUE 'WORKAREA'.            
018700*01  -COPY WORKAREA                                                       
018800     EJECT                                                                
018900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
019000     SKIP3                                                                
019100*01  MID -COPY W4I69801                                                   
019200     EJECT                                                                
019300                                                                          
019400 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
019500     SKIP3                                                                
019600*01  -COPY WMSGAREA                                                       
019700     05 -COPY W4I67501 -PRE 4675-  -RED MSG-MID-OUT                       
019800     EJECT                                                                
019900                                                                          
020000 01  WEB-BOLLA-SEND-HEADER.                                               
020100*    03  -COPY W406981 -PRE WEB-                                          
020200                                                                          
020300 01  WEB-BOLLA-SEND-RAD.                                                  
020400*    03  -COPY W406982 -PRE WEB-                                          
020500                                                                          
020600 01  FILLER                      PIC X(16)   VALUE 'SEND-RADER'.          
020700 01  SEND-RADER.                                                          
020800                                                                          
020900     03 HEADER.                                                           
021000       05 FILLER               PIC X(28) VALUE SPACE.                     
021100       05 RAD-TYP-IDSHIP       PIC X(14).                                 
021200       05 FILLER               PIC X(3)  VALUE SPACE.                     
021300       05 RAD-DATE             PIC 9(6).                                  
021400       05 FILLER               PIC X(2)  VALUE SPACE.                     
021500       05 RAD-TRP              PIC ZZ9.                                   
021600       05 FILLER               PIC X(1)  VALUE SPACE.                     
021700       05 RAD-CARRIER          PIC X(12).                                 
021800       05 FILLER               PIC X(2)  VALUE SPACE.                     
021900       05 RAD-IDDC             PIC X(2).                                  
022000       05 FILLER               PIC X(5)  VALUE SPACE.                     
022100       05 RAD-SIDNR            PIC Z9.                                    
022200                                                                          
022300     03 RUBRIKRAD.                                                        
022400       05 FILLER               PIC X(6)  VALUE SPACE.                     
022500       05 FILLER               PIC X(11) VALUE 'CESSIONARIO'.             
022600       05 FILLER               PIC X(28) VALUE SPACE.                     
022700       05 FILLER               PIC X(30) VALUE 'LUOGO DI DESTINAZI        
022800-                                              'ONE DEI BENI'.            
022900       05 FILLER               PIC X(5)  VALUE SPACE.                     
023000                                                                          
023100     03 DETALJRAD-1.                                                      
023200       05 FILLER               PIC X(6)   VALUE SPACE.                    
023300       05 RAD1-BEBET           PIC X(27).                                 
023400       05 FILLER               PIC X(12)  VALUE SPACE.                    
023500       05 RAD1-BEGODSM         PIC X(27).                                 
023600       05 FILLER               PIC X(10)  VALUE SPACE.                    
023700                                                                          
023800     03 DETALJRAD-2.                                                      
023900       05 FILLER               PIC X(6)   VALUE SPACE.                    
024000       05 RAD2-ADBET-GATA      PIC X(27).                                 
024100       05 FILLER               PIC X(12)  VALUE SPACE.                    
024200       05 RAD2-ADGODSM-GATA    PIC X(27).                                 
024300       05 FILLER               PIC X(10)  VALUE SPACE.                    
024400                                                                          
024500     03 DETALJRAD-3.                                                      
024600       05 FILLER               PIC X(6)   VALUE SPACE.                    
024700       05 RAD3-ADBET-ORT       PIC X(27).                                 
024800       05 FILLER               PIC X(12)  VALUE SPACE.                    
024900       05 RAD3-ADGODSM-ORT     PIC X(27).                                 
025000       05 FILLER               PIC X(10)  VALUE SPACE.                    
025100                                                                          
025200     03 DETALJRAD-4.                                                      
025300       05 FILLER               PIC X(6)   VALUE SPACE.                    
025400       05 RAD4-BE              PIC X(27)  VALUE SPACE.                    
025500***         'VOLVO AUTO ITALIA S.P.A.   '.                                
025600       05 FILLER               PIC X(8)   VALUE SPACE.                    
025700       05 RAD4-ASPETTO         PIC X(13)  VALUE SPACE.                    
025800       05 FILLER               PIC X(3)  VALUE SPACE.                     
025900       05 RAD4-KVKOLLI         PIC ZZZ.                                   
026000       05 FILLER               PIC X(2)  VALUE SPACE.                     
026100       05 RAD4-VKORDBTO        PIC Z(4).                                  
026200       05 FILLER               PIC X     VALUE SPACE.                     
026300       05 RAD4-PORTO           PIC X(13).                                 
026400                                                                          
026500     03 DETALJRAD-5.                                                      
026600       05 FILLER               PIC X(44)  VALUE SPACE.                    
026700       05 RAD5-BEEMBTYP        PIC X(13).                                 
026800       05 RAD5-KVEMBTYP        PIC ZZZ.                                   
026900       05 FILLER               PIC X(7)  VALUE SPACE.                     
027000       05 RAD5-PORTO           PIC X(13).                                 
027100                                                                          
027200     03 DETALJRAD-6.                                                      
027300       05 FILLER               PIC X(4)   VALUE SPACE.                    
027400       05 RAD6-AD-GATA         PIC X(27)  VALUE SPACE.                    
027500***         'VIA ENRICO MATTEI 66       '.                                
027600       05 FILLER               PIC X(13)  VALUE SPACE.                    
027700       05 RAD6-BEEMBTYP        PIC X(13).                                 
027800       05 RAD6-KVEMBTYP        PIC ZZZ.                                   
027900       05 FILLER               PIC X(7)  VALUE SPACE.                     
028000       05 RAD6-PORTO           PIC X(13).                                 
028100                                                                          
028200     03 DETALJRAD-7.                                                      
028300       05 FILLER               PIC X(44)  VALUE SPACE.                    
028400       05 RAD7-BEEMBTYP        PIC X(13).                                 
028500       05 RAD7-KVEMBTYP        PIC ZZZ.                                   
028600       05 FILLER               PIC X(20) VALUE SPACE.                     
028700                                                                          
028800     03 DETALJRAD-8.                                                      
028900       05 FILLER               PIC X(4)   VALUE SPACE.                    
029000       05 RAD8-AD-ORT          PIC X(33)  VALUE SPACE.                    
029100***         '40138 BOLOGNA P.IVA IT00326610375'.                          
029200       05 FILLER               PIC X(7)   VALUE SPACE.                    
029300       05 RAD8-BEEMBTYP        PIC X(13).                                 
029400       05 RAD8-KVEMBTYP        PIC ZZZ.                                   
029500                                                                          
029600     03 DETALJRAD-9.                                                      
029700       05 FILLER               PIC X(44)  VALUE SPACE.                    
029800       05 RAD9-BEEMBTYP        PIC X(13).                                 
029900       05 RAD9-KVEMBTYP        PIC ZZZ.                                   
030000                                                                          
030100*    03 DETALJRAD-10.                                                     
030200*      05 FILLER               PIC X(44)  VALUE SPACE.                    
030300*      05 RAD10-BEEMBTYP       PIC X(13).                                 
030400*      05 RAD10-KVEMBTYP       PIC ZZZ.                                   
030500*      05 FILLER               PIC X(23)  VALUE SPACE.                    
030600                                                                          
030700     03 DETALJRAD-10A.                                                    
030800       05 FILLER               PIC X(6)   VALUE SPACE.                    
030900       05 RAD10A-BETRP         PIC X(27).                                 
031000       05 FILLER               PIC X(49)  VALUE SPACE.                    
031100                                                                          
031200     03 DETALJRAD-10B.                                                    
031300       05 FILLER               PIC X(8)   VALUE SPACE.                    
031400       05 RAD10B-ADTRP-GATA    PIC X(27).                                 
031500       05 FILLER               PIC X(47)  VALUE SPACE.                    
031600                                                                          
031700     03 DETALJRAD-11.                                                     
031800       05 FILLER               PIC X(8)   VALUE SPACE.                    
031900       05 RAD11-ADTRP-ORT      PIC X(27).                                 
032000       05 FILLER               PIC X(10)  VALUE SPACE.                    
032100       05 RAD11-CAUSALE        PIC X(35).                                 
032200       05 FILLER               PIC X(2)   VALUE SPACE.                    
032300                                                                          
032400     03 BLANKRAD.                                                         
032500       05 FILLER               PIC X(82)  VALUE SPACE.                    
032600                                                                          
032700     03 DETALJRAD-11A.                                                    
032800       05 FILLER               PIC X(17)  VALUE SPACE.                    
032900       05 RAD11A-TRPDAT-DD     PIC X(2).                                  
033000       05 RAD11A-SLASH1        PIC X      VALUE '/'.                      
033100       05 RAD11A-TRPDAT-MM     PIC X(2).                                  
033200       05 RAD11A-SLASH2        PIC X      VALUE '/'.                      
033300       05 RAD11A-TRPDAT-AAAA   PIC X(4).                                  
033400       05 FILLER               PIC X(2)   VALUE SPACE.                    
033500       05 RAD11A-TRPTID-HH     PIC X(2).                                  
033600       05 RAD11A-PUNKT         PIC X      VALUE '.'.                      
033700       05 RAD11A-TRPTID-MM     PIC X(2).                                  
033800       05 FILLER               PIC X(48)  VALUE SPACE.                    
033900                                                                          
034000     03 DETALJRAD-12.                                                     
034100       05 FILLER               PIC X(6)   VALUE SPACE.                    
034200       05 RAD12-IDKUNDNR       PIC Z(5)9.                                 
034300       05 FILLER               PIC X(15)  VALUE '   ORDINE      '.        
034400       05 RAD12-IDTRPBO        PIC X(8).                                  
034500       05 FILLER               PIC X(2)   VALUE SPACE.                    
034600       05 RAD12-BOLLADAT-DD    PIC 9(2).                                  
034700       05 FILLER               PIC X      VALUE '/'.                      
034800       05 RAD12-BOLLADAT-MM    PIC 9(2).                                  
034900       05 FILLER               PIC X      VALUE '/'.                      
035000       05 RAD12-BOLLADAT-AAAA  PIC 9(4).                                  
035100       05 FILLER               PIC X(3)   VALUE SPACE.                    
035200       05 RAD12-COD-NOTE       PIC X(32).                                 
035300                                                                          
035400     03 DETALJRAD-13.                                                     
035500       05 FILLER               PIC X(15)  VALUE SPACE.                    
035600       05 WS-KDORDKL-TEXT      PIC X(14).                                 
035700       05 FILLER               PIC X(21)  VALUE SPACE.                    
035800       05 RAD13-INSTR-NOTE     PIC X(32).                                 
035900                                                                          
036000     03 DETALJRAD-14.                                                     
036100       05 FILLER               PIC X(15)  VALUE SPACE.                    
036200       05 RAD14-IDORDNR        PIC Z(4)9.                                 
036300       05 FILLER               PIC X(30)  VALUE SPACE.                    
036400       05 RAD14-INSTR-NOTE     PIC X(32).                                 
036500                                                                          
036600     03 ARTIKELRAD.                                                       
036700       05 FILLER               PIC X(5)  VALUE SPACE.                     
036800       05 ARTRAD-SEKVNR        PIC ZZ9.                                   
036900       05 FILLER               PIC X(2)  VALUE SPACE.                     
037000       05 ARTRAD-IDARTNR       PIC Z(8)9.                                 
037100       05 FILLER               PIC X(2)   VALUE SPACE.                    
037200       05 ARTRAD-BEART         PIC X(20).                                 
037300       05 FILLER               PIC X      VALUE SPACE.                    
037400       05 ARTRAD-KVLEVART      PIC Z(3)9.                                 
037500       05 FILLER               PIC X(5)  VALUE SPACE.                     
037600       05 ARTRAD-IDKOLLI       PIC Z(5).                                  
037700       05 FILLER               PIC X(11) VALUE SPACE.                     
037800       05 ARTRAD-KDPRODSL      PIC ZZ9.                                   
037900       05 FILLER               PIC X(12) VALUE SPACE.                     
038000                                                                          
038100     03 FINALRAD.                                                         
038200       05 FILLER               PIC X(1)  VALUE SPACE.                     
038300       05 FILLER               PIC X(26)  VALUE ALL '*'.                  
038400       05 FILLER     PIC X(23)  VALUE '  F I N E   B O L L A  '.          
038500       05 FILLER               PIC X(29)  VALUE ALL '*'.                  
038600     EJECT                                                                
038700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
038800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
038900     SKIP3                                                                
039000 01  NYCKLAR-TILL-DLI.                                                    
039100     03  W-IDGMT-X.                                                       
039200         05  W-IDDISTR-B2        PIC S9(5)   VALUE ZERO COMP-3.           
039300         05  W-IDKUNDNR-B2       PIC S9(7)   VALUE ZERO COMP-3.           
039400                                                                          
039500     03  W-IDGMT-X-B7.                                                    
039600         05  W-IDDISTR-B7        PIC S9(5)   VALUE ZERO COMP-3.           
039700         05  W-IDKUNDNR-B7       PIC S9(7)   VALUE ZERO COMP-3.           
039800                                                                          
039900     03  W-WDB7ASEQ-X.                                                    
040000         05  W-IDDEALER          PIC X(6)    VALUE SPACE.                 
040100         05  W-IDLANDX2          PIC X(2)    VALUE SPACE.                 
040200                                                                          
040300     03  W-WDB7A1KY-X.                                                    
040400         05  W-IDEALER-B7A       PIC X(6)    VALUE SPACE.                 
040500         05  W-IDDISTR-B7A       PIC S9(5)   VALUE ZERO COMP-3.           
040600         05  W-IDKUNDNR-B7A      PIC S9(7)   VALUE ZERO COMP-3.           
040700                                                                          
040800     03  W-WDGXKEY-4495-X.                                                
040900         05  FILLER              PIC X(4)     VALUE '4495'.               
041000         05  W-IDDC-4495         PIC X(2)     VALUE SPACE.                
041100         05  W-IDTRPTNR          PIC S9(3)    VALUE ZERO COMP-3.          
041200         05  W-IDLBBET           PIC X(12)    VALUE SPACE.                
041300         05  FILLER              PIC X(10)    VALUE LOW-VALUE.            
041400                                                                          
041500     03  W-WDGXKEY-4498-X.                                                
041600       05  W-IDGMTREF-4498.                                               
041700         07  W-IDDISTR           PIC S9(5)    VALUE ZERO COMP-3.          
041800         07  W-IDKUNDNR          PIC S9(7)    VALUE ZERO COMP-3.          
041900         07  W-KDFAKTYP          PIC X        VALUE SPACE.                
042000         07  W-IDKUNDRF          PIC X(10)    VALUE SPACE.                
042100         07  W-IDPRODNR          PIC S9(7)    VALUE ZERO COMP-3.          
042200         07  W-IDKOLLI           PIC S9(5)    VALUE ZERO COMP-3.          
042300                                                                          
042400     03  W-WDE4FSEQ-MIN-X.                                                
042500       05  W-IDPRODNR-X.                                                  
042600         07  W-IDPRODNR-MIN      PIC S9(7)    VALUE ZERO COMP-3.          
042700       05  W-IDKOLLI-MIN         PIC S9(5)    VALUE ZERO COMP-3.          
042800                                                                          
042900     03  W-WDE4FSEQ-MAX-X.                                                
043000       05  W-IDPRODNR-MAX        PIC S9(7)    VALUE ZERO COMP-3.          
043100       05  W-IDKOLLI-MAX         PIC S9(5)    VALUE ZERO COMP-3.          
043200                                                                          
043300     03  W-IDGMTREF-X.                                                    
043400         05  W-IDDISTR-Q2        PIC S9(5)    VALUE ZERO COMP-3.          
043500         05  W-IDKUNDNR-Q2       PIC S9(7)    VALUE ZERO COMP-3.          
043600         05  W-IDKUNDRF-Q2       PIC X(10)    VALUE SPACE.                
043700                                                                          
043800     03  W-WDGXKEY-4491-X.                                                
043900         05  FILLER              PIC X(4)     VALUE '4491'.               
044000         05  W-IDDC-4491         PIC X(2)     VALUE SPACE.                
044100         05  FILLER              PIC X(24)    VALUE LOW-VALUE.            
044200                                                                          
044300     03  W-WDGXKEY-4738-X.                                                
044400         05  FILLER              PIC X(4)     VALUE '4738'.               
044500         05  W-KDEMBTYP          PIC S9(3)    VALUE +0  COMP-3.           
044600         05  FILLER              PIC X(24)    VALUE LOW-VALUE.            
044700     EJECT                                                                
044800     03  W-KY4494-MIN-X.                                                  
044900         05  W-DALASTN-MIN       PIC  9(8)    VALUE ZERO.                 
045000         05  FILLER              PIC X(26)    VALUE LOW-VALUE.            
045100                                                                          
045200     03  W-KY4494-MAX-X.                                                  
045300         05  W-DALASTN-MAX       PIC  9(8) VALUE ZERO.                    
045400         05  FILLER              PIC X(26) VALUE HIGH-VALUE.              
045500                                                                          
045600     03  W-IDDC-B6-X.                                                     
045700         05 W-IDDC-B6            PIC X(2).                                
045800                                                                          
045900     EJECT                                                                
046000*    --- STATUS-KOD FRÅN IMS                                              
046100 01  STATUS-WS                   PIC XX.                                  
046200     88  SEGMENT-FINNS                       VALUE '  '.                  
046300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
046400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
046500     88  BASEN-SLUT                          VALUE 'GB'.                  
046600     SKIP2                                                                
046700 01  GODK-STATUSKODER.                                                    
046800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
046900     SKIP3                                                                
047000 01  SSA1                        PIC X(128).                              
047100 01  SSA2                        PIC X(256).                              
047200     EJECT                                                                
047300*    --- IMS FUNKTIONSKODER                                               
047400*01  -COPY W0003                                                          
047500     EJECT                                                                
047600*    ---  DLI INPUT-OUTPUT AREA                                           
047700 01  FILLER                      PIC X(16)   VALUE 'WDB201-AREA'.         
047800 01  DLI-IO-AREA-B201.                                                    
047900*    03  -COPY WDB201                                                     
048000     EJECT                                                                
048100 01  FILLER                      PIC X(16)   VALUE 'WDB701-AREA'.         
048200 01  DLI-IO-AREA-B701.                                                    
048300*    03  -COPY WDB701                                                     
048400     EJECT                                                                
048500 01  FILLER                      PIC X(16)   VALUE 'WDB7AQ-AREA'.         
048600 01  DLI-IO-AREA-B7AQ.                                                    
048700*    03  -COPY WDB701  -PRE ASEQ-                                         
048800     EJECT                                                                
048900 01  FILLER                      PIC X(16)   VALUE 'WDE601-AREA'.         
049000 01  DLI-IO-AREA-E601.                                                    
049100*    03  -COPY WDE601                                                     
049200     EJECT                                                                
049300 01  FILLER                      PIC X(16)   VALUE 'WDE611-AREA'.         
049400 01  DLI-IO-AREA-E611.                                                    
049500*    03  -COPY WDE611                                                     
049600     EJECT                                                                
049700 01  FILLER                      PIC X(16)   VALUE 'WDE411-AREA'.         
049800 01  DLI-IO-AREA-E411.                                                    
049900*    03  -COPY WDE411                                                     
050000     EJECT                                                                
050100 01  FILLER                      PIC X(16)   VALUE 'WDE421-AREA'.         
050200 01  DLI-IO-AREA-E421.                                                    
050300*    03  -COPY WDE421                                                     
050400     EJECT                                                                
050500 01  FILLER                      PIC X(16)   VALUE 'WDQ201-AREA'.         
050600 01  DLI-IO-AREA-Q201.                                                    
050700*    03  -COPY WDQ201                                                     
050800     EJECT                                                                
050900 01  FILLER                      PIC X(16)   VALUE '4495-AREA'.           
051000 01  DLI-IO-AREA-449501.                                                  
051100*    05  -COPY WDGX4495                                                   
051200     EJECT                                                                
051300 01  FILLER                      PIC X(16)   VALUE '4498-AREA'.           
051400 01  DLI-IO-AREA-449512.                                                  
051500*    05  -COPY WDGX4498                                                   
051600     EJECT                                                                
051700 01  FILLER                      PIC X(16)   VALUE '4492-AREA'.           
051800 01  DLI-IO-AREA-449111.                                                  
051900*    03  -COPY WDGX4492                                                   
052000     EJECT                                                                
052100 01  FILLER                      PIC X(16)   VALUE '4494-AREA'.           
052200 01  DLI-IO-AREA-449112.                                                  
052300*    03  -COPY WDGX4494                                                   
052400     EJECT                                                                
052500 01  FILLER                      PIC X(16)   VALUE '473811-AREA'.         
052600 01  DLI-IO-AREA-473811.                                                  
052700*    03  -COPY WDGX4738                                                   
052800     EJECT                                                                
053400 01  FILLER         PIC X(16) VALUE 'WDB601 - AREA'.                      
053500 01  DLI-IO-AREA-B601.                                                    
053600*    03 -COPY WDB601                                                      
053700     EJECT                                                                
053800                                                                          
053900 LINKAGE SECTION.                                                         
054000                                                                          
054100*01  -COPY W0009   -PRE MSG-                                              
054200     EJECT                                                                
054300*01  -COPY W0009   -PRE DISTRDOC-                                         
054400     EJECT                                                                
054500*01  -COPY W0009   -PRE DISTRWEB-                                         
054600     EJECT                                                                
054700*01  -COPY W0009   -PRE 4675-                                             
054800     EJECT                                                                
054900*01  -COPY W0009   -PRE 4675X-                                            
055000     EJECT                                                                
055100*01  -COPY W0008  -PRE WDB2-                                              
055200     05  FILLER                  PIC X.                                   
055300     EJECT                                                                
055400*01  -COPY W0008  -PRE WDB7-                                              
055500     05  FILLER                  PIC X.                                   
055600     EJECT                                                                
055700*01  -COPY W0008  -PRE WDB7AQ-                                            
055800     05  FILLER                  PIC X.                                   
055900     EJECT                                                                
056000*01  -COPY W0008  -PRE WDE6-                                              
056100     05  FILLER                  PIC X.                                   
056200     EJECT                                                                
056300*01  -COPY W0008  -PRE WDE4-                                              
056400     05  FILLER                  PIC X.                                   
056500     EJECT                                                                
056600*01  -COPY W0008  -PRE WDQ2-                                              
056700     05  FILLER                  PIC X.                                   
056800     EJECT                                                                
056900*01  -COPY W0008  -PRE 4495-                                              
057000     05  FILLER                  PIC X.                                   
057100     EJECT                                                                
057200*01  -COPY W0008  -PRE 4491-                                              
057300     05  FILLER                  PIC X.                                   
057400     EJECT                                                                
057500*01  -COPY W0008  -PRE 4738-                                              
057600     05  FILLER                  PIC X.                                   
057700*                                                                         
057800*01  -COPY W0008  -PRE WDB6-                                              
057900     05  FILLER                  PIC X.                                   
058000*                                                                         
058100 01  LISB-PCB                    PIC X.                                   
058200     EJECT                                                                
058300                                                                          
058400 PROCEDURE DIVISION  USING MSG-PCB DISTRDOC-PCB DISTRWEB-PCB              
058500                     4675-PCB 4675X-PCB                                   
058600                     WDB2-PCB                                             
058700                     WDB7-PCB WDB7AQ-PCB WDE6-PCB WDE4-PCB                
058800                     WDQ2-PCB 4495-PCB 4491-PCB 4738-PCB                  
058900                     WDB6-PCB LISB-PCB.                                   
059000     ENTRY 'DLITCBL' USING MSG-PCB DISTRDOC-PCB DISTRWEB-PCB              
059100                     4675-PCB 4675X-PCB                                   
059200                     WDB2-PCB                                             
059300                     WDB7-PCB WDB7AQ-PCB WDE6-PCB WDE4-PCB                
059400                     WDQ2-PCB 4495-PCB 4491-PCB 4738-PCB                  
059500                     WDB6-PCB LISB-PCB.                                   
059600                                                                          
059700     PERFORM IMS-GET-MSG                                                  
059800     IF SEGMENT-FINNS                                                     
059900       PERFORM A-INIT                                                     
060000       PERFORM B-RENSA-GAMLA-4494                                         
060100       PERFORM C-SKAPA-BOLLA                                              
060200     MOVE HDR-IDOUTREC TO FELTEXT                                         
060300       PERFORM Z-FINIT                                                    
060400       PERFORM D-STARTA-FAKTURA-RELEASE                                   
060500     END-IF                                                               
060600                                                                          
060700     MOVE ZERO TO RETURN-CODE                                             
060800     GOBACK                                                               
060900     .                                                                    
061000     EJECT                                                                
061100 A-INIT SECTION.                                                          
061200     MOVE 'A-INIT'                     TO CURRENT-SECTION                 
061300                                                                          
061400     MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I69801                    
061500     IF MSG-INDATA-MINUS-1-TRANSKOD(1:1) = 'E'                            
061600       MOVE MSG-INDATA-MINUS-1-TRANSKOD (10:20)                           
061700                                     TO MID-W4I69801                      
061800     END-IF                                                               
061900     MOVE MSG-KDMFSFOR-1               TO W-KDMFSFOR                      
062000                                                                          
062100                                                                          
062200     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DATUM-Y2K                 
062300     MOVE MID-IDDC        TO WS-IDDC                                      
062400     MOVE WS-IDDC           TO W-IDDC-B6                                  
062500     PERFORM IMS-GU-WDB601                                                
062600     .                                                                    
062700     EJECT                                                                
062800 B-RENSA-GAMLA-4494 SECTION.                                              
062900     MOVE 'B-RENSA-GAMLA-4494'         TO CURRENT-SECTION                 
063000                                                                          
063100** ALLA SEGM. SOM ÄR ÄLDRE ÄN 5 ARB.DAGAR RENSAS.                         
063200     MOVE WS-IDDC         TO WORK-IDDC                                    
063300                             W-IDDC-4491                                  
063400                             W-IDDC-4495                                  
063500     MOVE 5               TO WORK-KVWORKD                                 
063600     MOVE DAGENS-DATUM    TO WORK-TIAAMMDD-TOM                            
063700     MOVE 003             TO WORK-KDCALL                                  
063800     CALL WORKDAY   USING WORK-KDCALL                                     
063900                          WORK-DATE-AREA                                  
064000                          WORK-KDSVAR                                     
064100     IF WORK-KDSVAR-FEL                                                   
064200        MOVE 'FEL FRÅN WORKDAY I B-SECTION'                               
064300                          TO FELTEXT                                      
064400        CALL ABEND USING RKOD-ABEND-MED-DUMP                              
064500     END-IF                                                               
064600                                                                          
064700     MOVE 1                TO WS-UPPD                                     
064800     PERFORM IMS-GU-4491                                                  
064900     MOVE WORK-TIAAMMDD-FOM TO W-DALASTN-MIN                              
065000     IF WORK-TIAAMMDD-FOM NOT = ZERO                                      
065100       IF WORK-TIAAMMDD-FOM < 500000                                      
065200         MOVE 20           TO W-DALASTN-MIN (1:2)                         
065300       ELSE                                                               
065400         IF WORK-TIAAMMDD-FOM < 999999                                    
065500           MOVE 19         TO W-DALASTN-MIN (1:2)                         
065600         ELSE                                                             
065700           MOVE 99999999   TO W-DALASTN-MIN                               
065800         END-IF                                                           
065900       END-IF                                                             
066000     END-IF                                                               
066100                                                                          
066200     PERFORM IMS-GHNP-4494                                                
066300     PERFORM UNTIL SEGMENT-SAKNAS OR WS-UPPD > 113                        
066400                                                                          
066500       PERFORM IMS-DLET-4494                                              
066600       ADD 1               TO WS-UPPD                                     
066700       PERFORM IMS-GHNP-4494                                              
066800     END-PERFORM                                                          
066900     .                                                                    
067000     EJECT                                                                
067100 C-SKAPA-BOLLA SECTION.                                                   
067200     MOVE 'C-SKAPA-BOLLA'              TO CURRENT-SECTION                 
067300                                                                          
067400     MOVE MID-IDTRPTNR     TO W-IDTRPTNR                                  
067500     MOVE MID-IDLBBET      TO W-IDLBBET                                   
067600     PERFORM CA-ENGAANGS-INFO                                             
067700                                                                          
067800     PERFORM IMS-GU-4495                                                  
067900     PERFORM IMS-GHNP-4498                                                
068000     IF SEGMENT-SAKNAS                                                    
068100       MOVE 'MAN BÖR HITTA ETT SEGMENT!!!!! ' TO FELTEXT                  
068200       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
068300     END-IF                                                               
068400                                                                          
068500     PERFORM UNTIL SEGMENT-SAKNAS                                         
068600                                                                          
068700       IF FIRST-TIME                                                      
068800          IF MSG-INDATA-MINUS-1-TRANSKOD(1:1) = 'E'                       
068900            PERFORM S90-SEND-OPEN-WEB                                     
069000            PERFORM S90-PUT-DAP-START-WEB                                 
069100          ELSE                                                            
069200            PERFORM S90-SEND-OPEN                                         
069300            PERFORM S90-PUT-DAP-START                                     
069400          END-IF                                                          
069500          MOVE NEJ TO FIRST-TIME-SW                                       
069600       END-IF                                                             
069700                                                                          
069800       IF 4498-IDDISTR   = SPAR-IDDISTR   AND                             
069900          4498-IDKUNDNR  = SPAR-IDKUNDNR  AND                             
070000          4498-IDKUNDRF  = SPAR-IDKUNDRF                                  
070100         CONTINUE                                                         
070200       ELSE                                                               
070300         PERFORM CB-INFO-PER-BOLLA                                        
070400         PERFORM CC-SKAPA-BOLLA-SEGM                                      
070500                                                                          
070600         MOVE 4498-IDORDNR7  TO RAD14-IDORDNR                             
070700                                WEB-HEAD-IDORDNR                          
070800         MOVE WS-IDTRPBO     TO RAD12-IDTRPBO                             
070900                                WEB-HEAD-IDTRPBOT                         
071000         MOVE WS-IDTRPBON    TO WEB-HEAD-IDTRPBON                         
071100         ADD  +1             TO WS-IDTRPBON                               
071200         MOVE '1'            TO WEB-HEAD-IDAFPRCD                         
071300                                                                          
071400         IF MSG-INDATA-MINUS-1-TRANSKOD(1:1) = 'E'                        
071500           PERFORM S90-PUT-BOLLA-HEAD-WEB                                 
071600         ELSE                                                             
071700           PERFORM S01-SKRIV-BOLLA-HUVUD                                  
071800         END-IF                                                           
071900       END-IF                                                             
072000       PERFORM CD-SKRIV-RADER                                             
072100                                                                          
072200       MOVE 4498-IDDISTR   TO SPAR-IDDISTR                                
072300       MOVE 4498-IDKUNDNR  TO SPAR-IDKUNDNR                               
072400       MOVE 4498-IDKUNDRF  TO SPAR-IDKUNDRF                               
072500                                                                          
072600*      PERFORM IMS-DLET-4498                                              
072700*      MOVE +0             TO 4498-IDKUNDNR                               
072800*      PERFORM IMS-ISRT-4498                                              
072900                                                                          
073000       PERFORM IMS-GHNP-4498                                              
073100     END-PERFORM                                                          
073200                                                                          
073300     PERFORM CE-UPPDAT-BOLLANR                                            
073400     .                                                                    
073500     EJECT                                                                
073600 CA-ENGAANGS-INFO SECTION.                                                
073700     MOVE 'CA-ENGAANGS-INFO'         TO CURRENT-SECTION                   
073800                                                                          
073900     MOVE DAGENS-AAAA-Y2K       TO RAD11A-TRPDAT-AAAA                     
074000                                   RAD12-BOLLADAT-AAAA                    
074100     MOVE DAGENS-MM             TO RAD11A-TRPDAT-MM                       
074200                                   RAD12-BOLLADAT-MM                      
074300     MOVE DAGENS-DD             TO RAD11A-TRPDAT-DD                       
074400                                   RAD12-BOLLADAT-DD                      
074500     ACCEPT DAGENS-TID          FROM TIME                                 
074600     MOVE DAGENS-HH             TO RAD11A-TRPTID-HH                       
074700     MOVE DAGENS-MIN            TO RAD11A-TRPTID-MM                       
074800     MOVE '/'                   TO RAD11A-SLASH1                          
074900                                 RAD11A-SLASH2                            
075000     MOVE '.'                   TO RAD11A-PUNKT                           
075100     MOVE DAGENS-DATUM-Y2K      TO WEB-HEAD-DATRP                         
075200                                   WEB-HEAD-DABOLLA                       
075300*    ACCEPT WEB-HEAD-TITRP FROM TIME                                      
075400     MOVE FUNCTION CURRENT-DATE (9:2)  TO WEB-HH                          
075500     MOVE FUNCTION CURRENT-DATE (11:2) TO WEB-MM                          
075600     MOVE WEB-TID                      TO WEB-HEAD-TITRP                  
075700                                                                          
075800                                                                          
075900*1:A GÅNGEN TAS BOLLANR FRÅN REG SEDAN ÖKAS MED 1/ORDER.                  
076000     PERFORM IMS-GHNP-4492                                                
076100                                                                          
076200     IF DAGENS-AA = 4492-TIAA                                             
076300       MOVE 4492-IDTRPBON       TO WS-IDTRPBON                            
076400     ELSE                                                                 
076500       MOVE 20000               TO WS-IDTRPBON                            
076600     END-IF                                                               
076700                                                                          
076800     MOVE DAGENS-DATUM-Y2K      TO W-DALASTN-MIN                          
076900                                   W-DALASTN-MAX                          
077000                                                                          
077100     PERFORM IMS-GNP-4494                                                 
077200     IF SEGMENT-FINNS                                                     
077300       MOVE 4494-IDTRPTNR       TO WS-IDTRPTNR                            
077400     ELSE                                                                 
077500       MOVE MID-IDTRPTNR        TO WS-IDTRPTNR                            
077600     END-IF                                                               
077700                                                                          
077800     MOVE DAGENS-DATUM-Y2K      TO 4494-DALASTN                           
077900     MOVE WS-IDTRPTNR           TO 4494-IDTRPTNR                          
078000     MOVE MID-IDTRPTNR          TO 4494-IDTRPTNR-GRUND                    
078100     EJECT                                                                
078200     MOVE SPACE TO RAD10A-BETRP                                           
078300     MOVE +1 TO INDX-4492                                                 
078400     PERFORM UNTIL INDX-4492 > MAX-INDX-4492                              
078500       IF 4492-IDTRPTNR (INDX-4492) = WS-IDTRPTNR                         
078600         MOVE 4492-BETRPFIR (INDX-4492) TO RAD10A-BETRP                   
078700                                           WEB-HEAD-BETRPFIR              
078800         MOVE 4492-ADTRPFIR-RAD1 (INDX-4492) TO RAD10B-ADTRP-GATA         
078900                                     WEB-HEAD-ADTRPFIR-RAD1               
079000         MOVE 4492-ADTRPFIR-RAD2 (INDX-4492) TO RAD11-ADTRP-ORT           
079100                                     WEB-HEAD-ADTRPFIR-RAD2               
079200                                                                          
079300         MOVE +11                       TO INDX-4492                      
079400       ELSE                                                               
079500         ADD +1                         TO INDX-4492                      
079600       END-IF                                                             
079700     END-PERFORM                                                          
079800                                                                          
079900     IF RAD10A-BETRP = SPACE                                              
080000       MOVE 4492-BETRPFIR(1)      TO  RAD10A-BETRP                        
080100                                      WEB-HEAD-BETRPFIR                   
080200       MOVE 4492-ADTRPFIR-RAD1(1) TO RAD10B-ADTRP-GATA                    
080300                                      WEB-HEAD-ADTRPFIR-RAD1              
080400       MOVE 4492-ADTRPFIR-RAD2(1) TO RAD11-ADTRP-ORT                      
080500                                      WEB-HEAD-ADTRPFIR-RAD2              
080600     END-IF                                                               
080700     .                                                                    
080800     EJECT                                                                
080900 CB-INFO-PER-BOLLA SECTION.                                               
081000     MOVE 'CB-INFO-PER-BOLLA'        TO CURRENT-SECTION                   
081100                                                                          
081200     MOVE 1               TO WS-RAD-RAKNARE                               
081300                             WS-SID-RAKNARE                               
081400                             RAD-INDX                                     
081500                             INDX-KVKOLLI                                 
081600                                                                          
081700     IF (4498-IDKUNDNR NOT = SPAR-IDKUNDNR) OR                            
081800        (4498-IDDISTR  NOT = SPAR-IDDISTR)                                
081900       PERFORM CBA-HAMTA-KUNDINFO                                         
082000     END-IF                                                               
082100                                                                          
082200     IF GMT-FLLDCKND = JA                                                 
082300       MOVE SPACE              TO RAD12-COD-NOTE                          
082400                                  WEB-HEAD-COD-NOTE                       
082500     ELSE                                                                 
082600       IF GMT-FLCOD = JA                                                  
082700         MOVE 'SPEDIZIONE CONTRASSEGNO'                                   
082800                               TO RAD12-COD-NOTE                          
082900                                  WEB-HEAD-COD-NOTE                       
083000       ELSE                                                               
083100         MOVE SPACE            TO RAD12-COD-NOTE                          
083200                                  WEB-HEAD-COD-NOTE                       
083300       END-IF                                                             
083400     END-IF                                                               
083500                                                                          
083600     PERFORM CBB-CAUSALE-PORTO-INFO                                       
083700     PERFORM CBC-KOLLI-INFO                                               
083800     .                                                                    
083900     EJECT                                                                
084000 CBA-HAMTA-KUNDINFO SECTION.                                              
084100     MOVE 'CBA-HAMTA-KUNDINFO'       TO CURRENT-SECTION                   
084200                                                                          
084300     MOVE 4498-IDDISTR         TO W-IDDISTR-B2                            
084400                                  W-IDDISTR-B7                            
084500                                  W-IDDISTR                               
084600                                  WS-4498-IDDISTR                         
084700                                  TEST-IDDISTR                            
084800     MOVE 4498-IDKUNDNR        TO W-IDKUNDNR-B2                           
084900                                  W-IDKUNDNR-B7                           
085000                                  W-IDKUNDNR                              
085100                                  RAD12-IDKUNDNR                          
085200                                  WEB-HEAD-IDKUNDNR                       
085300                                  WS-4498-IDKUNDNR                        
085400     PERFORM IMS-GU-WDB701                                                
085500     IF SEGMENT-FINNS                                                     
085600        PERFORM CBA1-ADR-FROM-VIPS-AND-PULS                               
085700     ELSE                                                                 
085800        PERFORM CBA2-ALL-ADR-FROM-PULS                                    
085900     END-IF                                                               
086000     .                                                                    
086100     EJECT                                                                
086200 CBA1-ADR-FROM-VIPS-AND-PULS SECTION.                                     
086300     MOVE 'CBA1-ADR-FROM-VIPS'       TO CURRENT-SECTION                   
086400                                                                          
086500     IF GMTD-IDDEALER-VIPS = GMTD-IDDEALER-VIPSINV                        
086600        PERFORM CBA11-VIPS-INV-ADR                                        
086700        PERFORM CBA10-GMT-ADR-FROM-PULS                                   
086800     ELSE                                                                 
086900        MOVE GMTD-IDDEALER-VIPSINV  TO W-IDDEALER                         
087000        MOVE GMTD-IDLANDX2          TO W-IDLANDX2                         
087100        PERFORM IMS-GU-WDB7ASEQ                                           
087200        IF SEGMENT-FINNS                                                  
087300           PERFORM UNTIL SEGMENT-SAKNAS OR TRAFF                          
087400              IF ASEQ-GMTD-IDDEALER-VIPS =                                
087500                              ASEQ-GMTD-IDDEALER-VIPSINV                  
087600                 PERFORM CBA12-ASEQ-VIPS-INV-ADRESS                       
087700                 MOVE JA TO TRAFF-SW                                      
087800              END-IF                                                      
087900              PERFORM IMS-GU-WDB7ASEQ                                     
088000           END-PERFORM                                                    
088100           IF TRAFF                                                       
088200              PERFORM CBA10-GMT-ADR-FROM-PULS                             
088300           ELSE                                                           
088400              PERFORM CBA2-ALL-ADR-FROM-PULS                              
088500           END-IF                                                         
088600        ELSE                                                              
088700           PERFORM CBA2-ALL-ADR-FROM-PULS                                 
088800        END-IF                                                            
088900     END-IF                                                               
089000     .                                                                    
089100                                                                          
089200                                                                          
089300 CBA10-GMT-ADR-FROM-PULS SECTION.                                         
089400     MOVE 'CBA10-GMT-ADR-FROM'       TO CURRENT-SECTION                   
089500*GMT ADRESS FROM PULS                                                     
089600     PERFORM IMS-GU-WDB201                                                
089700     MOVE GMT-BEGMT-RAD1       TO RAD1-BEGODSM                            
089800                                  WEB-HEAD-BEGMT-RAD1                     
089900     MOVE GMT-ADGMT-GATA       TO RAD2-ADGODSM-GATA                       
090000                                  WEB-HEAD-ADGMT-GATA                     
090100     MOVE GMT-ADGMT-PADR       TO RAD3-ADGODSM-ORT                        
090200                                  WEB-HEAD-ADGMT-PADR                     
090300     MOVE GMT-IDZON            TO 4494-IDZON                              
090400                                                                          
090500     .                                                                    
090600     EJECT                                                                
090700                                                                          
090800 CBA11-VIPS-INV-ADR SECTION.                                              
090900     MOVE 'CBA11-VIPS-INV-ADR'       TO CURRENT-SECTION                   
091000                                                                          
091100*VIPS INV ADRESS                                                          
091200     MOVE GMTD-BEDEALER-VIPSINV   TO RAD1-BEBET                           
091300                                     WEB-HEAD-BEBETRAD                    
091400     MOVE GMTD-ADDEALER-INVRAD1   TO RAD2-ADBET-GATA                      
091500                                     WEB-HEAD-ADBET-STREET                
091600     MOVE GMTD-ADPOSTNR-INV       TO WS-IT-POSTNR                         
091700     MOVE GMTD-ADCITY-INV         TO WS-IT-ORT                            
091800     MOVE WS-IT-POSTADRESS        TO RAD3-ADBET-ORT                       
091900                                     WEB-HEAD-ADBET-CITY                  
092000     .                                                                    
092100     EJECT                                                                
092200                                                                          
092300 CBA12-ASEQ-VIPS-INV-ADRESS SECTION.                                      
092400     MOVE 'CBA12-ASEQ-VIPS-IN'       TO CURRENT-SECTION                   
092500                                                                          
092600*VIPS INV ADRESS                                                          
092700     MOVE ASEQ-GMTD-BEDEALER-VIPSINV TO RAD1-BEBET                        
092800                                        WEB-HEAD-BEBETRAD                 
092900     MOVE ASEQ-GMTD-ADDEALER-INVRAD1 TO RAD2-ADBET-GATA                   
093000                                        WEB-HEAD-ADBET-STREET             
093100     MOVE ASEQ-GMTD-ADPOSTNR-INV     TO WS-IT-POSTNR                      
093200     MOVE ASEQ-GMTD-ADCITY-INV       TO WS-IT-ORT                         
093300     MOVE WS-IT-POSTADRESS           TO RAD3-ADBET-ORT                    
093400                                        WEB-HEAD-ADBET-CITY               
093500     .                                                                    
093600     EJECT                                                                
093700 CBA2-ALL-ADR-FROM-PULS SECTION.                                          
093800     MOVE 'CBA2-ALL-ADR-FROM-'       TO CURRENT-SECTION                   
093900                                                                          
094000     PERFORM IMS-GU-WDB201                                                
094100     MOVE GMT-BEGMT-RAD1       TO RAD1-BEBET                              
094200                                  RAD1-BEGODSM                            
094300                                  WEB-HEAD-BEBETRAD                       
094400                                  WEB-HEAD-BEGMT-RAD1                     
094500     MOVE GMT-ADGMT-GATA       TO RAD2-ADBET-GATA                         
094600                                  RAD2-ADGODSM-GATA                       
094700                                  WEB-HEAD-ADBET-STREET                   
094800                                  WEB-HEAD-ADGMT-GATA                     
094900     MOVE GMT-ADGMT-PADR       TO RAD3-ADBET-ORT                          
095000                                  RAD3-ADGODSM-ORT                        
095100                                  WEB-HEAD-ADBET-CITY                     
095200                                  WEB-HEAD-ADGMT-PADR                     
095300     MOVE GMT-IDZON            TO 4494-IDZON                              
095400                                                                          
095500     IF DIST09-DC-ADRESS                                                  
095600       MOVE DCS-BEGMT-RAD1     TO RAD1-BEBET                              
095700                                  WEB-HEAD-BEBETRAD                       
095800       MOVE DCS-ADGMT-GATA     TO RAD2-ADBET-GATA                         
095900                                  WEB-HEAD-ADBET-STREET                   
096000       MOVE DCS-ADGMT-PADR     TO RAD3-ADBET-ORT                          
096100                                  WEB-HEAD-ADBET-CITY                     
096200     END-IF                                                               
096300     .                                                                    
096400     EJECT                                                                
096500                                                                          
096600 CBB-CAUSALE-PORTO-INFO SECTION.                                          
096700     MOVE 'CBB-CAUSALE-PORTO-'       TO CURRENT-SECTION                   
096800                                                                          
096900     MOVE 4498-IDDISTR   TO W-IDDISTR-Q2                                  
097000     MOVE 4498-IDKUNDNR  TO W-IDKUNDNR-Q2                                 
097100     MOVE 4498-IDKUNDRF  TO W-IDKUNDRF-Q2                                 
097200                            W-IDKUNDRF                                    
097300                                                                          
097400     PERFORM IMS-GU-WDQ201                                                
097500                                                                          
097600     MOVE OHUV-KDORDKL              TO WS-KDORDKL                         
097700                                                                          
097800*    MOVE 'NUM.TOTALE COLLI'    TO RAD4-ASPETTO                           
097900*                                  WEB-HEAD-ASPETTO                       
098000     EVALUATE TRUE                                                        
098100     WHEN WS-KDORDKL = 0                                                  
098200       OR WS-KDORDKL = 1                                                  
098300         MOVE 'ADDEBITO'            TO RAD4-PORTO                         
098400                                       WEB-HEAD-PORTO-1                   
098500         MOVE 'SU'                  TO RAD5-PORTO                         
098600                                       WEB-HEAD-PORTO-2                   
098700         MOVE 'FATTURA'             TO RAD6-PORTO                         
098800                                       WEB-HEAD-PORTO-3                   
098900     WHEN OTHER                                                           
099000         MOVE 'FRANCO'              TO RAD4-PORTO                         
099100                                       WEB-HEAD-PORTO-1                   
099200         MOVE SPACE                 TO RAD5-PORTO                         
099300                                       WEB-HEAD-PORTO-2                   
099400         MOVE SPACE                 TO RAD6-PORTO                         
099500                                       WEB-HEAD-PORTO-3                   
099600     END-EVALUATE                                                         
099700                                                                          
099800     MOVE '                  (VENDITA)'                                   
099900                                TO RAD11-CAUSALE                          
100000                                   WEB-HEAD-CAUSALE                       
100100     IF 4498-IDDISTR = 1870                                               
100200       MOVE '(TRANSFERIMENTO INTERNO)'  TO RAD11-CAUSALE                  
100300                                           WEB-HEAD-CAUSALE               
100400     END-IF                                                               
100500                                                                          
100600     IF OHUV-BELAGINS-DEL1(1:3) = 'SG.' OR 'SG '                          
100700       MOVE '(SPEDIZIONE GRATUITA)' TO RAD11-CAUSALE                      
100800                                       WEB-HEAD-CAUSALE                   
100900       MOVE SPACE                   TO RAD12-COD-NOTE                     
101000                                       WEB-HEAD-COD-NOTE                  
101100     END-IF                                                               
101200     IF OHUV-BELAGINS-DEL1(1:4) = 'MMT.' OR 'MMT '                        
101300       MOVE '(MATERIALE MANC. DA TELAIO NUMERO)'                          
101400                                    TO RAD11-CAUSALE                      
101500                                       WEB-HEAD-CAUSALE                   
101600       MOVE SPACE                   TO RAD12-COD-NOTE                     
101700                                       WEB-HEAD-COD-NOTE                  
101800     END-IF                                                               
101900                                                                          
102000     MOVE OHUV-BELAGINS-DEL2        TO WS-RAD-INFO                        
102100                                                                          
102200     MOVE WS-RAD-INFO-13            TO RAD13-INSTR-NOTE                   
102300     MOVE WS-RAD-INFO-14            TO RAD14-INSTR-NOTE                   
102400                                                                          
102500     IF OHUV-BELAGINS-DEL2 > SPACE                                        
102600       MOVE WS-RAD-INFO-13          TO WEB-HEAD-INSTR-NOTE-13             
102700       MOVE WS-RAD-INFO-14          TO WEB-HEAD-INSTR-NOTE-14             
102800     ELSE                                                                 
102900       MOVE OHUV-BEKUNDRF           TO WEB-HEAD-INSTR-NOTE-13             
103000       MOVE SPACE                   TO WEB-HEAD-INSTR-NOTE-14             
103100     END-IF                                                               
103200                                                                          
103300     MOVE OHUV-TIREGDAT             TO 4494-TIORDREG                      
103400                                                                          
103500     EVALUATE TRUE                                                        
103600     WHEN WS-KDORDKL = 0                                                  
103700       OR WS-KDORDKL = 1                                                  
103800       MOVE 'URGENTE       ' TO WEB-HEAD-KDORDKL-TEXT                     
103900     WHEN WS-KDORDKL = 3                                                  
104000       MOVE 'PIANIFICATO   ' TO WEB-HEAD-KDORDKL-TEXT                     
104100     WHEN WS-KDORDKL = 4                                                  
104200       MOVE 'STOCK         ' TO WEB-HEAD-KDORDKL-TEXT                     
104300     WHEN OTHER                                                           
104400       MOVE SPACE            TO WEB-HEAD-KDORDKL-TEXT                     
104500     END-EVALUATE                                                         
104600                                                                          
104700     .                                                                    
104800     EJECT                                                                
104900 CBC-KOLLI-INFO SECTION.                                                  
105000     MOVE 'CBC-KOLLI-INFO'          TO CURRENT-SECTION                    
105100                                                                          
105200     PERFORM CBCA-NOLLA-KOLLIINFO                                         
105300                                                                          
105400     MOVE 4498-IDPRODNR           TO W-IDPRODNR-MIN                       
105500                                                                          
105600     PERFORM IMS-GU-WDE601                                                
105700     PERFORM IMS-GNP-WDE611                                               
105800     PERFORM UNTIL SEGMENT-SAKNAS                                         
105900       IF KOLLI-IDLBBET  = 4495-IDLBBET  AND                              
106000          KOLLI-TILASTN NOT = +9999999                                    
106010          IF KOLLI-KDEMBTYP = 0                                           
106020            MOVE +1   TO KOLLI-KDEMBTYP                                   
106030          END-IF                                                          
106100         ADD 1                    TO W-KVKOLLI                            
106200                                     W-KVKOLLITYP(KOLLI-KDEMBTYP)         
106300         ADD KOLLI-VKORDBTO-KOLLI TO W-VKORDBTO                           
106400         ADD KOLLI-VLORDBTO-KOLLI TO W-VLORDBTO                           
106500       END-IF                                                             
106600       PERFORM IMS-GNP-WDE611                                             
106700     END-PERFORM                                                          
106800     MOVE W-KVKOLLI               TO RAD4-KVKOLLI                         
106900                                     WEB-HEAD-KVKOLLI                     
107000                                     4494-KVKOLLI                         
107100                                                                          
107200     MOVE W-VKORDBTO              TO RAD4-VKORDBTO                        
107300                                     WEB-HEAD-VKORDBTO                    
107400                                     4494-VKORDBTO                        
107500                                                                          
107600     MOVE W-VLORDBTO              TO 4494-VLORDBTO                        
107700                                                                          
107800     PERFORM CBCB-HAMTA-EMB-NAMN                                          
107900     .                                                                    
108000     EJECT                                                                
108100 CBCA-NOLLA-KOLLIINFO SECTION.                                            
108200     MOVE 'CBCA-NOLLA-KOLLIINFO'    TO CURRENT-SECTION                    
108300                                                                          
108400     MOVE ZERO                     TO W-VKORDBTO                          
108500                                      W-VLORDBTO                          
108600                                      W-KVKOLLI                           
108700                                      RAD5-KVEMBTYP                       
108800                                      WEB-HEAD-KVEMBTYP-5                 
108900                                      RAD6-KVEMBTYP                       
109000                                      WEB-HEAD-KVEMBTYP-6                 
109100                                      RAD7-KVEMBTYP                       
109200                                      WEB-HEAD-KVEMBTYP-7                 
109300                                      RAD8-KVEMBTYP                       
109400                                      WEB-HEAD-KVEMBTYP-8                 
109500                                      RAD9-KVEMBTYP                       
109600                                      WEB-HEAD-KVEMBTYP-9                 
109700*                                     RAD10-KVEMBTYP                      
109800*                                     WEB-HEAD-KVEMBTYP-10                
109900     MOVE SPACE                    TO RAD5-BEEMBTYP                       
110000                                      WEB-HEAD-BEEMBTYP-5                 
110100                                      RAD6-BEEMBTYP                       
110200                                      WEB-HEAD-BEEMBTYP-6                 
110300                                      RAD7-BEEMBTYP                       
110400                                      WEB-HEAD-BEEMBTYP-7                 
110500                                      RAD8-BEEMBTYP                       
110600                                      WEB-HEAD-BEEMBTYP-8                 
110700                                      RAD9-BEEMBTYP                       
110800                                      WEB-HEAD-BEEMBTYP-9                 
110900*                                     RAD10-BEEMBTYP                      
111000*                                     WEB-HEAD-BEEMBTYP-10                
111100     MOVE +1 TO INDX                                                      
111200     PERFORM UNTIL INDX > MAX-INDX                                        
111300       MOVE ZERO                   TO W-KVKOLLITYP(INDX)                  
111400       ADD +1                      TO INDX                                
111500     END-PERFORM                                                          
111600     .                                                                    
111700     EJECT                                                                
111800 CBCB-HAMTA-EMB-NAMN SECTION.                                             
111900     MOVE 'CBCB-HAMTA-EMB-NAMN'     TO CURRENT-SECTION                    
112000                                                                          
112100     MOVE +1 TO INDX                                                      
112200     PERFORM UNTIL INDX > MAX-INDX                                        
112300      IF W-KVKOLLITYP(INDX) > ZERO                                        
112400       MOVE INDX                         TO W-KDEMBTYP                    
112500       PERFORM IMS-GU-WL473811                                            
112600        IF RAD5-BEEMBTYP = SPACE                                          
112700           MOVE EMBTYP-BEEMBTYP(6)    TO RAD5-BEEMBTYP                    
112800                                         WEB-HEAD-BEEMBTYP-5              
112900           MOVE W-KVKOLLITYP(INDX)    TO RAD5-KVEMBTYP                    
113000                                         WEB-HEAD-KVEMBTYP-5              
113100        ELSE                                                              
113200         IF RAD6-BEEMBTYP = SPACE                                         
113300           MOVE EMBTYP-BEEMBTYP(6)    TO RAD6-BEEMBTYP                    
113400                                         WEB-HEAD-BEEMBTYP-6              
113500           MOVE W-KVKOLLITYP(INDX)    TO RAD6-KVEMBTYP                    
113600                                         WEB-HEAD-KVEMBTYP-6              
113700         ELSE                                                             
113800           IF RAD7-BEEMBTYP = SPACE                                       
113900            MOVE EMBTYP-BEEMBTYP(6)   TO RAD7-BEEMBTYP                    
114000                                         WEB-HEAD-BEEMBTYP-7              
114100            MOVE W-KVKOLLITYP(INDX)   TO RAD7-KVEMBTYP                    
114200                                         WEB-HEAD-KVEMBTYP-7              
114300           ELSE                                                           
114400            IF RAD8-BEEMBTYP = SPACE                                      
114500             MOVE EMBTYP-BEEMBTYP(6)   TO RAD8-BEEMBTYP                   
114600                                          WEB-HEAD-BEEMBTYP-8             
114700             MOVE W-KVKOLLITYP(INDX)   TO RAD8-KVEMBTYP                   
114800                                          WEB-HEAD-KVEMBTYP-8             
114900            ELSE                                                          
115000             IF RAD9-BEEMBTYP = SPACE                                     
115100              MOVE EMBTYP-BEEMBTYP(6) TO RAD9-BEEMBTYP                    
115200                                         WEB-HEAD-BEEMBTYP-9              
115300              MOVE W-KVKOLLITYP(INDX) TO RAD9-KVEMBTYP                    
115400                                         WEB-HEAD-KVEMBTYP-9              
115500*            ELSE                                                         
115600*             MOVE EMBTYP-BEEMBTYP(6) TO RAD10-BEEMBTYP                   
115700*                                        WEB-HEAD-BEEMBTYP-10             
115800*             MOVE W-KVKOLLITYP(INDX) TO RAD10-KVEMBTYP                   
115900*                                        WEB-HEAD-KVEMBTYP-10             
116000             END-IF                                                       
116100            END-IF                                                        
116200           END-IF                                                         
116300          END-IF                                                          
116400        END-IF                                                            
116500      END-IF                                                              
116600      ADD +1                      TO INDX                                 
116700     END-PERFORM                                                          
116800     .                                                                    
116900     EJECT                                                                
117000 CC-SKAPA-BOLLA-SEGM SECTION.                                             
117100     MOVE 'CC-SKAPA-BOLLA-SEGM'     TO CURRENT-SECTION                    
117200     MOVE 4498-IDDISTR   TO 4494-IDDISTR                                  
117300     MOVE 4498-IDKUNDNR  TO 4494-IDKUNDNR                                 
117400     MOVE SPACE          TO 4494-IDKUNDRF                                 
117500     MOVE 4498-IDORDNR7  TO 4494-IDORDNR7                                 
117600     MOVE 4498-IDKOLLI   TO 4494-IDKOLLI                                  
117700     MOVE 4498-IDPRODNR  TO 4494-IDPRODNR                                 
117800     MOVE W-IDLBBET      TO 4494-IDLBBET                                  
117900                                                                          
118000     MOVE 'H'            TO 4494-IDTRPBOT                                 
118100     MOVE WS-IDTRPBON    TO 4494-IDTRPBON                                 
118200     MOVE +0             TO 4494-IDTRPBOR                                 
118300                                                                          
118400     PERFORM IMS-ISRT-4494                                                
118500     .                                                                    
118600     EJECT                                                                
118700 CD-SKRIV-RADER SECTION.                                                  
118800     MOVE 'CD-SKRIV-RADER'          TO CURRENT-SECTION                    
118900                                                                          
119000     MOVE 4498-IDPRODNR    TO W-IDPRODNR-MIN                              
119100                              W-IDPRODNR-MAX                              
119200     MOVE 4498-IDKOLLI     TO W-IDKOLLI-MIN                               
119300                              W-IDKOLLI-MAX                               
119400     PERFORM IMS-GU-WDE411-FSEQ                                           
119500                                                                          
119600     MOVE SPACE          TO SEND-RAD                                      
119700     MOVE WS-SKIP1       TO STYRTECKEN-RAD                                
119800                                                                          
119900     IF MSG-INDATA-MINUS-1-TRANSKOD(1:1) NOT = 'E'                        
120000       PERFORM S90-PUT-DOC-LINE                                           
120100     END-IF                                                               
120200                                                                          
120300     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
120400       PERFORM IMS-GNP-WDE421                                             
120500       PERFORM CDA-REDIGERA-RAD                                           
120600       IF MSG-INDATA-MINUS-1-TRANSKOD(1:1) = 'E'                          
120700          PERFORM S90-PUT-BOLLA-LINE-WEB                                  
120800       ELSE                                                               
120900         MOVE ARTIKELRAD     TO SEND-RAD                                  
121000         PERFORM S02-PRINT-RAD                                            
121100       END-IF                                                             
121200                                                                          
121300       ADD 1               TO  RAD-INDX                                   
121400       IF MSG-INDATA-MINUS-1-TRANSKOD(1:1) = 'E'                          
121500         CONTINUE                                                         
121600       ELSE                                                               
121700         IF RAD-INDX > 17                                                 
121800           PERFORM S01-SKRIV-BOLLA-HUVUD                                  
121900           MOVE 1            TO  RAD-INDX                                 
122000         END-IF                                                           
122100       END-IF                                                             
122200       ADD 1               TO  WS-RAD-RAKNARE                             
122300       PERFORM IMS-GN-WDE411-FSEQ                                         
122400     END-PERFORM                                                          
122500                                                                          
122600     IF INDX-KVKOLLI = W-KVKOLLI                                          
122700       IF RAD-INDX = 1                                                    
122800         MOVE WS-SKIP1     TO STYRTECKEN-RAD                              
122900       ELSE                                                               
123000         MOVE WS-SKIP1     TO STYRTECKEN-RAD                              
123100       END-IF                                                             
123200       MOVE FINALRAD       TO SEND-RAD                                    
123300       IF  MSG-INDATA-MINUS-1-TRANSKOD(1:1) NOT = 'E'                     
123400         PERFORM S02-PRINT-RAD                                            
123500       END-IF                                                             
123600     ELSE                                                                 
123700       ADD +1              TO INDX-KVKOLLI                                
123800     END-IF                                                               
123900     .                                                                    
124000     EJECT                                                                
124100 CDA-REDIGERA-RAD SECTION.                                                
124200     MOVE 'CDA-REDIGERA-RAD'        TO CURRENT-SECTION                    
124300                                                                          
124400     MOVE WS-RAD-RAKNARE  TO ARTRAD-SEKVNR                                
124500     MOVE ORAD-IDARTNR    TO ARTRAD-IDARTNR                               
124600     MOVE ORAD-BEART (1:20) TO ARTRAD-BEART                               
124700     MOVE KKOLLI-KVLEVART TO ARTRAD-KVLEVART                              
124800     MOVE 4498-IDKOLLI    TO ARTRAD-IDKOLLI                               
124900     MOVE ORAD-KDPRODSL   TO ARTRAD-KDPRODSL                              
125000     MOVE ORAD-KDORDKL    TO WS-KDORDKL                                   
125100                                                                          
125200     MOVE '2'             TO WEB-LINE-IDAFPRCD                            
125300     MOVE WS-RAD-RAKNARE  TO WEB-LINE-IDSEKVNR                            
125400     MOVE ORAD-IDARTNR    TO WEB-LINE-IDARTNR                             
125500     MOVE ORAD-BEART (1:20) TO WEB-LINE-BEART                             
125600     MOVE KKOLLI-KVLEVART TO WEB-LINE-KVLEVART                            
125700     MOVE 4498-IDKOLLI    TO WEB-LINE-IDKOLLI                             
125800     MOVE ORAD-KDPRODSL   TO WEB-LINE-KDPRODSL                            
125900                                                                          
126000     IF RAD-INDX = 1                                                      
126100       MOVE WS-SKIP1      TO STYRTECKEN-RAD                               
126200     ELSE                                                                 
126300       MOVE WS-SKIP1      TO STYRTECKEN-RAD                               
126400     END-IF                                                               
126500     .                                                                    
126600     EJECT                                                                
126700 CE-UPPDAT-BOLLANR SECTION.                                               
126800     MOVE 'CE-UPPDAT-BOLLANR'       TO CURRENT-SECTION                    
126900                                                                          
127000     PERFORM IMS-GHNP-4492                                                
127100                                                                          
127200     MOVE WS-IDTRPBON      TO 4492-IDTRPBON                               
127300     IF DAGENS-AA = 4492-TIAA                                             
127400       PERFORM IMS-REPL-4492                                              
127500     ELSE                                                                 
127600       PERFORM IMS-DLET-4492                                              
127700       MOVE DAGENS-AA      TO 4492-TIAA                                   
127800       MOVE 1000           TO 4492-IDTRPBOR                               
127900       PERFORM IMS-ISRT-4492                                              
128000     END-IF                                                               
128100     .                                                                    
128200     EJECT                                                                
128300 D-STARTA-FAKTURA-RELEASE SECTION.                                        
128400     MOVE 'D-STARTA-FAKTURA-REL'    TO CURRENT-SECTION                    
128500                                                                          
128600     IF MSG-INDATA-MINUS-1-TRANSKOD(1:1) = 'E'                            
128700        CONTINUE                                                          
128800     ELSE                                                                 
128900        PERFORM DA-START-SHIPMENT-RELEASE                                 
129000     END-IF                                                               
129100     .                                                                    
129200     EJECT                                                                
129300 DA-START-SHIPMENT-RELEASE SECTION.                                       
129400     MOVE 'DA-START-SHIPMENENT-R'    TO CURRENT-SECTION                   
129500                                                                          
129600     MOVE ALL '+'              TO  4675-MID-W4I67501                      
129700     MOVE MID-IDTRPTNR         TO  4675-MID-IDTRPTNR-IN                   
129800     MOVE MID-IDLBBET          TO  4675-MID-IDLBBET-IN                    
129900     MOVE MID-FLFARLIG         TO  4675-MID-FLFARLIG-IN                   
130000     MOVE WS-IDDC              TO  4675-MID-IDDC-IN                       
130100     MOVE ZERO                 TO  4675-MID-IDSHIPM                       
130200     MOVE MID-FLSKRIV-NU       TO  4675-MID-FLSKRIV-NU                    
130300                                                                          
130400     MOVE SPAR-IDDISTR         TO  TEST-IDDISTR                           
130500                                                                          
130600     PERFORM S10-VILKEN-TRANS                                             
130700     IF FL-XTRANS                                                         
130800*          BAKGRUNDS-TRANS                                                
130900       MOVE 'W4T675X '           TO MSG-KDTRANS-1                         
131000       MOVE '4698'               TO MSG-IDTRANS-1                         
131100       MOVE W-KDMFSFOR           TO MSG-KDMFSFOR-1                        
131200                                                                          
131300       COMPUTE MSG-KVLL =  LENGTH OF 4675-MID-W4I67501 + 17               
131400       PERFORM IMS-ISRT-MSG-4675X                                         
131500     ELSE                                                                 
131600*           UPPDATERINGS-TRANS FÖR IMPORTÖRER                             
131700       MOVE 'W4T675  '           TO MSG-KDTRANS-1                         
131800       MOVE '4698'               TO MSG-IDTRANS-1                         
131900       MOVE W-KDMFSFOR           TO MSG-KDMFSFOR-1                        
132000       COMPUTE MSG-KVLL =  LENGTH OF 4675-MID-W4I67501 + 17               
132100       PERFORM IMS-ISRT-MSG-4675                                          
132200     END-IF                                                               
132300                                                                          
132400     .                                                                    
132500     EJECT                                                                
132600 S01-SKRIV-BOLLA-HUVUD SECTION.                                           
132700     MOVE 'S01-SKRIV-BOLLA-HUVUD'    TO CURRENT-SECTION                   
132800                                                                          
132900                                                                          
133000     MOVE SPACE              TO SEND-RAD                                  
133100     MOVE WS-PAGESKIP        TO STYRTECKEN-RAD                            
133200     PERFORM S90-PUT-DOC-LINE                                             
133300     MOVE WS-SKIP2           TO STYRTECKEN-RAD                            
133400     PERFORM S90-PUT-DOC-LINE                                             
133500     MOVE WS-SKIP1           TO STYRTECKEN-RAD                            
133600                                                                          
133700     MOVE 'BOLLA DOCUMENT'   TO RAD-TYP-IDSHIP                            
133800     MOVE DAGENS-DATUM       TO RAD-DATE                                  
133900     MOVE W-IDTRPTNR         TO RAD-TRP                                   
134000     MOVE W-IDLBBET          TO RAD-CARRIER                               
134100     MOVE WS-IDDC            TO RAD-IDDC                                  
134200     MOVE WS-SID-RAKNARE     TO RAD-SIDNR                                 
134300                                                                          
134400     MOVE HEADER             TO SEND-RAD                                  
134500     PERFORM S02-PRINT-RAD                                                
134600                                                                          
134700     ADD +1                  TO WS-SID-RAKNARE                            
134800     MOVE SPACE              TO SEND-RAD                                  
134900     MOVE WS-SKIP1           TO STYRTECKEN-RAD                            
135000     PERFORM S90-PUT-DOC-LINE                                             
135100                                                                          
135200     MOVE RUBRIKRAD          TO SEND-RAD                                  
135300     PERFORM S02-PRINT-RAD                                                
135400                                                                          
135500     MOVE DETALJRAD-1        TO SEND-RAD                                  
135600     MOVE WS-SKIP1           TO STYRTECKEN-RAD                            
135700     PERFORM S02-PRINT-RAD                                                
135800                                                                          
135900     MOVE DETALJRAD-2        TO SEND-RAD                                  
136000     PERFORM S02-PRINT-RAD                                                
136100                                                                          
136200     MOVE DETALJRAD-3        TO SEND-RAD                                  
136300     PERFORM S02-PRINT-RAD                                                
136400                                                                          
136500     MOVE DETALJRAD-4        TO SEND-RAD                                  
136600     MOVE WS-SKIP3           TO STYRTECKEN-RAD                            
136700     PERFORM S02-PRINT-RAD                                                
136800                                                                          
136900     MOVE WS-SKIP1           TO STYRTECKEN-RAD                            
137000     MOVE DETALJRAD-5        TO SEND-RAD                                  
137100     PERFORM S02-PRINT-RAD                                                
137200                                                                          
137300     MOVE DETALJRAD-6        TO SEND-RAD                                  
137400     PERFORM S02-PRINT-RAD                                                
137500                                                                          
137600     MOVE DETALJRAD-7        TO SEND-RAD                                  
137700     PERFORM S02-PRINT-RAD                                                
137800                                                                          
137900     MOVE DETALJRAD-8        TO SEND-RAD                                  
138000     PERFORM S02-PRINT-RAD                                                
138100                                                                          
138200     MOVE DETALJRAD-9        TO SEND-RAD                                  
138300     PERFORM S02-PRINT-RAD                                                
138400                                                                          
138500*    MOVE DETALJRAD-10       TO SEND-RAD                                  
138600*    PERFORM S02-PRINT-RAD                                                
138700                                                                          
138800     MOVE DETALJRAD-10A      TO SEND-RAD                                  
138900     MOVE WS-SKIP2           TO STYRTECKEN-RAD                            
139000     PERFORM S02-PRINT-RAD                                                
139100                                                                          
139200     MOVE DETALJRAD-10B      TO SEND-RAD                                  
139300     MOVE WS-SKIP2           TO STYRTECKEN-RAD                            
139400     PERFORM S02-PRINT-RAD                                                
139500                                                                          
139600     MOVE DETALJRAD-11       TO SEND-RAD                                  
139700     MOVE WS-SKIP2           TO STYRTECKEN-RAD                            
139800     PERFORM S02-PRINT-RAD                                                
139900                                                                          
140000     MOVE BLANKRAD           TO SEND-RAD                                  
140100     MOVE WS-SKIP1           TO STYRTECKEN-RAD                            
140200     PERFORM S02-PRINT-RAD                                                
140300                                                                          
140400     MOVE DETALJRAD-11A      TO SEND-RAD                                  
140500     MOVE WS-SKIP2           TO STYRTECKEN-RAD                            
140600     PERFORM S02-PRINT-RAD                                                
140700                                                                          
140800     MOVE DETALJRAD-12       TO SEND-RAD                                  
140900     MOVE WS-SKIP2           TO STYRTECKEN-RAD                            
141000     PERFORM S02-PRINT-RAD                                                
141100                                                                          
141200                                                                          
141300     MOVE SPACE              TO WS-KDORDKL-TEXT                           
141400                                                                          
141500     EVALUATE TRUE                                                        
141600     WHEN WS-KDORDKL = 0                                                  
141700       OR WS-KDORDKL = 1                                                  
141800       MOVE 'URGENTE       ' TO WS-KDORDKL-TEXT                           
141900     WHEN WS-KDORDKL = 3                                                  
142000       MOVE 'PIANIFICATO   ' TO WS-KDORDKL-TEXT                           
142100     WHEN WS-KDORDKL = 4                                                  
142200       MOVE 'STOCK         ' TO WS-KDORDKL-TEXT                           
142300     WHEN OTHER                                                           
142400       MOVE SPACE            TO WS-KDORDKL-TEXT                           
142500     END-EVALUATE                                                         
142600     MOVE DETALJRAD-13       TO SEND-RAD                                  
142700     MOVE WS-SKIP1           TO STYRTECKEN-RAD                            
142800                                                                          
142900     PERFORM S02-PRINT-RAD                                                
143000                                                                          
143100     MOVE DETALJRAD-14       TO SEND-RAD                                  
143200     MOVE WS-SKIP1           TO STYRTECKEN-RAD                            
143300     PERFORM S02-PRINT-RAD                                                
143400                                                                          
143500     MOVE BLANKRAD           TO SEND-RAD                                  
143600     MOVE WS-SKIP2           TO STYRTECKEN-RAD                            
143700     PERFORM S02-PRINT-RAD                                                
143800     .                                                                    
143900     EJECT                                                                
144000                                                                          
144100 S02-PRINT-RAD SECTION.                                                   
144200                                                                          
144300     MOVE 'W40698' TO PRT-PFDEF-A4S                                       
144400                                                                          
144500     PERFORM S90-PUT-DOC-LINE                                             
144600     .                                                                    
144700     EJECT                                                                
144800                                                                          
144900 S10-VILKEN-TRANS    SECTION.                                             
145000     MOVE 'S10-VILKEN-TRANS'         TO CURRENT-SECTION                   
145100                                                                          
145200     MOVE SPAR-IDDISTR                   TO W-IDDISTR-NUM                 
145300     MOVE NEJ                            TO SW-FLTRANS                    
145400     SET IDDC-IX  TO  1                                                   
145500     SEARCH TRANS-TABELL                                                  
145600              AT END                                                      
145700                     MOVE NEJ            TO SW-FLTRANS                    
145800            WHEN TRA-IDDC (IDDC-IX) = WS-IDDC                             
145900       AND                                                                
146000            TRA-IDDISTR-FOM (IDDC-IX) NOT > W-IDDISTR-ALFA                
146100       AND                                                                
146200            TRA-IDDISTR-TOM (IDDC-IX) NOT < W-IDDISTR-ALFA                
146300                                                                          
146400                   MOVE JA               TO SW-FLTRANS                    
146500     END-SEARCH                                                           
146600     .                                                                    
146700     SKIP2                                                                
146800                                                                          
146900 Z-FINIT SECTION.                                                         
147000     MOVE 'Z-FINIT'                  TO CURRENT-SECTION                   
147100                                                                          
147200*    CALL FELLOG                                                          
147300     PERFORM S90-SEND-CLOSE                                               
147400     .                                                                    
147500     EJECT                                                                
147600                                                                          
147700 S05-NUM-TEXT  SECTION.                                                   
147800     INSPECT WS-REDUIN REPLACING LEADING ZERO BY SPACE                    
147900     CALL W009REDU USING WS-REDUIN WS-REDUUT                              
148000     .                                                                    
148100     EJECT                                                                
148200 S90-SEND-OPEN SECTION.                                                   
148300     MOVE 'S90-SEND-OPEN'            TO CURRENT-SECTION                   
148400     MOVE 'OPEN'                        TO SEND-KDFUNC                    
148500     MOVE 'CARPARTS.DAP.DISTRDOC'       TO SEND-ADDISPABS                 
148600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
148700                         SEND-OPEN-AREA                                   
148800     IF SEND-KDRC > 0                                                     
148900       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
149000       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
149100       DELIMITED BY SIZE INTO ERRTEXT                                     
149200       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
149300     END-IF                                                               
149400     .                                                                    
149500     EJECT                                                                
149600 S90-SEND-OPEN-WEB SECTION.                                               
149700     MOVE 'S90-SEND-OPEN-WEB'          TO CURRENT-SECTION                 
149800     MOVE 'OPEN'                        TO SEND-KDFUNC                    
149900     MOVE 'CARPARTS.DAP.DISTRWEB'       TO SEND-ADDISPABS                 
150000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
150100                         SEND-OPEN-AREA                                   
150200     IF SEND-KDRC > 0                                                     
150300       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
150400       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
150500       DELIMITED BY SIZE INTO ERRTEXT                                     
150600       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
150700     END-IF                                                               
150800     .                                                                    
150900     EJECT                                                                
151000 S90-PUT-DAP-START SECTION.                                               
151100                                                                          
151200     MOVE 1                       TO REQU-IDMSGVER                        
151300     MOVE SPACE                   TO REQU-KDPGMACT                        
151400     MOVE IDPGM                   TO REQU-IDUSER                          
151500     MOVE 'SHIPDOC-BOL'           TO HDR-IDOUTTYPE                        
151600     MOVE SPACE                   TO HDR-IDOUTREC                         
151700                                     HDR-IDLIST                           
151800     MOVE WS-IDDC                 TO WS-DAP-IDDC                          
151900     MOVE 4498-IDDISTR            TO WS-NUM5                              
152000     MOVE WS-NUM5                 TO WS-REDUIN                            
152100     PERFORM S05-NUM-TEXT                                                 
152200     MOVE WS-REDUUT               TO WS-DAP-IDDISTR                       
152300     MOVE WS-DAP-HDR              TO HDR-IDOUTREC                         
152400     MOVE WS-IDTRPBO              TO HDR-IDLIST                           
152500     MOVE 'PUT'                   TO SEND-KDFUNC                          
152600     MOVE LENGTH OF HDR-AREA      TO SEND-KVDLEN                          
152700     CALL WZ01SEND USING SEND-CONTROL-AREA                                
152800                         SEND-KVDLEN                                      
152900                         HDR-AREA                                         
153000     IF SEND-KDRC > ZERO                                                  
153100       MOVE SEND-KDRC             TO KDRC-DISPLAY                         
153200       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
153300       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
153400       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
153500     END-IF                                                               
153600     .                                                                    
153700     EJECT                                                                
153800 S90-PUT-DAP-START-WEB SECTION.                                           
153900     MOVE 1                       TO REQU-IDMSGVER                        
154000     MOVE SPACE                   TO REQU-KDPGMACT                        
154100     MOVE IDPGM                   TO REQU-IDUSER                          
154200     MOVE 'SHIPDOC-BOL-WEB'       TO HDR-IDOUTTYPE                        
154300     MOVE SPACE                   TO HDR-IDOUTREC                         
154400                                     HDR-IDLIST                           
154500     MOVE WS-IDDC                 TO WS-DAP-IDDC                          
154600     MOVE 4498-IDDISTR            TO WS-NUM5                              
154700     MOVE WS-NUM5                 TO WS-REDUIN                            
154800     PERFORM S05-NUM-TEXT                                                 
154900     MOVE WS-REDUUT               TO WS-DAP-IDDISTR                       
155000     MOVE WS-DAP-HDR              TO HDR-IDOUTREC                         
155100     MOVE WS-IDTRPBO              TO HDR-IDLIST                           
155200     MOVE 'PUT'                   TO SEND-KDFUNC                          
155300     MOVE LENGTH OF HDR-AREA      TO SEND-KVDLEN                          
155400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
155500                         SEND-KVDLEN                                      
155600                         HDR-AREA                                         
155700     IF SEND-KDRC > ZERO                                                  
155800       MOVE SEND-KDRC             TO KDRC-DISPLAY                         
155900       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
156000       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
156100       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
156200     END-IF                                                               
156300     .                                                                    
156400     EJECT                                                                
156500 S90-PUT-DOC-LINE SECTION.                                                
156600     MOVE 'PUT'                           TO SEND-KDFUNC                  
156700     MOVE LENGTH OF SEND-RAD              TO SEND-KVDLEN                  
156800     CALL WZ01SEND USING SEND-CONTROL-AREA                                
156900                         SEND-KVDLEN                                      
157000                         SEND-RAD                                         
157100     IF SEND-KDRC > ZERO                                                  
157200       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
157300       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
157400       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
157500       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
157600     END-IF                                                               
157700     .                                                                    
157800     EJECT                                                                
157900 S90-PUT-BOLLA-HEAD-WEB SECTION.                                          
158000     MOVE 'PUT'                           TO SEND-KDFUNC                  
158100     MOVE LENGTH OF WEB-BOLLA-SEND-HEADER TO SEND-KVDLEN                  
158200     CALL WZ01SEND USING SEND-CONTROL-AREA                                
158300                         SEND-KVDLEN                                      
158400                         WEB-BOLLA-SEND-HEADER                            
158500     IF SEND-KDRC > ZERO                                                  
158600       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
158700       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
158800       DELIMITED BY SIZE INTO FELTEXT                                     
158900       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
159000     END-IF                                                               
159100     .                                                                    
159200     EJECT                                                                
159300                                                                          
159400 S90-PUT-BOLLA-LINE-WEB SECTION.                                          
159500     MOVE 'PUT'                           TO SEND-KDFUNC                  
159600     MOVE LENGTH OF WEB-BOLLA-SEND-RAD    TO SEND-KVDLEN                  
159700     CALL WZ01SEND USING SEND-CONTROL-AREA                                
159800                         SEND-KVDLEN                                      
159900                         WEB-BOLLA-SEND-RAD                               
160000                                                                          
160100     IF SEND-KDRC > ZERO                                                  
160200       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
160300       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
160400       DELIMITED BY SIZE INTO FELTEXT                                     
160500       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
160600     END-IF                                                               
160700     .                                                                    
160800     EJECT                                                                
160900 S90-SEND-CLOSE SECTION.                                                  
161000     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
161100     CALL WZ01SEND USING SEND-CONTROL-AREA                                
161200                                                                          
161300     IF SEND-KDRC > 0                                                     
161400       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
161500       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
161600       DELIMITED BY SIZE INTO ERRTEXT                                     
161700       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
161800     END-IF                                                               
161900     .                                                                    
162000     EJECT                                                                
162100* --- IMS SEKTIONER ---                                                   
162200     SKIP3                                                                
162300 IMS-GET-MSG SECTION.                                                     
162400                                                                          
162500     MOVE '  QC' TO GODK-STATUSKODER                                      
162600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
162700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
162800     PERFORM IMS-STATUSKONTROLL                                           
162900     .                                                                    
163000     SKIP3                                                                
163100 IMS-ISRT-MSG-4675 SECTION.                                               
163200                                                                          
163300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
163400     MOVE SPACE TO GODK-STATUSKODER                                       
163500     CALL CBLTDLI USING ISRT 4675-PCB MSG-IO-AREA                         
163600     MOVE 4675-STATUS-CODE TO STATUS-WS                                   
163700     PERFORM IMS-STATUSKONTROLL                                           
163800     .                                                                    
163900     SKIP2                                                                
164000 IMS-ISRT-MSG-4675X SECTION.                                              
164100                                                                          
164200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
164300     MOVE SPACE TO GODK-STATUSKODER                                       
164400     CALL CBLTDLI USING ISRT 4675X-PCB MSG-IO-AREA                        
164500     MOVE 4675X-STATUS-CODE TO STATUS-WS                                  
164600     PERFORM IMS-STATUSKONTROLL                                           
164700     .                                                                    
164800     SKIP2                                                                
164900 IMS-GU-WDB201 SECTION.                                                   
165000                                                                          
165100     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
165200          DELIMITED BY SIZE INTO SSA1                                     
165300     MOVE '  ' TO GODK-STATUSKODER                                        
165400     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-AREA-B201 SSA1                 
165500     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
165600     PERFORM IMS-STATUSKONTROLL                                           
165700     .                                                                    
165800     EJECT                                                                
165900 IMS-GU-WDB701 SECTION.                                                   
166000                                                                          
166100     STRING 'WDB701  (IDGMT    =' W-IDGMT-X-B7 ')'                        
166200          DELIMITED BY SIZE INTO SSA1                                     
166300     MOVE '  GE' TO GODK-STATUSKODER                                      
166400     CALL CBLTDLI USING GU WDB7-PCB DLI-IO-AREA-B701 SSA1                 
166500     MOVE WDB7-STATUS-CODE TO STATUS-WS                                   
166600     PERFORM IMS-STATUSKONTROLL                                           
166700     .                                                                    
166800     EJECT                                                                
166900 IMS-GU-WDB7ASEQ SECTION.                                                 
167000                                                                          
167100     STRING 'WDB701  (WDB7ASEQ =' W-WDB7ASEQ-X ')'                        
167200          DELIMITED BY SIZE INTO SSA1                                     
167300     MOVE '  GE' TO GODK-STATUSKODER                                      
167400     CALL CBLTDLI USING GU WDB7AQ-PCB DLI-IO-AREA-B7AQ SSA1               
167500     MOVE WDB7AQ-STATUS-CODE TO STATUS-WS                                 
167600     PERFORM IMS-STATUSKONTROLL                                           
167700     .                                                                    
167800     EJECT                                                                
167900 IMS-GU-WDE601 SECTION.                                                   
168000                                                                          
168100     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
168200          DELIMITED BY SIZE INTO SSA1                                     
168300     MOVE '  ' TO GODK-STATUSKODER                                        
168400     CALL CBLTDLI USING GU  WDE6-PCB DLI-IO-AREA-E601 SSA1                
168500     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
168600     PERFORM IMS-STATUSKONTROLL                                           
168700     .                                                                    
168800     SKIP2                                                                
168900 IMS-GNP-WDE611 SECTION.                                                  
169000                                                                          
169100     MOVE   'WDE611'        TO SSA1                                       
169200     MOVE '  GE' TO GODK-STATUSKODER                                      
169300     CALL CBLTDLI USING GNP WDE6-PCB DLI-IO-AREA-E611 SSA1                
169400     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
169500     PERFORM IMS-STATUSKONTROLL                                           
169600     .                                                                    
169700     EJECT                                                                
169800 IMS-GU-WDE411-FSEQ SECTION.                                              
169900                                                                          
170000     STRING 'WDE411  (WDE4FSEQ>=' W-WDE4FSEQ-MIN-X                        
170100                    '&WDE4FSEQ<=' W-WDE4FSEQ-MAX-X ')'                    
170200          DELIMITED BY SIZE INTO SSA1                                     
170300     MOVE '  GE' TO GODK-STATUSKODER                                      
170400     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-AREA-E411 SSA1                 
170500     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
170600     PERFORM IMS-STATUSKONTROLL                                           
170700     .                                                                    
170800     SKIP3                                                                
170900 IMS-GN-WDE411-FSEQ SECTION.                                              
171000                                                                          
171100     STRING 'WDE411  (WDE4FSEQ>=' W-WDE4FSEQ-MIN-X                        
171200                    '&WDE4FSEQ<=' W-WDE4FSEQ-MAX-X ')'                    
171300          DELIMITED BY SIZE INTO SSA1                                     
171400     MOVE '  GEGB' TO GODK-STATUSKODER                                    
171500     CALL CBLTDLI USING GN WDE4-PCB DLI-IO-AREA-E411 SSA1                 
171600     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
171700     PERFORM IMS-STATUSKONTROLL                                           
171800     .                                                                    
171900     SKIP3                                                                
172000 IMS-GNP-WDE421 SECTION.                                                  
172100                                                                          
172200     STRING 'WDE421  (WDE421KY =' W-WDE4FSEQ-MIN-X ')'                    
172300          DELIMITED BY SIZE INTO SSA1                                     
172400     MOVE '    ' TO GODK-STATUSKODER                                      
172500     CALL CBLTDLI USING GNP WDE4-PCB DLI-IO-AREA-E421 SSA1                
172600     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
172700     PERFORM IMS-STATUSKONTROLL                                           
172800     .                                                                    
172900     EJECT                                                                
173000 IMS-GU-WDQ201 SECTION.                                                   
173100                                                                          
173200     STRING 'WDQ201  (WDQ2CSEQ =' W-IDGMTREF-X ')'                        
173300          DELIMITED BY SIZE INTO SSA1                                     
173400     MOVE '  ' TO GODK-STATUSKODER                                        
173500     CALL CBLTDLI USING GU WDQ2-PCB DLI-IO-AREA-Q201 SSA1                 
173600     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
173700     PERFORM IMS-STATUSKONTROLL                                           
173800     .                                                                    
173900     SKIP2                                                                
174000 IMS-GU-4495 SECTION.                                                     
174100                                                                          
174200     STRING 'WL449501(WDGXKEY  =' W-WDGXKEY-4495-X ')'                    
174300          DELIMITED BY SIZE INTO SSA1                                     
174400     MOVE '  GE' TO GODK-STATUSKODER                                      
174500     CALL CBLTDLI USING GU 4495-PCB DLI-IO-AREA-449501 SSA1               
174600     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
174700     PERFORM IMS-STATUSKONTROLL                                           
174800     .                                                                    
174900     SKIP2                                                                
175000 IMS-GHNP-4498 SECTION.                                                   
175100                                                                          
175200     STRING 'WL449512(WDGXKEY >=' W-WDGXKEY-4498-X ')'                    
175300          DELIMITED BY SIZE INTO SSA1                                     
175400     MOVE '  GEGB' TO GODK-STATUSKODER                                    
175500     CALL CBLTDLI USING GHNP 4495-PCB DLI-IO-AREA-449512 SSA1             
175600     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
175700     PERFORM IMS-STATUSKONTROLL                                           
175800     .                                                                    
175900     EJECT                                                                
176000*IMS-ISRT-4498 SECTION.                                                   
176100*                                                                         
176200*    MOVE   'WL449512'        TO SSA1                                     
176300*    MOVE '  ' TO GODK-STATUSKODER                                        
176400*    CALL CBLTDLI USING ISRT 4495-PCB DLI-IO-AREA-449512 SSA1             
176500*    MOVE 4495-STATUS-CODE TO STATUS-WS                                   
176600*    PERFORM IMS-STATUSKONTROLL                                           
176700*    .                                                                    
176800*    SKIP3                                                                
176900*IMS-DLET-4498     SECTION.                                               
177000*                                                                         
177100*    MOVE '  ' TO GODK-STATUSKODER                                        
177200*    CALL CBLTDLI USING DLET 4495-PCB DLI-IO-AREA-449512                  
177300*    MOVE 4495-STATUS-CODE TO STATUS-WS                                   
177400*    PERFORM IMS-STATUSKONTROLL                                           
177500*    .                                                                    
177600*    EJECT                                                                
177700 IMS-GU-4491 SECTION.                                                     
177800                                                                          
177900     STRING 'WL449101(WDGXKEY  =' W-WDGXKEY-4491-X ')'                    
178000          DELIMITED BY SIZE INTO SSA1                                     
178100     MOVE '  ' TO GODK-STATUSKODER                                        
178200     CALL CBLTDLI USING GU 4491-PCB DLI-IO-AREA-449112 SSA1               
178300     MOVE 4491-STATUS-CODE TO STATUS-WS                                   
178400     PERFORM IMS-STATUSKONTROLL                                           
178500     .                                                                    
178600     SKIP2                                                                
178700 IMS-GHNP-4492 SECTION.                                                   
178800                                                                          
178900     MOVE   'WL449111*F'        TO SSA1                                   
179000     MOVE '  ' TO GODK-STATUSKODER                                        
179100     CALL CBLTDLI USING GHNP 4491-PCB DLI-IO-AREA-449111 SSA1             
179200     MOVE 4491-STATUS-CODE TO STATUS-WS                                   
179300     PERFORM IMS-STATUSKONTROLL                                           
179400     .                                                                    
179500     EJECT                                                                
179600 IMS-REPL-4492 SECTION.                                                   
179700                                                                          
179800     MOVE '  ' TO GODK-STATUSKODER                                        
179900     CALL CBLTDLI USING REPL 4491-PCB DLI-IO-AREA-449111                  
180000     MOVE 4491-STATUS-CODE TO STATUS-WS                                   
180100     PERFORM IMS-STATUSKONTROLL                                           
180200     .                                                                    
180300     SKIP2                                                                
180400 IMS-DLET-4492 SECTION.                                                   
180500                                                                          
180600     MOVE '  ' TO GODK-STATUSKODER                                        
180700     CALL CBLTDLI USING DLET 4491-PCB DLI-IO-AREA-449111                  
180800     MOVE 4491-STATUS-CODE TO STATUS-WS                                   
180900     PERFORM IMS-STATUSKONTROLL                                           
181000     .                                                                    
181100     SKIP2                                                                
181200 IMS-ISRT-4492 SECTION.                                                   
181300                                                                          
181400     MOVE   'WL449111'        TO SSA1                                     
181500     MOVE '    ' TO GODK-STATUSKODER                                      
181600     CALL CBLTDLI USING ISRT 4491-PCB DLI-IO-AREA-449111 SSA1             
181700     MOVE 4491-STATUS-CODE TO STATUS-WS                                   
181800     PERFORM IMS-STATUSKONTROLL                                           
181900     .                                                                    
182000     EJECT                                                                
182100 IMS-GHNP-4494 SECTION.                                                   
182200                                                                          
182300     STRING 'WL449112(KY4494   <' W-KY4494-MIN-X ')'                      
182400          DELIMITED BY SIZE INTO SSA1                                     
182500     MOVE '  GE' TO GODK-STATUSKODER                                      
182600     CALL CBLTDLI USING GHNP 4491-PCB DLI-IO-AREA-449112 SSA1             
182700     MOVE 4491-STATUS-CODE TO STATUS-WS                                   
182800     PERFORM IMS-STATUSKONTROLL                                           
182900     .                                                                    
183000     SKIP3                                                                
183100 IMS-DLET-4494 SECTION.                                                   
183200                                                                          
183300     MOVE '  ' TO GODK-STATUSKODER                                        
183400     CALL CBLTDLI USING DLET 4491-PCB DLI-IO-AREA-449112                  
183500     MOVE 4491-STATUS-CODE TO STATUS-WS                                   
183600     PERFORM IMS-STATUSKONTROLL                                           
183700     .                                                                    
183800     EJECT                                                                
183900 IMS-GNP-4494 SECTION.                                                    
184000                                                                          
184100     STRING 'WL449112(KY4494  >=' W-KY4494-MIN-X                          
184200                    '&KY4494  <=' W-KY4494-MAX-X                          
184300                    '&IDLBBET  =' W-IDLBBET ')'                           
184400          DELIMITED BY SIZE INTO SSA1                                     
184500     MOVE '  GE' TO GODK-STATUSKODER                                      
184600     CALL CBLTDLI USING GNP 4491-PCB DLI-IO-AREA-449112 SSA1              
184700     MOVE 4491-STATUS-CODE TO STATUS-WS                                   
184800     PERFORM IMS-STATUSKONTROLL                                           
184900     .                                                                    
185000     SKIP2                                                                
185100 IMS-ISRT-4494 SECTION.                                                   
185200                                                                          
185300     MOVE   'WL449112'        TO SSA1                                     
185400     MOVE '    ' TO GODK-STATUSKODER                                      
185500     CALL CBLTDLI USING ISRT 4491-PCB DLI-IO-AREA-449112 SSA1             
185600     MOVE 4491-STATUS-CODE TO STATUS-WS                                   
185700     PERFORM IMS-STATUSKONTROLL                                           
185800     .                                                                    
185900     SKIP2                                                                
186000 IMS-GU-WL473811 SECTION.                                                 
186100                                                                          
186200     STRING 'WL473801(WDGXKEY  =' W-WDGXKEY-4738-X ')'                    
186300          DELIMITED BY SIZE INTO SSA1                                     
186400     MOVE   'WL473811'        TO SSA2                                     
186500     MOVE '    ' TO GODK-STATUSKODER                                      
186600     CALL CBLTDLI USING GU 4738-PCB DLI-IO-AREA-473811 SSA1 SSA2          
186700     MOVE 4738-STATUS-CODE TO STATUS-WS                                   
186800     PERFORM IMS-STATUSKONTROLL                                           
186900     .                                                                    
187000     EJECT                                                                
187100 IMS-GU-WDB601 SECTION.                                                   
187200                                                                          
187300     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
187400        DELIMITED BY SIZE INTO SSA1                                       
187500     MOVE '  GE' TO GODK-STATUSKODER                                      
187600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
187700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
187800     PERFORM IMS-STATUSKONTROLL                                           
187900     IF SEGMENT-SAKNAS                                                    
188000       MOVE SPACE TO DCS-KDDC                                             
188100     END-IF                                                               
188200     .                                                                    
188300     EJECT                                                                
188400                                                                          
188500                                                                          
188600 IMS-STATUSKONTROLL SECTION.                                              
188700                                                                          
188800     SET STATUS-IX TO 1                                                   
188900     SEARCH GODK-STATUS                                                   
189000       AT END                                                             
189100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
189200         DELIMITED BY SIZE INTO FELTEXT                                   
189300         CALL FELLOG                                                      
189400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
189500         CONTINUE                                                         
189600     END-SEARCH                                                           
189700     .                                                                    
