      *COMPOPT VPOSIX=YES                                                       
000100 ID DIVISION.                                                             
000300 PROGRAM-ID.     W4032500.                                                
000400 AUTHOR.         M LUNDBERG.                                              
000500 DATE-WRITTEN.   FEB  1986.                                               
000510 DATE-COMPILED.                                                           
000600                                                                          
001200*    FUNKTION.                                                            
001300*        BEKRÄFTA NOLLOR, ALLA KLASSER.                                   
001410*                                                                         
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSAKTION: W4T325                                              
001800*        MID:         W4I32501                                            
001900*                                                                         
002000*    UTDATA.                                                              
002100*        MOD:         W4O32501                                            
002200                                                                          
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP3                                                                
002500 DATA DIVISION.                                                           
002600     EJECT                                                                
002700 WORKING-STORAGE SECTION.                                                 
002701                                                                          
002710*    -- CHECKED BY WY2000                                                 
002800 77    IDPGM                     PIC X(8)    VALUE 'W4032500'.            
003100 77    JA                        PIC X       VALUE 'J'.                   
003200 77    NEJ                       PIC X       VALUE 'N'.                   
003300 77    SPRAK-INDX                PIC S9(9)   VALUE +0   COMP SYNC.        
003400 77    MAX-LINE                  PIC S9(9)   VALUE +13  COMP SYNC.        
003500 77    MAX-MOD-LAENGD            PIC S9(4)  VALUE +191  COMP SYNC.        
003600                                                                          
003800 77    WS-KDMFSFOR               PIC 9       VALUE ZERO.                  
003900 77    WS-KDFEL                  PIC 9(2)    VALUE ZERO COMP-3.           
004000 77    IDPW-WS                   PIC X(8).                                
004100 77    WS-IDUSER                 PIC X(7)    VALUE 'NOLLJAG'.             
004200 77    WS-EGEN-BILD              PIC X(4)    VALUE '4325'.                
004300 77    WS-PASSWORD-OK            PIC X(1)    VALUE 'N'.                   
004400 77    WS-RAD-FUNNEN             PIC X(1)    VALUE 'N'.                   
004500 77    WS-OGAE12-FINNS           PIC X(1)    VALUE 'N'.                   
004600 77    WS-IDKUNDRF               PIC X(10).                               
004700 77    WS-SPAR-KDORDSTA          PIC S9(1)              COMP-3.           
004800 77    WS-SPAR-KVORDRAD-LEVPL    PIC S9(5)   VALUE ZERO COMP-3.           
005000 77    SPAR-IDPRODNR             PIC S9(7)   VALUE ZERO COMP-3.           
005100 77    SPAR-IDPLKLST             PIC S9(3)   VALUE ZERO COMP-3.           
005200 77    SPAR-IDUSER               PIC X(8)    VALUE ZERO.                  
005300 77    TREFF                     PIC X       VALUE 'N'.                   
005320 77    WS-KORD-IDDC              PIC X(2).                                
005321*                                                                         
005700 01    DAGENS-DATUM              PIC S9(6).                               
005800                                                                          
005900 01    WS-IDANSTNR                           PIC X(5).                    
006000 01    IDANSTNR-WS REDEFINES WS-IDANSTNR     PIC 9(5).                    
006100 01    WS-IDDISTR                            PIC X(4).                    
006200 01    IDDISTR-WS REDEFINES WS-IDDISTR       PIC 9(4).                    
006300 01    WS-IDKUNDNR                           PIC X(6).                    
006400 01    IDKUNDNR-WS REDEFINES WS-IDKUNDNR     PIC 9(6).                    
006500 01    WS-IDORDNR                            PIC X(5).                    
006600 01    IDORDNR-WS REDEFINES WS-IDORDNR       PIC 9(5).                    
006700 01    WS-IDPRODNR                           PIC X(7).                    
006800 01    IDPRODNR-WS REDEFINES WS-IDPRODNR     PIC 9(7).                    
006900 01    WS-IDRADNR                            PIC X(4).                    
007000 01    IDRADNR-WS REDEFINES WS-IDRADNR       PIC 9(4).                    
007100 01    WS-IDARTNR                            PIC X(9).                    
007200 01    IDARTNR-WS  REDEFINES WS-IDARTNR      PIC 9(9).                    
007300 01    IDPLKLST-WS                           PIC 9(3).                    
007400**********ARBETSFÄLT FÖR ANNULLATION*******************                   
007500 01    WS-FLJANEJ                            PIC X.                       
007600     EJECT                                                                
007700 01    WS-GENERELLA-SUBPROGRAM.                                           
007800       03   WSECURIT             PIC X(8)    VALUE 'WSECURIT'.            
007900       03   CBLTDLI              PIC X(8)    VALUE 'CBLTDLI '.            
008000       03   FELLOG               PIC X(8)    VALUE 'FELLOG  '.            
             03  W488ORCN              PIC X(8)    VALUE 'W488ORCN'.            
008100                                                                          
008200 77    WS-SPARA-IDRADNR-FROM     PIC 9(5)    VALUE ZERO COMP-3.           
008300 77    WS-SPARA-IDRADNR-TOM      PIC 9(5)    VALUE ZERO COMP-3.           
008500     SKIP2                                                                
008600 77    SUM-KVUTRS                PIC S9(7)   VALUE +0   COMP-3.           
008900     SKIP2                                                                
009000 01    NYCKEL-ID                 PIC 9(2)    COMP-3.                      
009100   88  NYCKEL-IDPRODNR           VALUE 1.                                 
009200   88  NYCKEL-ORDERID            VALUE 2.                                 
009300     SKIP2                                                                
009400 01    FRAN-BILD                 PIC 9(4).                                
009500   88  FRAN-BILD-OK              VALUE 4325.                              
009600     SKIP2                                                                
009700 01    WS-SLINGA-KLAR            PIC X(1).                                
009800   88  SLINGA-KLAR               VALUE 'J'.                               
009900     EJECT                                                                
010000 01 FILLER                       PIC X(16) VALUE 'WDE420-SPAR '.          
010100 01  -COPY WDE411 -PRE SPAR-.                                             
010200     EJECT                                                                
010300 01    NYCKLAR-TILL-DLI.                                                  
010400   03    W-WDE601-IDPRODNR-X.                                             
010500     05    W-WDE601-IDPRODNR     PIC S9(7)            COMP-3.             
010600                                                                          
010610   03    W-IDPRODNR-X.                                                    
010620     05    W-IDPRODNR            PIC S9(7)            COMP-3.             
010630                                                                          
010700   03    W-IDANSTNR-X.                                                    
010800     05    W-IDANSTNR            PIC S9(5)            COMP-3.             
010900                                                                          
011000   03    W-IDRADNR-X.                                                     
011100     05    W-IDRADNR             PIC S9(5)            COMP-3.             
011200                                                                          
011300   03    W-IDRADNR-ISRT-X.                                                
011400     05    W-IDRADNR-ISRT        PIC S9(5)            COMP-3.             
011500                                                                          
011600   03    W-IDARTNR-X.                                                     
011700     05    W-IDARTNR             PIC S9(9)            COMP-3.             
011800                                                                          
011810   03    W-KDSEGKEY-X.                                                    
011820     05    W-KDSEGKEY            PIC X                VALUE '1'.          
011830                                                                          
011900   03    W-WDE4A1-KUNDORDER-X.                                            
012000     05    W-4A1-IDDISTR         PIC S9(5)            COMP-3.             
012100     05    W-4A1-IDKUNDNR        PIC S9(7)            COMP-3.             
012200     05    W-4A1-IDKUNDRF.                                                
012300       07    W-4A1-IDORDNR       PIC X(5).                                
012400       07    FILLER              PIC X(5)   VALUE SPACE.                  
012500                                                                          
012600   03    W-WDE401-KUNDORDER-X.                                            
012700     05    W-401-IDDISTR         PIC S9(5)            COMP-3.             
012800     05    W-401-IDKUNDNR        PIC S9(7)            COMP-3.             
012900     05    W-401-IDKUNDRF.                                                
013000       07    W-401-IDORDNR       PIC X(5).                                
013100       07    FILLER              PIC X(5)   VALUE SPACE.                  
013200     05    W-401-IDPRODNR        PIC S9(7)            COMP-3.             
013300     05    W-401-IDPLKLST        PIC S9(3)            COMP-3.             
013400                                                                          
013500   03    W-WDE4B-KEYSEQ-MIN-X.                                            
013600     05    W-420-IDPRODNR-MIN    PIC S9(7)            COMP-3.             
013700     05    W-420-IDPURAD-MIN     PIC S9(5)            COMP-3.             
013800                                                                          
013900   03    W-WDE4B-KEYSEQ-MAX-X.                                            
014000     05    W-420-IDPRODNR-MAX    PIC S9(7)            COMP-3.             
014100     05    W-420-IDPURAD-MAX     PIC S9(5)            COMP-3.             
014200                                                                          
014300   03    W-WDE420-IDPURAD-X.                                              
014400     05    W-420-IDPURAD         PIC S9(5)            COMP-3.             
014500                                                                          
014600   03    W-IDDC-X.                                                        
014700     05    W-IDDC-ARTS           PIC X(2).                                
014710                                                                          
014720   03  W-IDDC-B6-X.                                                       
014730       05 W-IDDC-B6                  PIC X(2).                            
014800     EJECT                                                                
014900 01    MEDDELANDE.                                                        
015000   03    FEL1.                                                            
015100     05    FILLER                PIC X(40)   VALUE                        
015200             '701. ORDERN SAKNAS                     '.                   
015300     05    FILLER                PIC X(40)   VALUE                        
015400             '701. ORDER MISSING                     '.                   
015500   03    FILLER REDEFINES FEL1.                                           
015600     05    FEL-1 OCCURS 2        PIC X(40).                               
015700     SKIP2                                                                
015800   03    FEL2.                                                            
015900     05    FILLER                PIC X(40)   VALUE                        
016000             '716. ORDERN EJ DELAD                   '.                   
016100     05    FILLER                PIC X(40)   VALUE                        
016200             '716. ORDER HAS NOT BEEN SPLIT          '.                   
016300   03    FILLER REDEFINES FEL2.                                           
016400     05    FEL-2 OCCURS 2        PIC X(40).                               
016500     SKIP2                                                                
016600   03    FEL3.                                                            
016700     05    FILLER                PIC X(40)   VALUE                        
016800             '829. RADEN EJ UTDELAD                  '.                   
016900     05    FILLER                PIC X(40)   VALUE                        
017000             '829  LINE HAS NOT BEEN SPLIT           '.                   
017100   03    FILLER REDEFINES FEL3.                                           
017200     05    FEL-3 OCCURS 2        PIC X(40).                               
017300     SKIP2                                                                
017400   03    FEL4.                                                            
017500     05    FILLER                PIC X(40)   VALUE                        
017600             '821. RADNUMMER SAKNAS PÅ ORDERN        '.                   
017700     05    FILLER                PIC X(40)   VALUE                        
017800             '821  LINE NUMBER MISSING               '.                   
017900   03    FILLER REDEFINES FEL4.                                           
018000     05    FEL-4 OCCURS 2        PIC X(40).                               
018100     EJECT                                                                
018200   03    FEL5.                                                            
018300     05    FILLER                PIC X(40)   VALUE                        
018400             '766. RAD REDAN RAPPORTERAD             '.                   
018500     05    FILLER                PIC X(40)   VALUE                        
018600             '766 LINE ALREADY REPORTED              '.                   
018700   03    FILLER REDEFINES FEL5.                                           
018800     05    FEL-5 OCCURS 2        PIC X(40).                               
018900     SKIP2                                                                
019000   03    FEL6.                                                            
019100     05    FILLER                PIC X(40)   VALUE                        
019200             '767. RADEN DELVIS RAPPORTERAD          '.                   
019300     05    FILLER                PIC X(40)   VALUE                        
019400             '767 LINE PARTLY REPORTED               '.                   
019500   03    FILLER REDEFINES FEL6.                                           
019600     05    FEL-6 OCCURS 2        PIC X(40).                               
019700     SKIP2                                                                
019800   03    FEL7.                                                            
019900     05    FILLER                PIC X(40)   VALUE                        
020000             '768. FEL ARTIKELNR ANGIVET             '.                   
020100     05    FILLER                PIC X(40)   VALUE                        
020200             '768 WRONG PART NUMBER                  '.                   
020300   03    FILLER REDEFINES FEL7.                                           
020400     05    FEL-7 OCCURS 2        PIC X(40).                               
020500     SKIP2                                                                
020600   03    FEL8.                                                            
020700     05    FILLER                PIC X(40)   VALUE                        
020800             '769. ARTIKEL SAKNAS PÅ ARTREG          '.                   
020900     05    FILLER                PIC X(40)   VALUE                        
021000             '769 PART NUMBER MISSING                '.                   
021100   03    FILLER REDEFINES FEL8.                                           
021200     05    FEL-8 OCCURS 2        PIC X(40).                               
021300     EJECT                                                                
021400   03    FEL9.                                                            
021500     05    FILLER                PIC X(40)   VALUE                        
021600             '711. ANGIVEN PACKARE SAKNAS PÅ ORDERN  '.                   
021700     05    FILLER                PIC X(40)   VALUE                        
021800             '711 PACKER AND ORDER DO NOT MATCH      '.                   
021900   03    FILLER REDEFINES FEL9.                                           
022000     05    FEL-9 OCCURS 2        PIC X(40).                               
022100     SKIP2                                                                
022200   03    FEL10.                                                           
022300     05    FILLER                PIC X(40)   VALUE                        
022400             '710. ORDERN FÄRDIGRAPPORTERAD          '.                   
022500     05    FILLER                PIC X(40)   VALUE                        
022600             '710 ORDER TOTALLY REPORTED             '.                   
022700   03    FILLER REDEFINES FEL10.                                          
022800     05    FEL-10 OCCURS 2       PIC X(40).                               
022900     SKIP2                                                                
023000   03    FEL11.                                                           
023100     05    FILLER                PIC X(40)   VALUE                        
023200             '774. RAD REDAN RAPPORT. AV NOLLJAGARE  '.                   
023300     05    FILLER                PIC X(40)   VALUE                        
023310             '710 LINE REPORTED BY ZERO HUNTER       '.                   
023500   03    FILLER REDEFINES FEL11.                                          
023600     05    FEL-11 OCCURS 2       PIC X(40).                               
023700     SKIP2                                                                
023800   03    FEL12.                                                           
023900     05    FILLER                PIC X(40)   VALUE                        
024000             '806. EJ AUKTORISERAD ANVÄNDARE         '.                   
024100     05    FILLER                PIC X(40)   VALUE                        
024200             '806 USER NOT AUTHORISED                '.                   
024300   03    FILLER REDEFINES FEL12.                                          
024400     05    FEL-12 OCCURS 2       PIC X(40).                               
024500     EJECT                                                                
024600   03    FEL13.                                                           
024700     05    FILLER                PIC X(40)   VALUE                        
024800             'DÅLIGT OBJEKT FÅR EJ UPPDATERAS        '.                   
024900     05    FILLER                PIC X(40)   VALUE                        
025000             'BAD CORE WILL NOT BE UPDATED           '.                   
025100   03    FILLER REDEFINES FEL13.                                          
025200     05    FEL-13 OCCURS 2        PIC X(40).                              
025300     SKIP2                                                                
025400   03    FEL17.                                                           
025500     05    FILLER                PIC X(40)   VALUE                        
025600             '748. UPPLYSTA FÄLT FEL                 '.                   
025700     05    FILLER                PIC X(40)   VALUE                        
025800             '748 HIGHLIT FIELDS WRONG               '.                   
025900   03    FILLER REDEFINES FEL17.                                          
026000     05    FEL-17 OCCURS 2       PIC X(40).                               
026100     SKIP2                                                                
026200   03    FEL18.                                                           
026300     05    FILLER                PIC X(40)   VALUE                        
026400             '760. UPPGIFTER SAKNAS                  '.                   
026500     05    FILLER                PIC X(40)   VALUE                        
026600             '760 INFORMATION MISSING                '.                   
026700   03    FILLER REDEFINES FEL18.                                          
026800     05    FEL-18 OCCURS 2       PIC X(40).                               
026900     SKIP2                                                                
027000   03    FEL19.                                                           
027100     05    FILLER                PIC X(40)   VALUE                        
027200             '749. FEL NYCKEL                        '.                   
027300     05    FILLER                PIC X(40)   VALUE                        
027400             '749 WRONG KEY                          '.                   
027500   03    FILLER REDEFINES FEL19.                                          
027600     05    FEL-19 OCCURS 2       PIC X(40).                               
027700     EJECT                                                                
027800   03    FEL21.                                                           
027900     05    FILLER                PIC X(40)   VALUE                        
028000             '804 AVVIKELSEUPPDATERING PÅGÅR         '.                   
028100     05    FILLER                PIC X(40)   VALUE                        
028110             '756 DEVIATION CONTROL IN PROGRESS      '.                   
028300   03    FILLER REDEFINES FEL21.                                          
028400     05    FEL-21 OCCURS 2       PIC X(40).                               
028500     SKIP2                                                                
028600   03    MED1.                                                            
028700     05    FILLER                PIC X(40)   VALUE                        
028800             '756. UPPDATERING UTFÖRD                '.                   
028900     05    FILLER                PIC X(40)   VALUE                        
029000             '756 UPDATING HAS BEEN DONE             '.                   
029100   03    FILLER REDEFINES MED1.                                           
029200     05    MED-1 OCCURS 2        PIC X(40).                               
029300*** MEDDELANDE OM ANNULLATION ***                                         
029400     SKIP2                                                                
029500   03    MED2.                                                            
029600     05    FILLER                PIC X(40)    VALUE                       
029700            '    ANNULLERING UTFÖRD                  '.                   
029800     05    FILLER                PIC X(40)    VALUE                       
029900            '    CANCELATION REGISTERED              '.                   
030000   03    FILLER REDEFINES MED2.                                           
030100     05    MED-2 OCCURS 2        PIC X(40).                               
030200     EJECT                                                                
030300*01   -COPY  WSECAREA                                                     
030400     EJECT                                                                
       01  FILLER                      PIC X(16)   VALUE 'W488ORCN'.            
      *01 -COPY W488ORCN                                                        
           EJECT                                                                
030500* - - - - - - - - - -  BYTES-OBJEKTTEST                                   
030600*                                                                         
030700 01  TEST-IDARTNR              PIC 9(9)    COMP-3.                        
030800*01  FILLER -COPY WWBYT09      -RED TEST-IDARTNR                          
030900     EJECT                                                                
031000******************************************************************        
031100*                                                                         
031200*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
031300*                                                                         
031400 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
031500                                                                          
031600*01    MID -COPY W4I32501.                                                
031700     EJECT                                                                
031800*01    -COPY WMSGAREA                                                     
031900     EJECT                                                                
032000*  03    MOD -COPY W4O32501  -RED MSG-AREA.                               
032100     EJECT                                                                
032200*01    -COPY WMFSAREA                                                     
032300     EJECT                                                                
032400******************************************************************        
032500*                                                                         
032600*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
032700*                                                                         
032800 01    IMS-WS.                                                            
032900   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
033000                                                                          
033100*                        **** STATUS-KOD FRÅN IMS                         
033200   03    STATUS-KUNDORDER-SEK-WS PIC XX.                                  
033300     88    KUNDORDER-SEK-FINNS              VALUE '  '.                   
033400     88    KUNDORDER-SEK-SAKNAS             VALUE 'GE' 'GB'.              
033500   03    STATUS-WS               PIC XX.                                  
033600     88    SEGMENT-FINNS                    VALUE '  '.                   
033700     88    SEGMENT-SAKNAS                   VALUE 'GE'.                   
033800     88    SEGMENT-FINNS-REDAN              VALUE 'II'.                   
033900     88    SLUT-PA-BASEN                    VALUE 'GB'.                   
034000     SKIP3                                                                
034100*                        **** LEVEL-KOD FRÅN IMS                          
034200   03    LEVEL-WS                PIC X(2).                                
034300     88    ROT-SEG-SAKNAS                   VALUE '00'.                   
034400     SKIP3                                                                
034500   03    GODK-STATUSKODER.                                                
034600     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
034700     SKIP3                                                                
034800 01    SSA1                      PIC X(64).                               
034900 01    SSA2                      PIC X(64).                               
035000 01    SSA3                      PIC X(64).                               
035200     EJECT                                                                
035300*                            IMS FUNKTIONSKODER                           
035400*01    -COPY W0003                                                        
035500     EJECT                                                                
035600*                            DLI INPUT-OUTPUT AREA                        
035610 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
035700 01    DLI-IO-AREA.                                                       
035800   03    IO-AREA                 PIC X(500)  VALUE SPACE.                 
035900     SKIP3                                                                
036000*  03    WDE401   -COPY WDE401              -RED IO-AREA.                 
036100     EJECT                                                                
036200*  03    WDE411   -COPY WDE411              -RED IO-AREA.                 
036300     EJECT                                                                
036600*                            DLI INPUT-OUTPUT AREA                        
036610 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
036700 01    DLI-IO-AREA2.                                                      
036800   03    IO-AREA2                PIC X(900)  VALUE SPACE.                 
036900     SKIP3                                                                
037000*  03    WLARTC11 -COPY WDK611 -RED IO-AREA2.                             
037100     EJECT                                                                
037110*  03    WLARTS11 -COPY WDK711 -RED IO-AREA2.                             
037120     EJECT                                                                
037130 01    DLI-IO-WDE601.                                                     
037151*  03    WDE601   -COPY WDE601.                                           
037152     EJECT                                                                
037153 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
037154 01   DLI-IO-AREA-B601.                                                   
037155*     03  -COPY WDB601                                                    
037156                                                                          
037160*                                                                         
037200 01  FILLER                     PIC X(16) VALUE 'ALT-IO-AREA'.            
037300 01    ALT-IO-AREA.                                                       
037400     03    ALT-LL               PIC S9(4) COMP SYNC VALUE +63.            
037500     03    ALT-Z1               PIC X     VALUE LOW-VALUE.                
037600     03    ALT-Z2               PIC X     VALUE LOW-VALUE.                
037700     03    ALT-TRANSKOD         PIC X(8)  VALUE 'W5T108X '.               
038000     03    ALT-IDTRANS          PIC X(4)  VALUE '4325'.                   
038100     03    ALT-KDMFSFOR         PIC X     VALUE SPACE.                    
038200     03    ALT-IDARTNR-IN       PIC X(9).                                 
038300     03    ALT-IDARTNR-UT       PIC X(9)  VALUE ZERO.                     
038400     03    ALT-IDPW-IN          PIC X(8).                                 
038500     03    ALT-IDPW-UT          PIC X(8)  VALUE SPACE.                    
038510     03    ALT-IDDC-IN          PIC X(2)  VALUE SPACE.                    
038520     03    ALT-IDDC-UT          PIC X(2)  VALUE SPACE.                    
038600     03    ALT-IDPRODNR         PIC 9(7)  VALUE ZERO.                     
038700     03    ALT-KDORDKL          PIC 9     VALUE ZERO.                     
038801                                                                          
038820     EJECT                                                                
038900 LINKAGE SECTION.                                                         
039000*01    -COPY W0009     -PRE MSG-                                          
039100                                                                          
039310*01    -COPY W0009     -PRE ALT-                                          
039320     EJECT                                                                
      *01    -COPY W0009     -PRE SYNQ-                                         
           EJECT                                                                
039400*01    -COPY W0008     -PRE WDE4-                                         
039500     05  FILLER                  PIC X.                                   
039600                                                                          
039700*01    -COPY W0008     -PRE WDE4A-                                        
039800     05  FILLER                  PIC X.                                   
039900     EJECT                                                                
039910*01    -COPY W0008     -PRE WDE6-                                         
039920     05  FILLER                  PIC X.                                   
039930     EJECT                                                                
040000*01    -COPY W0008     -PRE ARTC-                                         
040100     05  FILLER                  PIC X.                                   
040200                                                                          
040210*01    -COPY W0008     -PRE ARTS-                                         
040220     05  FILLER                  PIC X.                                   
040230     EJECT                                                                
040300*01    -COPY W0008     -PRE WDE4B-                                        
040400     05  FILLER                  PIC X.                                   
040500     EJECT                                                                
040510*01    -COPY W0008     -PRE WDB6-                                         
040520     05  FILLER                  PIC X.                                   
040530     EJECT                                                                
       01  SYNQ-ATAB-PCB             PIC X.                                     
       01  WDQ3-PCB                  PIC X.                                     
           EJECT                                                                
040600 PROCEDURE DIVISION USING MSG-PCB   ALT-PCB  SYNQ-PCB                     
                                WDE4-PCB WDE4A-PCB                              
040701                          WDE6-PCB ARTC-PCB ARTS-PCB                      
040702                          WDE4B-PCB WDB6-PCB                              
                                SYNQ-ATAB-PCB WDQ3-PCB.                         
040710 MAIN SECTION.                                                            
040800     ENTRY 'DLITCBL' USING MSG-PCB   ALT-PCB  SYNQ-PCB                    
                                 WDE4-PCB WDE4A-PCB                             
040810                           WDE6-PCB ARTC-PCB ARTS-PCB                     
040811                           WDE4B-PCB WDB6-PCB                             
                                 SYNQ-ATAB-PCB WDQ3-PCB.                        
041000                                                                          
041100     ACCEPT DAGENS-DATUM FROM DATE                                        
041200     PERFORM IMS-GET-MSG                                                  
041300     IF SEGMENT-FINNS                                                     
041400        PERFORM A-INIT-SPARA-INPUT                                        
041500        IF FRAN-BILD-OK                                                   
041600           PERFORM B-KOLLA-INPUT                                          
041700           IF WS-KDFEL = ZERO                                             
041800              PERFORM C-KONTROLL-AV-ORDERDEL                              
041900              IF WS-KDFEL = ZERO                                          
042000***FRÅGA OM ANNULLERING***                                                
042100                 IF WS-FLJANEJ = 'N'                                      
042200                    PERFORM F-UPPDAT-UTREDN-SALDO                         
042300                 END-IF                                                   
042400                 IF WS-KDFEL = ZERO                                       
042500                    PERFORM H-UPPDAT-NOLLJ-FLAGGA                         
042600                 END-IF                                                   
042700              END-IF                                                      
042800           END-IF                                                         
042900           IF WS-KDFEL > ZERO                                             
043000              PERFORM J-HAMTA-MEDDELANDE                                  
043100              IF WS-KDFEL < 20                                            
043200                 PERFORM K-VISA-BILD-IGEN                                 
043300              END-IF                                                      
043400           END-IF                                                         
043500        ELSE                                                              
043600           PERFORM L-TOM-SKAERM                                           
043700        END-IF                                                            
043800        MOVE MAX-MOD-LAENGD TO MSG-KVLL                                   
043900        PERFORM IMS-INSERT-MSG                                            
044000     END-IF                                                               
044100     MOVE ZERO TO RETURN-CODE                                             
044200     GOBACK                                                               
044300     .                                                                    
044400     EJECT                                                                
044500 A-INIT-SPARA-INPUT SECTION.                                              
044600                                                                          
044700     IF MSG-DUBBLA-TRANSKODER                                             
044800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I32501                 
044900       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
045000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
045100     ELSE                                                                 
045200       MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W4I32501                   
045300       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
045400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
045500     END-IF                                                               
045600*                                                                         
045700     MOVE ZERO TO WS-KDFEL                                                
045800     MOVE LOW-VALUE TO MSG-AREA                                           
045900     MOVE MFS-IDTRANS TO FRAN-BILD                                        
046000     MOVE MFS-KDMFSFOR TO WS-KDMFSFOR ALT-KDMFSFOR                        
046100     PERFORM AB-INIT-NYCKLAR                                              
046200     MOVE 'W4O325N1' TO MFS-IDMOD                                         
046300     MOVE '4325' TO MOD-IDTRANS                                           
046400*                                                                         
046500     MOVE MFS-RENSA-FAELT TO MOD-IDPRODNR-IN                              
046600                             MOD-IDANSTNR-IN                              
046700                             MOD-IDKOLLI-IN                               
046800                             MOD-IDDISTR-IN                               
046900                             MOD-IDKUNDNR-IN                              
047000                             MOD-IDORDNR-IN                               
047100                             MOD-TEMFSFEL                                 
047200                             MOD-TEMFSINF                                 
047300     IF SWEDISH-TEXT                                                      
047400       MOVE +1 TO SPRAK-INDX                                              
047500     ELSE                                                                 
047600       MOVE +2 TO SPRAK-INDX                                              
047700     END-IF                                                               
047800     MOVE MFS-ROER-EJ-FAELT TO MOD-IDRADNR-IN                             
047900                               MOD-IDARTNR-IN                             
048000     .                                                                    
048100     EJECT                                                                
048200 AB-INIT-NYCKLAR SECTION.                                                 
048300                                                                          
048400     IF MID-IDPRODNR-IN NOT = ALL '+'                                     
048500       MOVE MID-IDPRODNR-IN TO WS-IDPRODNR                                
048600     ELSE                                                                 
048700       INSPECT MID-IDPRODNR-UT REPLACING LEADING SPACE BY ZERO            
048800       MOVE MID-IDPRODNR-UT TO WS-IDPRODNR                                
048900     END-IF                                                               
049000                                                                          
049100     IF MID-IDKUNDNR-IN NOT = ALL '+'                                     
049200       MOVE MID-IDKUNDNR-IN TO WS-IDKUNDNR                                
049300     ELSE                                                                 
049400       INSPECT MID-IDKUNDNR-UT REPLACING LEADING SPACE BY ZERO            
049500       MOVE MID-IDKUNDNR-UT TO WS-IDKUNDNR                                
049600     END-IF                                                               
049700                                                                          
049800     IF MID-IDDISTR-IN  NOT = ALL '+'                                     
049900       MOVE MID-IDDISTR-IN  TO WS-IDDISTR                                 
050000     ELSE                                                                 
050100       INSPECT MID-IDDISTR-UT REPLACING LEADING SPACE BY ZERO             
050200       MOVE MID-IDDISTR-UT  TO WS-IDDISTR                                 
050300     END-IF                                                               
050400                                                                          
050500     IF MID-IDORDNR-IN  NOT = ALL '+'                                     
050600       MOVE MID-IDORDNR-IN  TO WS-IDORDNR                                 
050700     ELSE                                                                 
050800       INSPECT MID-IDORDNR-UT  REPLACING LEADING SPACE BY ZERO            
050900       MOVE MID-IDORDNR-UT  TO WS-IDORDNR                                 
051000     END-IF                                                               
051010                                                                          
051020     IF MID-IDDC-IN = ALL '+'                                             
051030       IF MID-IDDC-UT = SPACE                                             
051040         MOVE 19               TO WS-KDFEL                                
051041         MOVE SPACE            TO W-IDDC-B6                               
051050       ELSE                                                               
051060         MOVE MID-IDDC-UT      TO W-IDDC-B6                               
051070       END-IF                                                             
051080     ELSE                                                                 
051090       MOVE MID-IDDC-IN        TO W-IDDC-B6                               
051093     END-IF                                                               
051094     PERFORM IMS-GU-WDB601                                                
051095                                                                          
051200***  VILKEN MARKERING FRÅN ANNULLATION GÄLLER ***                         
051300     IF MID-FLJANEJ-IN NOT = ALL '+'                                      
051400       MOVE MID-FLJANEJ-IN  TO WS-FLJANEJ                                 
051500     ELSE                                                                 
051600       MOVE MID-FLJANEJ-UT  TO WS-FLJANEJ                                 
051700     END-IF                                                               
051800     .                                                                    
051900     EJECT                                                                
052000 B-KOLLA-INPUT SECTION.                                                   
052100                                                                          
052200     MOVE ZERO   TO NYCKEL-ID                                             
052300                                                                          
052400     IF WS-IDPRODNR NOT NUMERIC                                           
052500       MOVE ZERO TO WS-IDPRODNR                                           
052600       MOVE 19 TO WS-KDFEL                                                
052700     END-IF                                                               
052710                                                                          
052800     IF WS-IDKUNDNR NOT NUMERIC                                           
052900       MOVE ZERO TO WS-IDKUNDNR                                           
053000       MOVE 19 TO WS-KDFEL                                                
053100     END-IF                                                               
053110                                                                          
053200     IF WS-IDDISTR NOT NUMERIC                                            
053300       MOVE ZERO TO WS-IDDISTR                                            
053400       MOVE 19 TO WS-KDFEL                                                
053500     END-IF                                                               
053510                                                                          
053600     IF WS-IDORDNR NOT NUMERIC                                            
053700       MOVE ZERO TO WS-IDORDNR                                            
053800       MOVE 19 TO WS-KDFEL                                                
053900     END-IF                                                               
053915                                                                          
053921     IF DCS-KDDC = SPACE OR DCS-DDC                                       
053922       MOVE 19                 TO WS-KDFEL                                
053924     ELSE                                                                 
053925       MOVE DCS-IDDC           TO W-IDDC-ARTS                             
053930     END-IF                                                               
054000                                                                          
054100     IF MID-IDPRODNR-IN NOT = ALL '+'                                     
054200       MOVE ZERO TO     WS-IDDISTR                                        
054300                        WS-IDKUNDNR                                       
054400                        WS-IDORDNR                                        
054500       MOVE 1    TO     NYCKEL-ID                                         
054600     ELSE                                                                 
054700       IF MID-IDDISTR-IN NOT = ALL '+'                                    
054800       OR MID-IDKUNDNR-IN NOT = ALL '+'                                   
054900       OR MID-IDORDNR-IN NOT = ALL '+'                                    
055000         MOVE ZERO TO     WS-IDPRODNR                                     
055100         MOVE 2    TO     NYCKEL-ID                                       
055200       ELSE                                                               
055300         IF WS-IDPRODNR NOT = ZERO                                        
055400           MOVE ZERO   TO   WS-IDDISTR                                    
055500                            WS-IDKUNDNR                                   
055600                            WS-IDORDNR                                    
055700           MOVE 1      TO   NYCKEL-ID                                     
055800         ELSE                                                             
055900           MOVE ZERO   TO   WS-IDPRODNR                                   
056000           MOVE 2      TO   NYCKEL-ID                                     
056100         END-IF                                                           
056200       END-IF                                                             
056300     END-IF                                                               
056400                                                                          
056500     IF MID-IDRADNR-IN NOT = ALL '+'                                      
056600       IF MID-IDRADNR-IN NUMERIC                                          
056700         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDRADNR-ATTR                     
056800         MOVE MID-IDRADNR-IN TO WS-IDRADNR                                
056900       ELSE                                                               
057000         MOVE ZERO TO WS-IDRADNR                                          
057100         MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-ATTR                       
057200         MOVE 17  TO WS-KDFEL                                             
057300       END-IF                                                             
057400     ELSE                                                                 
057500       MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-ATTR                         
057600       MOVE 17  TO WS-KDFEL                                               
057700     END-IF                                                               
057800                                                                          
057900     IF MID-IDARTNR-IN NOT = ALL '+'                                      
058000       IF MID-IDARTNR-IN NUMERIC                                          
058100         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDARTNR-ATTR                     
058200         MOVE MID-IDARTNR-IN TO WS-IDARTNR                                
058300                                ALT-IDARTNR-IN                            
058400         MOVE IDARTNR-WS TO     TEST-IDARTNR                              
058500         IF BYT09-OBJEKT                                                  
058600            MOVE 13 TO WS-KDFEL                                           
058700         END-IF                                                           
058800       ELSE                                                               
058900         MOVE ZERO  TO WS-IDARTNR                                         
059000         MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-ATTR                       
059100         MOVE 17  TO WS-KDFEL                                             
059200       END-IF                                                             
059300     ELSE                                                                 
059400       MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-ATTR                         
059500       MOVE 17  TO WS-KDFEL                                               
059600     END-IF                                                               
059700                                                                          
059800     IF MID-IDPW-IN = ALL '+'                                             
059900        IF MFS-IDTRANS = WS-EGEN-BILD                                     
060000            MOVE MID-IDPW-UT TO IDPW-WS                                   
060100        ELSE                                                              
060200            MOVE SPACE TO IDPW-WS                                         
060300        END-IF                                                            
060400     ELSE                                                                 
060500        MOVE MID-IDPW-IN TO IDPW-WS                                       
060600     END-IF                                                               
060700                                                                          
060800     PERFORM S01-KOLLA-TILLAATEN-PASSWORD                                 
060900     IF WS-PASSWORD-OK = NEJ                                              
061000         IF WS-KDFEL = ZERO                                               
061100             MOVE 12 TO WS-KDFEL                                          
061200         END-IF                                                           
061300     ELSE                                                                 
061400         MOVE IDPW-WS TO ALT-IDPW-IN                                      
061500     END-IF                                                               
061600                                                                          
061700     MOVE IDPW-WS TO MOD-IDPW-UT                                          
061800                                                                          
061900***FÖR RÄTT VÄRDE TILL IN- RESP UTFÄLTEN ***                              
062000     IF WS-FLJANEJ = '+' OR SPACE                                         
062100       MOVE 'N' TO WS-FLJANEJ                                             
062200     END-IF                                                               
062300     IF WS-FLJANEJ = 'J' OR 'N'                                           
062400       MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLJANEJ-ATTR                      
062500       MOVE WS-FLJANEJ TO MOD-FLJANEJ-UT                                  
062600     ELSE                                                                 
062640       MOVE WS-FLJANEJ TO MOD-FLJANEJ-UT                                  
062800       MOVE MFS-ALFA-FAELT-FEL TO MOD-FLJANEJ-ATTR                        
062900       MOVE 17 TO WS-KDFEL                                                
063000     END-IF                                                               
063010                                                                          
063021     IF WS-PASSWORD-OK = NEJ                                              
063031       MOVE ' '             TO MOD-FLJANEJ-UT                             
063040     END-IF                                                               
063100                                                                          
063200     MOVE MFS-RENSA-FAELT  TO MOD-IDRADNR-IN                              
063300                              MOD-IDARTNR-IN                              
063400                              MOD-IDPW-IN                                 
063500                              MOD-FLJANEJ-IN                              
063600     EJECT                                                                
063700     MOVE WS-IDANSTNR  TO MOD-IDANSTNR-UT                                 
063800     INSPECT MOD-IDANSTNR-UT REPLACING                                    
063900                             LEADING ZEROES BY SPACE                      
064000     MOVE WS-IDDISTR   TO MOD-IDDISTR-UT                                  
064100     INSPECT MOD-IDDISTR-UT REPLACING                                     
064200                            LEADING ZEROES BY SPACE                       
064300     MOVE WS-IDKUNDNR  TO MOD-IDKUNDNR-UT                                 
064400     INSPECT MOD-IDKUNDNR-UT REPLACING                                    
064500                             LEADING ZEROES BY SPACE                      
064600     MOVE WS-IDORDNR   TO MOD-IDORDNR-UT                                  
064700     INSPECT MOD-IDORDNR-UT REPLACING                                     
064800                            LEADING ZEROES BY SPACE                       
064900     MOVE WS-IDPRODNR  TO MOD-IDPRODNR-UT                                 
065000     INSPECT MOD-IDPRODNR-UT REPLACING                                    
065100                             LEADING ZEROES BY SPACE                      
065110     MOVE DCS-IDDC     TO MOD-IDDC-UT                                     
065120                                                                          
065200     MOVE WS-IDRADNR   TO MOD-IDRADNR-UT                                  
065300     INSPECT MOD-IDRADNR-UT REPLACING                                     
065400                            LEADING ZEROES BY SPACE                       
065500     MOVE WS-IDARTNR   TO MOD-IDARTNR-UT                                  
065600     INSPECT MOD-IDARTNR-UT REPLACING                                     
065700                            LEADING ZEROES BY SPACE                       
065800     .                                                                    
065900     EJECT                                                                
066000 C-KONTROLL-AV-ORDERDEL                  SECTION.                         
066100                                                                          
066200     MOVE NEJ                            TO WS-RAD-FUNNEN                 
066300     PERFORM CA-HAMTA-RAD-OCH-NYCKLAR                                     
066400                                                                          
066500     IF WS-RAD-FUNNEN = JA     AND                                        
066600        WS-KDFEL      = 0                                                 
066700       MOVE IDPRODNR-WS TO W-WDE601-IDPRODNR                              
066710       PERFORM CB-LAS-WDE601                                              
066800                                                                          
066900       IF VORD-KDORDSTA < 4                                               
067000         PERFORM CC-KONTROLLERA-RAD                                       
067100       ELSE                                                               
067200         MOVE 10                         TO WS-KDFEL                      
067300       END-IF                                                             
067400     ELSE                                                                 
067500       IF WS-RAD-FUNNEN = JA                                              
067600           CONTINUE                                                       
067700       ELSE                                                               
067800           MOVE 4                        TO WS-KDFEL                      
067900       END-IF                                                             
068000     END-IF                                                               
068100     .                                                                    
068200     EJECT                                                                
068300 CA-HAMTA-RAD-OCH-NYCKLAR                SECTION.                         
068400                                                                          
068500     IF NYCKEL-IDPRODNR                                                   
068600       PERFORM CAA-HAMTA-DIST-KUND-ORD-PLKLST                             
068700     ELSE                                                                 
068800       PERFORM CAB-HAMTA-PRODNR-PLKLST                                    
068900     END-IF                                                               
069000     .                                                                    
069100     EJECT                                                                
069200 CAA-HAMTA-DIST-KUND-ORD-PLKLST          SECTION.                         
069300                                                                          
069400     MOVE IDPRODNR-WS                    TO W-420-IDPRODNR-MIN            
069500                                            W-420-IDPRODNR-MAX            
069600     MOVE 0                              TO W-420-IDPURAD-MIN             
069700     MOVE 99999                          TO W-420-IDPURAD-MAX             
069800     PERFORM IMS-GU-WDE411-MED-IDPRODNR                                   
069900                                                                          
070000     IF SEGMENT-FINNS                                                     
070100       MOVE IDRADNR-WS                   TO W-420-IDPURAD-MIN             
070200       PERFORM IMS-GU-WDE411-BSEQ                                         
070300                                                                          
070400       IF SEGMENT-FINNS                                                   
070500         MOVE JA                         TO WS-RAD-FUNNEN                 
070600         MOVE ORAD-WDE411                TO SPAR-ORAD-WDE411              
070700         PERFORM IMS-GNP-WDE411-BSEQ                                      
070800         MOVE KORD-IDDISTR               TO IDDISTR-WS                    
070900         MOVE KORD-IDKUNDNR              TO IDKUNDNR-WS                   
071000         MOVE KORD-IDORDNR5              TO IDORDNR-WS                    
071100         MOVE KORD-IDPLKLST              TO IDPLKLST-WS                   
071110         MOVE KORD-IDPRODNR              TO IDPRODNR-WS                   
071200                                                                          
071300         IF KORD-IDUSER = '00000000'                                      
071400           MOVE 2                        TO WS-KDFEL                      
071500         END-IF                                                           
071600       END-IF                                                             
071700     ELSE                                                                 
071800       MOVE 1                            TO WS-KDFEL                      
071900     END-IF                                                               
072000     .                                                                    
072100     EJECT                                                                
072200 CAB-HAMTA-PRODNR-PLKLST                 SECTION.                         
072300                                                                          
072400     MOVE IDDISTR-WS                     TO W-4A1-IDDISTR                 
072500     MOVE IDKUNDNR-WS                    TO W-4A1-IDKUNDNR                
072600     MOVE IDORDNR-WS                     TO W-4A1-IDORDNR                 
072700     PERFORM IMS-GU-WDE401-ASEQ                                           
072800                                                                          
072900     IF SEGMENT-FINNS                                                     
073000       PERFORM UNTIL WS-RAD-FUNNEN = JA   OR                              
073100                     SEGMENT-SAKNAS       OR                              
073200                     SLUT-PA-BASEN                                        
073300         PERFORM CABA-LAS-WDE411                                          
073400                                                                          
073500         IF WS-OGAE12-FINNS = JA                                          
073600           MOVE JA                       TO WS-RAD-FUNNEN                 
073700           PERFORM CABB-BEHANDLA-RAD                                      
073800         ELSE                                                             
073900           PERFORM IMS-GN-WDE401-ASEQ                                     
074000         END-IF                                                           
074100       END-PERFORM                                                        
074200     ELSE                                                                 
074300       MOVE 1                            TO WS-KDFEL                      
074400     END-IF                                                               
074500     .                                                                    
074600     EJECT                                                                
074700 CABA-LAS-WDE411                         SECTION.                         
074800                                                                          
074900     MOVE KORD-IDUSER                    TO SPAR-IDUSER                   
075000     MOVE KORD-IDDISTR                   TO W-401-IDDISTR                 
075100     MOVE KORD-IDKUNDNR                  TO W-401-IDKUNDNR                
075200     MOVE KORD-IDKUNDRF                  TO W-401-IDKUNDRF                
075300     MOVE KORD-IDPRODNR                  TO W-401-IDPRODNR                
075400                                            SPAR-IDPRODNR                 
075500     MOVE KORD-IDPLKLST                  TO W-401-IDPLKLST                
075600                                            SPAR-IDPLKLST                 
075700     MOVE IDRADNR-WS                     TO W-420-IDPURAD                 
075710     MOVE KORD-IDDC                      TO WS-KORD-IDDC                  
075800     PERFORM IMS-GU-WDE411                                                
075900                                                                          
076000     IF SEGMENT-FINNS                                                     
076010        AND                                                               
076020        WS-KORD-IDDC      = DCS-IDDC                                      
076022                                                                          
076100       MOVE JA                           TO WS-OGAE12-FINNS               
076200     ELSE                                                                 
076300       MOVE NEJ                          TO WS-OGAE12-FINNS               
076400     END-IF                                                               
076500     .                                                                    
076600     EJECT                                                                
076700 CABB-BEHANDLA-RAD                       SECTION.                         
076800                                                                          
076900     MOVE SPAR-IDPRODNR                  TO IDPRODNR-WS                   
077000     MOVE SPAR-IDPLKLST                  TO IDPLKLST-WS                   
077100     MOVE ORAD-WDE411                    TO SPAR-ORAD-WDE411              
077200                                                                          
077300     IF SPAR-IDUSER = '00000000'                                          
077400       MOVE 2                            TO WS-KDFEL                      
077500     END-IF                                                               
077600     .                                                                    
077700     EJECT                                                                
077800 CB-LAS-WDE601                           SECTION.                         
077900                                                                          
078000     MOVE IDDISTR-WS                     TO W-401-IDDISTR                 
078100     MOVE IDKUNDNR-WS                    TO W-401-IDKUNDNR                
078200     MOVE IDORDNR-WS                     TO W-401-IDORDNR                 
078300     MOVE IDPRODNR-WS                    TO W-401-IDPRODNR                
078400     MOVE IDPLKLST-WS                    TO W-401-IDPLKLST                
078500     PERFORM IMS-GU-WDE601                                                
078600     .                                                                    
078700     EJECT                                                                
078800 CC-KONTROLLERA-RAD                      SECTION.                         
078900                                                                          
079000     EVALUATE TRUE                                                        
079100       WHEN SPAR-ORAD-KVLEVART > ZERO                                     
079200         MOVE 6                          TO WS-KDFEL                      
079300                                                                          
079400       WHEN SPAR-ORAD-FLNOLLJ = JA                                        
079500         MOVE 11                         TO WS-KDFEL                      
079600                                                                          
079700       WHEN SPAR-ORAD-KDRADSTA > 3                                        
079800         MOVE 5                          TO WS-KDFEL                      
079900                                                                          
080000       WHEN SPAR-ORAD-IDARTNR NOT = IDARTNR-WS                            
080100         MOVE 7                          TO WS-KDFEL                      
080200     END-EVALUATE                                                         
080300     .                                                                    
080400     EJECT                                                                
080500 F-UPPDAT-UTREDN-SALDO SECTION.                                           
080600                                                                          
080700     MOVE IDARTNR-WS                TO W-IDARTNR                          
080800     IF DCS-CDC                                                           
080900        PERFORM IMS-GU-ARTC11-CDC                                         
081000        IF SEGMENT-FINNS                                                  
081100          IF CLAG-KVUTRS = ZERO                                           
081200             MOVE IDPRODNR-WS       TO ALT-IDPRODNR                       
081300             MOVE SPAR-ORAD-KDORDKL TO ALT-KDORDKL                        
081310             MOVE DCS-IDDC          TO ALT-IDDC-IN                        
081400             PERFORM IMS-INSERT-ALT                                       
081500          END-IF                                                          
081600        ELSE                                                              
081700          MOVE 8 TO WS-KDFEL                                              
081800        END-IF                                                            
081810     ELSE                                                                 
081811        MOVE DCS-IDDC               TO W-IDDC-ARTS                        
081812        PERFORM IMS-GU-ARTS11-SDC-NDC-LDC                                 
081813        IF SEGMENT-FINNS                                                  
081814          IF SLAG-KVUTRS = ZERO                                           
081815             MOVE IDPRODNR-WS       TO ALT-IDPRODNR                       
081816             MOVE SPAR-ORAD-KDORDKL TO ALT-KDORDKL                        
081817             MOVE DCS-IDDC          TO ALT-IDDC-IN                        
081819             PERFORM IMS-INSERT-ALT                                       
081820          END-IF                                                          
081821        ELSE                                                              
081822          MOVE 8 TO WS-KDFEL                                              
081823        END-IF                                                            
081824     END-IF                                                               
081830                                                                          
081900     .                                                                    
082000     EJECT                                                                
082100 H-UPPDAT-NOLLJ-FLAGGA SECTION.                                           
082200                                                                          
082300     MOVE IDRADNR-WS                     TO W-420-IDPURAD                 
082400     PERFORM IMS-GHU-WDE411                                               
           IF W-IDDC-B6 = 11 AND                                                
              (ORAD-ADLAGOMR = 90 OR 98) AND                                    
              ORAD-KDRADSTA < 4                                                 
            MOVE 'PCK' TO SYNQC-ORDERTYPE                                       
            MOVE ORAD-IDPURAD TO SYNQC-IDRADNR                                  
            MOVE W-401-IDPRODNR TO SYNQC-IDPRODNR                               
            MOVE W-401-IDPLKLST TO SYNQC-IDPLKLST                               
            CALL W488ORCN USING  SYNQC-W488ORCN SYNQ-PCB                        
                               SYNQ-ATAB-PCB WDQ3-PCB                           
082500     END-IF                                                               
082500*                                                                         
082600     MOVE JA                 TO ORAD-FLNOLLJ                              
082700     PERFORM IMS-REPL-WDE411                                              
082800*                                                                         
082900     IF WS-FLJANEJ = 'J'                                                  
083000        MOVE 15 TO WS-KDFEL                                               
083100     ELSE                                                                 
083110        IF DCS-NDC                                                        
083111          MOVE ' '         TO MOD-FLJANEJ-UT                              
083130        END-IF                                                            
083200        MOVE 20 TO WS-KDFEL                                               
083300     END-IF                                                               
083400     .                                                                    
083500     EJECT                                                                
083600 J-HAMTA-MEDDELANDE SECTION.                                              
083700     SKIP2                                                                
083800                                                                          
083900     EVALUATE WS-KDFEL                                                    
084000         WHEN 1 MOVE FEL-1 (SPRAK-INDX) TO MOD-TEMFSFEL                   
084100         WHEN 2 MOVE FEL-2 (SPRAK-INDX) TO MOD-TEMFSFEL                   
084200         WHEN 3 MOVE FEL-3 (SPRAK-INDX) TO MOD-TEMFSFEL                   
084300         WHEN 4 MOVE FEL-4 (SPRAK-INDX) TO MOD-TEMFSFEL                   
084400         WHEN 5 MOVE FEL-5 (SPRAK-INDX) TO MOD-TEMFSFEL                   
084500         WHEN 6 MOVE FEL-6 (SPRAK-INDX) TO MOD-TEMFSFEL                   
084600         WHEN 7 MOVE FEL-7 (SPRAK-INDX) TO MOD-TEMFSFEL                   
084700         WHEN 8 MOVE FEL-8 (SPRAK-INDX) TO MOD-TEMFSFEL                   
084800         WHEN 9 MOVE FEL-9 (SPRAK-INDX) TO MOD-TEMFSFEL                   
084900         WHEN 10 MOVE FEL-10 (SPRAK-INDX) TO MOD-TEMFSFEL                 
085000         WHEN 11 MOVE FEL-11 (SPRAK-INDX) TO MOD-TEMFSFEL                 
085100         WHEN 12 MOVE FEL-12 (SPRAK-INDX) TO MOD-TEMFSFEL                 
085200         WHEN 13 MOVE FEL-13 (SPRAK-INDX) TO MOD-TEMFSFEL                 
085300*** FÖR TEXT ANNULERING UTFÖRD ***                                        
085400         WHEN 15 MOVE MED-2 (SPRAK-INDX) TO MOD-TEMFSINF                  
085510         WHEN 17 MOVE FEL-17 (SPRAK-INDX) TO MOD-TEMFSFEL                 
085600         WHEN 18 MOVE FEL-18 (SPRAK-INDX) TO MOD-TEMFSFEL                 
085700         WHEN 19 MOVE FEL-19 (SPRAK-INDX) TO MOD-TEMFSFEL                 
085800         WHEN 20 MOVE MED-1 (SPRAK-INDX) TO MOD-TEMFSINF                  
085900         WHEN 21 MOVE FEL-21 (SPRAK-INDX) TO MOD-TEMFSFEL                 
086000     END-EVALUATE                                                         
086100     .                                                                    
086200     EJECT                                                                
086300 K-VISA-BILD-IGEN SECTION.                                                
086400     MOVE MFS-ROER-EJ-FAELT TO  MOD-IDARTNR-IN                            
086500                                MOD-IDRADNR-IN                            
086600     MOVE MFS-RENSA-FAELT   TO  MOD-IDARTNR-UT                            
086700                                MOD-IDRADNR-UT                            
086800     .                                                                    
086900     EJECT                                                                
087000 L-TOM-SKAERM SECTION.                                                    
087100     MOVE MFS-RENSA-FAELT                TO MOD-TEMFSFEL                  
087200                                            MOD-IDANSTNR-IN               
087300                                            MOD-IDANSTNR-UT               
087400                                            MOD-IDDISTR-IN                
087500                                            MOD-IDDISTR-UT                
087600                                            MOD-IDKUNDNR-IN               
087700                                            MOD-IDKUNDNR-UT               
087800                                            MOD-IDORDNR-IN                
087900                                            MOD-IDORDNR-UT                
088000                                            MOD-IDKOLLI-IN                
088100                                            MOD-IDKOLLI-UT                
088200                                            MOD-IDPRODNR-IN               
088300                                            MOD-IDPRODNR-UT               
088400                                            MOD-IDARTNR-IN                
088500                                            MOD-IDARTNR-UT                
088600                                            MOD-IDRADNR-IN                
088700                                            MOD-IDRADNR-UT                
088800                                            MOD-IDPW-IN                   
088900                                            MOD-IDPW-UT                   
089000*** RENSA ANNULLERINGSFÄLTET ***                                          
089100                                            MOD-FLJANEJ-IN                
089200                                            MOD-FLJANEJ-UT                
089300                                            MOD-TEMFSINF                  
089400     .                                                                    
089500     EJECT                                                                
089600                                                                          
089700 S01-KOLLA-TILLAATEN-PASSWORD SECTION.                                    
089800     SKIP2                                                                
089900                                                                          
090000     MOVE WS-IDUSER     TO SEC-IDUSER                                     
090100     MOVE WS-EGEN-BILD  TO SEC-IDTRANS                                    
090200     MOVE IDPW-WS       TO SEC-IDKEY                                      
090300     CALL WSECURIT USING                                                  
090400          SEC-IDUSER                                                      
090500          SEC-IDTRANS                                                     
090600          SEC-IDKEY                                                       
090700          SEC-KDSVAR                                                      
090800                                                                          
090900     IF SEC-KDSVAR = SPACE                                                
091000         MOVE JA TO WS-PASSWORD-OK                                        
091100     ELSE                                                                 
091200         MOVE NEJ TO WS-PASSWORD-OK                                       
091300     END-IF                                                               
091400     .                                                                    
091500     EJECT                                                                
091600                                                                          
091700* IMS SEKTIONER                                                           
091800     SKIP2                                                                
091900                                                                          
092000 IMS-GET-MSG SECTION.                                                     
092100                                                                          
092200     MOVE '  QC' TO GODK-STATUSKODER                                      
092300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
092400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
092500     PERFORM IMS-STATUSKONTROLL                                           
092600     SKIP2                                                                
092700     .                                                                    
092800 IMS-INSERT-MSG SECTION.                                                  
092900                                                                          
093000     IF NOT ENGLISH-TEXT                                                  
093100       MOVE '0' TO MFS-KDHUVOMR                                           
093200     END-IF                                                               
093300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
093400     MOVE SPACE TO GODK-STATUSKODER                                       
093500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
093600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
093700     PERFORM IMS-STATUSKONTROLL                                           
093800     SKIP2                                                                
093900     .                                                                    
094000 IMS-INSERT-ALT SECTION.                                                  
094100                                                                          
094200     MOVE LOW-VALUE TO ALT-Z1 ALT-Z2                                      
094300     MOVE SPACE TO GODK-STATUSKODER                                       
094400     CALL CBLTDLI USING ISRT ALT-PCB ALT-IO-AREA                          
094500     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
094600     PERFORM IMS-STATUSKONTROLL                                           
094700     .                                                                    
094800     SKIP2                                                                
094900 IMS-GU-WDE411  SECTION.                                                  
095000                                                                          
095100     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
095200            DELIMITED BY SIZE INTO SSA1                                   
095300     STRING 'WDE411  (IDPURAD  =' W-WDE420-IDPURAD-X ')'                  
095400            DELIMITED BY SIZE INTO SSA2                                   
095500     MOVE '  GE' TO GODK-STATUSKODER                                      
095600     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-AREA SSA1 SSA2                 
095700     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
095800     PERFORM IMS-STATUSKONTROLL                                           
095900     .                                                                    
096000     SKIP3                                                                
096100 IMS-GHU-WDE411 SECTION.                                                  
096200                                                                          
096300     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
096400            DELIMITED BY SIZE INTO SSA1                                   
096500     STRING 'WDE411  (IDPURAD  =' W-WDE420-IDPURAD-X ')'                  
096600            DELIMITED BY SIZE INTO SSA2                                   
096700     MOVE '  ' TO GODK-STATUSKODER                                        
096800     CALL CBLTDLI USING GHU WDE4-PCB DLI-IO-AREA SSA1 SSA2                
096900     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
097000     PERFORM IMS-STATUSKONTROLL                                           
097100     .                                                                    
097200     SKIP3                                                                
097300 IMS-GU-WDE601   SECTION.                                                 
097400                                                                          
097500     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
097600            DELIMITED BY SIZE INTO SSA1                                   
097800     MOVE '  ' TO GODK-STATUSKODER                                        
097900     CALL CBLTDLI USING GU  WDE6-PCB DLI-IO-WDE601 SSA1                   
098000     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
098100     PERFORM IMS-STATUSKONTROLL                                           
098200     .                                                                    
098300     SKIP3                                                                
098400 IMS-REPL-WDE411                         SECTION.                         
098500     SKIP2                                                                
098600     MOVE '    ' TO GODK-STATUSKODER                                      
098700     CALL CBLTDLI USING REPL WDE4-PCB DLI-IO-AREA                         
098800     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
098900     PERFORM IMS-STATUSKONTROLL                                           
099000     .                                                                    
099100     EJECT                                                                
099200 IMS-GU-WDE401-ASEQ                      SECTION.                         
099300                                                                          
099400     STRING 'WDE401  (WDE4ASEQ =' W-WDE4A1-KUNDORDER-X ')'                
099500            DELIMITED BY SIZE INTO SSA1                                   
099600     MOVE '  GE' TO GODK-STATUSKODER                                      
099700     CALL CBLTDLI USING GU WDE4A-PCB DLI-IO-AREA SSA1                     
099800     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
099900     PERFORM IMS-STATUSKONTROLL                                           
100000     .                                                                    
100100     SKIP3                                                                
100200 IMS-GN-WDE401-ASEQ                      SECTION.                         
100300                                                                          
100400     STRING 'WDE401  (WDE4ASEQ =' W-WDE4A1-KUNDORDER-X ')'                
100500            DELIMITED BY SIZE INTO SSA1                                   
100600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
100700     CALL CBLTDLI USING GN WDE4A-PCB DLI-IO-AREA SSA1                     
100800     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
100900     PERFORM IMS-STATUSKONTROLL                                           
101000     .                                                                    
101100     EJECT                                                                
101200 IMS-GU-WDE411-MED-IDPRODNR              SECTION.                         
101300                                                                          
101400     STRING 'WDE411  (WDE4BSEQ>=' W-WDE4B-KEYSEQ-MIN-X                    
101500                    '&WDE4BSEQ<=' W-WDE4B-KEYSEQ-MAX-X ')'                
101600            DELIMITED BY SIZE INTO SSA1                                   
101700     MOVE '  GE' TO GODK-STATUSKODER                                      
101800     CALL CBLTDLI USING GU WDE4B-PCB DLI-IO-AREA SSA1                     
101900     MOVE WDE4B-STATUS-CODE TO STATUS-WS                                  
102000     PERFORM IMS-STATUSKONTROLL                                           
102100     .                                                                    
102200     SKIP3                                                                
102300 IMS-GU-WDE411-BSEQ                      SECTION.                         
102400                                                                          
102500     STRING 'WDE411  (WDE4BSEQ =' W-WDE4B-KEYSEQ-MIN-X ')'                
102600            DELIMITED BY SIZE INTO SSA1                                   
102700     MOVE '  GE' TO GODK-STATUSKODER                                      
102800     CALL CBLTDLI USING GU WDE4B-PCB DLI-IO-AREA SSA1                     
102900     MOVE WDE4B-STATUS-CODE TO STATUS-WS                                  
103000     PERFORM IMS-STATUSKONTROLL                                           
103100     .                                                                    
103200     SKIP3                                                                
103300 IMS-GNP-WDE411-BSEQ                     SECTION.                         
103400                                                                          
103500     STRING 'WDE411  (WDE4BSEQ =' W-WDE4B-KEYSEQ-MIN-X ')'                
103600            DELIMITED BY SIZE INTO SSA1                                   
103700     MOVE 'WDE401   ' TO SSA2                                             
103800     MOVE '  ' TO GODK-STATUSKODER                                        
103900     CALL CBLTDLI USING GNP WDE4B-PCB DLI-IO-AREA SSA1 SSA2               
104000     MOVE WDE4B-STATUS-CODE TO STATUS-WS                                  
104100     PERFORM IMS-STATUSKONTROLL                                           
104200     .                                                                    
104300     EJECT                                                                
104400 IMS-GU-ARTC11-CDC     SECTION.                                           
104500                                                                          
104600     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
104700            DELIMITED BY SIZE INTO SSA1                                   
104710     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
104720            DELIMITED BY SIZE INTO SSA2                                   
105100     MOVE '  GE' TO GODK-STATUSKODER                                      
105200     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA2 SSA1 SSA2                
105400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
105500     PERFORM IMS-STATUSKONTROLL                                           
105600     .                                                                    
105700     EJECT                                                                
105710 IMS-GU-ARTS11-SDC-NDC-LDC    SECTION.                                    
105720                                                                          
105730     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
105740            DELIMITED BY SIZE INTO SSA1                                   
105750     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
105760            DELIMITED BY SIZE INTO SSA2                                   
105770     MOVE '  GE' TO GODK-STATUSKODER                                      
105780     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-AREA2 SSA1 SSA2                
105790     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
105791     PERFORM IMS-STATUSKONTROLL                                           
105792     .                                                                    
105793     EJECT                                                                
105794 IMS-GU-WDB601    SECTION.                                                
105795     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
105796          DELIMITED BY SIZE INTO SSA1                                     
105797     MOVE '  GE' TO GODK-STATUSKODER                                      
105798     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
105799     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
105800     PERFORM IMS-STATUSKONTROLL                                           
105801     IF SEGMENT-SAKNAS                                                    
105802         MOVE SPACE TO DCS-KDDC                                           
105803     END-IF                                                               
105804     .                                                                    
105810 IMS-STATUSKONTROLL SECTION.                                              
105900     SKIP2                                                                
106000     SET STATUS-IX TO 1                                                   
106100     SEARCH GODK-STATUS                                                   
106110       AT END                                                             
106120         CALL FELLOG                                                      
106200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
106210         CONTINUE                                                         
106300     END-SEARCH                                                           
106500     .                                                                    
