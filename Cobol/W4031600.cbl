000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4031600.                                                
000400 AUTHOR.         CAP GEMINI AB/EP.                                        
000500     DATE-WRITTEN.   FEB-MAR 86.                                          
000600                                                                          
000700     REMARKS.                                                             
000800                                                                          
000900*    FUNKTION.                                                            
001000*        PROGRAMMET VISAR OCH ÄNDRAR INNEHÅLLET I PACKADE                 
001100*        KOLLIN.                                                          
001200*        ÄNDRINGEN ÄR ALLTID ÅTERFÖRANDE AV DELAR AV ELLER                
001300*        ALLA RADERNA I KOLLIT TILL OPACKAT STATUS.                       
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSAKTION: W4T316                                              
001700*        MID:         W4I31601                                            
001800*                                                                         
001900*        OBS! PROGRAMMET ANVÄNDER INTE STANDARD DLI-IO-AREA.              
002000*             FÖR ATT UNDERLÄTTA HANTERINGEN HAR IO-AREAN                 
002100*             DELATS I TRE: DLI-IO-AREA1,                                 
002200*                           DLI-IO-AREA2 OCH                              
002300*                           DLI-IO-AREA3.                                 
002400*        TÄNK PÅ DETTA IFALL DU ÄNDRA/LÄGGA TILL IMS-SEKTIONER.           
002500*                                                                         
002600*    UTDATA.                                                              
002700*        MOD:         W4O31601                                            
002800*        SE&O                                                             
002900     EJECT                                                                
003000******************************************************************        
003100*    ÄNDRINGSJOURNAL:                                                     
003200*    900927 - ÄNDRING GJORD AV BOO HAMMARIN, CGLI                         
003300*           - ANPASSNING TILL NYTT UTSEENDE PÅ FYSISK DB WDE4             
003400*           - ANPASSNING TILL NYTT UTSEENDE PÅ FYSISK DB WDE6             
003500*           - BORTTAG AV FYSISK DB RDE5                                   
003600******************************************************************        
003700     EJECT                                                                
003800 ENVIRONMENT DIVISION.                                                    
003900     SKIP3                                                                
004000 DATA DIVISION.                                                           
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300                                                                          
004400*    -- CHECKED BY WY2000                                                 
004500 77   PROGRAM-NAMN           VALUE 'W4031600'                             
004600                                 PIC X(8).                                
004700 77    YES                       PIC X       VALUE 'Y'.                   
004800 77    JA                        PIC X       VALUE 'J'.                   
004900 77    NEJ                       PIC X       VALUE 'N'.                   
005000 77    RAETT                     PIC X       VALUE 'R'.                   
005100 77    FEL                       PIC X       VALUE 'F'.                   
005200 77    TOM-BILD                  PIC X       VALUE 'J'.                   
005300 77    KOLLI-BORTTAGET           PIC X       VALUE 'N'.                   
005400 77    ARB-RAD-HELT-ORAPP        PIC X       VALUE 'N'.                   
005500 77    KOLLI-KDKOLSTA-1-BORT     PIC X       VALUE 'N'.                   
005600 77    NY-NYCKEL                 PIC X       VALUE 'N'.                   
005700 77    SOEK-VIA-PRODNR           PIC X       VALUE 'N'.                   
005800 77    IND1                      PIC S9(9)   VALUE +0   COMP SYNC.        
005900 77    IND2                      PIC S9(9)   VALUE +0   COMP SYNC.        
006000 77    INDX                      PIC S9(9)   VALUE +0   COMP SYNC.        
006100 77    INX                       PIC S9(9)   VALUE +0   COMP SYNC.        
006200 77    RAD-INX                   PIC S9(9)   VALUE +0   COMP SYNC.        
006300 77    MAX-RAD-INX               PIC S9(9)   VALUE +13  COMP SYNC.        
006400 77    FG-INDX                   PIC S9(9)   VALUE +0   COMP SYNC.        
006500 77    TAB-INDX                  PIC S9(9)   VALUE +0   COMP SYNC.        
006600 77    MAX-FG-INDX               PIC S9(9)   VALUE +10  COMP SYNC.        
006700 77    BILD-RAD                  PIC S9(9)   VALUE +0   COMP SYNC.        
006800 77    MAX-MOD-LAENGD            PIC S9(4)   VALUE +397 COMP SYNC.        
006900 77    MIN-MOD-LAENGD            PIC S9(4)   VALUE +82  COMP SYNC.        
007000 77    WS-KVORDRAD               PIC S9(5)   VALUE +0    COMP-3.          
007100 77    WS-KVORDRAD-SPAR          PIC S9(5)   VALUE +0    COMP-3.          
007200 77    WS-KVORDRAD-LEVPL-SPAR    PIC S9(5)   VALUE +0    COMP-3.          
007300 77    WS-KVFALRAD               PIC S9(5)   VALUE +0    COMP-3.          
007400 77    WS-KVKOLLI-BORT           PIC S9(5)   VALUE +0    COMP-3.          
007500 77    WS-KDMFSFOR               PIC 9(1)   VALUE ZERO.                   
007600 77    WS-KOLLI-ADFLGEO          PIC X(3)   VALUE SPACE.                  
007700 77    WS-KOLLI-ADFLOMR          PIC 9(3)   VALUE ZERO.                   
007800 01    WS-IDUSER.                                                         
007900   03  FILLER                    PIC X(3)   VALUE ZERO.                   
008000   03  WS-IDANSTNR               PIC X(5)   VALUE SPACE.                  
008100 77    WS-IDDISTR-N              PIC 9(4)   VALUE ZERO.                   
008200 77    WS-IDDISTR                PIC X(4)   VALUE SPACE.                  
008300 77    WS-IDKUNDNR-N             PIC 9(6)   VALUE ZERO.                   
008400 77    WS-IDKUNDNR               PIC X(6)   VALUE SPACE.                  
008500 77    WS-IDKOLLI                PIC X(5)   VALUE SPACE.                  
008600 77    WS-IDKOLLI-NUM            PIC 9(5).                                
008700 77    WS-IDPRODNR               PIC X(7)   VALUE SPACE.                  
008800 77    WS-JFR-IDPRODNR           PIC X(7).                                
008900 77    WS-IDPRODNR-N             PIC 9(7).                                
009000 77    WS-KORD-IDPRODNR          PIC 9(7).                                
009100 77    WS-JFR-IDKOLLI            PIC X(5).                                
009200 77    WS-IDRADNR                PIC X(4)   VALUE ZERO.                   
009300 77    WS-IDRADNR-START          PIC 9(4)   VALUE ZERO.                   
009400 77    WS-SPAR-RAD               PIC 9(4)   VALUE ZERO.                   
009500 77    WS-START-RAD              PIC 9(4)   VALUE ZERO.                   
009600 77    WS-SISTA-RAD              PIC 9(4)   VALUE ZERO.                   
009700 77    WS-KVLEVART               PIC 9(6)   VALUE ZERO.                   
009800 77    WS-KOLLI-VKORDBTO-KOLLI   PIC S9(6)V9(1) VALUE ZERO.               
009900 77    WS-KOLLI-VLORDBTO-KOLLI   PIC S9(4)V9(3) VALUE ZERO.               
010000 77    WS-TOT-ANT-RADER          PIC S9(3)  VALUE +0   COMP-3.            
010100 77    MAX-ANTAL-RADER           PIC S9(3)  VALUE +13  COMP-3.            
010200 77    MAX-ANTAL-RADER-PLUS-1    PIC S9(3)  VALUE +14  COMP-3.            
010300 77    WS-IDPLKLST               PIC S9(3)  VALUE ZERO COMP-3.            
010400*                                                                         
010500 77    FILLER                    PIC  X(08) VALUE 'DN WS   '.             
010600 77    WS-DNOT-IDORDER           PIC S9(07) VALUE ZERO COMP-3.            
010700 77    WS-DNOT-IDARTNR           PIC S9(09) VALUE ZERO COMP-3.            
010800 77    WS-DNOT-IDDC              PIC  X(02) VALUE ZERO.                   
010900 77    WS-DNOT-IDKOLLI           PIC S9(05) VALUE ZERO COMP-3.            
011000 77    WS-DNOT-IDPURAD           PIC S9(05) VALUE ZERO COMP-3.            
011100                                                                          
011200 77    WS-SPAR-IDPURAD           PIC S9(5)  COMP-3.                       
011300 77    WS-KVPTID-MIN             PIC S9(3)      COMP-3 VALUE ZERO.        
011400 77    WS-SUPTID-PRAPP-MIN-TOT   PIC S9(7)      COMP-3 VALUE ZERO.        
011500 77    WS-TRAEFF-KOLLI           PIC X(1).                                
011600 77    WS-RADER-OK               PIC X(1)   VALUE 'N'.                    
011700 77    WS-RADER-SAKNAS           PIC X(1).                                
011800 77    WS-RADER-RAPPORTERADE     PIC X(1).                                
011900 77    SW-FEL-PRODNR             PIC X(1)   VALUE 'N'.                    
012000 77    SW-FEL-PACKARE            PIC X(1)   VALUE 'N'.                    
012100 77    SW-ORDER-AVSLUTAD         PIC X(1)   VALUE 'N'.                    
012110 77    SW-TIKLAR-JUSTERAD        PIC X(1)   VALUE 'N'.                    
012200*                                                                         
012500*      --- VALID IDDD CODES                                               
012600*                                                                         
012700*01    -COPY WWDC99                                                       
012800       EJECT                                                              
012900 01    DYNAMISKA-SUBPROGRAM.                                              
013000   03  CBLTDLI                   PIC X(8)   VALUE 'CBLTDLI '.             
013100   03  FELLOG                    PIC X(8)   VALUE 'FELLOG  '.             
013110   03  ABEND                     PIC X(8)   VALUE 'ABEND   '.             
013200   03  W005INIT                  PIC X(8)   VALUE 'W005INIT'.             
013300*                                                                         
013400                                                                          
013500 01  FILLER                      PIC X(16)  VALUE 'WMSGINIT '.            
013600*01 -COPY WMSGINIT                                                        
013700     SKIP2                                                                
013800                                                                          
013900 01  GEMENSAMMA-SUBPROGRAM.                                               
014000                                                                          
014100     03  W411DNOT               PIC X(8)    VALUE 'W411DNOT'.             
014200*                                                                         
014300*    --- AREOR TILL GEMENSAMMA SUBPROGRAM                                 
014400*                                                                         
014500 01  FILLER                     PIC X(16)   VALUE 'W411DNOT '.            
014600*   -COPY W411DNOT                                                        
014700     EJECT                                                                
014800                                                                          
014900 77    FL-SLINGA-KLAR            PIC X(1).                                
015000   88  SLINGA-KLAR                          VALUE 'J'.                    
015100 77    WS-IDTRANS                PIC X(04).                               
015200   88  WS-SAMMA-BILD                        VALUE '4316'.                 
015300   88  WS-GODKAND-BILD                      VALUE '4311' '4312'           
015400                                                  '4313' '4314'           
015500                                                  '4315' '4316'           
015600                                                  '4317' '4318'.          
015700     SKIP2                                                                
015800 77    WS-INDATA-TEST            PIC X(01).                               
015900   88  WS-INDATA-FEL                        VALUE 'F'.                    
016000   88  WS-INDATA-RATT                       VALUE 'R'.                    
016100     SKIP2                                                                
016200 77    WS-KDKOLSTA               PIC 9(01).                               
016300     SKIP2                                                                
016400 77    WS-BEHANDLING-TEST        PIC X(01).                               
016500   88  WS-BEHANDLING-FEL                    VALUE 'F'.                    
016600   88  WS-BEHANDLING-RATT                   VALUE 'R'.                    
016700     SKIP2                                                                
016800 77    FL-HEL-RAD                PIC X(01).                               
016900   88  HEL-RAD                              VALUE 'J'.                    
017000   88  DELAD-RAD                            VALUE 'N'.                    
017100     SKIP2                                                                
017200 77    FL-521-SEGMENT            PIC X(01).                               
017300   88  UTDELADE-RADER-FINNS                 VALUE 'J'.                    
017400   88  UTDELADE-RADER-SAKNAS                VALUE 'N'.                    
017500     SKIP2                                                                
017600 77    FL-RADSTA-BACKAD          PIC X(01)  VALUE 'N'.                    
017700     EJECT                                                                
017800 77    FL-RAD-SEGMENT            PIC X(01).                               
017900   88  KOLLI-RAD-FINNS                      VALUE 'J'.                    
018000   88  KOLLI-RAD-SLUT                       VALUE 'N'.                    
018100     SKIP2                                                                
018200 01     WS-IDRADNR-X                              PIC X(4).               
018300 01     WS-IDRADNR-NUM  REDEFINES WS-IDRADNR-X    PIC 9(4).               
018400     SKIP2                                                                
018500 01     WS-IDKUNDRF.                                                      
018600   03   WS-IDORDNR               PIC X(5)   VALUE SPACE.                  
018700   03   FILLER                   PIC X(5)   VALUE SPACE.                  
018800 01     WS-JFR-IDANSTNR.                                                  
018900   03   FILLER                   PIC X(3).                                
019000   03   WS-JFR-IDANSTNR-5        PIC X(5).                                
019100*                                                                         
019200 01     WS-SUPTID-PRAPP         PIC 9(3)V99.                              
019300 01     FILLER REDEFINES WS-SUPTID-PRAPP.                                 
019400   03   WS-SUPTID-TIM           PIC 9(3).                                 
019500   03   WS-SUPTID-MIN           PIC 9(2).                                 
019600*                                                                         
019700 01     FILLER                  PIC X(11)   VALUE 'ARBETSAREOR'.          
019800 01     ARBETSAREOR.                                                      
019900   03   ARB-AREA-RAD.                                                     
020000     05 ARB-RAD                 PIC  9(4)         VALUE ZERO.             
020100     05 ARB-KVLEVART            PIC  9(6)         VALUE ZERO.             
020200*                                                                         
020300 01 WS-WDE44-ORAD-IDPURAD       PIC  9(4)         VALUE ZERO.             
020400*                                                                         
020500 01     FILLER                  PIC X(10)   VALUE 'HJÄLPAREOR'.           
020600 01     HJALPAREOR.                                                       
020700   03   HJALP-ODEL-DARFS        PIC 9(12).                                
020800   03   FILLER                  REDEFINES HJALP-ODEL-DARFS.               
020900     05 FILLER                  PIC  9(2).                                
021000     05 HJALP-ODEL-DARFS-6      PIC  9(6).                                
021100     05 FILLER                  PIC  9(4).                                
021200   03   HJALP-4472-TIRFS        PIC 9(11).                                
021300   03   FILLER                  REDEFINES HJALP-4472-TIRFS.               
021400     05 FILLER                  PIC  9(1).                                
021500     05 HJALP-4472-TIRFS-6      PIC  9(6).                                
021600     05 FILLER                  PIC  9(4).                                
021700*                                                                         
021800   03   ARB-KOLLI-UPPG-AREA.                                              
021900     05 ARB-KOLLI-VKORDNTO       PIC  9(6)V9(1)    VALUE ZERO.            
022000     05 ARB-KOLLI-KVFLAMP        PIC  S9(2)V9(1)   VALUE ZERO.            
022100     05 ARB-KOLLI-KDFARLIG       PIC  S9           VALUE ZERO.            
022200     05 ARB-KOLLI-KVORDRAD       PIC  S9(5)        VALUE ZERO.            
022300     05 ARB-KOLLI-SUORDV         PIC  S9(9)V9(2)   VALUE ZERO.            
022400     05 ARB-KOLLI-SUORDV-LOC     PIC  S9(9)V9(2)   VALUE ZERO.            
022500     05 ARB-KOLLI-SUORDV-LOCPREL PIC  S9(9)V9(2)   VALUE ZERO.            
022600     SKIP2                                                                
022700 01     FILLER                  PIC X(10)   VALUE 'SPAR-AREOR'.           
022800 01     SPAR-AREA.                                                        
022900   03   SPAR-PRAD-UPPG-AREA.                                              
023000     05 SPAR-PRAD-VKARTNTO      PIC  9(6)V9(3)    VALUE ZERO.             
023100     05 SPAR-PRAD-KVFLAMP       PIC  S9(2)V9(1)   VALUE ZERO.             
023200     05 SPAR-PRAD-KDFARLIG      PIC  S9           VALUE ZERO.             
023300     05 SPAR-PRAD-KVLEVART      PIC  S9(7)        VALUE ZERO.             
023400     05 SPAR-PRAD-PRARTNTO      PIC  S9(9)V9(2)   VALUE ZERO.             
023500     05 SPAR-PRAD-PRARTNTO-LOC  PIC  S9(9)V9(2)   VALUE ZERO.             
023600     05 SPAR-PRAD-PRARTNTO-LOCPREL  PIC  S9(9)V9(2)   VALUE ZERO.         
023700     EJECT                                                                
023800 01     FILLER                  PIC X(12)   VALUE 'SPAR-FG-AREA'.         
023900 01     SPAR-FG-AREA.                                                     
024000   03   SPAR-ORAD-UPPG-AREA.                                              
024100     05 SPAR-ORAD-IDPSN         PIC  9(3)              VALUE 0.           
024200     05 SPAR-ORAD-VKART-FG      PIC  S9(7)      COMP-3 VALUE 0.           
024300     05 SPAR-ORAD-VLFG          PIC  S9(4)V9(3) COMP-3 VALUE 0.           
024400     05 SPAR-ORAD-SUEQFG        PIC  S9(3)V9(4) COMP-3 VALUE 0.           
024500*                                                                         
024600 01     TOTAL-SUEQFG            PIC  S9(3)V9(4) COMP-3 VALUE 0.           
024700     EJECT                                                                
024800 01     FILLER                  PIC X(16)   VALUE 'FG-TABELL'.            
024900 01     FARLIGT-GODS-TABELL.                                              
025000   03   TABELL-POST OCCURS 10.                                            
025100     05 TAB-IDPSN               PIC  9(3)              VALUE 0.           
025200     05 TAB-VKART-FG            PIC  S9(7)      COMP-3 VALUE 0.           
025300     05 TAB-VLFG                PIC  S9(4)V9(3) COMP-3 VALUE 0.           
025400     EJECT                                                                
025500*------- SWITCHAR                                                         
025600*                                                                         
025700 01     FILLER                  PIC X(16)   VALUE 'SW-SWITCHAR'.          
025800 01     SW-SWITCHAR.                                                      
025900*                                                                         
026000*       * BILDSIDA RÄCKER INTE TILL FÖR ALLA RADER                        
026100  03    SW-SIDA-OVERFULL     PIC X(1)    VALUE 'N'.                       
026200*       * KORD UPPFYLLER VILLKOR FÖR VISNING                              
026300  03    SW-VISA-KORD         PIC X(1)    VALUE 'N'.                       
026400*       * SLUT PÅ RADNR PÅ BILDEN                                         
026500  03    SW-SLUT-RADNR        PIC X(1)    VALUE 'N'.                       
026600*       * ORAD HAR KKOLLI-SEGM FÖR INMATAT KOLLI-NR                       
026700  03    SW-KKOLLI-FINNS      PIC X(1)    VALUE 'N'.                       
026800     EJECT                                                                
026900 01    TEST-IDDISTR              PIC  9(5)               COMP-3.          
027000     SKIP3                                                                
027100*01    FILLER -COPY WWDIST07     -RED TEST-IDDISTR.                       
027200                                                                          
027300*01    FILLER -COPY WWDIST19     -RED TEST-IDDISTR.                       
027400*    ----DISTR-DEALER-PRICE-----                                          
027500*01    FILLER -COPY WWDIST79     -RED TEST-IDDISTR.                       
027600     EJECT                                                                
027700 01    NYCKLAR-TILL-DLI.                                                  
027800*                                                                         
027900   03    W-WDE4A1-KUNDORDER-X.                                            
028000     05    W-4A1-IDDISTR         PIC S9(5)   VALUE ZERO  COMP-3.          
028100     05    W-4A1-IDKUNDNR        PIC S9(7)   VALUE ZERO  COMP-3.          
028200     05    W-4A1-IDKUNDRF.                                                
028300       07  W-4A1-IDORDNR         PIC  9(5)   VALUE ZERO.                  
028400       07  FILLER                PIC  X(5)   VALUE SPACE.                 
028500*                                                                         
028600   03    W-WDE401-KUNDORDER-X.                                            
028700     05    W-401-IDDISTR         PIC S9(5)   VALUE ZERO  COMP-3.          
028800     05    W-401-IDKUNDNR        PIC S9(7)   VALUE ZERO  COMP-3.          
028900     05    W-401-IDKUNDRF.                                                
029000       07  W-401-IDORDNR         PIC  9(5)   VALUE ZERO.                  
029100       07  FILLER                PIC  X(5)   VALUE SPACE.                 
029200     05    W-401-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
029300     05    W-401-IDPLKLST        PIC S9(3)   VALUE ZERO  COMP-3.          
029400*                                                                         
029500   03    W-WDE411-IDPURAD-X.                                              
029600     05    W-411-IDPURAD2        PIC S9(5)   VALUE ZERO  COMP-3.          
029700*                                                                         
029800   03    W-WDE4B-KEYSEQ-MIN-X.                                            
029900     05    W-411-IDPRODNR-MIN    PIC S9(7)   VALUE ZERO  COMP-3.          
030000     05    W-411-IDPURAD-MIN     PIC S9(5)   VALUE ZERO  COMP-3.          
030100*                                                                         
030200   03    W-WDE4F1KY-MAX-X.                                                
030300     05    W-IDPRODNR-WDE4F-MAX  PIC S9(7)   VALUE ZERO  COMP-3.          
030400     05    W-IDKOLLI-WDE4F-MAX   PIC S9(5)   VALUE ZERO  COMP-3.          
030500     05    FILLER                PIC X(22)   VALUE HIGH-VALUE.            
030600                                                                          
030700   03    W-WDE4F1KY-MIN-X.                                                
030800     05    W-IDPRODNR-WDE4F-MIN  PIC S9(7)   VALUE ZERO  COMP-3.          
030900     05    W-IDKOLLI-WDE4F-MIN   PIC S9(5)   VALUE ZERO  COMP-3.          
031000     05    FILLER                PIC X(22)   VALUE LOW-VALUE.             
031100                                                                          
031200     EJECT                                                                
031300   03    W-WDE4B-KEYSEQ-MAX-X.                                            
031400     05    W-411-IDPRODNR-MAX    PIC S9(7)   VALUE ZERO  COMP-3.          
031500     05    W-411-IDPURAD-MAX     PIC S9(5)   VALUE ZERO  COMP-3.          
031600*                                                                         
031700   03    W-WDE4FSEQ-X.                                                    
031800     05    W-IDPRODNR-F          PIC S9(7)   VALUE ZERO  COMP-3.          
031900     05    W-IDKOLLI-F           PIC S9(5)   VALUE ZERO  COMP-3.          
032000*                                                                         
032100   03    W-WDE4B-KEYSEQ-X.                                                
032200     05    W-411-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
032300     05    W-411-IDPURAD         PIC S9(5)   VALUE ZERO  COMP-3.          
032400*                                                                         
032500   03    W-WDE421-IDKOLLI-X.                                              
032600     05    W-421-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
032700     05    W-421-IDKOLLI         PIC S9(5)   VALUE ZERO  COMP-3.          
032800*                                                                         
032900   03    W-WDE601-IDPRODNR-X.                                             
033000     05    W-601-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
033100*                                                                         
033200   03    W-WDE611-IDKOLLI-X.                                              
033300     05    W-611-IDKOLLI         PIC S9(5)   VALUE ZERO  COMP-3.          
033400*                                                                         
033500   03    W-WDEE4F-IDPLKST-X.                                              
033600     05    W-E4F-IDPLKST         PIC S9(3)   VALUE ZERO  COMP-3.          
033700*                                                                         
033800*                                                                         
033900   03    W-WDGXKEY-4321-X.                                                
034000     05    W-IDHTYP-4321         PIC  9(4)   VALUE 4321.                  
034100     05    FILLER                PIC X(26)   VALUE LOW-VALUE.             
034200*                                                                         
034300   03    W-WDQ301-ORDERDEL-X.                                             
034400     05    W-301-IDORDER         PIC S9(7)   COMP-3.                      
034500     05    W-301-IDDC            PIC X(2).                                
034600     05    W-301-IDPRODNR        PIC S9(7)   COMP-3.                      
034700     05    W-301-IDPLKLST        PIC S9(3)   COMP-3.                      
034800*                                                                         
034900   03    W-WDGXKEY-4471-X.                                                
035000     05    W-IDHTYP-4471         PIC  9(4)   VALUE 4471.                  
035100     05    W-IDDC-4471           PIC  X(2).                               
035200     05    W-IDPRC.                                                       
035300       07  W-IDPRCBAS-4471       PIC  X(3).                               
035400       07  W-IDPRCVAR-4471       PIC  X(1).                               
035500     05    FILLER                PIC  X(20)  VALUE LOW-VALUE.             
035600*                                                                         
035700   03    W-KDSEGKEY-4472-X.                                               
035800     05    W-KDSEGKEY-4472       PIC  X(1)   VALUE '1'.                   
035900*                                                                         
036000   03    W-WDGXKEY-4477-X.                                                
036100     05    W-IDHTYP-4477         PIC  9(4)   VALUE 4477.                  
036200     05    W-IDDC-4477           PIC  X(2).                               
036300     05    FILLER                PIC  X(24)  VALUE LOW-VALUE.             
036400*                                                                         
036500   03    W-WDGXKEY-4478-X.                                                
036600     05    W-IDSHIFT-4478        PIC  X(1).                               
036700     05    W-IDUSER-4478         PIC  X(8).                               
036800     05    FILLER                PIC  X(1)   VALUE LOW-VALUE.             
036900*                                                                         
037000     03  W-4301-WDGXKEY-X.                                                
037100         05 W-4301-IDHTYP        PIC X(4)  VALUE '4301'.                  
037200         05 W-4301-IDPRODNR      PIC S9(7) VALUE ZERO COMP-3.             
037300         05 W-4301-NYCKEL-VALFRI PIC X(22) VALUE LOW-VALUE.               
037400                                                                          
037500     03  W-4302-WDGXKEY-X.                                                
037600         05 W-4302-IDKOLLI-X.                                             
037700            07 W-4302-IDKOLLI    PIC S9(5) VALUE ZERO COMP-3.             
037800         05 W-4302-IDPLKLST-X.                                            
037900            07 W-4302-IDPLKLST   PIC S9(3) VALUE ZERO COMP-3.             
037910                                                                          
037920   03    W-WDA601KY-MIN-X.                                                
037930     05    W-A601KY-MIN-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
037940     05    W-A601KY-MIN-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
037941     05    W-A601KY-MIN-IDORDNR      PIC 9(07) VALUE ZERO.                
037942     05    FILLER                    PIC X(03) VALUE SPACE.               
037960     05    W-A601KY-MIN-TIREGDAT     PIC S9(7) VALUE ZERO COMP-3.         
037970     05    W-A601KY-MIN-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
037980     05    W-A601KY-MIN-TIREGTID     PIC S9(9) VALUE ZERO COMP-3.         
037990     05    W-A601KY-MIN-TIREGDAT-AVV PIC S9(7) VALUE ZERO COMP-3.         
037991     05    W-A601KY-MIN-TIREGTID-AVV PIC S9(9) VALUE ZERO COMP-3.         
037992     SKIP2                                                                
037993   03    W-WDA601KY-MAX-X.                                                
037994     05    W-A601KY-MAX-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
037995     05    W-A601KY-MAX-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
037996     05    W-A601KY-MAX-IDORDNR      PIC 9(07) VALUE ZERO.                
037997     05    FILLER                    PIC X(03) VALUE SPACE.               
037999     05    W-A601KY-MAX-TIREGDAT     PIC S9(7) VALUE ZERO COMP-3.         
038000     05    W-A601KY-MAX-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
038001     05    W-A601KY-MAX-TIREGTID     PIC S9(9) VALUE ZERO COMP-3.         
038002     05    W-A601KY-MAX-TIREGDAT-AVV PIC S9(7) VALUE ZERO COMP-3.         
038003     05    W-A601KY-MAX-TIREGTID-AVV PIC S9(9) VALUE ZERO COMP-3.         
038010*                                                                         
038100     EJECT                                                                
038200 01  W-RAETT-1.                                                           
038300     03 RAETT-1-SVE              PIC X(21)                                
038400        VALUE 'KOLLIT UPPDATERAT    '.                                    
038500     03 RAETT-1-ENG              PIC X(21)                                
038600        VALUE 'CASE UPDATED         '.                                    
038700 01  FILLER REDEFINES W-RAETT-1.                                          
038800     03 RAETT-1  OCCURS 2        PIC X(21).                               
038900     SKIP3                                                                
039000 01  W-RAETT-2.                                                           
039100     03 RAETT-2-SVE              PIC X(21)                                
039200        VALUE 'KOLLIT BORTTAGET     '.                                    
039300     03 RAETT-2-ENG              PIC X(21)                                
039400        VALUE 'CASE DELETED         '.                                    
039500 01  FILLER REDEFINES W-RAETT-2.                                          
039600     03 RAETT-2  OCCURS 2        PIC X(21).                               
039700*                                                                         
039800 01  W-RAETT-3.                                                           
039900     03 RAETT-3-SVE              PIC X(21)                                
040000        VALUE 'PF11 FÖR UPPDATERING '.                                    
040100     03 RAETT-3-ENG              PIC X(21)                                
040200        VALUE 'PRESS PF11 TO UPDATE '.                                    
040300 01  FILLER REDEFINES W-RAETT-3.                                          
040400     03 RAETT-3  OCCURS 2        PIC X(21).                               
040500*                                                                         
040600 01  W-RAETT-31.                                                          
040700     03 RAETT-31-SVE              PIC X(21)                               
040800        VALUE 'PF11 FÖR UPPDATERING1'.                                    
040900     03 RAETT-31-ENG              PIC X(21)                               
041000        VALUE 'PRESS PF11 TO UPDATE '.                                    
041100 01  FILLER REDEFINES W-RAETT-31.                                         
041200     03 RAETT-31  OCCURS 2        PIC X(21).                              
041300*                                                                         
041400 01  W-RAETT-32.                                                          
041500     03 RAETT-32-SVE              PIC X(21)                               
041600        VALUE 'PF11 FÖR UPPDATERING2'.                                    
041700     03 RAETT-32-ENG              PIC X(21)                               
041800        VALUE 'PRESS PF11 TO UPDATE '.                                    
041900 01  FILLER REDEFINES W-RAETT-32.                                         
042000     03 RAETT-32  OCCURS 2        PIC X(21).                              
042100*                                                                         
042200 01  W-RAETT-33.                                                          
042300     03 RAETT-33-SVE              PIC X(21)                               
042400        VALUE 'PF11 FÖR UPPDATERING3'.                                    
042500     03 RAETT-33-ENG              PIC X(21)                               
042600        VALUE 'PRESS PF11 TO UPDATE '.                                    
042700 01  FILLER REDEFINES W-RAETT-33.                                         
042800     03 RAETT-33  OCCURS 2        PIC X(21).                              
042900*                                                                         
043000 01  W-RAETT-4.                                                           
043100     03 RAETT-4-SVE              PIC X(29)                                
043200        VALUE 'INGET ÅTERBOKNINGSMÄRKT      '.                            
043300     03 RAETT-4-ENG              PIC X(29)                                
043400        VALUE 'NOTHING MARKED FOR BACK OUT  '.                            
043500 01  FILLER REDEFINES W-RAETT-4.                                          
043600     03 RAETT-4  OCCURS 2        PIC X(29).                               
043700     SKIP3                                                                
043800 01    MEDDELANDE.                                                        
043900   03   UPPLYSN-1.                                                        
044000     05 FILLER                   PIC X(61)  VALUE                         
044100        'MATA IN RADINTERVALL                   '.                        
044200     05 FILLER                   PIC X(61)  VALUE                         
044300        'ENTER LINE INTERVAL                    '.                        
044400   03    FILLER REDEFINES UPPLYSN-1.                                      
044500     05  UPPL-1        OCCURS 2   PIC X(61).                              
044600*                                                                         
044700   03   UPPLYSN-2.                                                        
044800     05 FILLER                   PIC X(61)  VALUE                         
044900        'INGEN ÄNDRING UTFÖRD, FÖRSÖK IGEN      '.                        
045000     05 FILLER                   PIC X(61)  VALUE                         
045100        'NO CHANGE UPDATED, TRY AGAIN           '.                        
045200   03    FILLER REDEFINES UPPLYSN-2.                                      
045300     05  UPPL-2        OCCURS 2   PIC X(61).                              
045400     SKIP2                                                                
045500   03   UPPLYSN-3.                                                        
045600     05 FILLER                   PIC X(61)  VALUE                         
045700        'MER FINNS                              '.                        
045800     05 FILLER                   PIC X(61)  VALUE                         
045900        'MORE .... PRESS PF8                    '.                        
046000   03    FILLER REDEFINES UPPLYSN-3.                                      
046100     05  UPPL-3        OCCURS 2   PIC X(61).                              
046200     SKIP2                                                                
046300   03    FEL-1.                                                           
046400     05  FILLER                  PIC X(40)   VALUE                        
046500        '701 ORDERN SAKNAS                       '.                       
046600     05  FILLER                  PIC X(40)   VALUE                        
046700        '701 ORDER MISSING                       '.                       
046800   03    FILLER  REDEFINES  FEL-1.                                        
046900     05  FEL-701     OCCURS 2    PIC X(40).                               
047000     SKIP2                                                                
047100**********************************************                            
047200   03    FEL-41.                                                          
047300     05  FILLER                  PIC X(40)   VALUE                        
047400        '7011 ORDERN SAKNAS                      '.                       
047500     05  FILLER                  PIC X(40)   VALUE                        
047600        '7011 ORDER MISSING                      '.                       
047700   03    FILLER  REDEFINES  FEL-41.                                       
047800     05  FEL-7011    OCCURS 2    PIC X(40).                               
047900**                                                                        
048000   03    FEL-42.                                                          
048100     05  FILLER                  PIC X(40)   VALUE                        
048200        '7012 ORDERN SAKNAS                      '.                       
048300     05  FILLER                  PIC X(40)   VALUE                        
048400        '7012 ORDER MISSING                      '.                       
048500   03    FILLER  REDEFINES  FEL-42.                                       
048600     05  FEL-7012    OCCURS 2    PIC X(40).                               
048700**                                                                        
048800   03    FEL-43.                                                          
048900     05  FILLER                  PIC X(40)   VALUE                        
049000        '7013 ORDERN SAKNAS                      '.                       
049100     05  FILLER                  PIC X(40)   VALUE                        
049200        '7013 ORDER MISSING                      '.                       
049300   03    FILLER  REDEFINES  FEL-43.                                       
049400     05  FEL-7013    OCCURS 2    PIC X(40).                               
049500**                                                                        
049600   03    FEL-44.                                                          
049700     05  FILLER                  PIC X(40)   VALUE                        
049800        '7014 ORDERN SAKNAS                      '.                       
049900     05  FILLER                  PIC X(40)   VALUE                        
050000        '7014 ORDER MISSING                      '.                       
050100   03    FILLER  REDEFINES  FEL-44.                                       
050200     05  FEL-7014    OCCURS 2    PIC X(40).                               
050300**                                                                        
050400   03    FEL-45.                                                          
050500     05  FILLER                  PIC X(40)   VALUE                        
050600        '7015 ORDERN SAKNAS                      '.                       
050700     05  FILLER                  PIC X(40)   VALUE                        
050800        '7015 ORDER MISSING                      '.                       
050900   03    FILLER  REDEFINES  FEL-45.                                       
051000     05  FEL-7015    OCCURS 2    PIC X(40).                               
051100**********************************************                            
051200*                                                                         
051300   03    FEL-2.                                                           
051400     05  FILLER                  PIC X(40)   VALUE                        
051500        '716 ORDERN EJ DELAD                     '.                       
051600     05  FILLER                  PIC X(40)   VALUE                        
051700        '716 ORDER HAS NOT BEEN SPLIT            '.                       
051800   03    FILLER  REDEFINES  FEL-2.                                        
051900     05  FEL-716     OCCURS 2    PIC X(40).                               
052000*                                                                         
052100   03    FEL-3.                                                           
052200     05  FILLER                  PIC X(40)   VALUE                        
052300        '710 ORDERN FÄRDIGRAPPORTERAD            '.                       
052400     05  FILLER                  PIC X(40)   VALUE                        
052500        '710 ORDER TOTALLY REPORTED              '.                       
052600   03    FILLER  REDEFINES  FEL-3.                                        
052700     05  FEL-718     OCCURS 2    PIC X(40).                               
052800*                                                                         
052900   03    FEL-4.                                                           
053000     05  FILLER                  PIC X(40)   VALUE                        
053100        '719 ANGIVEN PACKARE SAKNAS PÅ ORDERN    '.                       
053200     05  FILLER                  PIC X(40)   VALUE                        
053300        '719 PACKER AND ORDER DO NOT MATCH       '.                       
053400   03    FILLER  REDEFINES  FEL-4.                                        
053500     05  FEL-719     OCCURS 2    PIC X(40).                               
053600*                                                                         
053700   03    FEL-5.                                                           
053800     05  FILLER                  PIC X(40)   VALUE                        
053900        '720 ANGIVEN PACKARES ORDERDEL REDAN KLAR'.                       
054000     05  FILLER                  PIC X(40)   VALUE                        
054100        '720 ORDER PART OF PACKER READY          '.                       
054200   03    FILLER  REDEFINES  FEL-5.                                        
054300     05  FEL-720     OCCURS 2    PIC X(40).                               
054400*                                                                         
054500   03    FEL-6.                                                           
054600     05  FILLER                  PIC X(40)   VALUE                        
054700        '722 INTERV. EL DELAR TILLHÖR EJ PACKAREN'.                       
054800     05  FILLER                  PIC X(40)   VALUE                        
054900        '722 INTERVAL DOES NOT BELONG TO PACKER  '.                       
055000   03    FILLER  REDEFINES  FEL-6.                                        
055100     05  FEL-722     OCCURS 2    PIC X(40).                               
055200*                                                                         
055300   03    FEL-7.                                                           
055400     05  FILLER                  PIC X(40)   VALUE                        
055500        '737 KOLLIT REDAN FAKTURERAT             '.                       
055600     05  FILLER                  PIC X(40)   VALUE                        
055700        '737 CASE IS ALREADY INVOICED.           '.                       
055800   03    FILLER  REDEFINES  FEL-7.                                        
055900     05  FEL-737     OCCURS 2    PIC X(40).                               
056000*                                                                         
056100   03    FEL-8.                                                           
056200     05  FILLER                  PIC X(40)   VALUE                        
056300        '741 ANTAL FÅR EJ ÖKAS                   '.                       
056400     05  FILLER                  PIC X(40)   VALUE                        
056500        '741 INCREASE OF QUANTITY IS NOT ALLOWED.'.                       
056600   03    FILLER  REDEFINES  FEL-8.                                        
056700     05  FEL-741     OCCURS 2    PIC X(40).                               
056710*                                                                         
056720   03    FEL-9.                                                           
056730     05  FILLER                  PIC X(40)   VALUE                        
056740        '    KOLLIT TILLHÖR ETT SAMLINGSKOLLI    '.                       
056750     05  FILLER                  PIC X(40)   VALUE                        
056760        '    CASE IS PART OF CONSOLIDATED CASE   '.                       
056770   03    FILLER  REDEFINES  FEL-9.                                        
056780     05  FEL-7XX     OCCURS 2    PIC X(40).                               
056800*                                                                         
056900   03    FEL-10.                                                          
057000     05  FILLER                  PIC X(40)   VALUE                        
057100        '742 KOLLIT FÅR EJ TÖMMAS HELT           '.                       
057200     05  FILLER                  PIC X(40)   VALUE                        
057300        '742 CASE MAT NOT BE COMPLETELY EMPTY    '.                       
057400   03    FILLER  REDEFINES  FEL-10.                                       
057500     05  FEL-742     OCCURS 2    PIC X(40).                               
057600*                                                                         
057700   03    FEL-11.                                                          
057800     05  FILLER                  PIC X(40)   VALUE                        
057900        '748 UPPLYSTA FÄLT FELAKTIGA             '.                       
058000     05  FILLER                  PIC X(40)   VALUE                        
058100        '748 HIGHLIGHT FIELDS WRONG              '.                       
058200   03    FILLER  REDEFINES  FEL-11.                                       
058300     05  FEL-748     OCCURS 2    PIC X(40).                               
058400     SKIP2                                                                
058500   03    FEL-12.                                                          
058600     05  FILLER                  PIC X(40)   VALUE                        
058700        '758 KOLLI SAKNAS                        '.                       
058800     05  FILLER                  PIC X(40)   VALUE                        
058900        '758 CASE MISSING                        '.                       
059000   03    FILLER  REDEFINES  FEL-12.                                       
059100     05  FEL-758     OCCURS 2    PIC X(40).                               
059200     SKIP2                                                                
059300   03    FEL-122.                                                         
059400     05  FILLER                  PIC X(40)   VALUE                        
059500        '7582 KOLLI SAKNAS                       '.                       
059600     05  FILLER                  PIC X(40)   VALUE                        
059700        '7582 CASE MISSING                       '.                       
059800   03    FILLER  REDEFINES  FEL-122.                                      
059900     05  FEL-7582    OCCURS 2    PIC X(40).                               
060000     SKIP2                                                                
060100   03    FEL-123.                                                         
060200     05  FILLER                  PIC X(40)   VALUE                        
060300        '7583 KOLLI SAKNAS                       '.                       
060400     05  FILLER                  PIC X(40)   VALUE                        
060500        '7583 CASE MISSING                       '.                       
060600   03    FILLER  REDEFINES  FEL-123.                                      
060700     05  FEL-7583    OCCURS 2    PIC X(40).                               
060800     SKIP2                                                                
060900   03    FEL-124.                                                         
061000     05  FILLER                  PIC X(40)   VALUE                        
061100        '7584 KOLLI SAKNAS                       '.                       
061200     05  FILLER                  PIC X(40)   VALUE                        
061300        '7584 CASE MISSING                       '.                       
061400   03    FILLER  REDEFINES  FEL-124.                                      
061500     05  FEL-7584    OCCURS 2    PIC X(40).                               
061600     SKIP2                                                                
061700   03    FEL-13.                                                          
061800     05  FILLER                  PIC X(40)   VALUE                        
061900        '787 EJ NYA NYCKLAR OCH PF11             '.                       
062000     05  FILLER                  PIC X(40)   VALUE                        
062100        '787 NEW KEYS AND PF11 NOT ALLOWED       '.                       
062200   03    FILLER  REDEFINES  FEL-13.                                       
062300     05  FEL-787     OCCURS 2    PIC X(40).                               
062400     SKIP2                                                                
062500   03    FEL-14.                                                          
062600     05  FILLER                  PIC X(40)   VALUE                        
062700        '804 AVVIKELSERAPPORT PÅGÅR              '.                       
062800     05  FILLER                  PIC X(40)   VALUE                        
062900        '804 DEVIATION CONTROL IN PROGRESS       '.                       
063000   03    FILLER  REDEFINES  FEL-14.                                       
063100     05  FEL-804     OCCURS 2    PIC X(40).                               
063200     SKIP2                                                                
063300   03    FEL-15.                                                          
063400     05  FILLER                  PIC X(40)   VALUE                        
063500        '805 FEL DC ANGIVET.                     '.                       
063600     05  FILLER                  PIC X(40)   VALUE                        
063700        '805 WRONG DC NOTED.                     '.                       
063800   03    FILLER  REDEFINES  FEL-15.                                       
063900     05  FEL-805     OCCURS 2    PIC X(40).                               
064000     SKIP2                                                                
064100   03    FEL-16.                                                          
064200     05  FILLER                  PIC X(40)   VALUE                        
064300        '806 FEL PACKARE.                        '.                       
064400     05  FILLER                  PIC X(40)   VALUE                        
064500        '806 WRONG PICKERID.                     '.                       
064600   03    FILLER  REDEFINES  FEL-16.                                       
064700     05  FEL-806     OCCURS 2    PIC X(40).                               
064800     SKIP2                                                                
064900   03    FEL-17.                                                          
065000     05  FILLER                  PIC X(40)   VALUE                        
065100        '807 FEL PRODNR.                         '.                       
065200     05  FILLER                  PIC X(40)   VALUE                        
065300        '807 WRONG PRODNO.                       '.                       
065400   03    FILLER  REDEFINES  FEL-17.                                       
065500     05  FEL-807     OCCURS 2    PIC X(40).                               
065600     SKIP2                                                                
065700   03    FEL-18.                                                          
065800     05  FILLER                  PIC X(40)   VALUE                        
065900        '808 ORDERN ÄR HELT AVSLUTAD.            '.                       
066000     05  FILLER                  PIC X(40)   VALUE                        
066100        '808 THE ORDER IS COMPLETY FINISHED.     '.                       
066200   03    FILLER  REDEFINES  FEL-18.                                       
066300     05  FEL-808     OCCURS 2    PIC X(40).                               
066400     EJECT                                                                
066500   03    FEL-19.                                                          
066600     05  FILLER                  PIC X(40)   VALUE                        
066700        '809 RAD SAKNAS.                         '.                       
066800     05  FILLER                  PIC X(40)   VALUE                        
066900        '809 LINE IS MISSING.                    '.                       
067000   03    FILLER  REDEFINES  FEL-19.                                       
067100     05  FEL-809     OCCURS 2    PIC X(40).                               
067200     EJECT                                                                
067300******************************************************************        
067400*                                                                *        
067500*                AREOR FÖR MFS OCH SKÄRMHANTERING                *        
067600*                                                                *        
067700******************************************************************        
067800 01    FILLER                 PIC X(16) VALUE 'MID W4I31601 MID'.         
067900     SKIP3                                                                
068000*01    MID -COPY W4I31601.                                                
068100     EJECT                                                                
068200*01    -COPY WMSGAREA                                                     
068300     EJECT                                                                
068400*  03    MOD -COPY W4O31601  -RED MSG-AREA.                               
068500     EJECT                                                                
068600 01    FILLER                    PIC X(16)   VALUE 'ALT-IO-AREA'.         
068700 01    ALT-IO-AREA.                                                       
068800   03    ALT-LL                  PIC S9(4)   COMP  SYNC.                  
068900   03    ALT-Z1                  PIC X(1).                                
069000   03    ALT-Z2                  PIC X(1).                                
069100   03    ALT-TRANSKOD            PIC X(6)    VALUE SPACE.                 
069200   03    FILLER                  PIC X(2)    VALUE SPACE.                 
069300   03    FILLER                  PIC X(4)    VALUE '4316'.                
069400   03    ALT-KDMFSFOR            PIC X(1)    VALUE SPACE.                 
069500   03    ALT-AREA                PIC X(100)  VALUE SPACE.                 
069600     SKIP3                                                                
069700     EJECT                                                                
069800 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
069900     SKIP3                                                                
070000*01    -COPY WMFSAREA                                                     
070100     EJECT                                                                
070200******************************************************************        
070300*                                                                         
070400*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
070500*                                                                         
070600 01    IMS-WS.                                                            
070700   03    FILLER                  PIC X(16)   VALUE 'IMS-WS*****'.         
070800     SKIP3                                                                
070900*                        **** STATUS-KOD FRÅN IMS                         
071000   03    STATUS-RAD-WS           PIC XX.                                  
071100     88    RAD-FINNS                         VALUE '  '.                  
071200     88    RAD-SAKNAS                        VALUE 'GE'.                  
071300   03    STATUS-KUNDORDER-SEK-WS PIC XX.                                  
071400     88    KUNDORDER-SEK-FINNS               VALUE '  '.                  
071500     88    KUNDORDER-SEK-SAKNAS              VALUE 'GE' 'GB'.             
071600   03    STATUS-WS               PIC XX.                                  
071700     88    SEGMENT-FINNS                     VALUE '  '.                  
071800     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
071810     88    BASEN-SLUT                        VALUE 'GB'.                  
071900     88    SEGMENT-FINNS-REDAN               VALUE 'II'.                  
072000     SKIP3                                                                
072100   03    GODK-STATUSKODER.                                                
072200     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
072300     SKIP3                                                                
072400 01    SSA1                      PIC X(128).                              
072500 01    SSA2                      PIC X(128).                              
072600 01    SSA3                      PIC X(80).                               
072700 01    SSA4                      PIC X(80).                               
072800     EJECT                                                                
072900*                            IMS FUNKTIONSKODER                           
073000*01    -COPY W0003                                                        
073100     EJECT                                                                
073200*                            DLI INPUT-OUTPUT AREA                        
073300 01    FILLER                PIC X(16)  VALUE 'IO-E401'.                  
073400 01    DLI-IO-E401.                                                       
073500*  03    WDE401 -COPY WDE401                                              
073600     EJECT                                                                
073700 01    FILLER                PIC X(16)  VALUE 'IO-E411'.                  
073800 01    DLI-IO-E411.                                                       
073900*  03    WDE411 -COPY WDE411                                              
074000     EJECT                                                                
074100 01    FILLER                PIC X(16)  VALUE 'IO-E421'.                  
074200 01    DLI-IO-E421.                                                       
074300*  03    WDE421 -COPY WDE421                                              
074400     EJECT                                                                
074500 01    DLI-IO-AREA3.                                                      
074600   03    IO-AREA3                PIC X(512)  VALUE SPACE.                 
074700*  03    WDE601 -COPY WDE601             -RED IO-AREA3.                   
074800     EJECT                                                                
074900*  03    WDE611 -COPY WDE611             -RED IO-AREA3.                   
075000     EJECT                                                                
075100*  03    WDE411 -COPY WDE411 -PRE KOPPL- -RED IO-AREA3.                   
075200     EJECT                                                                
075300 01    DLI-IO-AREA4.                                                      
075400   03    IO-AREA4                PIC X(100)  VALUE SPACE.                 
075500*  03    WDGX01   -COPY WDGX01             -RED IO-AREA4.                 
075600     EJECT                                                                
075700*  03    WDGX4322 -COPY WDGX4322           -RED IO-AREA4.                 
075800     EJECT                                                                
075900 01    DLI-IO-AREA5.                                                      
076000   03    IO-AREA5                PIC X(256)  VALUE SPACE.                 
076100*  03    WLORQA01 -COPY WDQ301             -RED IO-AREA5.                 
076200     EJECT                                                                
076300 01    DLI-IO-AREA6.                                                      
076400   03    IO-AREA6                PIC X(1000) VALUE SPACE.                 
076500*  03    WDGX4472 -COPY WDGX4472           -RED IO-AREA6.                 
076600     EJECT                                                                
076700*  03    WDGX4478 -COPY WDGX4478           -RED IO-AREA6.                 
076800     EJECT                                                                
076900 01    FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA8'.        
077000 01    DLI-IO-AREA8.                                                      
077100   03    WDE411 -COPY WDE411   -PRE WDE43-                                
077200     EJECT                                                                
077300 01    FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA9'.        
077400 01    DLI-IO-AREA9.                                                      
077500   03    IO-AREA9                PIC X(512)  VALUE SPACE.                 
077600*  03    WDE601 -COPY WDE601   -PRE WDE6-   -RED IO-AREA9.                
077700     EJECT                                                                
077800*  03    WDE611 -COPY WDE611   -PRE WDE6-   -RED IO-AREA9.                
077900     EJECT                                                                
078000 01    FILLER                    PIC X(16)   VALUE 'DLI-IO-E421'.         
078100 01    DLI-IO-WDE411-21.                                                  
078200     SKIP2                                                                
078300   03    WDE411 -COPY WDE411   -PRE WDE44-.                               
078400   03    WDE421 -COPY WDE421   -PRE WDE44-.                               
078500                                                                          
078501                                                                          
078510 01  FILLER                      PIC X(16)   VALUE 'A601-AREA'.           
078520 01  DLI-IO-AREA-WDA6.                                                    
078530*  03    -COPY WDA601                                                     
078540                                                                          
078550                                                                          
078560                                                                          
078600 LINKAGE SECTION.                                                         
078700*01    -COPY W0009     -PRE MSG-                                          
078800     EJECT                                                                
078900*01    -COPY W0009     -PRE ALT-                                          
079000     EJECT                                                                
079100*01    -COPY W0008     -PRE WDP7-                                         
079200     05  FILLER                  PIC X.                                   
079300     EJECT                                                                
079400*01    -COPY W0008     -PRE WDE41-                                        
079500     05  FILLER                  PIC X.                                   
079600     EJECT                                                                
079700*01    -COPY W0008     -PRE WDE4A-                                        
079800     05  FILLER                  PIC X.                                   
079900     EJECT                                                                
080000*01    -COPY W0008     -PRE WDE64-                                        
080100     05  FILLER                  PIC X.                                   
080200     EJECT                                                                
080300*01    -COPY W0008     -PRE WDE4-                                         
080400     05  FILLER                  PIC X.                                   
080500     EJECT                                                                
080600*01    -COPY W0008     -PRE WDE42-                                        
080700     05  FILLER                  PIC X.                                   
080800     EJECT                                                                
080900*01    -COPY W0008     -PRE WDE6-                                         
081000     05  FILLER                  PIC X.                                   
081100     EJECT                                                                
081200*01    -COPY W0008     -PRE XXJK-                                         
081300     05  FILLER                  PIC X.                                   
081400     EJECT                                                                
081500*01    -COPY W0008     -PRE ORQA-                                         
081600     05  FILLER                  PIC X.                                   
081700     EJECT                                                                
081800*01    -COPY W0008     -PRE XXKW-                                         
081900     05  FILLER                  PIC X.                                   
082000     EJECT                                                                
082100*01    -COPY W0008     -PRE XXLB-                                         
082200     05  FILLER                  PIC X.                                   
082300     EJECT                                                                
082400*01    -COPY W0008     -PRE WDE44-                                        
082500     05  FILLER                  PIC X.                                   
082600     EJECT                                                                
082700*01    -COPY W0008     -PRE WDE4F-                                        
082800     05  FILLER                  PIC X.                                   
082900     EJECT                                                                
083000*01    -COPY W0008     -PRE WDE43-                                        
083100     05  FILLER                  PIC X.                                   
083200     EJECT                                                                
083300*01    -COPY W0008     -PRE WDE62-                                        
083400     05  FILLER                  PIC X.                                   
083500     EJECT                                                                
083600*01    -COPY W0008     -PRE XXDU-                                         
083700     05  FILLER                  PIC X.                                   
083800     EJECT                                                                
083810*01  -COPY W0008      -PRE WDA6B-                                         
083820     05  FILLER                  PIC X.                                   
083830     EJECT                                                                
083900 01  DNOT-ORQP-PCB               PIC X.                                   
084000 01  DNOT-ORQP2-PCB              PIC X.                                   
084100 01  DNOT-ORQP3-PCB              PIC X.                                   
084200 01  DNOT-4013-PCB               PIC X.                                   
084300 01  DNOT-BENA-PCB               PIC X.                                   
084400     EJECT                                                                
084500 PROCEDURE DIVISION USING  MSG-PCB ALT-PCB WDP7-PCB WDE41-PCB             
084600      WDE4A-PCB WDE64-PCB WDE4-PCB WDE42-PCB WDE6-PCB XXJK-PCB            
084700      ORQA-PCB XXKW-PCB XXLB-PCB WDE44-PCB WDE4F-PCB                      
084800      WDE43-PCB WDE62-PCB XXDU-PCB WDA6B-PCB                              
084900      DNOT-ORQP-PCB                                                       
085000      DNOT-ORQP2-PCB                                                      
085100      DNOT-ORQP3-PCB                                                      
085200      DNOT-4013-PCB                                                       
085300      DNOT-BENA-PCB.                                                      
085400                                                                          
085500     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB WDP7-PCB WDE41-PCB             
085600      WDE4A-PCB WDE64-PCB WDE4-PCB WDE42-PCB WDE6-PCB XXJK-PCB            
085700      ORQA-PCB XXKW-PCB XXLB-PCB WDE44-PCB WDE4F-PCB                      
085800      WDE43-PCB WDE62-PCB XXDU-PCB WDA6B-PCB                              
085900      DNOT-ORQP-PCB                                                       
086000      DNOT-ORQP2-PCB                                                      
086100      DNOT-ORQP3-PCB                                                      
086200      DNOT-4013-PCB                                                       
086300      DNOT-BENA-PCB.                                                      
086400                                                                          
086500     PERFORM IMS-GET-MSG                                                  
086600     IF SEGMENT-FINNS                                                     
086700         PERFORM A-INIT                                                   
086800         IF MFS-UPDATE                                                    
086900             IF MID-IDANSTNR-IN NOT = ALL '+'                             
087000               OR MID-IDPRODNR-IN NOT = ALL '+'                           
087100               OR MID-IDDISTR-IN NOT = ALL '+'                            
087200               OR MID-IDKUNDNR-IN NOT = ALL '+'                           
087300               OR MID-IDORDNR-IN NOT = ALL '+'                            
087400               OR MID-IDKOLLI-IN NOT = ALL '+'                            
087500                 MOVE ' '            TO  MFS-IDPFK                        
087600                 MOVE FEL            TO  WS-INDATA-TEST                   
087700                 MOVE SPACE          TO  MFS-KDTRTYP                      
087800                 MOVE FEL-787 (INDX) TO  MOD-TEMFSFEL                     
087900             END-IF                                                       
088000         END-IF                                                           
088100     SKIP2                                                                
088200*                                                                         
088300         IF WS-GODKAND-BILD                                               
088400             PERFORM B-GENERELL-KONTROLL                                  
088500                                                                          
088600             IF WS-INDATA-RATT                                            
088700                 PERFORM C-RELATIONSKONTROLL                              
088800                                                                          
088900                 IF WS-INDATA-RATT                                        
089000                                                                          
089100                     IF MFS-UPDATE                                        
089200                        IF KOLLI-KVORDRAD NOT = MID-KVORDRAD-SPAR         
089300                             MOVE FEL TO WS-INDATA-TEST                   
089400                             MOVE '7' TO MFS-IDPFK                        
089500                             MOVE UPPL-2 (INDX) TO MOD-TEMFSINF           
089600                                                                          
089700                         ELSE                                             
089800                             MOVE ' ' TO MFS-IDPFK                        
089900                             MOVE +1 TO RAD-INX                           
090000                             PERFORM UNTIL RAD-INX NOT <                  
090100                                     MAX-ANTAL-RADER-PLUS-1               
090200                                 IF MID-FLBACKA (RAD-INX) = JA            
090300                                 OR MID-FLBACKA (RAD-INX) = YES           
090400                                 OR MID-FLBACKA-ALLA      = JA            
090500                                 OR MID-FLBACKA-ALLA      = YES           
090600                                     PERFORM E-BEH-ATERBOKNING            
090700                                 END-IF                                   
090800                                 ADD +1 TO RAD-INX                        
090900                             END-PERFORM                                  
091000                                                                          
091100                             IF WS-BEHANDLING-RATT                        
091200                                 PERFORM F-UPPDATERA-KOLLIREG             
091300                             END-IF                                       
091400                                                                          
091500                             IF  MID-IDRADNR (1) NUMERIC                  
091600                                 MOVE MID-IDRADNR (1) TO                  
091700                                      WS-IDRADNR-START                    
091800                             ELSE                                         
091900                                 MOVE ZERO TO                             
092000                                      WS-IDRADNR-START                    
092100                             END-IF                                       
092200                        END-IF                                            
092300                     ELSE                                                 
092800                         MOVE RAETT-31 (INDX) TO MOD-TEMFSINF             
093000                     END-IF                                               
093100                                                                          
093200                     IF MFS-IDPFK = '7'                                   
093300                         MOVE ZERO  TO WS-IDRADNR-START                   
093400                         PERFORM D-LAGG-UT-RADER                          
093500                     END-IF                                               
093600                                                                          
093700                     IF  (MFS-IDPFK = ' ' OR '8')                         
093800                                                                          
093900                         IF  KOLLI-BORTTAGET = NEJ                        
094000                             IF  NY-NYCKEL = JA                           
094100                                 MOVE ZERO TO WS-IDRADNR-START            
094200                             END-IF                                       
094300                             PERFORM D-LAGG-UT-RADER                      
094400                         ELSE                                             
094500                             PERFORM S02-RENSA-FALT                       
094600                         END-IF                                           
094700                     END-IF                                               
094800                 END-IF                                                   
094900             END-IF                                                       
095000             MOVE MAX-MOD-LAENGD TO  MSG-KVLL                             
095100         ELSE                                                             
095200             MOVE '4316'                 TO MFS-IDTRANS                   
095300             MOVE 'W4O316N1'             TO MFS-IDMOD                     
095400             MOVE +8                     TO MSG-KVLL                      
095500             MOVE FEL                    TO WS-INDATA-TEST                
095600         END-IF                                                           
095700                                                                          
095800         IF WS-SAMMA-BILD                                                 
095900           CONTINUE                                                       
096000         ELSE                                                             
096100             MOVE '4316'                 TO MFS-IDTRANS                   
096200         END-IF                                                           
096300                                                                          
096400         IF WS-INDATA-RATT                                                
096500             PERFORM H-AVSLUT                                             
096600         END-IF                                                           
096700                                                                          
096800         IF MFS-IDTRANS = '4316'                                          
096900             PERFORM IMS-INSERT-MSG                                       
097000         END-IF                                                           
097100     END-IF                                                               
097200     MOVE ZERO TO RETURN-CODE                                             
097300     GOBACK                                                               
097400     .                                                                    
097500     EJECT                                                                
097600 A-INIT             SECTION.                                              
097700                                                                          
097800     IF MSG-DUBBLA-TRANSKODER                                             
097900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO   MID-W4I31601               
098000       MOVE MSG-IDTRANS-2                 TO   MFS-IDTRANS                
098100                                               WS-IDTRANS                 
098200       MOVE MSG-KDMFSFOR-2                TO   MFS-KDMFSFOR               
098300                                               WS-KDMFSFOR                
098400       MOVE MSG-KDTRTYP                   TO   MFS-KDTRTYP                
098500       MOVE MSG-IDPFK                     TO   MFS-IDPFK                  
098600     ELSE                                                                 
098700       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO   MID-W4I31601               
098800       MOVE MSG-IDTRANS-1                 TO   MFS-IDTRANS                
098900                                               WS-IDTRANS                 
099000       MOVE MSG-KDMFSFOR-1                TO   MFS-KDMFSFOR               
099100                                               WS-KDMFSFOR                
099200       MOVE ' '                           TO   MFS-KDTRTYP                
099300     END-IF                                                               
099400*                                                                         
099500     MOVE LOW-VALUE                       TO   MSG-AREA                   
099600     MOVE 'W4O316N1'                      TO   MFS-IDMOD                  
099700     MOVE '4316'                          TO   MOD-IDTRANS                
099800*                                                                         
099900     MOVE ALL '+'                         TO MSGI-WMSGINIT                
100000     MOVE '013'                           TO MSGI-KDCALL                  
100100     MOVE MSG-SIGNON-USERID               TO MSGI-IDUSER                  
100200                                             MSGI-IDLTERM-USER            
100300     MOVE '4316'                          TO MSGI-IDTRANS                 
100400     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
100500*                                                                         
100600     IF MSGI-IDLAND-SPR = 'GB'                                            
100700       MOVE +2 TO INDX                                                    
100800     ELSE                                                                 
100900       MOVE +1 TO INDX                                                    
101000     END-IF                                                               
101100                                                                          
101200     MOVE RAETT                           TO WS-INDATA-TEST               
101300                                             WS-BEHANDLING-TEST           
101400     MOVE ZERO                            TO WS-IDRADNR-START             
101500                                             ARB-AREA-RAD                 
101600     MOVE 'N'                             TO SW-FEL-PRODNR                
101700                                             SW-FEL-PACKARE               
101800                                             SW-ORDER-AVSLUTAD            
101900                                                                          
102000     PERFORM AA-FLYTTA-NYCKLAR                                            
102100     MOVE MFS-RENSA-FAELT             TO   MOD-TEMFSFEL                   
102200                                           MOD-IDANSTNR-IN                
102300                                           MOD-IDDISTR-IN                 
102400                                           MOD-IDKUNDNR-IN                
102500                                           MOD-IDORDNR-IN                 
102600                                           MOD-IDKOLLI-IN                 
102700                                           MOD-IDPRODNR-IN                
102800                                           MOD-TEMFSINF                   
102900     MOVE MFS-ROER-EJ-FAELT           TO   MOD-IDRADNR-START              
103000                                           MOD-FLBACKA-ALLA               
103100                                           MOD-IDRADNR-SPAR               
103200                                           MOD-KVORDRAD-SPAR              
103300     MOVE MFS-NUM-FAELT-RAETT         TO   MOD-IDRADNR-START-ATTR         
103400     MOVE MFS-ALFA-FAELT-RAETT        TO   MOD-FLBACKA-ALLA-ATTR          
103500                                                                          
103600     MOVE 1                           TO   RAD-INX                        
103700     PERFORM UNTIL RAD-INX NOT < MAX-ANTAL-RADER-PLUS-1                   
103800          MOVE MFS-ROER-EJ-FAELT    TO MOD-IDRADNR  (RAD-INX)             
103900                                       MOD-KVLEVART (RAD-INX)             
104000                                       MOD-FLBACKA  (RAD-INX)             
104100          MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLBACKA-ATTR (RAD-INX)         
104200          ADD +1                    TO RAD-INX                            
104300     END-PERFORM                                                          
104400     .                                                                    
104500     EJECT                                                                
104600 AA-FLYTTA-NYCKLAR  SECTION.                                              
104700                                                                          
104800     MOVE NEJ                             TO NY-NYCKEL                    
104900     IF MID-IDANSTNR-IN = ALL '+'                                         
105000         MOVE MID-IDANSTNR-UT             TO   WS-IDANSTNR                
105100         INSPECT WS-IDANSTNR REPLACING LEADING SPACE BY ZERO              
105200     ELSE                                                                 
105300         MOVE MID-IDANSTNR-IN             TO   WS-IDANSTNR                
105400         MOVE JA                          TO NY-NYCKEL                    
105500     END-IF                                                               
105600                                                                          
105700     IF MID-IDPRODNR-IN = ALL '+'                                         
105800         MOVE MID-IDPRODNR-UT             TO   WS-IDPRODNR                
105900         INSPECT WS-IDPRODNR REPLACING LEADING SPACE BY ZERO              
106000     ELSE                                                                 
106100         MOVE MID-IDPRODNR-IN             TO   WS-IDPRODNR                
106200         MOVE JA                          TO NY-NYCKEL                    
106300     END-IF                                                               
106400                                                                          
106500     IF MID-IDDISTR-IN = ALL '+'                                          
106600         MOVE MID-IDDISTR-UT              TO   WS-IDDISTR                 
106700         INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO               
106800     ELSE                                                                 
106900         MOVE MID-IDDISTR-IN              TO   WS-IDDISTR                 
107000         MOVE JA                          TO NY-NYCKEL                    
107100     END-IF                                                               
107200                                                                          
107300     IF MID-IDKUNDNR-IN = ALL '+'                                         
107400         MOVE MID-IDKUNDNR-UT             TO   WS-IDKUNDNR                
107500         INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO              
107600     ELSE                                                                 
107700         MOVE MID-IDKUNDNR-IN             TO   WS-IDKUNDNR                
107800         MOVE JA                          TO NY-NYCKEL                    
107900     END-IF                                                               
108000                                                                          
108100     IF MID-IDORDNR-IN = ALL '+'                                          
108200         MOVE MID-IDORDNR-UT              TO   WS-IDORDNR                 
108300         INSPECT WS-IDORDNR REPLACING LEADING SPACE BY ZERO               
108400     ELSE                                                                 
108500         MOVE MID-IDORDNR-IN              TO   WS-IDORDNR                 
108600         MOVE JA                          TO NY-NYCKEL                    
108700     END-IF                                                               
108800                                                                          
108900     IF MID-IDKOLLI-IN = ALL '+'                                          
109000         MOVE MID-IDKOLLI-UT              TO   WS-IDKOLLI                 
109100         INSPECT WS-IDKOLLI REPLACING LEADING SPACE BY ZERO               
109200     ELSE                                                                 
109300         MOVE MID-IDKOLLI-IN              TO   WS-IDKOLLI                 
109400         MOVE JA                          TO NY-NYCKEL                    
109500     END-IF                                                               
109600                                                                          
109700     MOVE MSGI-IDDC                       TO WS-IDDC                      
109800                                                                          
109900     MOVE WS-IDANSTNR                     TO   MOD-IDANSTNR-UT            
110000     INSPECT MOD-IDANSTNR-UT REPLACING LEADING ZERO BY SPACE              
110100     MOVE WS-IDDISTR                      TO   MOD-IDDISTR-UT             
110200     INSPECT MOD-IDDISTR-UT  REPLACING LEADING ZERO BY SPACE              
110300     MOVE WS-IDKUNDNR                     TO   MOD-IDKUNDNR-UT            
110400     INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE              
110500     MOVE WS-IDORDNR                      TO   MOD-IDORDNR-UT             
110600     INSPECT MOD-IDORDNR-UT  REPLACING LEADING ZERO BY SPACE              
110700     MOVE WS-IDKOLLI                      TO   MOD-IDKOLLI-UT             
110800     INSPECT MOD-IDKOLLI-UT  REPLACING LEADING ZERO BY SPACE              
110900     MOVE WS-IDPRODNR                     TO   MOD-IDPRODNR-UT            
111000     INSPECT MOD-IDPRODNR-UT REPLACING LEADING ZERO BY SPACE              
111100     MOVE WS-IDDC                         TO   MOD-IDDC-UT                
111200     .                                                                    
111300     EJECT                                                                
111400 B-GENERELL-KONTROLL  SECTION.                                            
111500     SKIP3                                                                
111600     IF WS-IDANSTNR NOT NUMERIC                                           
111700         MOVE FEL                       TO   WS-INDATA-TEST               
111800         MOVE FEL-748 (INDX)            TO   MOD-TEMFSFEL                 
111900     END-IF                                                               
112000     SKIP2                                                                
112100     IF WS-IDDISTR  NOT NUMERIC                                           
112200         MOVE FEL                       TO   WS-INDATA-TEST               
112300         MOVE FEL-748 (INDX)            TO   MOD-TEMFSFEL                 
112400     ELSE                                                                 
112500         MOVE WS-IDDISTR                TO   W-401-IDDISTR                
112600                                             W-4A1-IDDISTR                
112700     END-IF                                                               
112800     SKIP2                                                                
112900     IF WS-IDKUNDNR NOT NUMERIC                                           
113000         MOVE FEL                       TO   WS-INDATA-TEST               
113100         MOVE FEL-748 (INDX)            TO   MOD-TEMFSFEL                 
113200     ELSE                                                                 
113300         MOVE WS-IDKUNDNR               TO   W-401-IDKUNDNR               
113400                                             W-4A1-IDKUNDNR               
113500     END-IF                                                               
113600     SKIP2                                                                
113700     IF WS-IDORDNR  NOT NUMERIC                                           
113800         MOVE FEL                       TO   WS-INDATA-TEST               
113900         MOVE FEL-748 (INDX)            TO   MOD-TEMFSFEL                 
114000     ELSE                                                                 
114100         MOVE WS-IDORDNR                TO   W-401-IDORDNR                
114200                                             W-4A1-IDORDNR                
114300     END-IF                                                               
114400     SKIP2                                                                
114500     IF WS-IDKOLLI NOT NUMERIC                                            
114600         MOVE FEL                       TO   WS-INDATA-TEST               
114700         MOVE FEL-748 (INDX)            TO   MOD-TEMFSFEL                 
114800     END-IF                                                               
114900     SKIP2                                                                
115000     IF WS-IDPRODNR NOT NUMERIC                                           
115100         MOVE FEL                       TO   WS-INDATA-TEST               
115200         MOVE FEL-748 (INDX)            TO   MOD-TEMFSFEL                 
115300     ELSE                                                                 
115400         MOVE WS-IDPRODNR                TO  WS-IDPRODNR-N                
115500     END-IF                                                               
115600                                                                          
115700     IF WS-IDDC IS > SPACE                                                
115800       CONTINUE                                                           
115900     ELSE                                                                 
116000       MOVE FEL                           TO WS-INDATA-TEST               
116100       MOVE FEL-805 (INDX)                TO MOD-TEMFSFEL                 
116200     END-IF                                                               
116300                                                                          
116400     IF WS-SAMMA-BILD                                                     
116500         IF MID-IDRADNR-START = ALL '+'                                   
116600             IF MID-IDRADNR-SPAR NUMERIC                                  
116700                MOVE MID-IDRADNR-SPAR     TO   WS-IDRADNR-START           
116800             ELSE                                                         
116900                MOVE ZERO                 TO   WS-IDRADNR-START           
117000             END-IF                                                       
117100         ELSE                                                             
117200             IF   MID-IDRADNR-START NOT NUMERIC                           
117300                  MOVE FEL                TO   WS-INDATA-TEST             
117400                  MOVE MFS-NUM-FAELT-FEL  TO                              
117500                                       MOD-IDRADNR-START-ATTR             
117600                  MOVE FEL-748 (INDX)     TO   MOD-TEMFSFEL               
117700             ELSE                                                         
117800                  MOVE MID-IDRADNR-START  TO   WS-IDRADNR-START           
117900             END-IF                                                       
118000         END-IF                                                           
118100                                                                          
118200     IF   MID-FLBACKA-ALLA NOT = JA                                       
118300     AND  MID-FLBACKA-ALLA NOT = YES                                      
118400     AND  MID-FLBACKA-ALLA NOT = NEJ                                      
118500     AND  MID-FLBACKA-ALLA NOT = '+'                                      
118600          MOVE FEL                  TO WS-INDATA-TEST                     
118700          MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLBACKA-ALLA-ATTR              
118800          MOVE FEL-748 (INDX)       TO MOD-TEMFSFEL                       
118900     END-IF                                                               
119000                                                                          
119100     IF  (MID-FLBACKA-ALLA = JA                                           
119200     OR   MID-FLBACKA-ALLA = YES)                                         
119300     AND  NOT MFS-UPDATE                                                  
119400          MOVE FEL                  TO WS-INDATA-TEST                     
119500          MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLBACKA-ALLA-ATTR              
119600          MOVE RAETT-32(INDX)       TO MOD-TEMFSFEL                       
119700     END-IF                                                               
119800                                                                          
119900     MOVE +1                        TO RAD-INX                            
120000     PERFORM UNTIL RAD-INX NOT < MAX-ANTAL-RADER-PLUS-1                   
120100         IF   MID-FLBACKA (RAD-INX) NOT = JA                              
120200         AND  MID-FLBACKA (RAD-INX) NOT = YES                             
120300         AND  MID-FLBACKA (RAD-INX) NOT = NEJ                             
120400         AND  MID-FLBACKA (RAD-INX) NOT = '+'                             
120500              MOVE FEL                TO WS-INDATA-TEST                   
120600              MOVE MFS-ALFA-FAELT-FEL TO                                  
120700                                    MOD-FLBACKA-ATTR (RAD-INX)            
120800              MOVE FEL-748 (INDX)     TO MOD-TEMFSFEL                     
120900         ELSE                                                             
121000              IF  (MID-FLBACKA (RAD-INX) = JA  OR                         
121100                   MID-FLBACKA (RAD-INX) = YES)                           
121200              AND (MID-FLBACKA-ALLA      = JA  OR                         
121300                   MID-FLBACKA-ALLA      = YES)                           
121400                  MOVE FEL         TO WS-INDATA-TEST                      
121500                  MOVE MFS-ALFA-FAELT-FEL TO                              
121600                                    MOD-FLBACKA-ATTR (RAD-INX)            
121700                  MOVE MFS-ALFA-FAELT-FEL TO                              
121800                                     MOD-FLBACKA-ALLA-ATTR                
121900                  MOVE FEL-748 (INDX) TO MOD-TEMFSFEL                     
122000              END-IF                                                      
122100         END-IF                                                           
122200                                                                          
122300         IF  (MID-FLBACKA (RAD-INX) = JA  OR                              
122400              MID-FLBACKA (RAD-INX) = YES)                                
122500         AND  NOT MFS-UPDATE                                              
122600              MOVE FEL              TO WS-INDATA-TEST                     
122700              MOVE MFS-ALFA-FAELT-FEL                                     
122800                             TO MOD-FLBACKA-ATTR (RAD-INX)                
122900              MOVE RAETT-33(INDX)   TO MOD-TEMFSFEL                       
123000         END-IF                                                           
123100         ADD +1                     TO RAD-INX                            
123200     END-PERFORM                                                          
123300     ELSE                                                                 
123400         MOVE ZERO                        TO   WS-IDRADNR-START           
123500     END-IF                                                               
123600*                                                                         
123700     .                                                                    
123800     EJECT                                                                
123900 C-RELATIONSKONTROLL  SECTION.                                            
124000                                                                          
124100     MOVE WS-IDDISTR                  TO WS-IDDISTR-N                     
124200     MOVE WS-IDKUNDNR                 TO WS-IDKUNDNR-N                    
124300                                                                          
124400     PERFORM CA-KONTROLLERA-KUNDORDNR                                     
124500                                                                          
124600     IF WS-INDATA-RATT                                                    
124700                                                                          
124800         PERFORM CB-KONTROLLERA-KOLLI                                     
124900         IF WS-INDATA-RATT                                                
125000           IF MFS-UPDATE                                                  
125100             PERFORM CC-KONTROLLERA-PACKARE                               
125200           END-IF                                                         
125300         END-IF                                                           
125400     END-IF                                                               
125500     .                                                                    
125600     EJECT                                                                
125700 CA-KONTROLLERA-KUNDORDNR SECTION.                                        
125800                                                                          
125900     IF MID-IDPRODNR-IN = ALL '+'                                         
126000         IF    MID-IDDISTR-IN  = ALL '+'                                  
126100           AND MID-IDKUNDNR-IN = ALL '+'                                  
126200           AND MID-IDORDNR-IN  = ALL '+'                                  
126300             IF WS-IDPRODNR > ZERO                                        
126400*--------------------------ANVÄNDS GAMLA PRODNR: MID-IDPRODNR-UT          
126500                 PERFORM CAB-HAMTA-IDGMTREF-I-WDE4                        
126600                 MOVE MFS-RENSA-FAELT TO  MOD-IDDISTR-UT                  
126700                                          MOD-IDKUNDNR-UT                 
126800                                          MOD-IDORDNR-UT                  
126900             ELSE                                                         
127000                 PERFORM CAA-HAMTA-PRODNR-I-WDE4-6                        
127100                 MOVE MFS-RENSA-FAELT TO  MOD-IDPRODNR-UT                 
127200             END-IF                                                       
127300         ELSE                                                             
127400             PERFORM CAA-HAMTA-PRODNR-I-WDE4-6                            
127500             MOVE MFS-RENSA-FAELT     TO  MOD-IDPRODNR-UT                 
127600         END-IF                                                           
127700     ELSE                                                                 
127800         PERFORM CAB-HAMTA-IDGMTREF-I-WDE4                                
127900         MOVE MFS-RENSA-FAELT          TO MOD-IDDISTR-UT                  
128000                                          MOD-IDKUNDNR-UT                 
128100                                          MOD-IDORDNR-UT                  
128200     END-IF                                                               
128300     .                                                                    
128400     EJECT                                                                
128500 CAA-HAMTA-PRODNR-I-WDE4-6  SECTION.                                      
128600*                                                                         
128700     MOVE WS-IDDISTR-N                   TO W-4A1-IDDISTR                 
128800     MOVE WS-IDKUNDNR-N                  TO W-4A1-IDKUNDNR                
128900     MOVE WS-IDORDNR                     TO W-4A1-IDORDNR                 
129000                                                                          
129100     PERFORM IMS-GU-KUNDORDER-SEK                                         
129200                                                                          
129300     IF KUNDORDER-SEK-FINNS                                               
129400       PERFORM UNTIL (KORD-IDDC = WS-IDDC AND                             
129500         KORD-KVORDRAD-LEVPL = ZERO) OR KUNDORDER-SEK-SAKNAS              
129600          PERFORM IMS-GN-KUNDORDER-SEK                                    
129700       END-PERFORM                                                        
129800                                                                          
129900       IF KUNDORDER-SEK-FINNS                                             
130000         MOVE KORD-IDPRODNR TO W-601-IDPRODNR                             
130100         PERFORM IMS-GU-WDE601                                            
130200                                                                          
130300         IF SEGMENT-FINNS                                                 
130500           MOVE VORD-IDPRODNR TO WS-IDPRODNR                              
130600                                 WS-IDPRODNR-N                            
130700         ELSE                                                             
130800           MOVE FEL                       TO   WS-INDATA-TEST             
130900           MOVE FEL-7011 (INDX)           TO   MOD-TEMFSFEL               
131000         END-IF                                                           
131100       ELSE                                                               
131200         MOVE FEL                          TO   WS-INDATA-TEST            
131300         MOVE FEL-7012 (INDX)              TO   MOD-TEMFSFEL              
131400       END-IF                                                             
131500     ELSE                                                                 
131600       MOVE FEL                          TO   WS-INDATA-TEST              
131700       MOVE FEL-7013 (INDX)              TO   MOD-TEMFSFEL                
131800                                                                          
131900     END-IF                                                               
132000     .                                                                    
132100     EJECT                                                                
132200 CAB-HAMTA-IDGMTREF-I-WDE4 SECTION.                                       
132300     SKIP3                                                                
132400     MOVE WS-IDPRODNR-N                 TO W-601-IDPRODNR                 
132500                                           W-411-IDPRODNR-MIN             
132600                                           W-411-IDPRODNR-MAX             
132700                                           W-411-IDPRODNR                 
132800     MOVE 1                             TO W-411-IDPURAD-MIN              
132900                                           W-411-IDPURAD                  
133000     MOVE 99999                         TO W-411-IDPURAD-MAX              
133100                                                                          
133200     PERFORM IMS-GU-KUNDORDER-SEK-INV-GE                                  
133300     IF SEGMENT-FINNS                                                     
133400         MOVE KORD-IDDISTR              TO WS-IDDISTR                     
133500                                           WS-IDDISTR-N                   
133600         MOVE KORD-IDKUNDNR             TO WS-IDKUNDNR                    
133700                                           WS-IDKUNDNR-N                  
133800         MOVE KORD-IDKUNDRF             TO WS-IDKUNDRF                    
133900     ELSE                                                                 
134000         MOVE FEL                       TO WS-INDATA-TEST                 
134100         MOVE FEL-701 (INDX)            TO MOD-TEMFSFEL                   
134200     END-IF                                                               
134300     .                                                                    
134400     EJECT                                                                
134500 CB-KONTROLLERA-KOLLI   SECTION.                                          
134600                                                                          
134700     MOVE WS-IDPRODNR-N                  TO   W-601-IDPRODNR              
134800     PERFORM IMS-GHU-KOLLIREG                                             
134900*                                                                         
135000     IF SEGMENT-FINNS                                                     
135200                                                                          
135300        IF VORD-IDDC = WS-IDDC                                            
135400           MOVE WS-IDKOLLI               TO  W-611-IDKOLLI                
135500           PERFORM IMS-GHU-KOLLI                                          
135600*                                                                         
135700           IF SEGMENT-FINNS                                               
135800              MOVE KOLLI-KVORDRAD        TO   MOD-KVORDRAD-SPAR           
135810              IF KOLLI-IDKOLLI-SAMP > ZERO                                
135820              AND KOLLI-KDSTASKLI   > SPACE                               
135821*---------------------------------------------INGÅR KOLLIT I ETT          
135822*---------------------------------------------SAMLINGSKOLLI               
135830                MOVE FEL            TO  WS-INDATA-TEST                    
135840                MOVE FEL-7XX (INDX) TO  MOD-TEMFSFEL                      
137000              ELSE                                                        
137010                IF KOLLI-KDKOLSTA  < 6                                    
137020                  IF MFS-UPDATE                                           
137030                    MOVE KOLLI-KVORDRAD     TO   WS-KVORDRAD              
137040                    MOVE KOLLI-KDKOLSTA     TO   WS-KDKOLSTA              
137050                    MOVE KOLLI-ADFLGEO      TO   WS-KOLLI-ADFLGEO         
137060                    MOVE KOLLI-ADFLOMR      TO   WS-KOLLI-ADFLOMR         
137070*      SPAR UNDAN ANTAL RADER MED KDFARLIG = +4, +7 FÖR ATT KUNNA         
137080*      SÄTTA KDFARLIG FÖR HELA KOLLIT. VID BORTTAG AV KOLLI-              
137090*      KOPPLING FÖR DESSA RADER RÄKNAR MAN NED KVFALRAD                   
137091                    MOVE KOLLI-KVFALRAD     TO   WS-KVFALRAD              
137092                  END-IF                                                  
137093                ELSE                                                      
137094*---------------------------------------------ÄR KOLLIT FAKTURERAT        
137095*---------------------------------------------EL. FAKTURA-RELEASAT        
137097                  MOVE FEL             TO   WS-INDATA-TEST                
137098                  MOVE FEL-737 (INDX)  TO   MOD-TEMFSFEL                  
137100                END-IF                                                    
137500              END-IF                                                      
137600           ELSE                                                           
137700              MOVE FEL                   TO   WS-INDATA-TEST              
137800              MOVE FEL-758 (INDX)        TO   MOD-TEMFSFEL                
137900           END-IF                                                         
138000        ELSE                                                              
138100           MOVE FEL                      TO   WS-INDATA-TEST              
138200           MOVE FEL-7582(INDX)           TO   MOD-TEMFSFEL                
138300        END-IF                                                            
138400     ELSE                                                                 
138500        MOVE FEL                         TO   WS-INDATA-TEST              
138600        MOVE FEL-7583(INDX)              TO   MOD-TEMFSFEL                
138700     END-IF                                                               
138800     .                                                                    
138900     EJECT                                                                
139000 CC-KONTROLLERA-PACKARE SECTION.                                          
139100                                                                          
139200     MOVE NEJ                           TO SW-SLUT-RADNR                  
139300     MOVE 1                             TO RAD-INX                        
139400     MOVE WS-IDPRODNR-N                 TO W-411-IDPRODNR                 
139500                                                                          
139600     PERFORM UNTIL (RAD-INX > MAX-ANTAL-RADER                             
139700                OR  WS-INDATA-FEL                                         
139800                OR  SW-SLUT-RADNR = JA)                                   
139900                                                                          
140000       MOVE MID-IDRADNR (RAD-INX)        TO WS-IDRADNR                    
140100       INSPECT WS-IDRADNR REPLACING LEADING SPACE BY ZERO                 
140200                                                                          
140300       IF  WS-IDRADNR > ZERO                                              
140400                                                                          
140500         IF  MID-FLBACKA-ALLA         = JA                                
140600         OR  MID-FLBACKA-ALLA         = YES                               
140700         OR  MID-FLBACKA (RAD-INX)    = JA                                
140800         OR  MID-FLBACKA (RAD-INX)    = YES                               
140900           MOVE WS-IDRADNR               TO W-411-IDPURAD                 
141000           PERFORM IMS-GU-WDE411-01-BSEQ                                  
141100           IF SEGMENT-FINNS                                               
141200             PERFORM CCB-KONTROLLERA-PLOCKLISTA                           
141300           ELSE                                                           
141400             MOVE FEL               TO   WS-INDATA-TEST                   
141500             MOVE FEL-809 (INDX)    TO   MOD-TEMFSFEL                     
141600           END-IF                                                         
141700         END-IF                                                           
141800       ELSE                                                               
141900         MOVE JA                         TO SW-SLUT-RADNR                 
142000       END-IF                                                             
142100                                                                          
142200       ADD +1                            TO RAD-INX                       
142300     END-PERFORM                                                          
142400     .                                                                    
142500     EJECT                                                                
142600 CCB-KONTROLLERA-PLOCKLISTA              SECTION.                         
142700                                                                          
142800     EVALUATE TRUE                                                        
142900       WHEN KORD-IDUSER NOT = WS-IDUSER                                   
143000*----------------------------------------------PACKARE STÄMMER EJ         
143100         MOVE FEL                   TO   WS-INDATA-TEST                   
143200         MOVE FEL-719 (INDX)        TO   MOD-TEMFSFEL                     
143300       WHEN KORD-KVORDRAD-PACK = KORD-KVORDRAD                            
143400*----------------------------------------------ÄR ANGIVEN PACKARES        
143500*----------------------------------------------ORDERDEL REDAN KLAR        
143600         MOVE FEL                   TO   WS-INDATA-TEST                   
143700         MOVE FEL-720 (INDX)        TO   MOD-TEMFSFEL                     
143800       WHEN KORD-KDPAKOLL NOT = ZERO                                      
143900*--------------------------------------------FÅR MAN EJ RÄTTA DÅ          
144000*--------------------------------------------AVVIK.KONTROLL PÅGÅR         
144100         MOVE FEL                   TO   WS-INDATA-TEST                   
144200         MOVE FEL-804 (INDX)        TO   MOD-TEMFSFEL                     
144300       WHEN OTHER                                                         
144400         CONTINUE                                                         
144500     END-EVALUATE                                                         
144600     .                                                                    
144700     EJECT                                                                
144800 D-LAGG-UT-RADER      SECTION.                                            
144900                                                                          
145000*    BILDSIDAN FYLLS MED INFO FRÅN ORDERRADER TILLHÖRANDE                 
145100*    AKTUELLT KOLLI.                                                      
145200*    AVBRYTS DÅ EN DISTR-KUND-ORDER'S ALLA PLOCKLISTOR ÄR                 
145300*    BEHANDLADE ELLER DÅ BILDSIDAN BLIR ÖVERFULL.                         
145400                                                                          
145500                                                                          
145600     PERFORM DA-INIT-BILDRADER                                            
145700     MOVE +1                            TO   RAD-INX                      
145800     MOVE NEJ                           TO   WS-TRAEFF-KOLLI              
145900     MOVE NEJ                           TO   SW-SIDA-OVERFULL             
146000*                                                                         
146100     MOVE WS-IDDISTR-N                  TO W-4A1-IDDISTR                  
146200     MOVE WS-IDKUNDNR-N                 TO W-4A1-IDKUNDNR                 
146300     MOVE WS-IDORDNR                    TO W-4A1-IDORDNR                  
146400     PERFORM IMS-GU-KUNDORDER-SEK                                         
146500*                                                                         
146600     PERFORM UNTIL SW-SIDA-OVERFULL = JA                                  
146700                OR KUNDORDER-SEK-SAKNAS                                   
146800                                                                          
146900       MOVE KORD-IDDISTR             TO W-401-IDDISTR                     
147000       MOVE KORD-IDKUNDNR            TO W-401-IDKUNDNR                    
147100       MOVE KORD-IDORDNR5            TO W-401-IDORDNR                     
147200       MOVE KORD-IDPRODNR            TO W-401-IDPRODNR                    
147300       MOVE KORD-IDPLKLST            TO W-401-IDPLKLST                    
147400       PERFORM IMS-GU-KUNDORDER                                           
147500                                                                          
147600       PERFORM DB-KTRL-KORD-VISNING                                       
147700                                                                          
147800       IF SW-VISA-KORD = JA                                               
147900         PERFORM DC-BEH-GODK-PLKLST                                       
148000       END-IF                                                             
148100                                                                          
148200       IF  SW-SIDA-OVERFULL = NEJ                                         
148300         PERFORM IMS-GN-KUNDORDER-SEK                                     
148400*BS       MOVE ZERO TO WS-IDRADNR-START                                   
148500       END-IF                                                             
148600                                                                          
148700     END-PERFORM                                                          
148800                                                                          
148900     IF WS-TRAEFF-KOLLI = NEJ                                             
149000      IF  MFS-UPDATE                                                      
149100      AND MFS-ENTER                                                       
149200*       * VID VISNING EFTER KORREKT UPPDATERING ÄR DET INGET FEL          
149300*       * ATT PACKAREN SAKNAR RADER I KOLLIT.                             
149400*       * FALLET UPPSTÅR DÅ NÅGON ANNAN PACKARE HAR RAD(ER)               
149500*       * I KOLLIT.                                                       
149600        CONTINUE                                                          
149700      ELSE                                                                
149800        EVALUATE TRUE                                                     
149900          WHEN SW-FEL-PRODNR = 'J'                                        
150000            MOVE FEL               TO   WS-BEHANDLING-TEST                
150100            MOVE FEL-807(INDX)    TO   MOD-TEMFSFEL                       
150200          WHEN SW-FEL-PACKARE = 'J'                                       
150300            MOVE FEL               TO   WS-BEHANDLING-TEST                
150400            MOVE FEL-806(INDX)    TO   MOD-TEMFSFEL                       
150500          WHEN SW-ORDER-AVSLUTAD = 'J'                                    
150600            MOVE FEL               TO   WS-BEHANDLING-TEST                
150700            MOVE FEL-808(INDX)    TO   MOD-TEMFSFEL                       
150800          WHEN OTHER                                                      
150900            MOVE FEL               TO   WS-BEHANDLING-TEST                
151000            MOVE FEL-7584(INDX)    TO   MOD-TEMFSFEL                      
151100          END-EVALUATE                                                    
151200      END-IF                                                              
151300     END-IF                                                               
151400     .                                                                    
151500     EJECT                                                                
151600 DA-INIT-BILDRADER    SECTION.                                            
151700     SKIP3                                                                
151800     MOVE MFS-RENSA-FAELT           TO MOD-IDRADNR-START                  
151900                                       MOD-FLBACKA-ALLA                   
152000     MOVE ZERO                      TO MOD-IDRADNR-SPAR                   
152100     MOVE +1                        TO RAD-INX                            
152200                                                                          
152300     PERFORM UNTIL RAD-INX > MAX-RAD-INX                                  
152400        MOVE MFS-RENSA-FAELT TO MOD-IDRADNR  (RAD-INX)                    
152500                                MOD-KVLEVART (RAD-INX)                    
152600                                MOD-FLBACKA  (RAD-INX)                    
152700        MOVE MFS-STAENG-FAELT                                             
152800                             TO MOD-FLBACKA-ATTR (RAD-INX)                
152900        ADD +1               TO RAD-INX                                   
153000     END-PERFORM                                                          
153100     .                                                                    
153200     EJECT                                                                
153300 DB-KTRL-KORD-VISNING SECTION.                                            
153400                                                                          
153500*    KONTROLLERAR VILLKOR FÖR ATT GODKÄNNA VISNING AV EN KORD.            
153600                                                                          
153700                                                                          
153800     MOVE NEJ                      TO SW-VISA-KORD                        
153900                                                                          
154000     EVALUATE TRUE                                                        
154100                                                                          
154200       WHEN KORD-IDPRODNR NOT = WS-IDPRODNR-N                             
154300           MOVE JA      TO SW-FEL-PRODNR                                  
154400                                                                          
154500       WHEN KORD-IDUSER NOT = WS-IDUSER                                   
154600         MOVE JA        TO SW-FEL-PACKARE                                 
154700                                                                          
154800       WHEN CDC                                                           
154900       AND  KORD-KVORDRAD-PACK = KORD-KVORDRAD                            
155000         MOVE JA        TO SW-ORDER-AVSLUTAD                              
155100                                                                          
155200       WHEN SDC                                                           
155300       AND  KORD-KVORDRAD-PACK = KORD-KVORDRAD                            
155400         MOVE JA        TO SW-ORDER-AVSLUTAD                              
155500                                                                          
155600       WHEN OTHER                                                         
155700*        * KORD GODKÄND FÖR VISNING                                       
155800         MOVE JA                    TO SW-VISA-KORD                       
155900                                                                          
156000     END-EVALUATE                                                         
156100     .                                                                    
156200     EJECT                                                                
156300 DC-BEH-GODK-PLKLST   SECTION.                                            
156400                                                                          
156500*    BEHANDLA EN FÖR VISNING GODKÄND PLOCKLISTA.                          
156600*    ORAD:ER LÄSES TILLS NÅGON ORAD BEFINNES TILLHÖRA AKTUELLT            
156700*    KOLLI; DÅ ANROPAS DCA-.                                              
156800                                                                          
156900     IF  WS-IDRADNR-START = ZERO                                          
157000       PERFORM IMS-GNP-RAD-OKVAL                                          
157100     ELSE                                                                 
157200       MOVE WS-IDRADNR-START TO W-411-IDPURAD2                            
157300       PERFORM IMS-GNP-RAD-FOM                                            
157400     END-IF                                                               
157500                                                                          
157600     MOVE NEJ                TO SW-KKOLLI-FINNS                           
157700                                                                          
157800     PERFORM UNTIL (SEGMENT-SAKNAS                                        
157900                OR  SW-KKOLLI-FINNS  = JA                                 
158000                OR  SW-SIDA-OVERFULL = JA)                                
158100                                                                          
158200       IF  ORAD-KVLEVART > ZERO                                           
158300         MOVE ORAD-IDPURAD        TO WS-SPAR-IDPURAD                      
158400         MOVE WS-IDPRODNR-N       TO W-421-IDPRODNR                       
158500         MOVE WS-IDKOLLI          TO W-421-IDKOLLI                        
158600         PERFORM IMS-GNP-KKOLLI-KVAL                                      
158700                                                                          
158800         IF  SEGMENT-FINNS                                                
158900           MOVE JA                TO SW-KKOLLI-FINNS                      
159000         ELSE                                                             
159100           PERFORM IMS-GNP-RAD-OKVAL                                      
159200         END-IF                                                           
159300       ELSE                                                               
159400         PERFORM IMS-GNP-RAD-OKVAL                                        
159500       END-IF                                                             
159600     END-PERFORM                                                          
159700                                                                          
159800     IF  SW-KKOLLI-FINNS = JA                                             
159900       PERFORM DCA-BEH-KOLLI-PLKLST                                       
160000     END-IF                                                               
160100     .                                                                    
160200     EJECT                                                                
160300 DCA-BEH-KOLLI-PLKLST SECTION.                                            
160400                                                                          
160500*    FÖR AKTUELL PLOCKLISTA BEHANDLAS ALLA RADER TILLHÖRANDE              
160600*    ANGIVET KOLLI.                                                       
160700                                                                          
160800     MOVE JA                     TO WS-TRAEFF-KOLLI                       
160900                                                                          
161000     PERFORM IMS-WDE43-GU-ORAD-LAST                                       
161100                                                                          
161200     MOVE WS-IDPRODNR-N          TO W-601-IDPRODNR                        
161300                                    W-421-IDPRODNR                        
161400                                    W-IDPRODNR-F                          
161500     MOVE WS-IDKOLLI             TO W-611-IDKOLLI                         
161600                                    W-421-IDKOLLI                         
161700                                    W-IDKOLLI-F                           
161800                                                                          
161900     IF (MFS-IDPFK = '8')                                                 
162000     OR  (MFS-IDPFK = ' '                                                 
162100     AND  MID-IDRADNR-START NOT = ALL '+')                                
162200       MOVE WS-IDRADNR-START    TO  W-411-IDPURAD2                        
162300       PERFORM IMS-GN-WDE411-21-FSEQ-SOK                                  
162400     ELSE                                                                 
162500       PERFORM IMS-GN-WDE411-21-FSEQ                                      
162600     END-IF                                                               
162700                                                                          
162800     PERFORM UNTIL (SEGMENT-SAKNAS                                        
162900                OR  SW-SIDA-OVERFULL = JA)                                
163000                                                                          
163100       MOVE WDE44-ORAD-IDPURAD  TO ARB-RAD                                
163200                             WS-WDE44-ORAD-IDPURAD                        
163300       MOVE WDE44-KKOLLI-KVLEVART TO ARB-KVLEVART                         
163400                                                                          
163500       MOVE WDE44-KKOLLI-IDKOLLI  TO W-421-IDKOLLI                        
163600                                                                          
163700                                                                          
163800       PERFORM DCAA-STYR-BILDRADER                                        
163900                                                                          
164000       IF SW-SIDA-OVERFULL = NEJ                                          
164100         PERFORM IMS-GN-WDE411-21-FSEQ                                    
164200       END-IF                                                             
164300                                                                          
164400     END-PERFORM                                                          
164500     .                                                                    
164600     EJECT                                                                
164700 DCAA-STYR-BILDRADER SECTION.                                             
164800                                                                          
164900*    REDIGERAR EN RAD I MOD.                                              
165000*    OM SIDAN ÖVERFULL, SPARAS SISTA MOD-RADENS NYCKEL                    
165100*      SAMT MEDDELANDE LÄGGS UT.                                          
165200                                                                          
165300     IF  RAD-INX <= MAX-RAD-INX                                           
165400*      * AKTUELL RAD FÅR PLATS PÅ SIDAN                                   
165500       PERFORM S05-SKRIV-MOD-RAD                                          
165600       ADD +1 TO RAD-INX                                                  
165700     ELSE                                                                 
165800*      * SIDAN ÖVERFULL                                                   
165900       MOVE JA                        TO SW-SIDA-OVERFULL                 
166000       MOVE MOD-IDRADNR (MAX-RAD-INX) TO WS-IDRADNR-X                     
166100       INSPECT WS-IDRADNR-X REPLACING LEADING SPACE BY ZERO               
166200       MOVE WS-IDRADNR-NUM            TO MOD-IDRADNR-SPAR                 
166300       MOVE UPPL-3 (INDX)             TO MOD-TEMFSINF                     
166400                                                                          
166500*      STRING '*'                                                         
166600*             SW-SIDA-OVERFULL                                            
166700*             '*'                                                         
166800*             WS-IDRADNR-X                                                
166900*             '*'                                                         
167000*             MOD-IDRADNR-SPAR                                            
167100*             '*'                                                         
167200*             MID-IDRADNR-SPAR                                            
167300*             '*'                                                         
167400*             WS-IDRADNR-START                                            
167500*             '*'                                                         
167600*             MID-IDRADNR-START                                           
167700*             '*'                                                         
167800*      DELIMITED BY SIZE INTO MOD-TEMFSINF                                
167900     END-IF                                                               
168000     .                                                                    
168100     EJECT                                                                
168200 E-BEH-ATERBOKNING    SECTION.                                            
168300     SKIP3                                                                
168400     IF MID-IDRADNR     (RAD-INX) = ZERO                                  
168500         MOVE MAX-ANTAL-RADER-PLUS-1    TO   RAD-INX                      
168600     ELSE                                                                 
168700         INSPECT MID-IDRADNR     (RAD-INX) REPLACING LEADING              
168800                     SPACE BY ZERO                                        
168900         MOVE MID-IDRADNR     (RAD-INX) TO   ARB-RAD                      
169000         INSPECT MID-KVLEVART (RAD-INX) REPLACING LEADING                 
169100                     SPACE BY ZERO                                        
169200         MOVE MID-KVLEVART (RAD-INX)    TO   ARB-KVLEVART                 
169300                                                                          
169400     SKIP2                                                                
169500         MOVE 'N'                       TO  WS-RADER-OK                   
169600*                                                                         
169700         MOVE WS-IDDISTR-N              TO W-4A1-IDDISTR                  
169800         MOVE WS-IDKUNDNR-N             TO W-4A1-IDKUNDNR                 
169900         MOVE WS-IDORDNR                TO W-4A1-IDORDNR                  
170000         PERFORM IMS-GU-KUNDORDER-SEK                                     
170100*                                                                         
170200         IF KUNDORDER-SEK-FINNS                                           
170300            PERFORM UNTIL KUNDORDER-SEK-SAKNAS OR                         
170400                          WS-RADER-OK = 'J'                               
170500            MOVE KORD-IDDISTR           TO W-401-IDDISTR                  
170600            MOVE KORD-IDKUNDNR          TO W-401-IDKUNDNR                 
170700            MOVE KORD-IDORDNR5          TO W-401-IDORDNR                  
170800            MOVE KORD-IDPRODNR          TO W-401-IDPRODNR                 
170900            MOVE KORD-IDPLKLST          TO W-401-IDPLKLST                 
171000            PERFORM IMS-GU-KUNDORDER                                      
171100*                                                                         
171200            MOVE KORD-IDUSER            TO WS-JFR-IDANSTNR                
171300                                                                          
171400            MOVE KORD-IDORDER           TO WS-DNOT-IDORDER                
171500            MOVE KORD-IDDC              TO WS-DNOT-IDDC                   
171600                                                                          
171700            IF WS-IDPRODNR-N = KORD-IDPRODNR AND                          
171800               WS-IDANSTNR = WS-JFR-IDANSTNR-5                            
171900               MOVE ARB-RAD TO W-411-IDPURAD2                             
172000               PERFORM IMS-GNP-RAD                                        
172100               IF SEGMENT-FINNS                                           
172200                  MOVE 'J' TO WS-RADER-OK                                 
172300                                                                          
172400                  MOVE ORAD-IDARTNR     TO WS-DNOT-IDARTNR                
172500                  MOVE ARB-RAD          TO WS-DNOT-IDPURAD                
172600                                                                          
172700               END-IF                                                     
172800            END-IF                                                        
172900*                                                                         
173000            PERFORM IMS-GN-KUNDORDER-SEK                                  
173100            END-PERFORM                                                   
173200         ELSE                                                             
173300            MOVE FEL             TO WS-BEHANDLING-TEST                    
173400            MOVE FEL-701 (INDX)  TO MOD-TEMFSFEL                          
173500         END-IF                                                           
173600*                                                                         
173700         IF WS-RADER-OK = 'N'                                             
173800            MOVE FEL             TO WS-BEHANDLING-TEST                    
173900            MOVE FEL-722 (INDX)  TO MOD-TEMFSFEL                          
174000         END-IF                                                           
174100*                                                                         
174200         IF WS-BEHANDLING-RATT                                            
174300            PERFORM EC-BEHANDLA-ORDERRAD                                  
174400            IF WS-BEHANDLING-RATT                                         
174500               MOVE WS-IDDISTR-N TO TEST-IDDISTR                          
174600               IF NOT DIST19-SATS                                         
174700                  PERFORM ED-UPPDATERA-PRODTAB                            
174800               END-IF                                                     
174900            END-IF                                                        
175000         END-IF                                                           
175100     END-IF                                                               
175200     .                                                                    
175300     EJECT                                                                
175400 EC-BEHANDLA-ORDERRAD       SECTION.                                      
175500     SKIP3                                                                
175600     MOVE NEJ              TO FL-RADSTA-BACKAD                            
175700                                                                          
175800     MOVE ARB-RAD          TO W-411-IDPURAD                               
175900     MOVE WS-IDPRODNR-N    TO W-421-IDPRODNR                              
176000     MOVE WS-IDKOLLI       TO W-421-IDKOLLI  W-611-IDKOLLI                
176100     MOVE W-401-IDPLKLST   TO W-E4F-IDPLKST                               
176200     SKIP2                                                                
176300     PERFORM IMS-GHN-KOLLI-KOPPL-SEK                                      
176400     IF SEGMENT-FINNS                                                     
176500         SUBTRACT +1 FROM WS-KVORDRAD                                     
176600         IF WS-KVORDRAD > ZERO                                            
176700         OR WS-KDKOLSTA = ZERO                                            
176800         OR (WS-KOLLI-ADFLGEO NOT = 'RAC'                                 
176900         AND WS-KDKOLSTA < 2)                                             
177000         OR (WS-KOLLI-ADFLGEO = 'RAC'                                     
177100         AND WS-KOLLI-ADFLOMR = 999                                       
177200         AND WS-KDKOLSTA < 2)                                             
177300             MOVE KKOLLI-KVLEVART TO WS-KVLEVART                          
177400             IF NDC-NA                                                    
177500                MOVE KKOLLI-IDKOLLI  TO WS-DNOT-IDKOLLI                   
177600                PERFORM S12-DATA-TILL-DEL-NOTE                            
177700             END-IF                                                       
177800             PERFORM IMS-DLET-KOLLI-KOPPL                                 
177900**  BORTTAG AV LAASNINGS SEGMENT(WDR4)                                    
178000             PERFORM IMS-WDE611-GU-KOLLI                                  
178100             MOVE W-601-IDPRODNR TO W-IDPRODNR-WDE4F-MAX                  
178200             MOVE W-611-IDKOLLI TO W-IDKOLLI-WDE4F-MAX                    
178300             MOVE W-601-IDPRODNR TO W-IDPRODNR-WDE4F-MIN                  
178400             MOVE W-611-IDKOLLI TO W-IDKOLLI-WDE4F-MIN                    
178500             PERFORM IMS-GET-WDE4F                                        
178600                                                                          
178700             IF SEGMENT-SAKNAS                                            
178800                MOVE WS-IDPRODNR   TO W-4301-IDPRODNR                     
178900                PERFORM IMS-GHU-XXDU01                                    
179000                                                                          
179100                IF SEGMENT-FINNS                                          
179200                   MOVE WS-IDKOLLI       TO W-4302-IDKOLLI                
179300                   MOVE W-401-IDPLKLST   TO W-4302-IDPLKLST               
179400                                                                          
179500                   PERFORM IMS-GHNP-XXDU11                                
179600                   IF SEGMENT-FINNS                                       
179700                     PERFORM IMS-DLET-XXDU                                
179800                                                                          
179900                                                                          
180000                     PERFORM IMS-GNP-XXDU11-FIRST                         
180100                     IF SEGMENT-SAKNAS                                    
180200                        PERFORM IMS-GHU-XXDU01                            
180300                        PERFORM IMS-DLET-XXDU                             
180400                     END-IF                                               
180500                   END-IF                                                 
180600                END-IF                                                    
180700             END-IF                                                       
180800                                                                          
180900         ELSE                                                             
181000             MOVE FEL            TO WS-BEHANDLING-TEST                    
181100             MOVE FEL-742 (INDX) TO MOD-TEMFSFEL                          
181200             MOVE +1             TO WS-KVORDRAD                           
181300         END-IF                                                           
181400     ELSE                                                                 
181500         MOVE FEL                TO WS-BEHANDLING-TEST                    
181600         MOVE UPPL-2 (INDX)      TO MOD-TEMFSINF                          
181700     END-IF                                                               
181800                                                                          
181900     IF  WS-BEHANDLING-RATT                                               
182000       PERFORM IMS-GHU-RAD-SEK                                            
182100       PERFORM ECB-KTRL-MID-MOT-DB                                        
182200       IF  WS-BEHANDLING-RATT                                             
182300         IF ORAD-KDFARLIG = +4                                            
182400         OR ORAD-KDFARLIG = +7                                            
182500             SUBTRACT +1 FROM WS-KVFALRAD                                 
182600         END-IF                                                           
182700                                                                          
182800         COMPUTE ORAD-KVLEVART                                            
182900                          = ORAD-KVLEVART - WS-KVLEVART                   
183000                                                                          
183100         IF ORAD-KDRADSTA = 4 OR 5                                        
183200            MOVE +3 TO ORAD-KDRADSTA                                      
183300            ADD +1  TO WS-TOT-ANT-RADER                                   
183400            MOVE JA TO FL-RADSTA-BACKAD                                   
183500         END-IF                                                           
183600                                                                          
183700         PERFORM ECA-SPARA-RAD-INFO                                       
183800                                                                          
183900         IF  ORAD-KVLEVART = ZERO                                         
184000             MOVE JA  TO ARB-RAD-HELT-ORAPP                               
184100         ELSE                                                             
184200             MOVE NEJ TO ARB-RAD-HELT-ORAPP                               
184300         END-IF                                                           
184310                                                                          
184320         IF KORD-KDORDKL = +0                                             
184330            PERFORM ECC-BACKA-VOR-TIKLAR                                  
184340         END-IF                                                           
184400                                                                          
184500         PERFORM IMS-REPL-RAD                                             
184600       END-IF                                                             
184700     END-IF                                                               
184800     .                                                                    
184900     EJECT                                                                
185000 ECA-SPARA-RAD-INFO        SECTION.                                       
185100     MOVE ORAD-VKARTNTO         TO  SPAR-PRAD-VKARTNTO                    
185200     MOVE ORAD-KVFLAMP          TO  SPAR-PRAD-KVFLAMP                     
185300     MOVE ORAD-KDFARLIG         TO  SPAR-PRAD-KDFARLIG                    
185400     MOVE ORAD-PRARTNTO-LOC     TO SPAR-PRAD-PRARTNTO-LOC                 
185500     MOVE ORAD-PRARTNTO-LOCPREL TO SPAR-PRAD-PRARTNTO-LOCPREL             
185600     MOVE ORAD-PRARTNTO         TO SPAR-PRAD-PRARTNTO                     
185700*                                                                         
185800     IF ORAD-IDPSN > ZERO                                                 
185900       MOVE ORAD-IDPSN          TO  SPAR-ORAD-IDPSN                       
186000       MOVE ORAD-VKART-FG       TO  SPAR-ORAD-VKART-FG                    
186100       MOVE ORAD-VLFG           TO  SPAR-ORAD-VLFG                        
186200       MOVE ORAD-SUEQFG         TO  SPAR-ORAD-SUEQFG                      
186300     END-IF                                                               
186400*                                                                         
186500     SKIP2                                                                
186600     IF ARB-KVLEVART            >   ZERO                                  
186700         MOVE ARB-KVLEVART      TO  SPAR-PRAD-KVLEVART                    
186800     ELSE                                                                 
186900         MOVE ORAD-KVAVBART     TO  SPAR-PRAD-KVLEVART                    
187000     END-IF                                                               
187100     SKIP2                                                                
187200     PERFORM S10-UPPD-SPAR-KOLLI                                          
187300     .                                                                    
187400 ECB-KTRL-MID-MOT-DB SECTION.                                             
187500                                                                          
187600*    KKOLLI KONTROLLERAS MOT MID ELLER ORAD.                              
187700*    OÖVERENSSTÄMMELSE (PGA DATA ÄNDRAD EFTER VISNING) MEDDELAS.          
187800                                                                          
187900     IF  ARB-KVLEVART > ZERO                                              
188000*      * LEV. I MID (ARB-) SKALL VARA = KKOLLI (WS-)                      
188100       IF  ARB-KVLEVART NOT = WS-KVLEVART                                 
188200         MOVE FEL                TO WS-BEHANDLING-TEST                    
188300         MOVE UPPL-2 (INDX)      TO MOD-TEMFSINF                          
188400       END-IF                                                             
188500     ELSE                                                                 
188600*      * LEV. I KKOLLI (WS-) SKALL VARA = ORAD'S AVBOKAT                  
188700       IF  WS-KVLEVART NOT = ORAD-KVAVBART                                
188800         MOVE FEL                TO WS-BEHANDLING-TEST                    
188900         MOVE UPPL-2 (INDX)      TO MOD-TEMFSINF                          
189000       END-IF                                                             
189100     END-IF                                                               
189200     .                                                                    
189300     EJECT                                                                
189310 ECC-BACKA-VOR-TIKLAR      SECTION.                                       
189320     .                                                                    
189330                                                                          
189340     MOVE LOW-VALUE              TO W-WDA601KY-MIN-X.                     
189350     MOVE HIGH-VALUE             TO W-WDA601KY-MAX-X.                     
189360     MOVE KORD-IDDISTR           TO W-A601KY-MIN-IDDISTR                  
189370                                    W-A601KY-MAX-IDDISTR                  
189380     MOVE KORD-IDKUNDNR          TO W-A601KY-MIN-IDKUNDNR                 
189390                                    W-A601KY-MAX-IDKUNDNR                 
189393     MOVE KORD-IDORDNR5          TO W-A601KY-MIN-IDORDNR                  
189394                                    W-A601KY-MAX-IDORDNR                  
189395                                                                          
189396     MOVE NEJ                    TO SW-TIKLAR-JUSTERAD                    
189397                                                                          
189398     PERFORM IMS-GHN-WDA6B                                                
189401     PERFORM UNTIL SEGMENT-SAKNAS                                         
189402                OR BASEN-SLUT                                             
189403                OR SW-TIKLAR-JUSTERAD = JA                                
189404                                                                          
189405         IF  VOR-IDARTNR = ORAD-IDARTNR                                   
189406         AND VOR-TIKLAR > +0                                              
189407                                                                          
189408             MOVE ZERO           TO VOR-TIKLAR                            
189409                                    VOR-TIKLATID                          
189410             PERFORM IMS-REPL-WDA6B                                       
189411             MOVE JA             TO SW-TIKLAR-JUSTERAD                    
189412         END-IF                                                           
189413                                                                          
189414         PERFORM IMS-GHN-WDA6B                                            
189415     END-PERFORM                                                          
189416     .                                                                    
189417                                                                          
189420 ED-UPPDATERA-PRODTAB      SECTION.                                       
189500     SKIP3                                                                
189600     IF  FL-RADSTA-BACKAD = JA                                            
189700*      * RAD-INTERVALLET HAR FÄRDIGPACKADE ORADER                         
189800                                                                          
189900       MOVE WS-IDPRODNR-N        TO W-411-IDPRODNR                        
190000       MOVE ARB-RAD              TO W-411-IDPURAD                         
190100       PERFORM IMS-GU-WDE42-KORD-BSEQ                                     
190200                                                                          
190300       PERFORM EDA-LAES-SHIFTTAB                                          
190400                                                                          
190500       MOVE KORD-IDORDER         TO W-301-IDORDER                         
190600       MOVE KORD-IDDC            TO W-301-IDDC                            
190700       MOVE KORD-IDPRODNR        TO W-301-IDPRODNR                        
190800       MOVE KORD-IDPLKLST        TO W-301-IDPLKLST                        
190900       PERFORM IMS-GU-ORQA01                                              
191000                                                                          
191100*        * PRODTAB UPPDATERAS FÖR ALLA PRODKL (MARS 2013)                 
191200       IF  ODEL-KDPRODKL = 'B'                                            
191300       OR  ODEL-KDPRODKL = 'C'                                            
191400*        * PRODTAB UPPDATERAS ENDAST FÖR PRODKL B OCH C.                  
191500                                                                          
191600         MOVE KORD-IDDC          TO W-IDDC-4471                           
191700         MOVE ODEL-IDPRCBAS      TO W-IDPRCBAS-4471                       
191800         MOVE ODEL-IDPRCVAR      TO W-IDPRCVAR-4471                       
191900         PERFORM IMS-GHU-XXKW11                                           
192000                                                                          
192100         IF  SEGMENT-FINNS                                                
192200*          * PRODTAB UPPDATERAS ENDAST OM ORDERDELENS PRC FINNS.          
192300                                                                          
192400           MOVE 1                TO IND1                                  
192500           MOVE W-IDSHIFT-4478   TO IND2                                  
192600           MOVE ODEL-DARFS       TO HJALP-ODEL-DARFS                      
192700           MOVE 4472-TIRFS (IND1) TO HJALP-4472-TIRFS                     
192800                                                                          
192900           PERFORM UNTIL IND1 = 30 OR                                     
193000                         4472-TIRFS (IND1) = ZERO OR                      
193100                         HJALP-ODEL-DARFS-6 = HJALP-4472-TIRFS-6          
193200             ADD 1                TO IND1                                 
193300             MOVE 4472-TIRFS (IND1) TO HJALP-4472-TIRFS                   
193400           END-PERFORM                                                    
193500                                                                          
193600           MOVE ODEL-DARFS (3:10) TO 4472-TIRFS (IND1)                    
193700           MOVE W-IDSHIFT-4478  TO 4472-IDSHIFT (IND1, IND2)              
193800           SUBTRACT 1         FROM 4472-KVRADER-PRAPP (IND1, IND2)        
193900           PERFORM EDB-SUBTR-TOTAL-PRODTID                                
194000           PERFORM IMS-REPL-XXKW11                                        
194100         END-IF                                                           
194200       END-IF                                                             
194300     END-IF                                                               
194400     .                                                                    
194500     EJECT                                                                
194600 EDA-LAES-SHIFTTAB         SECTION.                                       
194700*                                                                         
194800     MOVE KORD-IDDC         TO W-IDDC-4477                                
194900     MOVE '1'               TO W-IDSHIFT-4478                             
195000     MOVE KORD-IDUSER       TO W-IDUSER-4478                              
195100     PERFORM IMS-GU-XXLB                                                  
195200*                                                                         
195300     IF SEGMENT-SAKNAS                                                    
195400        MOVE '2'            TO W-IDSHIFT-4478                             
195500        PERFORM IMS-GU-XXLB                                               
195600*                                                                         
195700        IF SEGMENT-SAKNAS                                                 
195800           MOVE '3'         TO W-IDSHIFT-4478                             
195900           PERFORM IMS-GU-XXLB                                            
196000*                                                                         
196100           IF SEGMENT-SAKNAS                                              
196200              MOVE '1'      TO W-IDSHIFT-4478                             
196300           END-IF                                                         
196400        END-IF                                                            
196500     END-IF                                                               
196600     .                                                                    
196700     EJECT                                                                
196800 EDB-SUBTR-TOTAL-PRODTID             SECTION.                             
196900                                                                          
197000     MOVE ODEL-KVPTID                     TO WS-KVPTID-MIN                
197100                                                                          
197200     MOVE 4472-SUPTID-PRAPP (IND1, IND2)  TO WS-SUPTID-PRAPP              
197300                                                                          
197400     COMPUTE WS-SUPTID-PRAPP-MIN-TOT = WS-SUPTID-TIM * 60                 
197500                                                                          
197600     ADD WS-SUPTID-MIN        TO WS-SUPTID-PRAPP-MIN-TOT                  
197700     SUBTRACT WS-KVPTID-MIN FROM WS-SUPTID-PRAPP-MIN-TOT                  
197800                                                                          
197900     IF WS-SUPTID-PRAPP-MIN-TOT NEGATIVE                                  
198000        MOVE 0 TO WS-SUPTID-PRAPP-MIN-TOT                                 
198100     END-IF                                                               
198200                                                                          
198300     DIVIDE WS-SUPTID-PRAPP-MIN-TOT BY 60 GIVING WS-SUPTID-TIM            
198400     COMPUTE WS-SUPTID-PRAPP-MIN-TOT = WS-SUPTID-PRAPP-MIN-TOT -          
198500                                      (WS-SUPTID-TIM * 60)                
198600                                                                          
198700     MOVE WS-SUPTID-PRAPP-MIN-TOT TO WS-SUPTID-MIN                        
198800                                                                          
198900     MOVE WS-SUPTID-PRAPP TO 4472-SUPTID-PRAPP (IND1, IND2)               
199000     .                                                                    
199100     EJECT                                                                
199260 F-UPPDATERA-KOLLIREG      SECTION.                                       
199300     MOVE WS-IDKOLLI   TO  W-611-IDKOLLI                                  
199400     PERFORM IMS-GHU-KOLLI                                                
199500*                                                                         
199600     IF SEGMENT-FINNS                                                     
199700         PERFORM S11-UPPD-KOLLI-FRAN-ARB                                  
199800         MOVE WS-KVORDRAD         TO   KOLLI-KVORDRAD                     
199900         MOVE KOLLI-KVORDRAD      TO   MOD-KVORDRAD-SPAR                  
200000                                                                          
200100         IF  KOLLI-KVORDRAD = ZERO                                        
200310             IF  KOLLI-KDKOLSTA   = +1                                    
200400                 MOVE JA          TO KOLLI-KDKOLSTA-1-BORT                
200500                 MOVE KOLLI-VKORDBTO-KOLLI                                
200600                                  TO WS-KOLLI-VKORDBTO-KOLLI              
200700                 MOVE KOLLI-VLORDBTO-KOLLI                                
200800                                  TO WS-KOLLI-VLORDBTO-KOLLI              
200900             END-IF                                                       
201000             ADD +1 TO WS-KVKOLLI-BORT                                    
201100             MOVE JA              TO   KOLLI-BORTTAGET                    
201200             MOVE RAETT-2 (INDX)  TO   MOD-TEMFSINF                       
201300             PERFORM IMS-DLET-KOLLI                                       
201400             PERFORM FA-TA-BORT-KOLLI-FOR-EMA                             
201500             PERFORM FB-TA-BORT-LAS-SEGM                                  
201600         ELSE                                                             
201700             MOVE RAETT-1 (INDX)  TO   MOD-TEMFSINF                       
201800             PERFORM IMS-REPL-KOLLI                                       
201900         END-IF                                                           
202000         IF  KOLLI-KVORDRAD = MID-KVORDRAD-SPAR                           
202100             MOVE RAETT-4 (INDX)  TO   MOD-TEMFSINF                       
202200         END-IF                                                           
202300     END-IF                                                               
202400*                                                                         
202500     PERFORM IMS-GHU-KOLLIREG                                             
202600     IF SEGMENT-FINNS                                                     
202700         COMPUTE VORD-KVORDRAD-PACK                                       
202800                         = VORD-KVORDRAD-PACK - WS-TOT-ANT-RADER          
202900         IF VORD-KVORDRAD-PACK = ZERO                                     
203000             MOVE 1        TO  VORD-KDORDSTA                              
203100         END-IF                                                           
203200                                                                          
203330         IF  VORD-KVKOLPAC = ZERO                                         
203400           CONTINUE                                                       
203500         ELSE                                                             
203600           SUBTRACT WS-KVKOLLI-BORT FROM VORD-KVKOLPAC                    
203700         END-IF                                                           
203800*                                                                         
203900         IF WS-KDKOLSTA > 0                                               
204000            COMPUTE VORD-SUORDV-PACK-LOC =                                
204100                  VORD-SUORDV-PACK-LOC - ARB-KOLLI-SUORDV-LOC             
204110            IF VORD-SUORDV-PACK-LOC  < ZERO                               
204120              MOVE ZERO         TO VORD-SUORDV-PACK-LOC                   
204130            END-IF                                                        
204140                                                                          
204200            COMPUTE VORD-SUORDV-PACK-LOCPREL =                            
204300                   VORD-SUORDV-PACK-LOCPREL -                             
204400                                     ARB-KOLLI-SUORDV-LOCPREL             
204410            IF VORD-SUORDV-PACK-LOCPREL < ZERO                            
204420              MOVE ZERO         TO VORD-SUORDV-PACK-LOCPREL               
204430            END-IF                                                        
204440                                                                          
204500            COMPUTE VORD-SUORDV-PACK =                                    
204600                 VORD-SUORDV-PACK - ARB-KOLLI-SUORDV                      
204610            IF VORD-SUORDV-PACK-LOCPREL < ZERO                            
204620              MOVE ZERO         TO VORD-SUORDV-PACK                       
204630            END-IF                                                        
204700         END-IF                                                           
204810*                                                                         
204920         IF KOLLI-KDKOLSTA-1-BORT = JA                                    
205000            SUBTRACT WS-KOLLI-VKORDBTO-KOLLI                              
205100                              FROM  VORD-VKORDBTO                         
205110            IF VORD-VKORDBTO < ZERO                                       
205120              MOVE ZERO         TO VORD-VKORDBTO                          
205130            END-IF                                                        
205140                                                                          
205200            SUBTRACT WS-KOLLI-VLORDBTO-KOLLI                              
205300                              FROM  VORD-VLORDBTO                         
205310            IF VORD-VLORDBTO < ZERO                                       
205320              MOVE ZERO         TO VORD-VLORDBTO                          
205330            END-IF                                                        
205340                                                                          
205400            SUBTRACT 1            FROM VORD-KVKOLLI                       
205500         END-IF                                                           
205600*                                                                         
205700         PERFORM IMS-REPL-KOLLIREG                                        
205800*                                                                         
205900     ELSE                                                                 
206000*                                                                         
206100         MOVE FEL          TO  WS-BEHANDLING-TEST                         
206200     END-IF                                                               
206300     .                                                                    
206503                                                                          
206520     EJECT                                                                
206600 FA-TA-BORT-KOLLI-FOR-EMA SECTION.                                        
206700     PERFORM IMS-GHN-XXJK11                                               
206800                                                                          
206900     PERFORM UNTIL SEGMENT-SAKNAS                                         
207000       MOVE WS-IDKOLLI  TO WS-IDKOLLI-NUM                                 
207100                                                                          
207200       IF 4322-IDPRODNR = WS-IDPRODNR-N     AND                           
207300          4322-IDKOLLI  = WS-IDKOLLI-NUM                                  
207400         PERFORM IMS-DLET-XXJK                                            
207500       END-IF                                                             
207600                                                                          
207700       PERFORM IMS-GHN-XXJK11                                             
207800     END-PERFORM                                                          
207900     .                                                                    
208000     SKIP2                                                                
208100 FB-TA-BORT-LAS-SEGM  SECTION.                                            
208200                                                                          
208300     MOVE WS-IDPRODNR   TO W-4301-IDPRODNR                                
208400     PERFORM IMS-GHU-XXDU01                                               
208500     IF SEGMENT-FINNS                                                     
208600        MOVE WS-IDKOLLI  TO W-4302-IDKOLLI                                
208700                                                                          
208800        PERFORM IMS-GHNP-XXDU11-IDKOLLI                                   
208900        PERFORM UNTIL SEGMENT-SAKNAS                                      
209000           PERFORM IMS-DLET-XXDU                                          
209100           PERFORM IMS-GHNP-XXDU11-IDKOLLI                                
209200        END-PERFORM                                                       
209300                                                                          
209400        PERFORM IMS-GNP-XXDU11-FIRST                                      
209500        IF SEGMENT-SAKNAS                                                 
209600           PERFORM IMS-GHU-XXDU01                                         
209700           PERFORM IMS-DLET-XXDU                                          
209800        END-IF                                                            
209900     END-IF                                                               
210000     .                                                                    
210100     EJECT                                                                
210200 H-AVSLUT             SECTION.                                            
210300     SKIP3                                                                
210400     IF WS-BEHANDLING-RATT                                                
210500        CONTINUE                                                          
210600     ELSE                                                                 
210700        PERFORM IMS-ROLLBACK                                              
210800     END-IF                                                               
210900     .                                                                    
211000     EJECT                                                                
211100* MFS SEKTIONER                                                           
211200     SKIP3                                                                
211300 S02-RENSA-FALT       SECTION.                                            
211400     SKIP3                                                                
211500     MOVE +1 TO BILD-RAD                                                  
211600     PERFORM UNTIL BILD-RAD NOT < MAX-ANTAL-RADER-PLUS-1                  
211700         MOVE MFS-RENSA-FAELT   TO MOD-FLBACKA     (BILD-RAD)             
211800                                   MOD-IDRADNR     (BILD-RAD)             
211900                                   MOD-KVLEVART    (BILD-RAD)             
212000         MOVE MFS-STAENG-FAELT                                            
212100                                TO MOD-FLBACKA-ATTR (BILD-RAD)            
212200         ADD +1 TO BILD-RAD                                               
212300     END-PERFORM                                                          
212400     MOVE MFS-RENSA-FAELT       TO MOD-FLBACKA-ALLA                       
212500                                   MOD-IDRADNR-START                      
212600     MOVE ZERO                  TO MOD-IDRADNR-SPAR                       
212700                                   MOD-KVORDRAD-SPAR                      
212800     .                                                                    
212900     EJECT                                                                
213000 S05-SKRIV-MOD-RAD    SECTION.                                            
213100                                                                          
213200     MOVE ARB-RAD               TO  MOD-IDRADNR     (RAD-INX)             
213300     INSPECT MOD-IDRADNR     (RAD-INX) REPLACING LEADING ZERO             
213400                                       BY SPACE                           
213500     IF ARB-KVLEVART = ZERO                                               
213600         MOVE SPACE             TO  MOD-KVLEVART (RAD-INX)                
213700     ELSE                                                                 
213800         MOVE ARB-KVLEVART      TO  MOD-KVLEVART (RAD-INX)                
213900     END-IF                                                               
214000     INSPECT MOD-KVLEVART (RAD-INX) REPLACING LEADING ZERO                
214100                                       BY SPACE                           
214200     MOVE MFS-OEPPNA-ALFA-FAELT TO  MOD-FLBACKA-ATTR (RAD-INX)            
214300     MOVE MFS-RENSA-FAELT       TO  MOD-FLBACKA      (RAD-INX)            
214400     .                                                                    
214500     EJECT                                                                
214600 S10-UPPD-SPAR-KOLLI    SECTION.                                          
214700                                                                          
214800     COMPUTE ARB-KOLLI-VKORDNTO ROUNDED = ARB-KOLLI-VKORDNTO +            
214900                    SPAR-PRAD-VKARTNTO * SPAR-PRAD-KVLEVART               
215000*                                                                         
215100     MOVE  WS-IDDISTR-N      TO TEST-IDDISTR                              
215200                                                                          
215300     IF DIST79-DEALER-PRICE                                               
215400      IF  ORAD-PRARTNTO-LOCPREL > 0                                       
215500       COMPUTE ARB-KOLLI-SUORDV-LOCPREL = ARB-KOLLI-SUORDV-LOCPREL        
215600           + SPAR-PRAD-PRARTNTO-LOCPREL * SPAR-PRAD-KVLEVART              
215700      ELSE                                                                
215800       COMPUTE ARB-KOLLI-SUORDV-LOC = ARB-KOLLI-SUORDV-LOC                
215900           + SPAR-PRAD-PRARTNTO-LOC * SPAR-PRAD-KVLEVART                  
216000      END-IF                                                              
216100     ELSE                                                                 
216102       IF DIST79-ECOM-PRICE                                               
216103         COMPUTE ARB-KOLLI-SUORDV-LOC = ARB-KOLLI-SUORDV-LOC              
216104             + SPAR-PRAD-PRARTNTO-LOC * SPAR-PRAD-KVLEVART                
216110       ELSE                                                               
216200         COMPUTE ARB-KOLLI-SUORDV = ARB-KOLLI-SUORDV +                    
216300             SPAR-PRAD-PRARTNTO * SPAR-PRAD-KVLEVART                      
216400       END-IF                                                             
216410     END-IF                                                               
216500*                                                                         
216600     IF SPAR-ORAD-IDPSN > ZERO                                            
216700       MOVE +1 TO TAB-INDX                                                
216800                                                                          
216900       PERFORM UNTIL TAB-INDX > MAX-FG-INDX                               
217000         IF TAB-IDPSN(TAB-INDX) = 0                                       
217100           MOVE SPAR-ORAD-IDPSN TO TAB-IDPSN(TAB-INDX)                    
217200           PERFORM S10A-BERAEKNA-FG-DATA                                  
217300           MOVE +10 TO TAB-INDX                                           
217400                                                                          
217500         ELSE                                                             
217600           IF SPAR-ORAD-IDPSN = TAB-IDPSN(TAB-INDX)                       
217700             PERFORM S10A-BERAEKNA-FG-DATA                                
217800             MOVE +10 TO TAB-INDX                                         
217900           END-IF                                                         
218000         END-IF                                                           
218100                                                                          
218200         ADD +1 TO TAB-INDX                                               
218300       END-PERFORM                                                        
218400     END-IF                                                               
218500     .                                                                    
218600     EJECT                                                                
218700 S10A-BERAEKNA-FG-DATA SECTION.                                           
218800     SKIP3                                                                
218900     COMPUTE TAB-VLFG(TAB-INDX) = TAB-VLFG(TAB-INDX)   +                  
219000                                  (SPAR-ORAD-VLFG      *                  
219100                                   SPAR-PRAD-KVLEVART)                    
219200     IF SPAR-ORAD-IDPSN = 10 OR 11                                        
219300       COMPUTE TAB-VKART-FG(TAB-INDX) = TAB-VKART-FG(TAB-INDX) +          
219400                                        (SPAR-ORAD-VKART-FG    *          
219500                                         SPAR-PRAD-KVLEVART)              
219600     ELSE                                                                 
219700       MOVE ZERO TO TAB-VKART-FG(TAB-INDX)                                
219800     END-IF                                                               
219900                                                                          
220000     COMPUTE TOTAL-SUEQFG = TOTAL-SUEQFG        +                         
220100                            (SPAR-ORAD-SUEQFG   *                         
220200                             SPAR-PRAD-KVLEVART)                          
220300     .                                                                    
220400     EJECT                                                                
220500 S11-UPPD-KOLLI-FRAN-ARB   SECTION.                                       
220600     SKIP3                                                                
220700     SUBTRACT ARB-KOLLI-KVORDRAD FROM KOLLI-KVORDRAD                      
220800     SUBTRACT ARB-KOLLI-VKORDNTO FROM KOLLI-VKORDNTO-KOLLI                
220900*                                                                         
221000     SUBTRACT ARB-KOLLI-SUORDV-LOC FROM KOLLI-SUORDV-LOC                  
221100     SUBTRACT ARB-KOLLI-SUORDV-LOCPREL                                    
221200                                   FROM KOLLI-SUORDV-LOCPREL              
221300     SUBTRACT ARB-KOLLI-SUORDV     FROM KOLLI-SUORDV-KOLLI                
221400*                                                                         
221500     IF WS-KVFALRAD = ZERO                                                
221600         MOVE ZERO               TO KOLLI-KDFARLIG-KOLLI                  
221700     END-IF                                                               
221800     MOVE WS-KVFALRAD            TO KOLLI-KVFALRAD                        
221900*                                                                         
222000     MOVE +1 TO TAB-INDX                                                  
222100     PERFORM UNTIL TAB-INDX > MAX-FG-INDX                                 
222200       IF TAB-IDPSN(TAB-INDX) > ZERO                                      
222300         MOVE +1 TO FG-INDX                                               
222400                                                                          
222500         PERFORM UNTIL FG-INDX > MAX-FG-INDX                              
222600           IF TAB-IDPSN(TAB-INDX) = KOLLI-IDPSN(FG-INDX)                  
222700             SUBTRACT TAB-VLFG(TAB-INDX) FROM                             
222800                      KOLLI-VLFG(FG-INDX)                                 
222900                                                                          
223000             IF KOLLI-IDPSN(FG-INDX) = 10                                 
223100               SUBTRACT TAB-VKART-FG(TAB-INDX) FROM                       
223200                        KOLLI-VKART-FG(FG-INDX)                           
223300             END-IF                                                       
223400             IF KOLLI-VLFG(FG-INDX) = ZERO                                
223500               MOVE ZERO TO KOLLI-IDPSN(FG-INDX)                          
223600             END-IF                                                       
223700                                                                          
223800             MOVE +10 TO FG-INDX                                          
223900           END-IF                                                         
224000           ADD +1 TO FG-INDX                                              
224100         END-PERFORM                                                      
224200       END-IF                                                             
224300       ADD +1 TO TAB-INDX                                                 
224400     END-PERFORM                                                          
224500                                                                          
224600     IF TOTAL-SUEQFG > ZERO                                               
224700       SUBTRACT TOTAL-SUEQFG FROM KOLLI-SUEQFG                            
224800     END-IF                                                               
224900     .                                                                    
225000     EJECT                                                                
225100                                                                          
225200                                                                          
225300 S12-DATA-TILL-DEL-NOTE SECTION.                                          
225400                                                                          
225500     MOVE WS-IDDISTR-N               TO TEST-IDDISTR                      
225600     IF DIST07-USA-RETAILER-DNOTE                                         
225700     OR DIST07-CAN-RETAILER                                               
225800        INITIALIZE DNOT-ORDER-INFO                                        
225900                                                                          
226000        MOVE PROGRAM-NAMN             TO DNOT-IDPGM                       
226100        MOVE WS-DNOT-IDORDER          TO DNOT-IDORDER                     
226200        MOVE WS-DNOT-IDARTNR          TO DNOT-IDARTNR                     
226300        MOVE WS-DNOT-IDDC             TO DNOT-IDDC                        
226400        MOVE WS-DNOT-IDKOLLI          TO DNOT-IDKOLLI-BORT                
226500        MOVE WS-DNOT-IDPURAD          TO DNOT-IDPURAD                     
226600                                                                          
226700        CALL W411DNOT USING DNOT-W411DNOT                                 
226800                            DNOT-ORQP-PCB                                 
226900                            DNOT-ORQP2-PCB                                
227000                            DNOT-ORQP3-PCB                                
227100                            DNOT-4013-PCB                                 
227200                            DNOT-BENA-PCB                                 
227300     END-IF                                                               
227400     .                                                                    
227500     EJECT                                                                
227600                                                                          
227700                                                                          
227800* IMS SEKTIONER                                                           
227900     SKIP3                                                                
228000 IMS-GET-MSG SECTION.                                                     
228100                                                                          
228200     MOVE '  QC' TO GODK-STATUSKODER                                      
228300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
228400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
228500     PERFORM IMS-STATUSKONTROLL                                           
228600     SKIP3                                                                
228700     .                                                                    
228800 IMS-INSERT-MSG SECTION.                                                  
228900                                                                          
229000     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
229100       MOVE '0' TO MFS-KDHUVOMR                                           
229200     END-IF                                                               
229300*                                                                         
229400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
229500     MOVE SPACE TO GODK-STATUSKODER                                       
229600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
229700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
229800     PERFORM IMS-STATUSKONTROLL                                           
229900     SKIP3                                                                
230000     .                                                                    
230100 IMS-GU-KUNDORDER SECTION.                                                
230200     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
230300            DELIMITED BY SIZE INTO SSA1                                   
230400     MOVE '    ' TO GODK-STATUSKODER                                      
230500     CALL CBLTDLI USING GU     WDE41-PCB DLI-IO-E401 SSA1                 
230600     MOVE WDE41-STATUS-CODE TO STATUS-WS                                  
230700     PERFORM IMS-STATUSKONTROLL                                           
230800     SKIP2                                                                
230900     .                                                                    
231000 IMS-GNP-RAD       SECTION.                                               
231100     STRING 'WDE411  (IDPURAD  =' W-WDE411-IDPURAD-X ')'                  
231200            DELIMITED BY SIZE INTO SSA1                                   
231300     MOVE '  GE' TO GODK-STATUSKODER                                      
231400     CALL CBLTDLI USING GNP    WDE41-PCB DLI-IO-E411 SSA1                 
231500     MOVE WDE41-STATUS-CODE TO STATUS-WS                                  
231600                              STATUS-RAD-WS                               
231700     PERFORM IMS-STATUSKONTROLL                                           
231800     SKIP2                                                                
231900     .                                                                    
232000 IMS-GNP-RAD-OKVAL SECTION.                                               
232100     MOVE 'WDE411  ' TO SSA1                                              
232200     MOVE '  GE' TO GODK-STATUSKODER                                      
232300     CALL CBLTDLI USING GNP    WDE41-PCB DLI-IO-E411 SSA1                 
232400     MOVE WDE41-STATUS-CODE TO STATUS-WS                                  
232500                              STATUS-RAD-WS                               
232600     PERFORM IMS-STATUSKONTROLL                                           
232700     .                                                                    
232800     SKIP2                                                                
232900 IMS-GNP-KKOLLI-KVAL SECTION.                                             
233000     STRING 'WDE421  (WDE421KY =' W-WDE421-IDKOLLI-X ')'                  
233100            DELIMITED BY SIZE INTO SSA1                                   
233200     MOVE '  GE' TO GODK-STATUSKODER                                      
233300     CALL CBLTDLI USING GNP    WDE41-PCB DLI-IO-E421 SSA1                 
233400     MOVE WDE41-STATUS-CODE TO STATUS-WS                                  
233500     PERFORM IMS-STATUSKONTROLL                                           
233600     .                                                                    
233700     SKIP2                                                                
233800 IMS-GNP-RAD-FOM   SECTION.                                               
233900     STRING 'WDE411  (IDPURAD >=' W-WDE411-IDPURAD-X ')'                  
234000            DELIMITED BY SIZE INTO SSA1                                   
234100     MOVE '  GE' TO GODK-STATUSKODER                                      
234200     CALL CBLTDLI USING GNP    WDE41-PCB DLI-IO-E411 SSA1                 
234300     MOVE WDE41-STATUS-CODE TO STATUS-WS                                  
234400                              STATUS-RAD-WS                               
234500     PERFORM IMS-STATUSKONTROLL                                           
234600     .                                                                    
234700     SKIP2                                                                
234800 IMS-GU-WDE601    SECTION.                                                
234900                                                                          
235000     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
235100            DELIMITED BY SIZE INTO SSA1                                   
235200     MOVE '  GE' TO GODK-STATUSKODER                                      
235300     CALL CBLTDLI USING GU    WDE6-PCB DLI-IO-AREA3 SSA1                  
235400     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
235500     PERFORM IMS-STATUSKONTROLL                                           
235600     SKIP3                                                                
235700     .                                                                    
235800     EJECT                                                                
235900 IMS-GU-KUNDORDER-SEK-INV-GE SECTION.                                     
236000     STRING 'WDE411  (WDE4BSEQ>=' W-WDE4B-KEYSEQ-MIN-X                    
236100                    '&WDE4BSEQ<=' W-WDE4B-KEYSEQ-MAX-X ')'                
236200            DELIMITED BY SIZE INTO SSA1                                   
236300     MOVE 'WDE401  ' TO SSA2                                              
236400     MOVE '  GEGB' TO GODK-STATUSKODER                                    
236500     CALL CBLTDLI USING GU   WDE42-PCB DLI-IO-E401 SSA1 SSA2              
236600     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
236700     PERFORM IMS-STATUSKONTROLL                                           
236800     SKIP2                                                                
236900     .                                                                    
237000 IMS-GU-WDE42-KORD-BSEQ SECTION.                                          
237100     STRING 'WDE411  (WDE4BSEQ =' W-WDE4B-KEYSEQ-X ')'                    
237200            DELIMITED BY SIZE INTO SSA1                                   
237300     MOVE 'WDE401  ' TO SSA2                                              
237400     MOVE '  ' TO GODK-STATUSKODER                                        
237500     CALL CBLTDLI USING GU   WDE42-PCB DLI-IO-E401 SSA1 SSA2              
237600     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
237700     PERFORM IMS-STATUSKONTROLL                                           
237800     SKIP2                                                                
237900     .                                                                    
238000 IMS-GHU-RAD-SEK    SECTION.                                              
238100     STRING 'WDE411  (WDE4BSEQ =' W-WDE4B-KEYSEQ-X ')'                    
238200            DELIMITED BY SIZE INTO SSA1                                   
238300     MOVE '    ' TO GODK-STATUSKODER                                      
238400     CALL CBLTDLI USING GHU    WDE4-PCB DLI-IO-E411 SSA1                  
238500     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
238600     PERFORM IMS-STATUSKONTROLL                                           
238700     SKIP2                                                                
238800     .                                                                    
238900 IMS-REPL-RAD           SECTION.                                          
239000     MOVE '    ' TO GODK-STATUSKODER                                      
239100     CALL CBLTDLI USING REPL WDE4-PCB DLI-IO-E411                         
239200     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
239300     PERFORM IMS-STATUSKONTROLL                                           
239400     SKIP2                                                                
239500     .                                                                    
239600 IMS-GHN-KOLLI-KOPPL-SEK SECTION.                                         
239700     STRING 'WDE411  (WDE4BSEQ =' W-WDE4B-KEYSEQ-X ')'                    
239800            DELIMITED BY SIZE INTO SSA1                                   
239900     STRING 'WDE421  (WDE421KY =' W-WDE421-IDKOLLI-X ')'                  
240000            DELIMITED BY SIZE INTO SSA2                                   
240100     MOVE '  GE' TO GODK-STATUSKODER                                      
240200     CALL CBLTDLI USING GHN    WDE4-PCB DLI-IO-E421 SSA1 SSA2             
240300     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
240400     PERFORM IMS-STATUSKONTROLL                                           
240500     SKIP2                                                                
240600     .                                                                    
240700 IMS-DLET-KOLLI-KOPPL  SECTION.                                           
240800     MOVE 'WDE421   ' TO SSA1                                             
240900     MOVE '  '   TO GODK-STATUSKODER                                      
241000     CALL CBLTDLI USING DLET WDE4-PCB DLI-IO-E421 SSA1                    
241100     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
241200     PERFORM IMS-STATUSKONTROLL                                           
241300     SKIP2                                                                
241400     .                                                                    
241500 IMS-GHU-KOLLIREG SECTION.                                                
241600     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
241700            DELIMITED BY SIZE INTO SSA1                                   
241800     MOVE '  GE' TO GODK-STATUSKODER                                      
241900     CALL CBLTDLI USING GHU    WDE6-PCB DLI-IO-AREA3 SSA1                 
242000     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
242100     PERFORM IMS-STATUSKONTROLL                                           
242200     SKIP2                                                                
242300     .                                                                    
242400 IMS-REPL-KOLLIREG SECTION.                                               
242500     MOVE '    ' TO GODK-STATUSKODER                                      
242600     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-AREA3                        
242700     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
242800     PERFORM IMS-STATUSKONTROLL                                           
242900     .                                                                    
243000     EJECT                                                                
243100 IMS-GHU-KOLLI    SECTION.                                                
243200     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
243300            DELIMITED BY SIZE INTO SSA1                                   
243400     STRING 'WDE611  (IDKOLLI  =' W-WDE611-IDKOLLI-X ')'                  
243500            DELIMITED BY SIZE INTO SSA2                                   
243600     MOVE '  GE' TO GODK-STATUSKODER                                      
243700     CALL CBLTDLI USING GHU    WDE64-PCB DLI-IO-AREA3 SSA1 SSA2           
243800     MOVE WDE64-STATUS-CODE TO STATUS-WS                                  
243900     PERFORM IMS-STATUSKONTROLL                                           
244000     SKIP2                                                                
244100     .                                                                    
244200 IMS-REPL-KOLLI    SECTION.                                               
244300     MOVE '    ' TO GODK-STATUSKODER                                      
244400     CALL CBLTDLI USING REPL WDE64-PCB DLI-IO-AREA3                       
244500     MOVE WDE64-STATUS-CODE TO STATUS-WS                                  
244600     PERFORM IMS-STATUSKONTROLL                                           
244700     SKIP2                                                                
244800     .                                                                    
244900 IMS-DLET-KOLLI    SECTION.                                               
245000     MOVE '    ' TO GODK-STATUSKODER                                      
245100     CALL CBLTDLI USING DLET WDE64-PCB DLI-IO-AREA3                       
245200     MOVE WDE64-STATUS-CODE TO STATUS-WS                                  
245300     PERFORM IMS-STATUSKONTROLL                                           
245400     SKIP2                                                                
245500     .                                                                    
245600     EJECT                                                                
245700 IMS-GU-KUNDORDER-SEK SECTION.                                            
245800     STRING 'WDE401  (WDE4ASEQ =' W-WDE4A1-KUNDORDER-X ')'                
245900            DELIMITED BY SIZE INTO SSA1                                   
246000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
246100     CALL CBLTDLI USING GU     WDE4A-PCB DLI-IO-E401 SSA1                 
246200     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
246300                               STATUS-KUNDORDER-SEK-WS                    
246400     PERFORM IMS-STATUSKONTROLL                                           
246500     SKIP2                                                                
246600     .                                                                    
246700 IMS-GN-KUNDORDER-SEK SECTION.                                            
246800     STRING 'WDE401  (WDE4ASEQ =' W-WDE4A1-KUNDORDER-X ')'                
246900            DELIMITED BY SIZE INTO SSA1                                   
247000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
247100     CALL CBLTDLI USING GN     WDE4A-PCB DLI-IO-E401 SSA1                 
247200     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
247300                               STATUS-KUNDORDER-SEK-WS                    
247400     PERFORM IMS-STATUSKONTROLL                                           
247500     .                                                                    
247600     EJECT                                                                
247700 IMS-GHN-XXJK11   SECTION.                                                
247800     STRING 'WLXXJK01(WDGXKEY  =' W-WDGXKEY-4321-X ')'                    
247900            DELIMITED BY SIZE INTO SSA1                                   
248000     MOVE 'WLXXJK11'         TO SSA2                                      
248100     MOVE '  GEGB'           TO GODK-STATUSKODER                          
248200     CALL CBLTDLI USING GHN    XXJK-PCB DLI-IO-AREA4 SSA1 SSA2            
248300     MOVE XXJK-STATUS-CODE   TO STATUS-WS                                 
248400     PERFORM IMS-STATUSKONTROLL                                           
248500     SKIP2                                                                
248600                                                                          
248700     .                                                                    
248800 IMS-DLET-XXJK     SECTION.                                               
248900     MOVE '  ' TO GODK-STATUSKODER                                        
249000     CALL CBLTDLI USING DLET XXJK-PCB DLI-IO-AREA4                        
249100     MOVE XXJK-STATUS-CODE TO STATUS-WS                                   
249200     PERFORM IMS-STATUSKONTROLL                                           
249300     .                                                                    
249400     EJECT                                                                
249500 IMS-GU-ORQA01    SECTION.                                                
249600     STRING 'WLORQA01(WDQ301KY =' W-WDQ301-ORDERDEL-X ')'                 
249700            DELIMITED BY SIZE INTO SSA1                                   
249800     MOVE '    ' TO GODK-STATUSKODER                                      
249900     CALL CBLTDLI USING GU     ORQA-PCB DLI-IO-AREA5 SSA1                 
250000     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
250100     PERFORM IMS-STATUSKONTROLL                                           
250200     .                                                                    
250300     EJECT                                                                
250400 IMS-GHU-XXKW11 SECTION.                                                  
250500     STRING 'WLXXKW01(WDGXKEY  =' W-WDGXKEY-4471-X ')'                    
250600            DELIMITED BY SIZE INTO SSA1                                   
250700     STRING 'WLXXKW11(KDSEGKEY =' W-KDSEGKEY-4472-X ')'                   
250800            DELIMITED BY SIZE INTO SSA2                                   
250900     MOVE '    GE'           TO GODK-STATUSKODER                          
251000     CALL CBLTDLI USING GHU    XXKW-PCB DLI-IO-AREA6 SSA1 SSA2            
251100     MOVE XXKW-STATUS-CODE   TO STATUS-WS                                 
251200     PERFORM IMS-STATUSKONTROLL                                           
251300     .                                                                    
251400     SKIP2                                                                
251500 IMS-REPL-XXKW11   SECTION.                                               
251600     MOVE '    ' TO GODK-STATUSKODER                                      
251700     CALL CBLTDLI USING REPL XXKW-PCB DLI-IO-AREA6                        
251800     MOVE XXKW-STATUS-CODE TO STATUS-WS                                   
251900     PERFORM IMS-STATUSKONTROLL                                           
252000     .                                                                    
252100     EJECT                                                                
252200 IMS-GU-XXLB      SECTION.                                                
252300     STRING 'WLXXLB01(WDGXKEY  =' W-WDGXKEY-4477-X ')'                    
252400            DELIMITED BY SIZE INTO SSA1                                   
252500     STRING 'WLXXLB11(WDGXKEY  =' W-WDGXKEY-4478-X ')'                    
252600            DELIMITED BY SIZE INTO SSA2                                   
252700     MOVE '  GE'             TO GODK-STATUSKODER                          
252800     CALL CBLTDLI USING GU     XXLB-PCB DLI-IO-AREA6 SSA1 SSA2            
252900     MOVE XXLB-STATUS-CODE   TO STATUS-WS                                 
253000     PERFORM IMS-STATUSKONTROLL                                           
253100     .                                                                    
253200     EJECT                                                                
253300 IMS-GU-WDE411-01-BSEQ SECTION.                                           
253400     STRING 'WDE411  (WDE4BSEQ =' W-WDE4B-KEYSEQ-X ')'                    
253500            DELIMITED BY SIZE INTO SSA1                                   
253600     MOVE 'WDE401  ' TO SSA2                                              
253700     MOVE '  GE' TO GODK-STATUSKODER                                      
253800     CALL CBLTDLI USING GU   WDE42-PCB DLI-IO-E401 SSA1 SSA2              
253900     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
254000     PERFORM IMS-STATUSKONTROLL                                           
254100     .                                                                    
254200     SKIP2                                                                
254300 IMS-WDE611-GU-KOLLI SECTION.                                             
254400     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
254500            DELIMITED BY SIZE INTO SSA1                                   
254600     STRING 'WDE611  (IDKOLLI  =' W-WDE611-IDKOLLI-X ')'                  
254700            DELIMITED BY SIZE INTO SSA2                                   
254800     MOVE '  ' TO GODK-STATUSKODER                                        
254900     CALL CBLTDLI USING GU WDE62-PCB DLI-IO-AREA9 SSA1 SSA2               
255000     MOVE WDE62-STATUS-CODE TO STATUS-WS                                  
255100     PERFORM IMS-STATUSKONTROLL                                           
255200     .                                                                    
255300     SKIP2                                                                
255400* LAESN. FÖR KONTROLL OM LAASSEGMENT SKALL TAS BORT                       
255500 IMS-GET-WDE4F SECTION.                                                   
255600     STRING 'WDE4F1  (WDE4F1KY>=' W-WDE4F1KY-MIN-X                        
255700                    '&WDE4F1KY<=' W-WDE4F1KY-MAX-X                        
255800                    '&IDPLKLST =' W-WDEE4F-IDPLKST-X ')'                  
255900            DELIMITED BY SIZE INTO SSA1                                   
256000     MOVE '  GE' TO GODK-STATUSKODER                                      
256100     CALL CBLTDLI USING GN WDE4F-PCB DLI-IO-AREA9 SSA1                    
256200     MOVE WDE4F-STATUS-CODE TO STATUS-WS                                  
256300     PERFORM IMS-STATUSKONTROLL                                           
256400     .                                                                    
256500     EJECT                                                                
256600 IMS-WDE43-GU-ORAD-LAST SECTION.                                          
256700     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
256800            DELIMITED BY SIZE INTO SSA1                                   
256900     MOVE 'WDE411  *L ' TO SSA2                                           
257000     MOVE '  ' TO GODK-STATUSKODER                                        
257100     CALL CBLTDLI USING GU   WDE43-PCB DLI-IO-AREA8 SSA1 SSA2             
257200     MOVE WDE43-STATUS-CODE TO STATUS-WS                                  
257300     PERFORM IMS-STATUSKONTROLL                                           
257400     .                                                                    
257500     EJECT                                                                
257600 IMS-GN-WDE411-21-FSEQ  SECTION.                                          
257700     STRING 'WDE411  *D(WDE4FSEQ =' W-WDE4FSEQ-X ')'                      
257800            DELIMITED BY SIZE INTO SSA1                                   
257900     STRING 'WDE421  (WDE421KY =' W-WDE421-IDKOLLI-X ')'                  
258000            DELIMITED BY SIZE INTO SSA2                                   
258100     MOVE '  GE' TO GODK-STATUSKODER                                      
258200     CALL CBLTDLI USING GN WDE44-PCB DLI-IO-WDE411-21 SSA1 SSA2           
258300     MOVE WDE44-STATUS-CODE TO STATUS-WS                                  
258400     PERFORM IMS-STATUSKONTROLL                                           
258500     .                                                                    
258600     EJECT                                                                
258700 IMS-GN-WDE411-21-FSEQ-SOK  SECTION.                                      
258800     STRING 'WDE411  *D(WDE4FSEQ =' W-WDE4FSEQ-X                          
258900                      '&IDPURAD =>' W-WDE411-IDPURAD-X ')'                
259000            DELIMITED BY SIZE INTO SSA1                                   
259100     STRING 'WDE421  (WDE421KY =' W-WDE421-IDKOLLI-X ')'                  
259200            DELIMITED BY SIZE INTO SSA2                                   
259300     MOVE '  GE' TO GODK-STATUSKODER                                      
259400     CALL CBLTDLI USING GN WDE44-PCB DLI-IO-WDE411-21 SSA1 SSA2           
259500     MOVE WDE44-STATUS-CODE TO STATUS-WS                                  
259600     PERFORM IMS-STATUSKONTROLL                                           
259700     .                                                                    
259800     EJECT                                                                
259900 IMS-GHU-XXDU01      SECTION.                                             
260000                                                                          
260100     STRING 'WLXXDU01(WDGXKEY  =' W-4301-WDGXKEY-X ')'                    
260200            DELIMITED BY SIZE INTO SSA1                                   
260300     MOVE '  GE' TO GODK-STATUSKODER                                      
260400     CALL CBLTDLI USING GHU XXDU-PCB DLI-IO-AREA9 SSA1                    
260500     MOVE XXDU-STATUS-CODE TO STATUS-WS                                   
260600     PERFORM IMS-STATUSKONTROLL                                           
260700     .                                                                    
260800 IMS-GHNP-XXDU11-IDKOLLI         SECTION.                                 
260900*    LÄS MED SAMMA IDKOLLI OCH NÄSTA PLOCKLISTA                           
261000*                                                                         
261100     STRING 'WLXXDU11(IDKOLLI  =' W-4302-IDKOLLI-X ')'                    
261200            DELIMITED BY SIZE INTO SSA1                                   
261300     MOVE '  GE' TO GODK-STATUSKODER                                      
261400     CALL CBLTDLI USING GHNP XXDU-PCB DLI-IO-AREA9 SSA1                   
261500     MOVE XXDU-STATUS-CODE TO STATUS-WS                                   
261600     PERFORM IMS-STATUSKONTROLL                                           
261700     .                                                                    
261800 IMS-GHNP-XXDU11      SECTION.                                            
261900                                                                          
262000     STRING 'WLXXDU11(WDGXKEY  =' W-4302-WDGXKEY-X ')'                    
262100            DELIMITED BY SIZE INTO SSA1                                   
262200     MOVE '  GE' TO GODK-STATUSKODER                                      
262300     CALL CBLTDLI USING GHNP XXDU-PCB DLI-IO-AREA9 SSA1                   
262400     MOVE XXDU-STATUS-CODE TO STATUS-WS                                   
262500     PERFORM IMS-STATUSKONTROLL                                           
262600     .                                                                    
262700 IMS-GNP-XXDU11-FIRST SECTION.                                            
262800                                                                          
262900     MOVE 'WLXXDU11*F '  TO SSA1                                          
263000     MOVE '  GE' TO GODK-STATUSKODER                                      
263100     CALL CBLTDLI USING GNP XXDU-PCB DLI-IO-AREA9 SSA1                    
263200     MOVE XXDU-STATUS-CODE TO STATUS-WS                                   
263300     PERFORM IMS-STATUSKONTROLL                                           
263400     .                                                                    
263500                                                                          
263600 IMS-DLET-XXDU       SECTION.                                             
263700                                                                          
263800     MOVE '  ' TO GODK-STATUSKODER                                        
263900     CALL CBLTDLI USING DLET XXDU-PCB DLI-IO-AREA9                        
264000     MOVE XXDU-STATUS-CODE TO STATUS-WS                                   
264100     PERFORM IMS-STATUSKONTROLL                                           
264200     .                                                                    
264201                                                                          
264210 IMS-GHN-WDA6B SECTION.                                                   
264220     STRING 'WDA601  (WDA6BSEQ>=' W-WDA601KY-MIN-X                        
264230                    '&WDA6BSEQ<=' W-WDA601KY-MAX-X ')'                    
264240            DELIMITED BY SIZE INTO SSA1                                   
264250     MOVE '  GEGB'               TO GODK-STATUSKODER                      
264260     CALL  CBLTDLI  USING GHN   WDA6B-PCB DLI-IO-AREA-WDA6 SSA1           
264270     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
264280     PERFORM IMS-STATUSKONTROLL                                           
264290     .                                                                    
264291 IMS-REPL-WDA6B SECTION.                                                  
264292     MOVE 'WDA601  '           TO SSA1                                    
264293     MOVE '    '               TO GODK-STATUSKODER                        
264294     CALL  CBLTDLI  USING REPL WDA6B-PCB DLI-IO-AREA-WDA6 SSA1            
264295     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
264296     PERFORM IMS-STATUSKONTROLL                                           
264297     .                                                                    
264300 IMS-ROLLBACK    SECTION.                                                 
264400     CALL CBLTDLI USING ROLB    MSG-PCB                                   
264500     .                                                                    
264600     SKIP3                                                                
264700 IMS-STATUSKONTROLL SECTION.                                              
264800     SET STATUS-IX TO 1                                                   
264900     SEARCH GODK-STATUS AT END CALL FELLOG                                
265000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
265100     END-SEARCH                                                           
265200     .                                                                    
