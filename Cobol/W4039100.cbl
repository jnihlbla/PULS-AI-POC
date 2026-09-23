000400 ID DIVISION.                                                             
000500     SKIP2                                                                
000510*                                                                         
000600 PROGRAM-ID.     W4039100.                                                
000700*              PROGRAM CONVERTED BY                                       
000800*              COBOL CONVERSION AID PO 5785-ABJ                           
000900*              CONVERSION DATE 05/25/91 11:00:13.                         
001000*AUTHOR.         BIRGITTA BÅTH.                                           
001100*DATE-WRITTEN.   DEC-85 - JAN-86.                                         
001200                                                                          
001300*    REMARKS.                                                             
001310* PGM 4391-96 BEHÖVER KOMPILERAS VID INST. PGA ÄT I DDGS.                 
001400                                                                          
001700*    FUNKTION.                                                            
001800*        BILD 4391  AVVIKELSERAPPORTERING : SVERIGE + UDDA.               
001900*                                                                         
002000*        HUVUDSLINGA.                                                     
002100*        A  INIT                                                          
002200*        B  KONTROLL AV INDATA                                            
002300*        C  SÄTTA NYCKLAR                                                 
002400*        D  KONTROLL AV RELATIONER                                        
002500*        E  UPPDARERA HÄNDELSE                                            
002600*        F  HÄMTA 4316-SEGMENT OCH INDEX                                  
002700*        G  BEARBETA 4316 TYP 004                                         
002800*        H  BEARBETA 4316 TYP 002 OCH 003                                 
002900*        I  AVSLUTNING AV BEARBETNING                                     
003000*        S  GEMENSAMMA SEKTIONER                                          
003100*                                                                         
003200*                                                                         
003300*    INDATA.                                                              
003400*        MID:         W4I39101-MID.                                       
003500*        TRANSAKTION: W4T391.                                             
003600*                                                                         
003700*    UTDATA.                                                              
003800*        MOD:         W4O30102-MOD  (OM INGA KOLLIUPPG. ).                
003900*        MOD:         W4O30202-MOD  (OM INGA URSP.UPPG. ).                
004000*        MOD.         W4030302-MOD                                        
004100*        MOD:         W4O39201-MOD  (OM KOLLIUPPG. FINNS).                
004200*        MOD:         W4O39101-MOD  (OM OMSTART BEHÖVS).                  
004300*        MOD:         W4O39401-MOD  (OM URSP.UPPG. FINNS).                
004400*        MOD:         OSPECIFIK     (EJ GODK TRANS SKICKAS TILLB).        
004500*        TRANSAKTION  W0T605        (TILL STARTPROGRAMMET).               
004600     EJECT                                                                
004700 ENVIRONMENT DIVISION.                                                    
004800     SKIP3                                                                
004900 DATA DIVISION.                                                           
005000     EJECT                                                                
005100 WORKING-STORAGE SECTION.                                                 
005110                                                                          
005200*    -- CHECKED BY WY2000                                                 
005600*                                                                         
005700 77    PROGRAM-NAMN              PIC X(8)    VALUE 'W4039100'.            
005900 77    INDX                      PIC S9(9)   VALUE +0   COMP SYNC.        
006000 77    RADINDX                   PIC S9(9)   VALUE +0   COMP SYNC.        
006100 77    MAX-RADINDX               PIC S9(9)   VALUE +13  COMP SYNC.        
006200 77    MAX-RADINDX-PLUS-ETT      PIC S9(9)   VALUE +14  COMP SYNC.        
006300 77    4316-2-INDX               PIC S9(9)   VALUE +0   COMP SYNC.        
006400 77    MAX-4316-2-INDX           PIC S9(9)   VALUE +12  COMP SYNC.        
006500 77    MAX-4316-2-INDX-PLUS-ETT  PIC S9(9)   VALUE +13  COMP SYNC.        
006600 77    4316-4-INDX               PIC S9(9)   VALUE +0   COMP SYNC.        
006700 77    SPAR-4316-4-INDX          PIC S9(9)   VALUE +0   COMP SYNC.        
006800 77    MAX-4316-4-INDX           PIC S9(9)   VALUE +26  COMP SYNC.        
006900 77    MAX-4316-4-INDX-PLUS-ETT  PIC S9(9)   VALUE +27  COMP SYNC.        
007000 77    MAX2-4316-4-INDX          PIC S9(9)   VALUE +0   COMP SYNC.        
007100 77    MAX-MOD-LAENGD            PIC S9(4)   VALUE +224 COMP SYNC.        
007200 77    MIN-MOD-LAENGD            PIC S9(4)   VALUE +48  COMP SYNC.        
007300 77    4301-MOD-LAENGD           PIC S9(4)   VALUE +473 COMP SYNC.        
007400 77    4302-MOD-LAENGD           PIC S9(4)   VALUE +733 COMP SYNC.        
007500 77    4303-MOD-LAENGD           PIC S9(4)   VALUE +421 COMP SYNC.        
007600 77    4392-MOD-LAENGD           PIC S9(4)   VALUE +82  COMP SYNC.        
007700 77    4393-MOD-LAENGD           PIC S9(4)   VALUE +84  COMP SYNC.        
007800 77    4394-MOD-LAENGD           PIC S9(4)   VALUE +82  COMP SYNC.        
007900 77    4395-MOD-LAENGD           PIC S9(4)   VALUE +82  COMP SYNC.        
008000 77    4396-MOD-LAENGD           PIC S9(4)   VALUE +84  COMP SYNC.        
008100 77    RAETT                     PIC X       VALUE 'R'.                   
008200 77    FEL                       PIC X       VALUE 'F'.                   
008300 77    JA                        PIC X       VALUE 'J'.                   
008400 77    NEJ                       PIC X       VALUE 'N'.                   
008500 77    WS-IDPURAD-START          PIC 9(4)    VALUE 0.                     
008600 77    MAX-4316-C-MID-IDRADNR    PIC 9(4)    VALUE 0.                     
008700 77    WS-RADER-KVAR-ATT-BEHANDLA PIC X      VALUE 'N'.                   
008800 77    HELT-NOLLAD-ORDER         PIC X       VALUE 'N'.                   
008810 77    WS-IDDC                   PIC X(2).                                
008820 77    WS-PRTVAL                 PIC X(2).                                
008900                                                                          
009000 77    WS-KONTROLL               PIC X       VALUE SPACE.                 
009100       88  WS-KONTROLL-FEL                   VALUE 'F'.                   
009200       88  WS-KONTROLL-RAETT                 VALUE 'R'.                   
009300                                                                          
009400 77    WS-IDTRANS                PIC X(4)    VALUE SPACE.                 
009500       88  WS-EGEN-BILD          VALUE '4391'.                            
009600       88  WS-GODK-BILD          VALUE '4301' '4302' '4303'               
009700                                       '4392' '4393' '4394'               
009800                                       '4396'.                            
009900                                                                          
010000 77    WS-FLOMSTART              PIC X       VALUE SPACE.                 
010100       88  OMSTART                           VALUE 'J'.                   
010200                                                                          
010300 77    WS-FLREPLACE              PIC X       VALUE SPACE.                 
010400       88  WS-FLREPL                         VALUE 'J'.                   
010500                                                                          
010600 77    WS-FLREPLACE-TYP3         PIC X       VALUE 'N'.                   
010700       88  WS-FLREPL-TYP3                    VALUE 'J'.                   
010900                                                                          
011400 01  WS-MSG-CALL-GRP.                                                     
011500   03  WS-MSG-CALL               PIC X.                                   
011600     88  VISA-NAESTA-BILD                    VALUE '0'.                   
011700     88  STARTA-4301                         VALUE '1'.                   
011800     88  STARTA-4302                         VALUE '2'.                   
011900     88  STARTA-4303                         VALUE '3'.                   
012000     88  STARTA-4391                         VALUE '4'.                   
012100     88  STARTA-4392                         VALUE '5'.                   
012200     88  STARTA-4393                         VALUE '6'.                   
012300     88  STARTA-4394                         VALUE '7'.                   
012400     88  STARTA-4395                         VALUE '8'.                   
012500     88  STARTA-4396                         VALUE '9'.                   
012600   03  WS-VISA-NAESTA-BILD       PIC X       VALUE '0'.                   
012700   03  WS-STARTA-4301            PIC X       VALUE '1'.                   
012800   03  WS-STARTA-4302            PIC X       VALUE '2'.                   
012900   03  WS-STARTA-4303            PIC X       VALUE '3'.                   
013000   03  WS-STARTA-4391            PIC X       VALUE '4'.                   
013100   03  WS-STARTA-4392            PIC X       VALUE '5'.                   
013200   03  WS-STARTA-4393            PIC X       VALUE '6'.                   
013300   03  WS-STARTA-4394            PIC X       VALUE '7'.                   
013400   03  WS-STARTA-4395            PIC X       VALUE '8'.                   
013500   03  WS-STARTA-4396            PIC X       VALUE '9'.                   
013600     EJECT                                                                
013700 01    FILLER                    PIC X(16)   VALUE 'WS-MODNAMN'.          
013800 01    WS-MODNAMN.                                                        
013900   03    FILLER                  PIC X       VALUE 'W'.                   
014000   03    WS-MOD-IDTRANS-POS-1    PIC X.                                   
014100   03    FILLER                  PIC X       VALUE 'O'.                   
014200   03    WS-MOD-IDTRANS-POS-2-4  PIC X(3).                                
014300   03    FILLER                  PIC X(2)    VALUE '01'.                  
014400                                                                          
014500                                                                          
014600 01    FILLER                  PIC X(16)   VALUE 'WS-IDTRANS-MOD'.        
014700 01    WS-IDTRANS-MOD.                                                    
014800   03    WS-IDTRANS-POS-1-MOD    PIC X.                                   
014900   03    WS-IDTRANS-POS-2-4-MOD  PIC X(3).                                
015000     EJECT                                                                
015100 01    DYNAMISKA-SUBPROGRAM.                                              
015200   03    CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
015300   03    FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
015400     EJECT                                                                
015500 01    FILLER                    PIC X(16)   VALUE 'DIVERSE'.             
015600 01    DIVERSE.                                                           
015700                                                                          
015800   03    WS-4312IDTRANS          PIC X(4)    VALUE SPACE.                 
015900                                                                          
016000   03    WS-4316IDKOLLI          PIC S9(5)  COMP-3.                       
016100   03    WS-4316IDKOLLI-N        PIC 9(5).                                
016200   03    WS-4316IDKOLLI-X REDEFINES WS-4316IDKOLLI-N PIC X(5).            
016300                                                                          
016400   03    MAX-ANT-RAD             PIC S9(9)   VALUE +100.                  
016500   03    WS-ANT-RAD-REST         PIC S9(9)   VALUE ZERO.                  
016600   03    WS-ANT-RAD-INT          PIC S9(9)   VALUE ZERO.                  
016700   03    WS-TOM-SISTA            PIC 9(4)    VALUE ZERO.                  
016800   03    WS-FOM-INT              PIC 9(4)    VALUE ZERO.                  
016900   03    WS-TOM-INT              PIC 9(4)    VALUE ZERO.                  
017000   03    WS-IND                  PIC S9(9)   VALUE +0 COMP SYNC.          
017100                                                                          
017200   03    WS-FOM                  PIC 9(4)   VALUE ZERO.                   
017300   03    WS-TOM                  PIC 9(4)   VALUE ZERO.                   
017400                                                                          
017500   03    WS-RADNR                PIC 9(4)   VALUE ZERO.                   
017600                                                                          
017700   03    WS-KVORDRAD             PIC S9(5)   VALUE ZERO.                  
017800   03    WS-KVLEVART             PIC S9(6)   VALUE ZERO.                  
017900                                                                          
018000   03    WS-JMFRADNR             PIC S9(5)   VALUE ZERO.                  
018100   03    WS-RADNR-SEKV           PIC S9(5)   VALUE ZERO.                  
018200   03    WS-RADNR-TRAEFF         PIC S9(5)   VALUE ZERO.                  
018300                                                                          
018400   03    WS-IDPRODNR             PIC 9(7)    VALUE ZERO.                  
018500   03    WS-IDPURAD              PIC 9(5)    VALUE ZERO.                  
018600   03    WS-IDDISTR              PIC 9(4)    VALUE ZERO.                  
018700   03    WS-IDKUNDNR             PIC 9(6)    VALUE ZERO.                  
018800   03    WS-KDFRAKT              PIC 9(2)    VALUE ZERO.                  
018900   03    WS-KDORDKL              PIC 9       VALUE ZERO.                  
019000                                                                          
019100     EJECT                                                                
019200 01    FILLER                    PIC X(16)                                
019300                                 VALUE 'NYCKLAR-TILL-DLI'.                
019400 01    NYCKLAR-TILL-DLI.                                                  
019500                                                                          
019600********************** LÅSNINGSREGISTRET ***********************          
019700   03    W-WDGXKEY-4305-X.                                                
019800     05    FILLER                PIC X(4)    VALUE '4305'.                
019900     05    W-IDDC-4305           PIC X(2).                                
020000     05    FILLER                PIC X(24)   VALUE LOW-VALUE.             
020100                                                                          
020200   03    W-WDGXKEY-4306-X.                                                
020300     05    W-IDPRODNR-4306       PIC S9(7)   VALUE ZERO  COMP-3.          
020400     05    FILLER                PIC X(6)    VALUE LOW-VALUE.             
020500                                                                          
020600********************** HÄNDELSEREGISTRET ************************         
020700   03    W-WDGXKEY-4311-X.                                                
020800     05    FILLER                PIC X(4)    VALUE '4311'.                
020900     05    W-IDDC-4311           PIC X(2).                                
021000     05    FILLER                PIC X(24)   VALUE LOW-VALUE.             
021100                                                                          
021200   03    W-WDGXKEY-4312-X.                                                
021300     05    W-IDPRODNR-4312       PIC S9(7)   VALUE ZERO  COMP-3.          
021400     05    FILLER                PIC X(6)    VALUE LOW-VALUE.             
021500                                                                          
021600   03    W-WDGXKEY-IDUSER-4312   PIC X(8)    VALUE SPACE.                 
021700                                                                          
021800********************** PACKNINGSREGISTRET ************************        
021900   03    W-WDGXKEY-4315-X.                                                
022000     05    FILLER                PIC X(4)    VALUE '4315'.                
022100     05    FILLER                PIC X(26)   VALUE LOW-VALUE.             
022200                                                                          
022300   03    W-WDGXKEY-4316-X.                                                
022400     05    W-IDPRODNR-4316       PIC S9(7)   VALUE ZERO  COMP-3.          
022500     05    W-IDPTYP-4316         PIC X(3)    VALUE SPACE.                 
022600     05    W-IDKOLLI-4316        PIC S9(5)   VALUE ZERO  COMP-3.          
022700     05    FILLER                PIC X(10)   VALUE LOW-VALUE.             
022800                                                                          
022900   03    W-WDGXKEY-4316-X-MIN.                                            
023000     05    W-IDPRODNR-4316-MIN   PIC S9(7)   VALUE ZERO  COMP-3.          
023100     05    W-IDPTYP-4316-MIN     PIC X(3)    VALUE SPACE.                 
023200     05    FILLER                PIC X(13)   VALUE LOW-VALUE.             
023300                                                                          
023400   03    W-WDGXKEY-4316-X-MAX.                                            
023500     05    W-IDPRODNR-4316-MAX   PIC S9(7)   VALUE ZERO  COMP-3.          
023600     05    W-IDPTYP-4316-MAX     PIC X(3)    VALUE SPACE.                 
023700     05    FILLER                PIC X(13)   VALUE HIGH-VALUE.            
023800                                                                          
023900********************** SEQ INDEX  ********************************        
024000                                                                          
024100   03    W-WDE4B1KY-X.                                                    
024200     05    W-WDE4B1KY-IDPRODNR   PIC S9(7)   VALUE ZERO COMP-3.           
024300     05    W-WDE4B1KY-IDPURAD    PIC S9(5)   VALUE ZERO COMP-3.           
024400                                                                          
024500   03    W-WDE4B1KY-MIN-X.                                                
024600     05    W-WDE4B1KY-IDPRODNR-MIN   PIC S9(7) VALUE ZERO COMP-3.         
024700     05    W-WDE4B1KY-IDPURAD-MIN    PIC S9(5) VALUE ZERO COMP-3.         
024800                                                                          
024900   03    W-WDE4B1KY-MAX-X.                                                
025000     05    W-WDE4B1KY-IDPRODNR-MAX   PIC S9(7) VALUE ZERO COMP-3.         
025100     05    W-WDE4B1KY-IDPURAD-MAX    PIC S9(5) VALUE ZERO COMP-3.         
025200                                                                          
025300     EJECT                                                                
025400 01    FILLER                    PIC X(16)   VALUE 'MEDDELANDE'.          
025500 01    MEDDELANDE.                                                        
025600*****************************************************************         
025700*      FELMEDDELANDEN TILL RAD 1                                          
025800*****************************************************************         
025900                                                                          
026000   03    FEL748.                                                          
026100      05    FILLER               PIC X(40)   VALUE                        
026200           '748. UPPLYSTA FÄLT FEL'.                                      
026300      05    FILLER               PIC X(40)   VALUE                        
026400           '748. HIGH-LIGHTED FIELD(S) WRONG       '.                     
026500   03    FILLER  REDEFINES FEL748.                                        
026600      05    FEL-748              PIC X(40)   OCCURS 2.                    
026700                                                                          
026800                                                                          
026900   03    FEL749.                                                          
027000      05    FILLER               PIC X(40)   VALUE                        
027100           '749. FEL NYCKEL   '.                                          
027200      05    FILLER               PIC X(40)   VALUE                        
027300           '749. WRONG KEY                 '.                             
027400   03    FILLER  REDEFINES FEL749.                                        
027500      05    FEL-749              PIC X(40)   OCCURS 2.                    
027600                                                                          
027700                                                                          
027800   03    FEL750.                                                          
027900      05    FILLER               PIC X(40)   VALUE                        
028000           '750. INGÅNG VIA ANNAN MENY'.                                  
028100      05    FILLER               PIC X(40)   VALUE                        
028200           '750. ENTRANCE IN OTHER MENU         '.                        
028300   03    FILLER  REDEFINES FEL750.                                        
028400      05    FEL-750              PIC X(40)   OCCURS 2.                    
028600*                                                                         
028700   03    FEL751.                                                          
028800      05    FILLER               PIC X(40)   VALUE                        
028900           '751. FELAKTIGT RADNUMMER  '.                                  
029000      05    FILLER               PIC X(40)   VALUE                        
029100           '751. WRONG LINENUMBER  '.                                     
029200   03    FILLER  REDEFINES FEL751.                                        
029300      05    FEL-751              PIC X(40)   OCCURS 2.                    
029410*                                                                         
029420   03    FEL752.                                                          
029430      05    FILLER               PIC X(40)   VALUE                        
029440           '752. MAX 26 RADER. OM FLER, ANV. 4315. '.                     
029450      05    FILLER               PIC X(40)   VALUE                        
029460           '752. MAX 26 LINES. IF MORE, USE. 4315. '.                     
029470   03    FILLER  REDEFINES FEL752.                                        
029480      05    FEL-752              PIC X(40)   OCCURS 2.                    
029490*                                                                         
029600   03    FEL760.                                                          
029700      05    FILLER               PIC X(40)   VALUE                        
029800           '760. UPPGIFTER SAKNAS     '.                                  
029900      05    FILLER               PIC X(40)   VALUE                        
030000           '760. INFORMATION MISSING '.                                   
030100   03    FILLER  REDEFINES FEL760.                                        
030200      05    FEL-760              PIC X(40)   OCCURS 2.                    
030300*                                                                         
030500*****************************************************************         
030600*        INFORMATION TILL RAD 23                                          
030700*****************************************************************         
030800                                                                          
030900   03    INF1.                                                            
031000      05    FILLER               PIC X(40)   VALUE                        
031100           'UPPDATERING UTFÖRD'.                                          
031200      05    FILLER               PIC X(40)   VALUE                        
031300           'UPDATING PERFORMED     '.                                     
031400   03    FILLER  REDEFINES INF1.                                          
031500      05    INF-1                PIC X(40)   OCCURS 2.                    
031600                                                                          
031700     EJECT                                                                
031800******************************************************************        
031900*                                                                         
032000*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
032100*                                                                         
032200******************************************************************        
032300 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
032400     SKIP3                                                                
032500 01    FILLER                    PIC X(16)                                
032600                                 VALUE 'MID W4I39101 MID'.                
032700     SKIP3                                                                
032800*01    -COPY W4I39101                                                     
032900     EJECT                                                                
033000*01    -COPY WMSGAREA                                                     
033100     EJECT                                                                
033200*  03  MOD -COPY W4O30101   -RED MSG-AREA -PRE M4301-                     
033300     EJECT                                                                
033400*  03  MOD -COPY W4O30201   -RED MSG-AREA -PRE M4302-                     
033500     EJECT                                                                
033600*  03  MOD -COPY W4O30301   -RED MSG-AREA -PRE M4303-                     
033700     EJECT                                                                
033800*  03  MOD -COPY W4O39101   -RED MSG-AREA                                 
033900     EJECT                                                                
034000*  03  MOD -COPY W4O39201   -RED MSG-AREA -PRE M4392-                     
034100     EJECT                                                                
034200*  03  MOD -COPY W4O39301   -RED MSG-AREA -PRE M4393-                     
034300     EJECT                                                                
034400*  03  MOD -COPY W4O39401   -RED MSG-AREA -PRE M4394-                     
034500     EJECT                                                                
034600*  03  MOD -COPY W4O39501   -RED MSG-AREA -PRE M4395-                     
034700     EJECT                                                                
034800*  03  MOD -COPY W4O39601   -RED MSG-AREA -PRE M4396-                     
034900     EJECT                                                                
035000 01    FILLER                    PIC X(16)   VALUE 'P-TO-P-SW'.           
035100 01    P-TO-P-SW.                                                         
035200       03  PTOP-LL               PIC S9(4)   VALUE +17 COMP SYNC.         
035300       03  PTOP-Z1               PIC X       VALUE LOW-VALUE.             
035400       03  PTOP-Z2               PIC X       VALUE LOW-VALUE.             
035500       03  PTOP-TRANSKOD         PIC X(7)    VALUE 'W0T605U'.             
035600       03  FILLER                PIC X       VALUE SPACE.                 
035700       03  FILLER                PIC X(4)    VALUE '4391'.                
035800       03  PTOP-KDMFSFOR         PIC X.                                   
035900     EJECT                                                                
036000 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-SW1'.          
036100 01  P-TO-P-SW1.                                                          
036200       03  PTOP1-LL              PIC S9(4)   VALUE +187 COMP SYNC.        
036300       03  PTOP1-Z1              PIC X       VALUE LOW-VALUE.             
036400       03  PTOP1-Z2              PIC X       VALUE LOW-VALUE.             
036500       03  PTOP1-TRANSKOD        PIC X(7)    VALUE 'W4T391U'.             
036600       03  FILLER                PIC X       VALUE SPACE.                 
036700       03  FILLER                PIC X(4)    VALUE '4301'.                
036800       03  PTOP1-KDMFSFOR        PIC X.                                   
036900       03  PTOP1-IDPRODNR-IN     PIC X(7).                                
037000       03  PTOP1-IDPRODNR-UT     PIC X(7).                                
037100       03  PTOP1-IDDISTR-UT      PIC X(4).                                
037200       03  PTOP1-IDKUNDNR-UT     PIC X(6).                                
037300       03  PTOP1-KDFRAKT-UT      PIC X(2).                                
037400       03  PTOP1-IDORDNR-UT      PIC X(5).                                
037500       03  PTOP1-KDORDKL-UT      PIC X(1).                                
037510       03  PTOP1-IDDC-UT         PIC X(2).                                
037520       03  PTOP1-PRTVAL-ADRESSFL PIC X(2).                                
037600       03  FILLER                PIC X(136)  VALUE ALL '+'.               
037700     EJECT                                                                
037800 01    P-TO-P-SW2.                                                        
037900       03  PTOP2-LL              PIC S9(4)   VALUE +89 COMP SYNC.         
038000       03  PTOP2-Z1              PIC X       VALUE LOW-VALUE.             
038100       03  PTOP2-Z2              PIC X       VALUE LOW-VALUE.             
038200       03  PTOP2-TRANSKOD        PIC X(7)    VALUE 'W4T392U'.             
038300       03  FILLER                PIC X       VALUE SPACE.                 
038400       03  FILLER                PIC X(4)    VALUE '4391'.                
038500       03  PTOP2-KDMFSFOR        PIC X.                                   
038600       03  PTOP2-IDPRODNR-IN     PIC X(7).                                
038700       03  PTOP2-IDPRODNR-UT     PIC X(7).                                
038800       03  PTOP2-IDDISTR-UT      PIC X(4).                                
038900       03  PTOP2-IDKUNDNR-UT     PIC X(6).                                
039000       03  PTOP2-KDFRAKT-UT      PIC X(2).                                
039100       03  PTOP2-IDORDNR-UT      PIC X(5).                                
039200       03  PTOP2-KDORDKL-UT      PIC X(1).                                
039210       03  PTOP2-IDDC-UT         PIC X(2).                                
039220       03  PTOP2-PRTVAL-ADRESSFL PIC X(2).                                
039300****** 03  MID -COPY W0I60501   -PRE MOD-                                 
039400     EJECT                                                                
039500 01  P-TO-P-SW3.                                                          
039600       03  PTOP3-LL              PIC S9(4)   VALUE +183 COMP SYNC.        
039700       03  PTOP3-Z1              PIC X       VALUE LOW-VALUE.             
039800       03  PTOP3-Z2              PIC X       VALUE LOW-VALUE.             
039900       03  PTOP3-TRANSKOD        PIC X(7)    VALUE 'W4T391U'.             
040000       03  FILLER                PIC X       VALUE SPACE.                 
040100       03  FILLER                PIC X(4)    VALUE '4301'.                
040200       03  PTOP3-KDMFSFOR        PIC X.                                   
040300*      03  -COPY W4I39101 -PRE MOD-                                       
040400     EJECT                                                                
040500*01    -COPY WMFSAREA                                                     
040600     EJECT                                                                
040700******************************************************************        
040800*                                                                         
040900*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
041000*                                                                         
041100*****************************************************************         
041200 01    IMS-WS.                                                            
041300   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
041400     SKIP3                                                                
041500*                        **** STATUS-KOD FRÅN IMS                         
041600   03    STATUS-WS               PIC XX.                                  
041700     88    SEGMENT-FINNS                     VALUE '  '.                  
041800     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
041900     88    SEGMENT-SLUT                      VALUE 'GB'.                  
042000     SKIP3                                                                
042100   03    GODK-STATUSKODER.                                                
042200     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
042300     SKIP3                                                                
042400 01    SSA1                      PIC X(96).                               
042500 01    SSA2                      PIC X(64).                               
042600     EJECT                                                                
042700*                            IMS FUNKTIONSKODER                           
042800*01    -COPY W0003                                                        
042900     EJECT                                                                
043000*                            DLI INPUT-OUTPUT AREA                        
043100 01    FILLER                   PIC X(16) VALUE 'DLI-IO-AREA-1'.          
043200 01    DLI-IO-AREA.                                                       
043300   03    IO-AREA                 PIC X(370)  VALUE SPACE.                 
043400     SKIP3                                                                
043500*  03    XXDK11 -COPY WDGX4312   -PRE XXDK- -RED IO-AREA                  
043600     EJECT                                                                
043700*  03    XXDL11 -COPY WDGX4316   -PRE XXDL- -RED IO-AREA                  
043800     EJECT                                                                
043900*  08    AREA -COPY W4I31501   -RED XXDL-4316-FILLER -PRE 4316-A-         
044000     EJECT                                                                
044100*  08    AREA -COPY W4I31401   -RED XXDL-4316-FILLER -PRE 4316-B-         
044200     EJECT                                                                
044300*                            DLI INPUT-OUTPUT AREA 2                      
044400 01    FILLER                   PIC X(16) VALUE 'DLI-IO-AREA-2'.          
044500 01    DLI-IO-AREA-2.                                                     
044600   03    IO-AREA-2               PIC X(370)  VALUE SPACE.                 
044700     SKIP3                                                                
044800*  03    XXDL11 -COPY WDGX4316   -PRE XXDL-C- -RED IO-AREA-2              
044900     EJECT                                                                
045000*  08  AREA -COPY W4I39801   -RED XXDL-C-4316-FILLER -PRE 4316-C-         
045100     EJECT                                                                
045200 01    FILLER                   PIC X(16) VALUE 'DLI-IO-AREA-4'.          
045300 01    DLI-IO-AREA-4.                                                     
045400   03    IO-AREA-4               PIC X(370)  VALUE SPACE.                 
045500     SKIP3                                                                
045600*  03    XXDJ11 -COPY WDGX4306   -PRE XXDJ- -RED IO-AREA-4                
045700     EJECT                                                                
045800*  03    XXDJ21 -COPY WDGX4308   -PRE XXDJ- -RED IO-AREA-4                
045900     EJECT                                                                
046000 01    FILLER                   PIC X(16) VALUE 'DLI-IO-AREA-5'.          
046100 01    DLI-IO-AREA-5.                                                     
046200   03    IO-AREA-5               PIC X(30)   VALUE SPACE.                 
046300     SKIP3                                                                
046400*  03               -COPY WDE4B1      -RED IO-AREA-5                      
046500     EJECT                                                                
046600 LINKAGE SECTION.                                                         
046700*01    -COPY W0009     -PRE MSG-                                          
046800     EJECT                                                                
046900*01    -COPY W0009     -PRE ALT-                                          
047000     EJECT                                                                
047100*01    -COPY W0009     -PRE ALT1-                                         
047200     EJECT                                                                
047300*01    -COPY W0009     -PRE ALT2-                                         
047400     EJECT                                                                
047500*01    -COPY W0008     -PRE XXDJ-                                         
047600        05 FILLER                PIC X.                                   
047700     EJECT                                                                
047800*01    -COPY W0008     -PRE XXDK-                                         
047900        05 FILLER                PIC X.                                   
048000     EJECT                                                                
048100*01    -COPY W0008     -PRE XXDL-                                         
048200        05 FILLER                PIC X.                                   
048300     EJECT                                                                
048400*01    -COPY W0008     -PRE XXDL2-                                        
048500        05 FILLER                PIC X.                                   
048600     EJECT                                                                
048700*01    -COPY W0008     -PRE WDE4B-                                        
048800        05 FILLER                PIC X.                                   
048900     EJECT                                                                
049000 PROCEDURE DIVISION USING  MSG-PCB ALT-PCB ALT1-PCB ALT2-PCB              
049100     XXDJ-PCB XXDK-PCB XXDL-PCB XXDL2-PCB                                 
049200     WDE4B-PCB.                                                           
049300                                                                          
049400                                                                          
049500                                                                          
049600     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB ALT1-PCB ALT2-PCB              
049700     XXDJ-PCB XXDK-PCB XXDL-PCB XXDL2-PCB                                 
049800     WDE4B-PCB.                                                           
049900                                                                          
050000                                                                          
050100******************************************************************        
050200*           HUVUDSLINGA                                                   
050300******************************************************************        
050400                                                                          
050500     PERFORM IMS-GET-MSG                                                  
050600                                                                          
050700     IF SEGMENT-FINNS                                                     
050800                                                                          
050900       PERFORM A-INIT-SPARA-INPUT                                         
051000                                                                          
051100       MOVE MFS-IDTRANS            TO WS-IDTRANS                          
051200       IF (WS-EGEN-BILD OR WS-GODK-BILD)  AND MFS-UPDATE                  
051201                                                                          
051400         IF  (MID-IDPRODNR-UT  NUMERIC                                    
051500         AND  MID-IDDISTR-UT   NUMERIC                                    
051600         AND  MID-IDKUNDNR-UT  NUMERIC                                    
051700         AND  MID-KDFRAKT-UT   NUMERIC                                    
051710         AND  MID-KDORDKL-UT   NUMERIC                                    
051800         AND  MID-IDORDNR-UT   NUMERIC)                                   
052000         AND (MID-IDRADNR-SENAST NUMERIC                                  
052100         OR   MID-IDRADNR-SENAST = ALL '+')                               
052200                                                                          
052300           PERFORM C-FIXA-NYCKLAR                                         
052400                                                                          
052500           IF MID-RAPP-DEL NOT = ALL '+'                                  
052600           OR MID-IDRADNR-SENAST NOT = ALL '+'                            
052700           OR XXDK-4312-KDBEHAND-AVVIK = 4                                
052800                                                                          
052900             PERFORM B-KONTROLL-INDATA                                    
053000             IF WS-KONTROLL-RAETT                                         
053100                                                                          
053200               PERFORM D-KONTROLL-RELATIONER                              
053300               IF WS-KONTROLL-RAETT                                       
053400                                                                          
053500                 IF MID-RAPP-DEL NOT = ALL '+'                            
053600                 OR XXDK-4312-KDBEHAND-AVVIK = 4                          
053700                   PERFORM E-UPPDATERA-HAENDELSE                          
053800                   PERFORM F-HAEMTA-4316-OCH-4316-4-INDX                  
053900                   PERFORM G-BEARBETA-4316-TYP004                         
054000                   PERFORM H-BEARBETA-4316-TYP002-003                     
054100                 END-IF                                                   
054200                                                                          
054300                 IF 4316-4-INDX = MAX-4316-4-INDX-PLUS-ETT AND            
054400                    WS-RADER-KVAR-ATT-BEHANDLA = JA                       
054500                     MOVE JA        TO WS-FLOMSTART                       
054600                     PERFORM K-STARTA-OM-4391                             
054700                  ELSE                                                    
054800                     IF MID-IDRADNR (MAX-RADINDX) = ALL '+'               
054900                       PERFORM J-KONTROLL-NOLLAD-ORDER                    
055000                       PERFORM I-AVSLUTA-BEARBETA                         
055100                      ELSE                                                
055200                       MOVE MID-IDRADNR (MAX-RADINDX) TO                  
055300                                                MOD-IDRADNR-SENAST        
055400                       PERFORM S12-FORMATETS-ATTR                         
055500                       PERFORM S08-REDIGERA-UTFAELT                       
055600                     END-IF                                               
055700                 END-IF                                                   
055800               ELSE                                                       
055900                 MOVE FEL-748 (INDX) TO MOD-TEMFSFEL                      
056000                 PERFORM S01-ROER-EJ-INFAELT                              
056100                 PERFORM S08-REDIGERA-UTFAELT                             
056200               END-IF                                                     
056300             ELSE                                                         
056310                                                                          
056400               IF MOD-TEMFSFEL = SPACE                                    
056410                 MOVE FEL-748 (INDX) TO MOD-TEMFSFEL                      
056500                 PERFORM S01-ROER-EJ-INFAELT                              
056600                 PERFORM S08-REDIGERA-UTFAELT                             
056700               ELSE                                                       
056701                 PERFORM S01-ROER-EJ-INFAELT                              
056702                 PERFORM S08-REDIGERA-UTFAELT                             
056710               END-IF                                                     
056720             END-IF                                                       
056800           ELSE                                                           
056900             MOVE FEL-760 (INDX) TO MOD-TEMFSFEL                          
057000             PERFORM S01-ROER-EJ-INFAELT                                  
057100             PERFORM S08-REDIGERA-UTFAELT                                 
057200           END-IF                                                         
057300         ELSE                                                             
057400           MOVE FEL-749 (INDX) TO MOD-TEMFSFEL                            
057500           PERFORM S01-ROER-EJ-INFAELT                                    
057600           PERFORM S08-REDIGERA-UTFAELT                                   
057700         END-IF                                                           
057800       ELSE                                                               
057900         MOVE FEL-750 (INDX)         TO MOD-TEMFSFEL                      
058000         MOVE MFS-IDTRANS            TO WS-IDTRANS-MOD                    
058100         MOVE WS-IDTRANS-POS-1-MOD   TO WS-MOD-IDTRANS-POS-1              
058200         MOVE WS-IDTRANS-POS-2-4-MOD TO WS-MOD-IDTRANS-POS-2-4            
058300         MOVE WS-MODNAMN             TO MFS-IDMOD                         
058400         MOVE MIN-MOD-LAENGD         TO MSG-KVLL                          
058500       END-IF                                                             
058600       IF OMSTART                                                         
058700           CONTINUE                                                       
058800        ELSE                                                              
058900           EVALUATE TRUE                                                  
059000           WHEN VISA-NAESTA-BILD PERFORM IMS-INSERT-MSG                   
059100             CONTINUE                                                     
059200           WHEN STARTA-4391  PERFORM IMS-INSERT-MSG-ALT1-PCB              
059300             CONTINUE                                                     
059400           WHEN STARTA-4392  PERFORM IMS-INSERT-MSG-ALT2-PCB              
059500             CONTINUE                                                     
059600           END-EVALUATE                                                   
059700       END-IF                                                             
059800     END-IF                                                               
059900     MOVE ZERO TO RETURN-CODE                                             
060000     GOBACK                                                               
060100     .                                                                    
060200     EJECT                                                                
060300 A-INIT-SPARA-INPUT SECTION.                                              
060400******************************************************************        
060500*             INITSIERING                                                 
060600******************************************************************        
060710                                                                          
060800     IF MSG-DUBBLA-TRANSKODER                                             
060900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I39101                 
061000       MOVE MSG-IDTRANS-2        TO MFS-IDTRANS                           
061100       MOVE MSG-KDMFSFOR-2       TO MFS-KDMFSFOR                          
061200                                    PTOP3-KDMFSFOR                        
061300       MOVE MSG-IDPFK            TO MFS-IDPFK                             
061400     ELSE                                                                 
061500       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I39101                  
061600       MOVE MSG-IDTRANS-1        TO MFS-IDTRANS                           
061700       MOVE MSG-KDMFSFOR-1       TO MFS-KDMFSFOR                          
061800                                    PTOP3-KDMFSFOR                        
061900       MOVE ' '                  TO MFS-IDPFK                             
062000     END-IF                                                               
062100     MOVE MSG-KDTRTYP            TO MFS-KDTRTYP                           
062200     MOVE MAX-MOD-LAENGD         TO MSG-KVLL                              
062300     MOVE LOW-VALUE              TO MSG-AREA                              
062400     MOVE 'W4O39101'             TO MFS-IDMOD                             
062500     MOVE '4391'                 TO MOD-IDTRANS                           
062600                                                                          
062700     IF SWEDISH-TEXT                                                      
062800       MOVE +1                   TO INDX                                  
062900     ELSE                                                                 
063000       MOVE +2                   TO INDX                                  
063100     END-IF                                                               
063200     MOVE MFS-ROER-EJ-FAELT      TO MOD-IDPRODNR-UT                       
063300                                    MOD-IDDISTR-UT                        
063400                                    MOD-IDKUNDNR-UT                       
063500                                    MOD-KDFRAKT-UT                        
063600                                    MOD-IDORDNR-UT                        
063700                                    MOD-KDORDKL-UT                        
063710                                    MOD-IDDC-UT                           
063720                                    MOD-PRTVAL-ADRESSFL                   
063800                                                                          
063900     MOVE +1                     TO RADINDX                               
064000     PERFORM UNTIL                                                        
064100      NOT ( RADINDX < MAX-RADINDX-PLUS-ETT )                              
064200       MOVE MFS-RENSA-FAELT        TO MOD-IDRADNR  (RADINDX)              
064300                                      MOD-KVLEVART (RADINDX)              
064400       ADD +1                      TO RADINDX                             
064500     END-PERFORM                                                          
064600     MOVE MFS-RENSA-FAELT        TO MOD-IDRADNR-SENAST                    
064700                                    MOD-TEMFSFEL                          
064800                                    MOD-TEMFSINF                          
064900                                                                          
065000     INSPECT  MID-IDPRODNR-UT    REPLACING ALL SPACE BY ZERO              
065100     INSPECT  MID-IDDISTR-UT     REPLACING ALL SPACE BY ZERO              
065200     INSPECT  MID-IDKUNDNR-UT    REPLACING ALL SPACE BY ZERO              
065300     INSPECT  MID-KDFRAKT-UT     REPLACING ALL SPACE BY ZERO              
065400     INSPECT  MID-IDORDNR-UT     REPLACING ALL SPACE BY ZERO              
065500     INSPECT  MID-IDRADNR-SENAST REPLACING ALL SPACE BY ZERO              
065600                                                                          
065700     MOVE WS-VISA-NAESTA-BILD    TO WS-MSG-CALL                           
065800     MOVE NEJ                    TO WS-FLREPLACE-TYP3                     
065900                                    WS-FLOMSTART                          
066000     .                                                                    
066100     EJECT                                                                
066200 B-KONTROLL-INDATA SECTION.                                               
066300*****************************************************************         
066400*    KONTROLL AV INFÄLTEN                                                 
066500*    -  RADNR OCH LEVART SKALL VARA NUMERISKA                             
066600*    -  RADNR SKALL VARA STRÄNGT STIGANDE                                 
066700*    -  BLANKRADER FÅR FÖREKOMMA                                          
066800*    VID FEL LYSES AKTUELLT FÄLT UPP OCH BILDEN SKICKAS EFTER             
066900*    SLUTFÖRANDE AV ALLA KONTROLLER TILLBAKA                              
067000*****************************************************************         
067100                                                                          
067200     MOVE RAETT                   TO WS-KONTROLL                          
067300                                                                          
067400     IF MID-RAPP-DEL NOT = ALL '+'                                        
067500                                                                          
067600       IF MID-IDRADNR-SENAST = ALL '+'                                    
067700         MOVE +0                    TO WS-JMFRADNR                        
067800       ELSE                                                               
067900         MOVE MID-IDRADNR-SENAST    TO WS-JMFRADNR                        
068000       END-IF                                                             
068100       MOVE +1                      TO RADINDX                            
068200                                                                          
068300       PERFORM UNTIL                                                      
068400        NOT ( RADINDX < MAX-RADINDX-PLUS-ETT )                            
068500         IF MID-RAD (RADINDX) NOT = ALL '+'                               
068600                                                                          
068700           IF MID-IDRADNR (RADINDX) NUMERIC                               
068800             IF MID-IDRADNR (RADINDX) > WS-JMFRADNR                       
068900               MOVE MID-IDRADNR (RADINDX) TO WS-JMFRADNR                  
069000               MOVE MFS-NUM-FAELT-RAETT   TO                              
069100                                      MOD-IDRADNR-ATTR (RADINDX)          
069200             ELSE                                                         
069300               MOVE FEL               TO WS-KONTROLL                      
069400               MOVE MFS-NUM-FAELT-FEL TO                                  
069500                                      MOD-IDRADNR-ATTR (RADINDX)          
069600             END-IF                                                       
069700           ELSE                                                           
069800             MOVE FEL               TO WS-KONTROLL                        
069900             MOVE MFS-NUM-FAELT-FEL TO                                    
070000                                    MOD-IDRADNR-ATTR (RADINDX)            
070100           END-IF                                                         
070200           IF MID-KVLEVART (RADINDX) NUMERIC                              
070300             MOVE MFS-NUM-FAELT-RAETT    TO                               
070400                                    MOD-KVLEVART-ATTR (RADINDX)           
070500           ELSE                                                           
070600             MOVE FEL                 TO WS-KONTROLL                      
070700             MOVE MFS-NUM-FAELT-FEL   TO                                  
070800                                     MOD-KVLEVART-ATTR (RADINDX)          
070900           END-IF                                                         
071000         END-IF                                                           
071100         ADD +1                          TO RADINDX                       
071200       END-PERFORM                                                        
071210                                                                          
071211       IF MID-IDRADNR-SENAST NOT = ALL '+'                                
071252         MOVE +1 TO RADINDX                                               
071253         PERFORM UNTIL MID-IDRADNR(RADINDX) = SPACE                       
071254             ADD +1                   TO RADINDX                          
071255         END-PERFORM                                                      
071256                                                                          
071294         IF  RADINDX   > 13                                               
071295*FLER ÄN 26 RADER HAR MATATS IN PÅ 4391. PGA. MID TILL 4398 HAR           
071296*MAX 26 RADER SÅ LÄMNAS FELMED.                                           
071297             MOVE FEL-752(INDX)       TO MOD-TEMFSFEL                     
071298             MOVE FEL                 TO WS-KONTROLL                      
071299             MOVE MFS-NUM-FAELT-RAETT TO                                  
071300                  MOD-IDRADNR-ATTR (RADINDX)                              
071301         END-IF                                                           
071302       END-IF                                                             
071310     END-IF                                                               
071400     .                                                                    
071500     EJECT                                                                
071600 C-FIXA-NYCKLAR SECTION.                                                  
071700*****************************************************************         
071800*    ALLA NYCKLAR FÖRBEREDES                                              
071900*    -LÅSNINGSREGISTRET                                                   
072000*    -HÄNDELSEREGISTRET                                                   
072100*    -PACKNINGSREGISTRET                                                  
072200*****************************************************************         
072300                                                                          
072400     MOVE MID-IDDC-UT             TO WS-IDDC                              
072410                                     W-IDDC-4305                          
072500                                     W-IDDC-4311                          
072600                                                                          
072700     MOVE LOW-VALUE               TO W-WDE4B1KY-MIN-X                     
072800     MOVE HIGH-VALUE              TO W-WDE4B1KY-MAX-X                     
072900                                                                          
073000     MOVE MID-IDPRODNR-UT         TO W-IDPRODNR-4306                      
073100                                     W-IDPRODNR-4312                      
073200                                     W-IDPRODNR-4316                      
073300                                     W-IDPRODNR-4316-MIN                  
073400                                     W-IDPRODNR-4316-MAX                  
073500                                     W-WDE4B1KY-IDPRODNR                  
073600                                     W-WDE4B1KY-IDPRODNR-MIN              
073700                                     W-WDE4B1KY-IDPRODNR-MAX              
073710     MOVE MID-PRTVAL-ADRESSFL     TO WS-PRTVAL                            
073720                                                                          
073800     PERFORM IMS-GU-4311                                                  
073900     PERFORM IMS-GHNP-4312                                                
074000     .                                                                    
074100     EJECT                                                                
074200 D-KONTROLL-RELATIONER SECTION.                                           
074300*****************************************************************         
074400*    KONTROLL AV INFÄLTEN                                                 
074500*    -  RADNR SKALL VARA <  ELLER = TOTALA ANTALET RADER                  
074600*    -  RADNR FÅR EJ VARA ANNULERAT                                       
074700*    VID FEL LYSES AKTUELLT FÄLT UPP OCH BILDEN SKICKAS EFTER             
074800*    SLUTFÖRANDE AV ALLA KONTROLLER TILLBAKA                              
074900*****************************************************************         
075000                                                                          
075100     PERFORM IMS-GU-4305                                                  
075200     PERFORM IMS-GHNP-4306                                                
075300                                                                          
075400     MOVE XXDK-4312-IDTRANS  TO WS-4312IDTRANS                            
075500     MOVE XXDK-4312-KVORDRAD TO WS-KVORDRAD                               
075600                                                                          
075700     IF MID-RAPP-DEL NOT = ALL '+'                                        
075800                                                                          
075900       MOVE +1                         TO RADINDX                         
076000                                                                          
076100       PERFORM UNTIL                                                      
076200        NOT ( RADINDX < MAX-RADINDX-PLUS-ETT )                            
076300         IF MID-RAD (RADINDX) NOT = ALL '+'                               
076400                                                                          
076500           IF WS-4312IDTRANS = '4301' OR '4302' OR '4303'                 
076600                                                                          
076700             IF MID-IDRADNR (RADINDX) > XXDK-4312-KVORDRAD                
076800               MOVE FEL                TO WS-KONTROLL                     
076900               MOVE  MFS-NUM-FAELT-FEL TO                                 
077000                         MOD-IDRADNR-ATTR (RADINDX)                       
077100             ELSE                                                         
077200               MOVE MID-IDRADNR (RADINDX) TO W-WDE4B1KY-IDPURAD           
077300               PERFORM IMS-GU-WDE4B1                                      
077400                                                                          
077500               IF MID-KVLEVART (RADINDX) > SEQB-KVAVBART                  
077600               OR MID-KVLEVART (RADINDX) = SEQB-KVAVBART                  
077700                 MOVE FEL                TO WS-KONTROLL                   
077800                 MOVE  MFS-NUM-FAELT-FEL TO                               
077900                               MOD-KVLEVART-ATTR (RADINDX)                
078000               ELSE                                                       
078100                 PERFORM IMS-GNP-4308-F                                   
078200                 PERFORM UNTIL                                            
078300                  NOT ( SEGMENT-FINNS AND XXDJ-4308-KVANNANT NOT          
078400                   = ZERO )                                               
078500                   PERFORM IMS-GNP-4308                                   
078600                 END-PERFORM                                              
078700                 PERFORM UNTIL                                            
078800                  NOT ( SEGMENT-FINNS AND (MID-IDRADNR (RADINDX)          
078900                   >                                                      
079000                    XXDJ-4308-IDRADNR-ORD-FROM OR MID-IDRADNR             
079100                   (RADINDX)                                              
079200                    = XXDJ-4308-IDRADNR-ORD-FROM) )                       
079300                   IF MID-IDRADNR (RADINDX) <                             
079400                                     XXDJ-4308-IDRADNR-ORD-TOM            
079500                   OR MID-IDRADNR (RADINDX) =                             
079600                                         XXDJ-4308-IDRADNR-ORD-TOM        
079700                     MOVE FEL                  TO WS-KONTROLL             
079800                     MOVE  MFS-NUM-FAELT-FEL   TO                         
079900                                          MOD-IDRADNR-ATTR                
080000                     (RADINDX)                                            
080100                   END-IF                                                 
080200                   PERFORM IMS-GNP-4308                                   
080300                   PERFORM UNTIL                                          
080400                    NOT ( SEGMENT-FINNS AND XXDJ-4308-KVANNANT            
080500                     NOT = ZERO )                                         
080600                     PERFORM IMS-GNP-4308                                 
080700                   END-PERFORM                                            
080800                 END-PERFORM                                              
080900               END-IF                                                     
081000             END-IF                                                       
081100           ELSE                                                           
081200             MOVE MID-IDRADNR (RADINDX) TO  W-WDE4B1KY-IDPURAD            
081300             PERFORM IMS-GU-WDE4B1-STAT-GE                                
081400             IF SEGMENT-FINNS AND SEQB-KVAVBART > 0                       
081500               IF MID-KVLEVART (RADINDX) NOT = ALL '+'                    
081600                 IF MID-KVLEVART (RADINDX) > SEQB-KVAVBART                
081700                 OR MID-KVLEVART (RADINDX) = SEQB-KVAVBART                
081800                   MOVE FEL                TO WS-KONTROLL                 
081900                   MOVE  MFS-NUM-FAELT-FEL TO                             
082000                                 MOD-KVLEVART-ATTR (RADINDX)              
082100                 END-IF                                                   
082200               END-IF                                                     
082300             ELSE                                                         
082400               MOVE FEL                  TO WS-KONTROLL                   
082500               MOVE  MFS-NUM-FAELT-FEL   TO                               
082600                             MOD-IDRADNR-ATTR (RADINDX)                   
082700                                                                          
082800             END-IF                                                       
082900           END-IF                                                         
083000         END-IF                                                           
083100         ADD +1             TO RADINDX                                    
083200                                                                          
083300       END-PERFORM                                                        
083400     END-IF                                                               
083500     .                                                                    
083600     EJECT                                                                
083700 E-UPPDATERA-HAENDELSE SECTION.                                           
083800***************************************************************           
083900*    HÄNDELSEREGISTRET UPPDATERAS TILLPÅBÖRJAD BEARBETNING                
084000****************************************************************          
084100                                                                          
084200                                                                          
084300     PERFORM IMS-GU-4311                                                  
084400     PERFORM IMS-GHNP-4312                                                
084500                                                                          
084600     MOVE 3                          TO XXDK-4312-KDBEHAND-AVVIK          
084700                                                                          
084800     PERFORM IMS-REPL-4312                                                
084900     .                                                                    
085000                                                                          
085100     EJECT                                                                
085200 F-HAEMTA-4316-OCH-4316-4-INDX SECTION.                                   
085300***************************************************************           
085400*    -DET SISTA 4316-SEGMENTET HÄMTAS                                     
085500*    -INDEXET TAS FRAM, DVS FÖRSTA TOMMA RAD                              
085600*    -VAR KOLLIT FULLT FÖRBEREDES ETT NYTT                                
085700****************************************************************          
085800                                                                          
085900     PERFORM IMS-GU-4315-TYP4                                             
086000     MOVE JA                         TO WS-FLREPLACE                      
086100     MOVE +1                         TO 4316-4-INDX                       
086200     MOVE +0                         TO WS-IDPURAD-START                  
086300     MOVE '004'                      TO W-IDPTYP-4316                     
086400     MOVE ZERO                       TO W-IDKOLLI-4316                    
086500                                                                          
086600     PERFORM IMS-GHNP-4316-TYP4-L                                         
086700                                                                          
086800     PERFORM UNTIL                                                        
086900      NOT ( (4316-4-INDX < MAX-4316-4-INDX-PLUS-ETT) AND                  
087000             (4316-C-MID-IDRADNR (4316-4-INDX) NOT = ALL '+' ) )          
087100       MOVE 4316-C-MID-IDRADNR (4316-4-INDX) TO WS-IDPURAD-START          
087200       ADD +1                      TO 4316-4-INDX                         
087300     END-PERFORM                                                          
087400     ADD +1                        TO WS-IDPURAD-START                    
087500                                                                          
087600     IF 4316-4-INDX > MAX-4316-4-INDX                                     
087610*      4398-TRANS FORTSÄTTER I YTTERLIGARE EN 4398-TRANS.                 
087620       MOVE JA                    TO 4316-C-MID-FLSVAR                    
087630       PERFORM IMS-REPL-4316-TYP4                                         
087640                                                                          
087700       PERFORM S03-SKAPA-4316-TYP4-GEMEN                                  
087800       MOVE +1                    TO 4316-4-INDX                          
087900       MOVE NEJ                   TO WS-FLREPLACE                         
088000     END-IF                                                               
088100                                                                          
088200     MOVE 4316-4-INDX             TO SPAR-4316-4-INDX                     
088300     .                                                                    
088400     EJECT                                                                
088500 G-BEARBETA-4316-TYP004 SECTION.                                          
088600*****************************************************************         
088800*   MAN BEHANDLAR INTE BARA DE RADER SOM ANGETTS PÅ BILDEN UTAN           
088900*   SAMTLIGA ORDERRADER. DETTA GÖR MAN FÖR ATT SKAPA AVVIKELSE-           
089000*   TRANSAR (4316) FÖR DE RADER SOM FÅTT AVVIKELSE VID                    
089100*   UTSKRIFTEN.                                                           
089200*****************************************************************         
089300     MOVE WS-IDPURAD-START         TO W-WDE4B1KY-IDPURAD                  
089400     PERFORM IMS-GU-WDE4B1-STAT-GE                                        
089500                                                                          
089600     MOVE JA                       TO WS-RADER-KVAR-ATT-BEHANDLA          
089700     MOVE +1                       TO RADINDX                             
089800     PERFORM GA-KONTR-OM-VIDARE-BEHANDLING                                
089900                                                                          
090000     PERFORM UNTIL WS-RADER-KVAR-ATT-BEHANDLA = NEJ OR                    
090100                   4316-4-INDX > MAX-4316-4-INDX                          
090200       IF RADINDX < MAX-RADINDX-PLUS-ETT                                  
090300         PERFORM GB-BEH-RADER-FORE-FYSAVV                                 
090400         PERFORM GC-BEH-FYSAVV                                            
090500         ADD +1                    TO RADINDX                             
090600       ELSE                                                               
090700         PERFORM GD-BEH-SISTA-RADERNA                                     
090800       END-IF                                                             
090900       PERFORM GA-KONTR-OM-VIDARE-BEHANDLING                              
091000     END-PERFORM                                                          
091200     .                                                                    
091300     EJECT                                                                
091400 GA-KONTR-OM-VIDARE-BEHANDLING           SECTION.                         
091500                                                                          
091600     PERFORM UNTIL RADINDX > MAX-RADINDX    OR                            
091700                   (MID-IDRADNR (RADINDX) NOT =  ALL '+' AND              
091800                    MID-IDRADNR (RADINDX) >= WS-IDPURAD-START)            
091900       ADD +1                            TO RADINDX                       
092000     END-PERFORM                                                          
092100     IF ( RADINDX > MAX-RADINDX                   AND                     
092200          MID-IDRADNR (MAX-RADINDX) NOT = ALL '+'     ) OR                
092300        SEGMENT-SAKNAS                                  OR                
092400        SEGMENT-SLUT                                                      
092500       MOVE NEJ                   TO WS-RADER-KVAR-ATT-BEHANDLA           
092600     END-IF                                                               
092700     .                                                                    
092800     EJECT                                                                
092900 GB-BEH-RADER-FORE-FYSAVV                SECTION.                         
093000                                                                          
093100     PERFORM UNTIL SEQB-IDPURAD >= MID-IDRADNR (RADINDX) OR               
093200                   4316-4-INDX  = MAX-4316-4-INDX-PLUS-ETT                
093300       PERFORM S13-BEHAND-AVV-VID-UTSKRIFT                                
093400       PERFORM IMS-GN-WDE4B1                                              
093500     END-PERFORM                                                          
093600     .                                                                    
093700     EJECT                                                                
093800 GC-BEH-FYSAVV                           SECTION.                         
093900                                                                          
094000     MOVE MID-IDRADNR (RADINDX)  TO                                       
094100     4316-C-MID-IDRADNR (4316-4-INDX)                                     
094200     MOVE MID-KVLEVART (RADINDX) TO                                       
094300     4316-C-MID-KVORAPP (4316-4-INDX)                                     
094400     ADD +1                  TO  4316-4-INDX                              
094500     PERFORM IMS-GN-WDE4B1                                                
094600     .                                                                    
094700     EJECT                                                                
094800 GD-BEH-SISTA-RADERNA                    SECTION.                         
094900                                                                          
095000     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
095100                   4316-4-INDX = MAX-4316-4-INDX-PLUS-ETT                 
095200       PERFORM S13-BEHAND-AVV-VID-UTSKRIFT                                
095300       PERFORM IMS-GN-WDE4B1                                              
095400     END-PERFORM                                                          
095500     .                                                                    
095600     EJECT                                                                
096800 H-BEARBETA-4316-TYP002-003 SECTION.                                      
096900****************************************************************          
097000*    -KOLLISEGMENTET (TYP 2) SOM SKAPATS I TIDIGARE PROGRAM               
097100*     HÄMTAS.                                                             
097200*    -AVVIKELSESEGMENTEN (TYP 4) HÄMTAS                                   
097300*    -KOLLIRADER SKAPAS GENOM ATT MED HJÄLP AV ANNULLATIONERNA            
097400*     SKAPA RADINTERVALL OCH SEDAN FÖR VARJE INTERVALL KOLLA              
097500*     OM DET FINNS NÅGRA AVVIKELSER                                       
097600*****************************************************************         
097700                                                                          
097800     PERFORM S15-TA-FRAM-MAX-4316-4-RADNR                                 
097900                                                                          
098000     MOVE '003'                      TO W-IDPTYP-4316-MIN                 
098100                                        W-IDPTYP-4316-MAX                 
098200     PERFORM IMS-GU-4315-TYP2                                             
098300     PERFORM IMS-GHNP-4316-TYP2-L                                         
098400     IF SEGMENT-SAKNAS                                                    
098500         MOVE '002'                  TO W-IDPTYP-4316-MIN                 
098600                                        W-IDPTYP-4316-MAX                 
098700         PERFORM IMS-GU-4315-TYP2                                         
098800         PERFORM IMS-GHNP-4316-TYP2                                       
098900         IF SPAR-4316-4-INDX         = +1                                 
099000             MOVE '0001'           TO  4316-A-MID-IDRADNR-FOM (1)         
099100             MOVE +1                 TO 4316-2-INDX                       
099200          ELSE                                                            
099300             PERFORM S07-TA-FRAM-4316-2-INDX                              
099400         END-IF                                                           
099500      ELSE                                                                
099600         MOVE JA                     TO WS-FLREPLACE-TYP3                 
099700         PERFORM S05-TA-FRAM-4316-2-INDX                                  
099800     END-IF                                                               
099900     MOVE MAX-ANT-RAD                TO WS-ANT-RAD-REST                   
100000                                                                          
100100     MOVE SPAR-4316-4-INDX           TO 4316-4-INDX                       
100200                                                                          
100300     MOVE WS-IDPURAD-START        TO  WS-FOM                              
100400                                                                          
100500     PERFORM HA-KOLLIRADER-4301-4303                                      
100600                                                                          
100700     IF XXDL-4316-IDPTYP = '002'                                          
100800       MOVE '002'                    TO W-IDPTYP-4316-MIN                 
100900                                        W-IDPTYP-4316-MAX                 
101000       PERFORM IMS-REPL-4316-TYP2-3                                       
101100                                                                          
101200       IF 4316-A-MID-RAD (MAX-4316-2-INDX) NOT = ALL '+'                  
101300         MOVE '003'                    TO W-IDPTYP-4316-MIN               
101400                                          W-IDPTYP-4316-MAX               
101500         PERFORM S02-SKAPA-4316-TYP3-GEMEN                                
101600         PERFORM IMS-ISRT-4316-TYP3                                       
101700       END-IF                                                             
101800     ELSE                                                                 
101900       MOVE '003'                    TO W-IDPTYP-4316-MIN                 
102000                                        W-IDPTYP-4316-MAX                 
102100       IF WS-FLREPL-TYP3                                                  
102200           PERFORM IMS-REPL-4316-TYP2-3                                   
102300           MOVE NEJ                  TO WS-FLREPLACE-TYP3                 
102400        ELSE                                                              
102500           PERFORM IMS-ISRT-4316-TYP3                                     
102600       END-IF                                                             
102700                                                                          
102800       IF 4316-B-MID-RAD (MAX-4316-2-INDX) NOT = ALL '+'                  
102900         MOVE '003'                    TO W-IDPTYP-4316-MIN               
103000                                          W-IDPTYP-4316-MAX               
103100         PERFORM S02-SKAPA-4316-TYP3-GEMEN                                
103200         PERFORM IMS-ISRT-4316-TYP3                                       
103300       END-IF                                                             
103400     END-IF                                                               
103600     IF WS-FLREPL                                                         
103700         MOVE '004'                 TO W-IDPTYP-4316                      
103800         MOVE ZERO                  TO W-IDKOLLI-4316                     
103900         PERFORM IMS-REPL-4316-TYP4                                       
104000      ELSE                                                                
104100         PERFORM IMS-ISRT-4316-TYP4                                       
104200      END-IF                                                              
104400     .                                                                    
104500     EJECT                                                                
104600 HA-KOLLIRADER-4301-4303 SECTION.                                         
104700***************************************************************           
104800*  -FÖR VARJE ANNULERINGSSEGMENT SKAPAS UPPGIFTERNA RADNR-TOM             
104900*   FÖR ETT INTERVALL SAMT RADNR-FOM FÖR NÄSTA INTERVALL.                 
105000*  -FINNS INGET INTERVALL MELLAN ANNULLERINGSSEGMENTEN SKAPAS             
105100*   EN NY UPPGIFT RADNR-FOM FÖR FÖREGÅENDE INTERVALL.                     
105200*  -INNAN RADNR-TOM SKRIVS MÅSTE AVVIKELSERNA KONTROLLERAS DÅ             
105300*   DESSA SKALL SÄRREDOVISAS VILKET INNEBÄR SPLITTRING AV                 
105400*   INTERVALLET                                                           
105500****************************************************************          
105600                                                                          
105700     PERFORM IMS-GNP-4308-F                                               
105800     PERFORM UNTIL                                                        
105900            SEGMENT-SAKNAS OR SEGMENT-SLUT OR                             
106000            (XXDJ-4308-KVANNANT = ZERO AND                                
106100             XXDJ-4308-IDRADNR-ORD-FROM >= WS-FOM)                        
106200       PERFORM IMS-GNP-4308                                               
106300     END-PERFORM                                                          
106400     PERFORM UNTIL                                                        
106500      NOT ( SEGMENT-FINNS )                                               
106600       IF  WS-FOM < XXDJ-4308-IDRADNR-ORD-FROM                            
106700         COMPUTE WS-TOM = XXDJ-4308-IDRADNR-ORD-FROM - 1                  
106800         PERFORM HAA-AVVIKELSER                                           
106900         IF XXDL-4316-IDPTYP = '002'                                      
107000           IF  4316-A-MID-RAD (4316-2-INDX) NOT  = ALL '+'                
107100             MOVE WS-TOM  TO 4316-A-MID-IDRADNR-TOM (4316-2-INDX)         
107200             PERFORM S09-TOM-TEST                                         
107300             ADD +1                       TO 4316-2-INDX                  
107400             PERFORM S06-4316-2-INDX-TEST                                 
107500           END-IF                                                         
107600         ELSE                                                             
107700           IF  4316-B-MID-RAD (4316-2-INDX) NOT  =  ALL '+'               
107800             MOVE WS-TOM  TO 4316-B-MID-IDRADNR-TOM (4316-2-INDX)         
107900             PERFORM S10-TOM-TEST                                         
108000             ADD +1                       TO 4316-2-INDX                  
108100             PERFORM S06-4316-2-INDX-TEST                                 
108200           END-IF                                                         
108300         END-IF                                                           
108400       END-IF                                                             
108500       COMPUTE WS-FOM = XXDJ-4308-IDRADNR-ORD-TOM + 1                     
108510       MOVE WS-KVORDRAD           TO WS-TOM                               
108600       PERFORM S04-FLYTTA-FOM                                             
108700                                                                          
108800       PERFORM IMS-GNP-4308                                               
108900       PERFORM UNTIL                                                      
109000        NOT ( SEGMENT-FINNS AND XXDJ-4308-KVANNANT NOT = ZERO )           
109100         PERFORM IMS-GNP-4308                                             
109200       END-PERFORM                                                        
109300     END-PERFORM                                                          
109400     MOVE WS-KVORDRAD             TO WS-TOM                               
109410     PERFORM S04-FLYTTA-FOM                                               
109500     PERFORM HAA-AVVIKELSER                                               
109600     IF XXDL-4316-IDPTYP = '002'                                          
109700       IF  4316-A-MID-RAD (4316-2-INDX) NOT  = ALL '+'                    
109800         IF WS-FOM   <   WS-TOM                                           
109900           MOVE WS-TOM    TO 4316-A-MID-IDRADNR-TOM (4316-2-INDX)         
110000         ELSE                                                             
110100           EVALUATE TRUE                                                  
110200           WHEN WS-FOM = WS-TOM                                           
110300             MOVE ALL '+' TO                                              
110400                        4316-A-MID-IDRADNR-TOM (4316-2-INDX)              
110500            WHEN OTHER                                                    
110600             MOVE ALL '+' TO                                              
110700                        4316-A-MID-IDRADNR-FOM (4316-2-INDX)              
110800           END-EVALUATE                                                   
110900         END-IF                                                           
111000       END-IF                                                             
111100     ELSE                                                                 
111200       IF  4316-B-MID-RAD (4316-2-INDX) NOT  =  ALL '+'                   
111300         IF WS-FOM  <  WS-TOM                                             
111400           MOVE WS-TOM    TO 4316-B-MID-IDRADNR-TOM (4316-2-INDX)         
111500         ELSE                                                             
111600           EVALUATE TRUE                                                  
111700           WHEN WS-FOM = WS-TOM                                           
111800             MOVE ALL '+' TO                                              
111900                        4316-B-MID-IDRADNR-TOM (4316-2-INDX)              
112000            WHEN OTHER                                                    
112100             MOVE ALL '+' TO                                              
112200                        4316-B-MID-IDRADNR-FOM (4316-2-INDX)              
112300           END-EVALUATE                                                   
112400         END-IF                                                           
112500       END-IF                                                             
112600     END-IF                                                               
112700     .                                                                    
112800     EJECT                                                                
112900 HAA-AVVIKELSER SECTION.                                                  
113000******************************************************************        
113100*   -ALLA AVVIKELSER REDOVISAS SEPARAT MED DEN LEVERERADE KVAN-           
113200*    TITEN ANGIVEN. DOCK EJ AVVIKELSER MED KVANT = 0                      
113300*   -OM RADERNA EJ FÅR PLATS I ETT KOLLI FÅR YTTERLIGARE KOLLIN           
113400*    SKAPAS                                                               
113500*   -I AVVIKELSE-SEGMENTET ERSÄTTS DEN LEVERERADE KVANTITEN MED           
113600*    DIFFERENSEN AV DET BESTÄLLDA OCH DET LEVERERADE                      
113700******************************************************************        
113800                                                                          
113900     PERFORM UNTIL 4316-4-INDX = MAX-4316-4-INDX-PLUS-ETT    OR           
114000                   4316-C-MID-RAD (4316-4-INDX) = ALL '+'    OR           
114100                   4316-C-MID-IDRADNR (4316-4-INDX) > WS-TOM              
114200                                                                          
114300       IF  4316-C-MID-IDRADNR (4316-4-INDX) = WS-FOM                      
114400                                                                          
114500         IF  4316-C-MID-IDRADNR (4316-4-INDX) < WS-TOM                    
114600                                                                          
114700           IF  4316-C-MID-KVORAPP  (4316-4-INDX) > 0                      
114800                                                                          
114900             IF XXDL-4316-IDPTYP = '002'                                  
115000               MOVE ALL '+'                               TO              
115100                             4316-A-MID-IDRADNR-TOM (4316-2-INDX)         
115200               MOVE 4316-C-MID-KVORAPP  (4316-4-INDX)     TO              
115300                             4316-A-MID-KVLEVART (4316-2-INDX)            
115400             ELSE                                                         
115500               MOVE ALL '+'                               TO              
115600                             4316-B-MID-IDRADNR-TOM (4316-2-INDX)         
115700               MOVE 4316-C-MID-KVORAPP  (4316-4-INDX)     TO              
115800                             4316-B-MID-KVLEVART (4316-2-INDX)            
115900             END-IF                                                       
116000             ADD +1                            TO 4316-2-INDX             
116100             PERFORM S06-4316-2-INDX-TEST                                 
116200           END-IF                                                         
116300           COMPUTE WS-FOM =                                               
116400                         4316-C-MID-IDRADNR (4316-4-INDX) + 1             
116500           PERFORM S04-FLYTTA-FOM                                         
116600                                                                          
116700         ELSE                                                             
116800           IF      4316-C-MID-KVORAPP  ( 4316-4-INDX) > 0                 
116900             IF XXDL-4316-IDPTYP = '002'                                  
117000               MOVE 4316-C-MID-KVORAPP  (4316-4-INDX)      TO             
117100                              4316-A-MID-KVLEVART (4316-2-INDX)           
117200             ELSE                                                         
117300               MOVE 4316-C-MID-KVORAPP  (4316-4-INDX)      TO             
117400                              4316-B-MID-KVLEVART (4316-2-INDX)           
117500             END-IF                                                       
117600           ELSE                                                           
117700             IF XXDL-4316-IDPTYP = '002'                                  
117800               MOVE ALL '+' TO                                            
117900                          4316-A-MID-IDRADNR-FOM (4316-2-INDX)            
118000             ELSE                                                         
118100               MOVE ALL '+' TO                                            
118200                          4316-B-MID-IDRADNR-FOM (4316-2-INDX)            
118300             END-IF                                                       
118400           END-IF                                                         
118500         END-IF                                                           
118600         PERFORM HAAA-UTRAEKNING-AV-DIFF                                  
118700       ELSE                                                               
118800         EVALUATE TRUE                                                    
118900         WHEN 4316-C-MID-IDRADNR (4316-4-INDX) > WS-FOM                   
119000           IF      4316-C-MID-IDRADNR (4316-4-INDX) < WS-TOM              
119100             COMPUTE WS-RADNR                                             
119200                          = 4316-C-MID-IDRADNR (4316-4-INDX) - 1          
119300                                                                          
119400             IF XXDL-4316-IDPTYP = '002'                                  
119500               MOVE WS-RADNR TO 4316-A-MID-IDRADNR-TOM                    
119600               (4316-2-INDX)                                              
119700               PERFORM S09-TOM-TEST                                       
119800             ELSE                                                         
119900               MOVE WS-RADNR TO 4316-B-MID-IDRADNR-TOM                    
120000               (4316-2-INDX)                                              
120100               PERFORM S10-TOM-TEST                                       
120200             END-IF                                                       
120300             ADD +1                            TO 4316-2-INDX             
120400             PERFORM S06-4316-2-INDX-TEST                                 
120500                                                                          
120600             IF      4316-C-MID-KVORAPP  ( 4316-4-INDX) > 0               
120700                                                                          
120800               IF XXDL-4316-IDPTYP = '002'                                
120900                 MOVE 4316-C-MID-IDRADNR (4316-4-INDX)      TO            
121000                               4316-A-MID-IDRADNR-FOM                     
121100                 (4316-2-INDX)                                            
121200                 MOVE ALL '+'                               TO            
121300                               4316-A-MID-IDRADNR-TOM                     
121400                 (4316-2-INDX)                                            
121500                 MOVE 4316-C-MID-KVORAPP  (4316-4-INDX)      TO           
121600                               4316-A-MID-KVLEVART (4316-2-INDX)          
121700               ELSE                                                       
121800                 MOVE 4316-C-MID-IDRADNR (4316-4-INDX)      TO            
121900                               4316-B-MID-IDRADNR-FOM                     
122000                 (4316-2-INDX)                                            
122100                 MOVE ALL '+'                               TO            
122200                               4316-B-MID-IDRADNR-TOM                     
122300                 (4316-2-INDX)                                            
122400                 MOVE 4316-C-MID-KVORAPP  (4316-4-INDX)     TO            
122500                               4316-B-MID-KVLEVART (4316-2-INDX)          
122600               END-IF                                                     
122700               ADD +1                            TO 4316-2-INDX           
122800               PERFORM S06-4316-2-INDX-TEST                               
122900             END-IF                                                       
123000             COMPUTE WS-FOM =                                             
123100                          4316-C-MID-IDRADNR (4316-4-INDX) + 1            
123200             PERFORM S04-FLYTTA-FOM                                       
123300                                                                          
123400           ELSE                                                           
123500             IF      4316-C-MID-KVORAPP  (4316-4-INDX) > 0                
123600                                                                          
123700               COMPUTE WS-RADNR                                           
123800                     = 4316-C-MID-IDRADNR (4316-4-INDX) - 1               
123900                                                                          
124000               IF XXDL-4316-IDPTYP = '002'                                
124100                 MOVE WS-RADNR                                            
124200                       TO 4316-A-MID-IDRADNR-TOM (4316-2-INDX)            
124300                 PERFORM S09-TOM-TEST                                     
124400               ELSE                                                       
124500                 MOVE WS-RADNR                                            
124600                       TO 4316-B-MID-IDRADNR-TOM (4316-2-INDX)            
124700                 PERFORM S10-TOM-TEST                                     
124800               END-IF                                                     
124900               ADD +1                            TO 4316-2-INDX           
125000               PERFORM S06-4316-2-INDX-TEST                               
125100               MOVE 4316-C-MID-IDRADNR (4316-4-INDX)      TO              
125200               WS-FOM                                                     
125300               PERFORM S04-FLYTTA-FOM                                     
125400               IF XXDL-4316-IDPTYP = '002'                                
125500                 MOVE 4316-C-MID-KVORAPP  (4316-4-INDX)   TO              
125600                                4316-A-MID-KVLEVART (4316-2-INDX)         
125700               ELSE                                                       
125800                 MOVE 4316-C-MID-KVORAPP  (4316-4-INDX)   TO              
125900                                4316-B-MID-KVLEVART (4316-2-INDX)         
126000               END-IF                                                     
126100             ELSE                                                         
126200               COMPUTE WS-TOM =                                           
126300                       4316-C-MID-IDRADNR (4316-4-INDX) - 1               
126400             END-IF                                                       
126500           END-IF                                                         
126600           PERFORM HAAA-UTRAEKNING-AV-DIFF                                
126700         END-EVALUATE                                                     
126800       END-IF                                                             
126900       ADD +1                            TO 4316-4-INDX                   
127000     END-PERFORM                                                          
127100     .                                                                    
127200     EJECT                                                                
127300 HAAA-UTRAEKNING-AV-DIFF SECTION.                                         
127400******************************************************************        
127500*                                                                         
127600*                                                                         
127700******************************************************************        
127800                                                                          
127900     MOVE 4316-C-MID-IDRADNR (4316-4-INDX) TO W-WDE4B1KY-IDPURAD          
128000     PERFORM IMS-GU-WDE4B1                                                
128100                                                                          
128200     COMPUTE WS-KVLEVART =                                                
128300          SEQB-KVAVBART - 4316-C-MID-KVORAPP (4316-4-INDX)                
128400     MOVE WS-KVLEVART      TO 4316-C-MID-KVORAPP  (4316-4-INDX)           
128500     .                                                                    
128600                                                                          
128700     EJECT                                                                
128800 I-AVSLUTA-BEARBETA SECTION.                                              
128900*****************************************************************         
129000*    OM KOLLIUPPGIFTER FINNS                                              
129100*    - HÄNDELSESEGMENTET UPPDATERAS (4312)                                
129200*     -PACKNINGSSEGMENTEN (4316) TYP=4  UPPDATERAS                        
129300*     -EN MOD TILL KOLLIPROGRAMMET LÄGGS UT                               
129400*    OM KOLLIUPPGIFTER INTE FINNS                                         
129500*     -HÄNDELSESEGMENET TAS BORT (4312)                                   
129600*     -LÅSNINGSSEGMENTET UPPDATERAS  (4306)                               
129700*     -PACKNINGSSEGMENTEN (4316) TYP=2,3,4 UPPDATERAS                     
129800*     -EN TRANS SKAPAS MED ALT-PCB                                        
129900*     -EN MOD LÄGGS UT                                                    
130000*****************************************************************         
130100                                                                          
130200     PERFORM IMS-GU-4311                                                  
130300     PERFORM IMS-GHNP-4312                                                
130400                                                                          
130500     IF  (XXDK-4312-KDBEHAND-KOL = 0  OR 2)                               
130600     AND (XXDK-4312-KDBEHAND-URS = 0  OR 2)                               
130700                                                                          
130800       PERFORM IMS-DLET-4312                                              
130900                                                                          
131000       PERFORM IMS-GU-4305                                                
131100       PERFORM IMS-GHNP-4306                                              
131110********* FIX 920518 PER BERGH *************                              
131120*    IF     XXDJ-4306-IDPRODNR  = 0150395                                 
131130*       PERFORM IMS-GHNP-4306                                             
131140*    END-IF                                                               
131150********* FIX SLUT   PER BERGH *************                              
131200       MOVE +3 TO XXDJ-4306-KDPACLAS                                      
131300       PERFORM IMS-REPL-4306                                              
131400                                                                          
131500       PERFORM IMS-GU-4315-TYP2                                           
131600       MOVE '002'                     TO W-IDPTYP-4316-MIN                
131700                                         W-IDPTYP-4316-MAX                
131800       PERFORM IMS-GHNP-4316-TYP2                                         
131900       IF  4316-A-MID-RAD (1) NOT = ALL '+'                               
132000         MOVE +1                    TO XXDL-4316-KDTRSTAT                 
132100         PERFORM IMS-REPL-4316-TYP2-3                                     
132200       ELSE                                                               
132300         PERFORM IMS-DLET-4316                                            
132400         MOVE JA TO HELT-NOLLAD-ORDER                                     
132500       END-IF                                                             
132600       PERFORM IMS-GHNP-4316-TYP2-STAT-GE                                 
132700       IF SEGMENT-FINNS                                                   
132800         IF  HELT-NOLLAD-ORDER = NEJ                                      
132900           MOVE +1                    TO XXDL-4316-KDTRSTAT               
133000           PERFORM IMS-REPL-4316-TYP2-3                                   
133100         ELSE                                                             
133200           PERFORM IMS-DLET-4316                                          
133300         END-IF                                                           
133400       END-IF                                                             
133500       MOVE '003'                     TO W-IDPTYP-4316-MIN                
133600                                         W-IDPTYP-4316-MAX                
133700       PERFORM IA-UPPDATERA-4316-TYP3-STATUS                              
133800                                                                          
133900       MOVE '004'                     TO W-IDPTYP-4316                    
134000       MOVE ZERO                      TO W-IDKOLLI-4316                   
134100                                                                          
134200       PERFORM IMS-GU-4315-TYP4                                           
134300       PERFORM IMS-GHNP-4316-TYP4-L                                       
134400                                                                          
134500       IF 4316-C-MID-RAD (MAX-4316-4-INDX)  NOT = ALL '+'                 
134501*        4398-TRANS FORTSÄTTER I YTTERLIGARE EN 4398-TRANS.               
134502         MOVE JA                    TO 4316-C-MID-FLSVAR                  
134503         PERFORM IMS-REPL-4316-TYP4                                       
134510                                                                          
134600         PERFORM S03-SKAPA-4316-TYP4-GEMEN                                
134700         PERFORM IMS-ISRT-4316-TYP4                                       
134800       END-IF                                                             
134900       PERFORM IB-UPPDATERA-4316-TYP4-STATUS                              
135000                                                                          
135100       MOVE MFS-KDMFSFOR            TO PTOP-KDMFSFOR                      
135200       PERFORM IMS-ISRT-MSG-ALT-PCB                                       
135300                                                                          
135400*********************************************************                 
135500                                                                          
135600*** FINNS DET FLER ORDER SOM LIGGER PÅ KÖ FÖR BEARBETNING                 
135700*** LÄGGS DEN FÖRSTA ORDERN UT PÅ DEN BILD SOM STÅR                       
135800*** I TUR FÖR BEARBETNING                                                 
135900*** KVAL MED USERID                                                       
136000                                                                          
136100       MOVE MSG-SIGNON-USERID TO W-WDGXKEY-IDUSER-4312                    
136200       PERFORM IMS-GET-XXDK-4312-STAT-GE-F                                
136300                                                                          
136400       PERFORM UNTIL                                                      
136500        NOT ( ((SEGMENT-FINNS) AND (XXDK-4312-KDBEHAND-AVVIK = +3         
136600          OR XXDK-4312-KDBEHAND-RAD = +3 OR                               
136700          XXDK-4312-KDBEHAND-DEL = +3 OR                                  
136800          XXDK-4312-KDBEHAND-URS = +3 OR                                  
136900          XXDK-4312-KDBEHAND-KOL = +3)) )                                 
137000         PERFORM IMS-GET-XXDK-4312-STAT-GE                                
137100       END-PERFORM                                                        
137200       IF SEGMENT-FINNS                                                   
137300         IF XXDK-4312-KDBEHAND-AVVIK = 1                                  
137400           MOVE 'W4O39101'         TO MFS-IDMOD                           
137500           MOVE MAX-MOD-LAENGD TO MSG-KVLL                                
137600           MOVE '4391'             TO MOD-IDTRANS                         
137700           MOVE XXDK-4312-IDPRODNR TO WS-IDPRODNR                         
137800           MOVE WS-IDPRODNR        TO MOD-IDPRODNR-UT                     
137900           MOVE XXDK-4312-IDDISTR  TO WS-IDDISTR                          
138000           MOVE WS-IDDISTR         TO MOD-IDDISTR-UT                      
138100           MOVE XXDK-4312-IDKUNDNR TO WS-IDKUNDNR                         
138200           MOVE WS-IDKUNDNR        TO MOD-IDKUNDNR-UT                     
138300           MOVE XXDK-4312-KDFRAKT  TO WS-KDFRAKT                          
138400           MOVE WS-KDFRAKT         TO MOD-KDFRAKT-UT                      
138500           MOVE XXDK-4312-IDKUNDRF TO MOD-IDORDNR-UT                      
138600           MOVE XXDK-4312-KDORDKL  TO WS-KDORDKL                          
138700           MOVE WS-KDORDKL         TO MOD-KDORDKL-UT                      
138710           MOVE WS-IDDC            TO MOD-IDDC-UT                         
138720           MOVE WS-PRTVAL          TO MOD-PRTVAL-ADRESSFL                 
138800                                                                          
138900           MOVE MFS-RENSA-FAELT    TO MOD-IDRADNR-SENAST                  
139000                                                                          
139100           INSPECT MOD-IDPRODNR-UT REPLACING                              
139200                                   LEADING ZERO BY SPACE                  
139300           INSPECT MOD-IDDISTR-UT REPLACING                               
139400                                   LEADING ZERO BY SPACE                  
139500           INSPECT MOD-IDKUNDNR-UT REPLACING                              
139600                                   LEADING ZERO BY SPACE                  
139700           INSPECT MOD-KDFRAKT-UT REPLACING                               
139800                                   LEADING ZERO BY SPACE                  
139900           INSPECT MOD-IDORDNR-UT REPLACING                               
140000                                   LEADING ZERO BY SPACE                  
140100                                                                          
140200           PERFORM S12-FORMATETS-ATTR                                     
140300           MOVE MFS-RENSA-FAELT  TO MOD-IDRADNR-SENAST                    
140400                                                                          
140500         ELSE                                                             
140600           EVALUATE TRUE                                                  
140700           WHEN XXDK-4312-KDBEHAND-AVVIK = 4                              
140800             MOVE XXDK-4312-IDPRODNR TO WS-IDPRODNR                       
140900             MOVE WS-IDPRODNR        TO PTOP1-IDPRODNR-IN                 
141000             PTOP1-IDPRODNR-UT                                            
141100             MOVE XXDK-4312-IDDISTR  TO WS-IDDISTR                        
141200             MOVE WS-IDDISTR         TO PTOP1-IDDISTR-UT                  
141300             MOVE XXDK-4312-IDKUNDNR TO WS-IDKUNDNR                       
141400             MOVE WS-IDKUNDNR        TO PTOP1-IDKUNDNR-UT                 
141500             MOVE XXDK-4312-KDFRAKT  TO WS-KDFRAKT                        
141600             MOVE WS-KDFRAKT         TO PTOP1-KDFRAKT-UT                  
141700             MOVE XXDK-4312-IDKUNDRF TO PTOP1-IDORDNR-UT                  
141800             MOVE XXDK-4312-KDORDKL  TO WS-KDORDKL                        
141900             MOVE WS-KDORDKL         TO PTOP1-KDORDKL-UT                  
141910             MOVE WS-IDDC            TO PTOP1-IDDC-UT                     
141920             MOVE WS-PRTVAL          TO PTOP1-PRTVAL-ADRESSFL             
142000             MOVE MFS-KDMFSFOR       TO PTOP1-KDMFSFOR                    
142100             MOVE WS-STARTA-4391     TO WS-MSG-CALL                       
142200           WHEN XXDK-4312-KDBEHAND-RAD = 1                                
142300             MOVE 'W4O39301'         TO MFS-IDMOD                         
142400             MOVE 4393-MOD-LAENGD    TO MSG-KVLL                          
142500             MOVE '4393'             TO M4393-MOD-IDTRANS                 
142600             MOVE XXDK-4312-IDPRODNR TO WS-IDPRODNR                       
142700             MOVE WS-IDPRODNR        TO M4393-MOD-IDPRODNR-UT             
142800             MOVE XXDK-4312-IDDISTR  TO WS-IDDISTR                        
142900             MOVE WS-IDDISTR         TO M4393-MOD-IDDISTR-UT              
143000             MOVE XXDK-4312-IDKUNDNR TO WS-IDKUNDNR                       
143100             MOVE WS-IDKUNDNR        TO M4393-MOD-IDKUNDNR-UT             
143200             MOVE XXDK-4312-KDFRAKT  TO WS-KDFRAKT                        
143300             MOVE WS-KDFRAKT         TO M4393-MOD-KDFRAKT-UT              
143400             MOVE XXDK-4312-IDKUNDRF TO M4393-MOD-IDORDNR-UT              
143500             MOVE XXDK-4312-KDORDKL  TO WS-KDORDKL                        
143600             MOVE WS-KDORDKL         TO M4393-MOD-KDORDKL-UT              
143610             MOVE WS-IDDC            TO M4393-MOD-IDDC-UT                 
143620             MOVE WS-PRTVAL          TO M4393-MOD-PRTVAL-ADRESSFL         
143700                                                                          
143800             INSPECT M4393-MOD-IDPRODNR-UT REPLACING                      
143900                                     LEADING ZERO BY SPACE                
144000             INSPECT M4393-MOD-IDDISTR-UT REPLACING                       
144100                                     LEADING ZERO BY SPACE                
144200             INSPECT M4393-MOD-IDKUNDNR-UT REPLACING                      
144300                                     LEADING ZERO BY SPACE                
144400             INSPECT M4393-MOD-KDFRAKT-UT REPLACING                       
144500                                     LEADING ZERO BY SPACE                
144600             INSPECT M4393-MOD-IDORDNR-UT REPLACING                       
144700                                     LEADING ZERO BY SPACE                
144800           WHEN XXDK-4312-KDBEHAND-URS = 1                                
144900             MOVE 'W4O39401' TO MFS-IDMOD                                 
145000             MOVE 4394-MOD-LAENGD TO MSG-KVLL                             
145100             MOVE '4394'             TO M4394-MOD-IDTRANS                 
145200             MOVE XXDK-4312-IDPRODNR TO WS-IDPRODNR                       
145300             MOVE WS-IDPRODNR        TO M4394-MOD-IDPRODNR-UT             
145400             MOVE XXDK-4312-IDDISTR  TO WS-IDDISTR                        
145500             MOVE WS-IDDISTR         TO M4394-MOD-IDDISTR-UT              
145600             MOVE XXDK-4312-IDKUNDNR TO WS-IDKUNDNR                       
145700             MOVE WS-IDKUNDNR        TO M4394-MOD-IDKUNDNR-UT             
145800             MOVE XXDK-4312-KDFRAKT  TO WS-KDFRAKT                        
145900             MOVE WS-KDFRAKT         TO M4394-MOD-KDFRAKT-UT              
146000             MOVE XXDK-4312-IDKUNDRF TO M4394-MOD-IDORDNR-UT              
146100             MOVE XXDK-4312-KDORDKL  TO WS-KDORDKL                        
146200             MOVE WS-KDORDKL         TO M4394-MOD-KDORDKL-UT              
146210             MOVE WS-IDDC            TO M4394-MOD-IDDC-UT                 
146220             MOVE WS-PRTVAL          TO M4394-MOD-PRTVAL-ADRESSFL         
146300                                                                          
146400             INSPECT M4394-MOD-IDPRODNR-UT REPLACING                      
146500                                     LEADING ZERO BY SPACE                
146600             INSPECT M4394-MOD-IDDISTR-UT REPLACING                       
146700                                     LEADING ZERO BY SPACE                
146800             INSPECT M4394-MOD-IDKUNDNR-UT REPLACING                      
146900                                     LEADING ZERO BY SPACE                
147000             INSPECT M4394-MOD-KDFRAKT-UT REPLACING                       
147100                                     LEADING ZERO BY SPACE                
147200             INSPECT M4394-MOD-IDORDNR-UT REPLACING                       
147300                                     LEADING ZERO BY SPACE                
147400           WHEN XXDK-4312-KDBEHAND-DEL = 1                                
147500             MOVE 'W4O39501' TO MFS-IDMOD                                 
147600             MOVE 4395-MOD-LAENGD TO MSG-KVLL                             
147700             MOVE '4395'             TO M4395-MOD-IDTRANS                 
147800             MOVE XXDK-4312-IDPRODNR TO WS-IDPRODNR                       
147900             MOVE WS-IDPRODNR        TO M4395-MOD-IDPRODNR-UT             
148000             MOVE XXDK-4312-IDDISTR  TO WS-IDDISTR                        
148100             MOVE WS-IDDISTR         TO M4395-MOD-IDDISTR-UT              
148200             MOVE XXDK-4312-IDKUNDNR TO WS-IDKUNDNR                       
148300             MOVE WS-IDKUNDNR        TO M4395-MOD-IDKUNDNR-UT             
148400             MOVE XXDK-4312-KDFRAKT  TO WS-KDFRAKT                        
148500             MOVE WS-KDFRAKT         TO M4395-MOD-KDFRAKT-UT              
148600             MOVE XXDK-4312-IDKUNDRF TO M4395-MOD-IDORDNR-UT              
148700             MOVE XXDK-4312-KDORDKL  TO WS-KDORDKL                        
148800             MOVE WS-KDORDKL         TO M4395-MOD-KDORDKL-UT              
148810             MOVE WS-IDDC            TO M4395-MOD-IDDC-UT                 
148820             MOVE WS-PRTVAL          TO M4395-MOD-PRTVAL-ADRESSFL         
148900                                                                          
149000             INSPECT M4395-MOD-IDPRODNR-UT REPLACING                      
149100                                     LEADING ZERO BY SPACE                
149200             INSPECT M4395-MOD-IDDISTR-UT REPLACING                       
149300                                     LEADING ZERO BY SPACE                
149400             INSPECT M4395-MOD-IDKUNDNR-UT REPLACING                      
149500                                     LEADING ZERO BY SPACE                
149600             INSPECT M4395-MOD-KDFRAKT-UT REPLACING                       
149700                                     LEADING ZERO BY SPACE                
149800             INSPECT M4395-MOD-IDORDNR-UT REPLACING                       
149900                                     LEADING ZERO BY SPACE                
150000            WHEN OTHER                                                    
150100             MOVE XXDK-4312-IDPRODNR TO WS-IDPRODNR                       
150200             MOVE WS-IDPRODNR        TO PTOP2-IDPRODNR-IN                 
150300                                        PTOP2-IDPRODNR-UT                 
150400             MOVE XXDK-4312-IDDISTR  TO WS-IDDISTR                        
150500             MOVE WS-IDDISTR         TO PTOP2-IDDISTR-UT                  
150600             MOVE XXDK-4312-IDKUNDNR TO WS-IDKUNDNR                       
150700             MOVE WS-IDKUNDNR        TO PTOP2-IDKUNDNR-UT                 
150800             MOVE XXDK-4312-KDFRAKT  TO WS-KDFRAKT                        
150900             MOVE WS-KDFRAKT         TO PTOP2-KDFRAKT-UT                  
151000             MOVE XXDK-4312-IDKUNDRF TO PTOP2-IDORDNR-UT                  
151100             MOVE XXDK-4312-KDORDKL  TO WS-KDORDKL                        
151200             MOVE WS-KDORDKL         TO PTOP2-KDORDKL-UT                  
151210             MOVE WS-IDDC            TO PTOP2-IDDC-UT                     
151220             MOVE WS-PRTVAL          TO PTOP2-PRTVAL-ADRESSFL             
151300             MOVE MFS-KDMFSFOR       TO PTOP2-KDMFSFOR                    
151400             MOVE WS-STARTA-4392     TO WS-MSG-CALL                       
151500           END-EVALUATE                                                   
151600         END-IF                                                           
151700       ELSE                                                               
151800         IF WS-4312IDTRANS = '4301'                                       
151900           MOVE 'W4O30101'            TO MFS-IDMOD                        
152000           MOVE 4301-MOD-LAENGD       TO MSG-KVLL                         
152100           MOVE '4301'                TO M4301-MOD-IDTRANS                
152200           MOVE +1 TO RADINDX                                             
152300           PERFORM UNTIL RADINDX NOT < MAX-RADINDX-PLUS-ETT               
152400             MOVE MFS-RENSA-FAELT    TO                                   
152410                           M4301-MOD-IDDISTR            (RADINDX)         
152500                           M4301-MOD-IDPRODNR           (RADINDX)         
152600                           M4301-MOD-FLAVVPACK          (RADINDX)         
152700                           M4301-MOD-KDKOLLI            (RADINDX)         
152800                           M4301-MOD-VKORDBTO           (RADINDX)         
152900                           M4301-MOD-KDEMBTYP           (RADINDX)         
153000                           M4301-MOD-IDPRODNR-SAMP      (RADINDX)         
153010             MOVE MFS-FORMATETS-ATTR TO                                   
153011                           M4301-MOD-IDDISTR-ATTR       (RADINDX)         
153020                           M4301-MOD-IDPRODNR-ATTR      (RADINDX)         
153030                           M4301-MOD-FLAVVPACK-ATTR     (RADINDX)         
153040                           M4301-MOD-KDKOLLI-ATTR       (RADINDX)         
153050                           M4301-MOD-VKORDBTO-ATTR      (RADINDX)         
153060                           M4301-MOD-KDEMBTYP-ATTR      (RADINDX)         
153070                           M4301-MOD-IDPRODNR-SAMP-ATTR (RADINDX)         
153100             ADD +1 TO RADINDX                                            
153200           END-PERFORM                                                    
153300           MOVE INF-1  (INDX)         TO M4301-MOD-TEMFSINF               
153400         ELSE                                                             
153500           EVALUATE TRUE                                                  
153600           WHEN WS-4312IDTRANS = '4302'                                   
153700             MOVE 'W4O30201'            TO MFS-IDMOD                      
153800             MOVE 4302-MOD-LAENGD       TO MSG-KVLL                       
153900             MOVE '4302'                TO M4302-MOD-IDTRANS              
154000             MOVE +1                    TO RADINDX                        
154100             PERFORM UNTIL RADINDX NOT < MAX-RADINDX-PLUS-ETT             
154200              MOVE MFS-RENSA-FAELT TO                                     
154210                           M4302-MOD-IDDISTR            (RADINDX)         
154300                           M4302-MOD-IDPRODNR           (RADINDX)         
154400                           M4302-MOD-FLAVVPACK          (RADINDX)         
154500                           M4302-MOD-IDKOLLI            (RADINDX)         
154600                           M4302-MOD-KDKOLLI            (RADINDX)         
154700                           M4302-MOD-VKORDBTO-KOLLI     (RADINDX)         
154800                           M4302-MOD-ADFLGEO            (RADINDX)         
154900                           M4302-MOD-ADRUTHYL           (RADINDX)         
155000                           M4302-MOD-KDEMBTYP           (RADINDX)         
155100                           M4302-MOD-DIKOLLIL           (RADINDX)         
155200                           M4302-MOD-DIKOLLIB           (RADINDX)         
155300                           M4302-MOD-DIKOLLIH           (RADINDX)         
155310              MOVE MFS-FORMATETS-ATTR TO                                  
155320                           M4302-MOD-IDDISTR-ATTR       (RADINDX)         
155330                           M4302-MOD-IDPRODNR-ATTR      (RADINDX)         
155340                           M4302-MOD-FLAVVPACK-ATTR     (RADINDX)         
155350                           M4302-MOD-IDKOLLI-ATTR       (RADINDX)         
155360                           M4302-MOD-KDKOLLI-ATTR       (RADINDX)         
155370                           M4302-MOD-VKORDBTO-KOLLI-ATTR(RADINDX)         
155380                           M4302-MOD-ADFLGEO-ATTR       (RADINDX)         
155390                           M4302-MOD-ADRUTHYL-ATTR      (RADINDX)         
155391                           M4302-MOD-KDEMBTYP-ATTR      (RADINDX)         
155392                           M4302-MOD-DIKOLLIL-ATTR      (RADINDX)         
155393                           M4302-MOD-DIKOLLIB-ATTR      (RADINDX)         
155400               ADD +1                 TO RADINDX                          
155500             END-PERFORM                                                  
155600             MOVE INF-1  (INDX)         TO M4302-MOD-TEMFSINF             
155700           WHEN WS-4312IDTRANS = '4303'                                   
155800             MOVE 'W4O30301'            TO MFS-IDMOD                      
155900             MOVE 4303-MOD-LAENGD       TO MSG-KVLL                       
156000             MOVE '4303'                TO M4303-MOD-IDTRANS              
156010             MOVE WS-IDDC               TO M4303-MOD-IDDC-UT              
156100             MOVE +1 TO RADINDX                                           
156200             PERFORM UNTIL RADINDX NOT < MAX-RADINDX-PLUS-ETT             
156300              MOVE MFS-RENSA-FAELT TO M4303-MOD-IDDISTR (RADINDX)         
156400                                      M4303-MOD-IDPRODNR(RADINDX)         
156500                                     M4303-MOD-FLAVVPACK(RADINDX)         
156510              MOVE MFS-FORMATETS-ATTR TO                                  
156520                           M4303-MOD-IDDISTR-ATTR       (RADINDX)         
156530                           M4303-MOD-IDPRODNR-ATTR      (RADINDX)         
156540                           M4303-MOD-FLAVVPACK-ATTR     (RADINDX)         
156600                ADD +1 TO RADINDX                                         
156700             END-PERFORM                                                  
156800             MOVE INF-1  (INDX)         TO M4303-MOD-TEMFSINF             
156900           END-EVALUATE                                                   
157000         END-IF                                                           
157100       END-IF                                                             
157200     ELSE                                                                 
157300       MOVE +2                      TO XXDK-4312-KDBEHAND-AVVIK           
157400       PERFORM IMS-REPL-4312                                              
157500                                                                          
157600       IF XXDK-4312-KDBEHAND-KOL = 1                                      
157700         MOVE XXDK-4312-IDPRODNR   TO WS-IDPRODNR                         
157800         MOVE WS-IDPRODNR          TO PTOP2-IDPRODNR-IN                   
157900                                      PTOP2-IDPRODNR-UT                   
158000         MOVE XXDK-4312-IDDISTR    TO WS-IDDISTR                          
158100         MOVE WS-IDDISTR           TO PTOP2-IDDISTR-UT                    
158200         MOVE XXDK-4312-IDKUNDNR   TO WS-IDKUNDNR                         
158300         MOVE WS-IDKUNDNR          TO PTOP2-IDKUNDNR-UT                   
158400         MOVE XXDK-4312-KDFRAKT    TO WS-KDFRAKT                          
158500         MOVE WS-KDFRAKT           TO PTOP2-KDFRAKT-UT                    
158600         MOVE XXDK-4312-IDKUNDRF   TO PTOP2-IDORDNR-UT                    
158700         MOVE XXDK-4312-KDORDKL    TO WS-KDORDKL                          
158800         MOVE WS-KDORDKL           TO PTOP2-KDORDKL-UT                    
158810         MOVE WS-IDDC              TO PTOP2-IDDC-UT                       
158820         MOVE WS-PRTVAL            TO PTOP2-PRTVAL-ADRESSFL               
158900         MOVE MFS-KDMFSFOR         TO PTOP2-KDMFSFOR                      
159000         MOVE WS-STARTA-4392       TO WS-MSG-CALL                         
159100       ELSE                                                               
159200         MOVE 'W4O39401'           TO MFS-IDMOD                           
159300         MOVE 4394-MOD-LAENGD      TO MSG-KVLL                            
159400         MOVE '4394'               TO M4394-MOD-IDTRANS                   
159500         MOVE MID-IDPRODNR-UT      TO M4394-MOD-IDPRODNR-UT               
159600         MOVE MID-IDDISTR-UT       TO M4394-MOD-IDDISTR-UT                
159700         MOVE MID-IDKUNDNR-UT      TO M4394-MOD-IDKUNDNR-UT               
159800         MOVE MID-KDFRAKT-UT       TO M4394-MOD-KDFRAKT-UT                
159900         MOVE MID-IDORDNR-UT       TO M4394-MOD-IDORDNR-UT                
160000         MOVE MID-KDORDKL-UT       TO M4394-MOD-KDORDKL-UT                
160010         MOVE MID-IDDC-UT          TO M4394-MOD-IDDC-UT                   
160020         MOVE WS-PRTVAL            TO M4394-MOD-PRTVAL-ADRESSFL           
160100                                                                          
160200         INSPECT M4394-MOD-IDPRODNR-UT                                    
160300                             REPLACING LEADING ZERO BY SPACE              
160400         INSPECT M4394-MOD-IDDISTR-UT                                     
160500                             REPLACING LEADING ZERO BY SPACE              
160600         INSPECT M4394-MOD-IDKUNDNR-UT                                    
160700                             REPLACING LEADING ZERO BY SPACE              
160800         INSPECT M4394-MOD-KDFRAKT-UT                                     
160900                             REPLACING LEADING ZERO BY SPACE              
161000         INSPECT M4394-MOD-IDORDNR-UT                                     
161100                             REPLACING LEADING ZERO BY SPACE              
161200       END-IF                                                             
161300     END-IF                                                               
161400     .                                                                    
161500     EJECT                                                                
161600 IA-UPPDATERA-4316-TYP3-STATUS SECTION.                                   
161700******************************************************************        
161800*                                                                         
161900******************************************************************        
162000                                                                          
162100     PERFORM IMS-GHNP-4316-TYP3                                           
162200     PERFORM UNTIL                                                        
162300      NOT ( SEGMENT-FINNS )                                               
162400       MOVE +1                    TO XXDL-4316-KDTRSTAT                   
162500       PERFORM IMS-REPL-4316-TYP2-3                                       
162600       PERFORM IMS-GHNP-4316-TYP3                                         
162700     END-PERFORM                                                          
162800     .                                                                    
162900     EJECT                                                                
163000 IB-UPPDATERA-4316-TYP4-STATUS SECTION.                                   
163100******************************************************************        
163200*                                                                         
163300******************************************************************        
163400                                                                          
163500     PERFORM IMS-GHNP-4316-TYP4-F                                         
163600     PERFORM UNTIL                                                        
163700      NOT ( SEGMENT-FINNS )                                               
163800       MOVE +1                    TO XXDL-C-4316-KDTRSTAT                 
163900       PERFORM IMS-REPL-4316-TYP4                                         
164000       PERFORM IMS-GHNP-4316-TYP4                                         
164100     END-PERFORM                                                          
164200     .                                                                    
164300     EJECT                                                                
164400 J-KONTROLL-NOLLAD-ORDER    SECTION.                                      
164500                                                                          
164600     PERFORM IMS-GU-4315-TYP2                                             
164700     MOVE '002'                  TO W-IDPTYP-4316-MIN                     
164800                                    W-IDPTYP-4316-MAX                     
164900     PERFORM IMS-GHNP-4316-TYP2                                           
165000                                                                          
165100     IF 4316-A-MID-RAD (1) = ALL '+'                                      
165200       PERFORM IMS-GU-4311                                                
165300       PERFORM IMS-GHNP-4312                                              
165400                                                                          
165500       IF XXDK-4312-KDBEHAND-KOL = 1                                      
165600         MOVE 0                  TO XXDK-4312-KDBEHAND-KOL                
165700         PERFORM IMS-REPL-4312                                            
165800       END-IF                                                             
165900     END-IF                                                               
166000     .                                                                    
166100     EJECT                                                                
166200                                                                          
166300 K-STARTA-OM-4391           SECTION.                                      
166400                                                                          
166500                                                                          
166600     PERFORM IMS-GU-4311                                                  
166700     PERFORM IMS-GHNP-4312                                                
166800                                                                          
166900     MOVE 4                          TO XXDK-4312-KDBEHAND-AVVIK          
167000                                                                          
167100     PERFORM IMS-REPL-4312                                                
167200                                                                          
167300     MOVE MID-W4I39101         TO  MOD-MID-W4I39101                       
167400     PERFORM IMS-INSERT2-MSG-ALT1-PCB                                     
167500     .                                                                    
167600     EJECT                                                                
167700******************************************************************        
167800*    GEMENSAMMA SEKTIONER                                                 
167900******************************************************************        
168000                                                                          
168100                                                                          
168200 S01-ROER-EJ-INFAELT SECTION.                                             
168300                                                                          
168400     MOVE +1                     TO RADINDX                               
168500     PERFORM UNTIL                                                        
168600      NOT ( RADINDX < MAX-RADINDX-PLUS-ETT )                              
168700       IF MID-IDRADNR (RADINDX) NOT = ALL '+'                             
168800         MOVE MFS-ROER-EJ-FAELT      TO MOD-IDRADNR  (RADINDX)            
168900       END-IF                                                             
169000       IF MID-KVLEVART (RADINDX) NOT = ALL '+'                            
169100         MOVE MFS-ROER-EJ-FAELT      TO MOD-KVLEVART (RADINDX)            
169200       END-IF                                                             
169300       ADD +1                        TO RADINDX                           
169400     END-PERFORM                                                          
169500     IF MID-IDRADNR-SENAST    NOT = ALL '+'                               
169600       MOVE MFS-ROER-EJ-FAELT      TO MOD-IDRADNR-SENAST                  
169700     END-IF                                                               
169800     .                                                                    
169900     EJECT                                                                
170000 S02-SKAPA-4316-TYP3-GEMEN SECTION.                                       
170100******************************************************************        
170200*    FÖLJEKOLLISEGMENTET 4316-SEGMENTET SKAPAS MED ALLA FASTA             
170300*    UPPGIFTER                                                            
170400*****************************************************************         
170500                                                                          
170600     MOVE XXDL-4316-IDKOLLI       TO WS-4316IDKOLLI                       
170700     MOVE WS-4316IDKOLLI          TO WS-4316IDKOLLI-N                     
170800                                                                          
170900     MOVE ALL '+'                 TO XXDL-4316-WDGX4316                   
171000     MOVE '003'                   TO XXDL-4316-IDPTYP                     
171100     MOVE MID-IDPRODNR-UT         TO XXDL-4316-IDPRODNR                   
171200                                                                          
171300     IF WS-4312IDTRANS = '4302'                                           
171400       MOVE WS-4316IDKOLLI          TO XXDL-4316-IDKOLLI                  
171500     ELSE                                                                 
171600       MOVE +1                      TO XXDL-4316-IDKOLLI                  
171700     END-IF                                                               
171800     MOVE LOW-VALUE               TO XXDL-4316-LOWVALUE                   
171900     MOVE ZERO                    TO XXDL-4316-KDTRSTAT                   
172000     MOVE +277                    TO XXDL-4316-LL                         
172100     MOVE LOW-VALUE               TO XXDL-4316-Z1                         
172200     MOVE LOW-VALUE               TO XXDL-4316-Z2                         
172300     MOVE 'W4T314  '              TO XXDL-4316-KDTRANS                    
172400     MOVE '4391'                  TO XXDL-4316-IDTRANS                    
172500     MOVE MFS-KDMFSFOR            TO XXDL-4316-KDMFSFOR                   
172600                                                                          
172700     IF WS-4312IDTRANS = '4302'                                           
172800       MOVE WS-4316IDKOLLI-X        TO 4316-B-MID-IDKOLLI-IN              
172900                                       4316-B-MID-IDKOLLI-UT              
173000     ELSE                                                                 
173100       MOVE '00001'                 TO 4316-B-MID-IDKOLLI-IN              
173200       MOVE '00001'                 TO 4316-B-MID-IDKOLLI-UT              
173300     END-IF                                                               
173400     MOVE ZERO                    TO 4316-B-MID-IDANSTNR-UT               
173500                                                                          
173600     MOVE MID-IDDC-UT             TO 4316-B-MID-IDDC-IN                   
173700                                     4316-B-MID-IDDC-UT                   
173800                                                                          
173810     MOVE MID-IDPRODNR-UT         TO 4316-B-MID-IDPRODNR-IN               
173820                                     4316-B-MID-IDPRODNR-UT               
173830                                                                          
173900     MOVE MID-IDDISTR-UT          TO 4316-B-MID-IDDISTR-IN                
174000                                     4316-B-MID-IDDISTR-UT                
174100                                                                          
174200     MOVE MID-IDKUNDNR-UT         TO 4316-B-MID-IDKUNDNR-IN               
174300                                     4316-B-MID-IDKUNDNR-UT               
174400                                                                          
174500     MOVE MID-IDORDNR-UT          TO 4316-B-MID-IDORDNR-IN                
174600                                     4316-B-MID-IDORDNR-UT                
174700                                                                          
174800     MOVE '4391'                  TO 4316-B-MID-IDTRANS-START             
174900                                                                          
175000     MOVE '+'                     TO 4316-B-MID-FLSISTAK                  
175100     MOVE 'J'                     TO 4316-B-MID-FLFORTSK                  
175200     MOVE 0                       TO 4316-B-MID-IDRADNR-FOM-S             
175300     MOVE 0                       TO 4316-B-MID-IDRADNR-TOM-S             
175400     MOVE 0                       TO 4316-B-MID-KVLEVART-S                
175410     MOVE 'UU'                    TO 4316-B-MID-KDPRTVAL-FOLJEFL          
175420     MOVE WS-PRTVAL               TO 4316-B-MID-KDPRTVAL-ADRESSFL         
175500     .                                                                    
175600     EJECT                                                                
175700 S03-SKAPA-4316-TYP4-GEMEN SECTION.                                       
175800******************************************************************        
175900*    DET OBLIGATORISKA 4316-SEGMENTET SKAPAS MED ALLA FASTA               
176000*    UPPGIFTER                                                            
176100*****************************************************************         
176200                                                                          
176300                                                                          
176400     MOVE ALL '+'                 TO XXDL-C-4316-WDGX4316                 
176500     MOVE MID-IDPRODNR-UT         TO XXDL-C-4316-IDPRODNR                 
176600     MOVE '004'                   TO XXDL-C-4316-IDPTYP                   
176700     MOVE ZERO                    TO XXDL-C-4316-IDKOLLI                  
176800     MOVE LOW-VALUE               TO XXDL-C-4316-LOWVALUE                 
176900     MOVE ZERO                    TO XXDL-C-4316-KDTRSTAT                 
177000     MOVE +346                    TO XXDL-C-4316-LL                       
177100     MOVE LOW-VALUE               TO XXDL-C-4316-Z1                       
177200     MOVE LOW-VALUE               TO XXDL-C-4316-Z2                       
177300     MOVE 'W4T398X '              TO XXDL-C-4316-KDTRANS                  
177400     MOVE '4391'                  TO XXDL-C-4316-IDTRANS                  
177500     MOVE MFS-KDMFSFOR            TO XXDL-C-4316-KDMFSFOR                 
177600                                                                          
177700     MOVE ZERO                    TO 4316-C-MID-IDANSTNR-UT               
177800                                                                          
177900     MOVE MID-IDORDNR-UT          TO 4316-C-MID-IDORDNR-IN                
178000                                     4316-C-MID-IDORDNR-UT                
178100                                                                          
178200     MOVE MID-IDPRODNR-UT         TO 4316-C-MID-IDPRODNR-IN               
178300                                     4316-C-MID-IDPRODNR-UT               
178400                                                                          
178500     MOVE MID-IDDISTR-UT          TO 4316-C-MID-IDDISTR-IN                
178600                                     4316-C-MID-IDDISTR-UT                
178700                                                                          
178800     MOVE MID-IDKUNDNR-UT         TO 4316-C-MID-IDKUNDNR-IN               
178900                                     4316-C-MID-IDKUNDNR-UT               
178901                                                                          
178910     MOVE SPACE                   TO 4316-C-MID-FLSVAR                    
179000     .                                                                    
179100                                                                          
179200     EJECT                                                                
179300 S04-FLYTTA-FOM SECTION.                                                  
179400******************************************************************        
179500*    UPPGIFTEN RADNR-FOM FLYTTAS TILL 4316-SEGMENTET                      
179600******************************************************************        
179700                                                                          
179800     IF  XXDL-4316-IDPTYP = '002'                                         
179900         IF WS-FOM        > MAX-4316-C-MID-IDRADNR AND                    
180000                          > WS-TOM                                        
180100             CONTINUE                                                     
180200          ELSE                                                            
180300             MOVE WS-FOM  TO 4316-A-MID-IDRADNR-FOM (4316-2-INDX)         
180400         END-IF                                                           
180500     ELSE                                                                 
180600       IF WS-FOM          > MAX-4316-C-MID-IDRADNR AND                    
180610                          > WS-TOM                                        
180800           CONTINUE                                                       
180900        ELSE                                                              
181000           MOVE WS-FOM    TO 4316-B-MID-IDRADNR-FOM (4316-2-INDX)         
181100       END-IF                                                             
181200     END-IF                                                               
181300     .                                                                    
181400     EJECT                                                                
181500 S05-TA-FRAM-4316-2-INDX SECTION.                                         
181600******************************************************************        
181700*    LÄS FRAM TILL RÄTT 4316-2-INDX FÖR TYP 003                           
181800******************************************************************        
181900                                                                          
182000     MOVE +1 TO 4316-2-INDX                                               
182100     PERFORM UNTIL 4316-B-MID-IDRADNR-FOM (4316-2-INDX) = ALL '+'         
182200             OR    4316-B-MID-IDRADNR-FOM (4316-2-INDX) =                 
182300                   4316-C-MID-IDRADNR (SPAR-4316-4-INDX)                  
182400             OR    4316-2-INDX > MAX-4316-2-INDX                          
182500         ADD +1                TO 4316-2-INDX                             
182600     END-PERFORM                                                          
182700     MOVE WS-IDPURAD-START  TO                                            
182800                         4316-B-MID-IDRADNR-FOM(4316-2-INDX)              
182900     .                                                                    
183000     EJECT                                                                
183100 S06-4316-2-INDX-TEST SECTION.                                            
183200******************************************************************        
183300*    4316-SEGMENTET SKRIVS OCH ETT NYTT FÖRBEREDS                         
183400******************************************************************        
183500                                                                          
183600     COMPUTE WS-IND = 4316-2-INDX - 1                                     
183700                                                                          
183800     IF XXDL-4316-IDPTYP = '002'                                          
183900       MOVE 4316-A-MID-IDRADNR-FOM (WS-IND) TO WS-FOM-INT                 
184000                                                                          
184100       IF 4316-A-MID-IDRADNR-TOM (WS-IND) = ALL '+'                       
184200         MOVE WS-FOM-INT  TO WS-TOM-INT                                   
184300       ELSE                                                               
184400         MOVE 4316-A-MID-IDRADNR-TOM (WS-IND) TO WS-TOM-INT               
184500       END-IF                                                             
184600     ELSE                                                                 
184700       MOVE 4316-B-MID-IDRADNR-FOM (WS-IND) TO WS-FOM-INT                 
184800                                                                          
184900       IF 4316-B-MID-IDRADNR-TOM (WS-IND) = ALL '+'                       
185000         MOVE WS-FOM-INT  TO WS-TOM-INT                                   
185100       ELSE                                                               
185200         MOVE 4316-B-MID-IDRADNR-TOM (WS-IND) TO WS-TOM-INT               
185300       END-IF                                                             
185400     END-IF                                                               
185500     COMPUTE WS-ANT-RAD-INT = WS-TOM-INT - WS-FOM-INT + 1                 
185600                                                                          
185700     IF WS-ANT-RAD-INT > WS-ANT-RAD-REST                                  
185800       PERFORM S11-MAX-200-RAD                                            
185900     ELSE                                                                 
186000       IF 4316-2-INDX > MAX-4316-2-INDX                                   
186100       OR WS-ANT-RAD-INT = WS-ANT-RAD-REST                                
186200                                                                          
186300                                                                          
186400         IF XXDL-4316-IDPTYP = '002'                                      
186500           MOVE '002'               TO W-IDPTYP-4316-MIN                  
186600                                       W-IDPTYP-4316-MAX                  
186700           PERFORM IMS-REPL-4316-TYP2-3                                   
186800         ELSE                                                             
186900           MOVE '003'                   TO W-IDPTYP-4316-MIN              
187000                                           W-IDPTYP-4316-MAX              
187100           IF WS-FLREPL-TYP3                                              
187200               PERFORM IMS-REPL-4316-TYP2-3                               
187300               MOVE NEJ                 TO WS-FLREPLACE-TYP3              
187400            ELSE                                                          
187500               PERFORM IMS-ISRT-4316-TYP3                                 
187600           END-IF                                                         
187700         END-IF                                                           
187800         PERFORM S02-SKAPA-4316-TYP3-GEMEN                                
187900         MOVE +1                        TO 4316-2-INDX                    
188000         MOVE MAX-ANT-RAD               TO WS-ANT-RAD-REST                
188100                                                                          
188200       ELSE                                                               
188300         COMPUTE WS-ANT-RAD-REST = WS-ANT-RAD-REST -                      
188400                                   WS-ANT-RAD-INT                         
188500       END-IF                                                             
188600     END-IF                                                               
188700     .                                                                    
188800     EJECT                                                                
188900 S07-TA-FRAM-4316-2-INDX SECTION.                                         
189000******************************************************************        
189100*    LÄS FRAM TILL RÄTT 4316-2-INDX  FÖR TYP 002                          
189200******************************************************************        
189300                                                                          
189400     MOVE +1 TO 4316-2-INDX                                               
189500     PERFORM UNTIL 4316-A-MID-IDRADNR-FOM (4316-2-INDX) = ALL '+'         
189600             OR    4316-A-MID-IDRADNR-FOM (4316-2-INDX) =                 
189700                   4316-C-MID-IDRADNR (SPAR-4316-4-INDX)                  
189800             OR    4316-2-INDX > MAX-4316-2-INDX                          
189900             OR    (4316-A-MID-IDRADNR-TOM(4316-2-INDX) NUMERIC           
190000               AND  4316-A-MID-IDRADNR-TOM(4316-2-INDX) = WS-TOM)         
190100         ADD +1                TO 4316-2-INDX                             
190200     END-PERFORM                                                          
190300     MOVE WS-IDPURAD-START  TO                                            
190400                         4316-A-MID-IDRADNR-FOM(4316-2-INDX)              
190500     .                                                                    
190600     EJECT                                                                
190700 S08-REDIGERA-UTFAELT SECTION.                                            
190800******************************************************************        
190900*    ALLA NYCKLAR REDIGERAS                                               
191000******************************************************************        
191100                                                                          
191200     INSPECT  MOD-IDPRODNR-UT    REPLACING LEADING ZERO BY SPACE          
191300     INSPECT  MOD-IDDISTR-UT     REPLACING LEADING ZERO BY SPACE          
191400     INSPECT  MOD-IDKUNDNR-UT    REPLACING LEADING ZERO BY SPACE          
191500     INSPECT  MOD-KDFRAKT-UT     REPLACING LEADING ZERO BY SPACE          
191600     INSPECT  MOD-IDORDNR-UT     REPLACING LEADING ZERO BY SPACE          
191700     INSPECT  MOD-IDRADNR-SENAST REPLACING LEADING ZERO BY SPACE          
191800     .                                                                    
191900     EJECT                                                                
192000 S09-TOM-TEST SECTION.                                                    
192100******************************************************************        
192200*    OM FOM OCH TOM ÄR LIKA SKALL TOM VARA 'BLANK'                        
192300******************************************************************        
192400                                                                          
192500     IF 4316-A-MID-IDRADNR-FOM (4316-2-INDX)     =                        
192600                           4316-A-MID-IDRADNR-TOM (4316-2-INDX)           
192700       MOVE ALL '+' TO 4316-A-MID-IDRADNR-TOM (4316-2-INDX)               
192800     END-IF                                                               
192900     .                                                                    
193000     EJECT                                                                
193100 S10-TOM-TEST SECTION.                                                    
193200******************************************************************        
193300*    OM FOM OCH TOM ÄR LIKA SKALL TOM VARA 'BLANK'                        
193400******************************************************************        
193500                                                                          
193600     IF 4316-B-MID-IDRADNR-FOM (4316-2-INDX) =                            
193700                           4316-B-MID-IDRADNR-TOM (4316-2-INDX)           
193800       MOVE ALL '+' TO 4316-B-MID-IDRADNR-TOM (4316-2-INDX)               
193900     END-IF                                                               
194000     .                                                                    
194100     EJECT                                                                
194200 S11-MAX-200-RAD SECTION.                                                 
194300******************************************************************        
194400*    MAX 200 RADER I VARJE KOLLI-POST                                     
194500*    HÄR SKAPAS EV FLERA PT=3 OM DET SKAPADE INTERVALLET                  
194600*    ÄR FÖR STORT                                                         
194700******************************************************************        
194800                                                                          
194900     MOVE WS-TOM-INT   TO WS-TOM-SISTA                                    
195000     COMPUTE WS-TOM-INT = WS-FOM-INT + WS-ANT-RAD-REST - 1                
195100                                                                          
195200     IF XXDL-4316-IDPTYP = '002'                                          
195300       IF WS-FOM-INT = WS-TOM-INT                                         
195400         MOVE  ALL '+' TO                                                 
195500                  4316-A-MID-IDRADNR-TOM (WS-IND)                         
195600       ELSE                                                               
195700         MOVE  WS-TOM-INT  TO                                             
195800                  4316-A-MID-IDRADNR-TOM (WS-IND)                         
195900       END-IF                                                             
196000       PERFORM IMS-REPL-4316-TYP2-3                                       
196100                                                                          
196200     ELSE                                                                 
196300       IF WS-FOM-INT = WS-TOM-INT                                         
196400         MOVE  ALL '+' TO                                                 
196500                  4316-B-MID-IDRADNR-TOM (WS-IND)                         
196600       ELSE                                                               
196700         MOVE  WS-TOM-INT  TO                                             
196800                  4316-B-MID-IDRADNR-TOM (WS-IND)                         
196900       END-IF                                                             
197000       IF WS-FLREPL-TYP3                                                  
197100           PERFORM IMS-REPL-4316-TYP2-3                                   
197200           MOVE NEJ            TO WS-FLREPLACE-TYP3                       
197300        ELSE                                                              
197400           PERFORM IMS-ISRT-4316-TYP3                                     
197500        END-IF                                                            
197600     END-IF                                                               
197700     MOVE MAX-ANT-RAD          TO WS-ANT-RAD-REST                         
197800     MOVE +1                   TO 4316-2-INDX                             
197900                                                                          
198000*****  SKAPA KOLLI-POSTER TILLS ALLA RADER I INTERVALLET                  
198100*****  ÄR PLACERADE I PT = 2 ELLER 3 HÄR SKAPAS EV FLERA PT=3             
198200                                                                          
198300     PERFORM UNTIL                                                        
198400      NOT ( WS-TOM-INT < WS-TOM-SISTA )                                   
198500       PERFORM S02-SKAPA-4316-TYP3-GEMEN                                  
198600       COMPUTE WS-FOM-INT = WS-TOM-INT + 1                                
198700       COMPUTE WS-ANT-RAD-INT =                                           
198800               WS-TOM-SISTA - WS-FOM-INT + 1                              
198900                                                                          
199000       IF WS-ANT-RAD-INT > WS-ANT-RAD-REST                                
199100         COMPUTE WS-TOM-INT =                                             
199200                 WS-FOM-INT + WS-ANT-RAD-REST - 1                         
199300         MOVE WS-FOM-INT    TO                                            
199400                 4316-B-MID-IDRADNR-FOM (4316-2-INDX)                     
199500                                                                          
199600         IF WS-FOM-INT NOT = WS-TOM-INT                                   
199700           MOVE  WS-TOM-INT  TO                                           
199800                    4316-B-MID-IDRADNR-TOM (4316-2-INDX)                  
199900         END-IF                                                           
200000         PERFORM IMS-ISRT-4316-TYP3                                       
200100         MOVE MAX-ANT-RAD     TO WS-ANT-RAD-REST                          
200200         MOVE +1           TO 4316-2-INDX                                 
200300       ELSE                                                               
200400         MOVE WS-TOM-SISTA   TO WS-TOM-INT                                
200500         MOVE WS-FOM-INT TO                                               
200600                 4316-B-MID-IDRADNR-FOM (4316-2-INDX)                     
200700                                                                          
200800         IF WS-FOM-INT NOT = WS-TOM-INT                                   
200900           MOVE  WS-TOM-INT  TO                                           
201000                    4316-B-MID-IDRADNR-TOM (4316-2-INDX)                  
201100         END-IF                                                           
201200         COMPUTE WS-ANT-RAD-REST = WS-ANT-RAD-REST -                      
201300                                   WS-ANT-RAD-INT                         
201400         ADD +1                TO  4316-2-INDX                            
201500       END-IF                                                             
201600     END-PERFORM                                                          
201700     .                                                                    
201800     EJECT                                                                
201900 S12-FORMATETS-ATTR SECTION.                                              
202000                                                                          
202100     MOVE +1         TO RADINDX                                           
202200                                                                          
202300     PERFORM UNTIL                                                        
202400      NOT ( RADINDX < MAX-RADINDX-PLUS-ETT )                              
202500       MOVE MFS-FORMATETS-ATTR       TO                                   
202600                           MOD-IDRADNR-ATTR (RADINDX)                     
202700       MOVE MFS-FORMATETS-ATTR       TO                                   
202800                          MOD-KVLEVART-ATTR (RADINDX)                     
202900       ADD +1                        TO RADINDX                           
203000     END-PERFORM                                                          
203100     .                                                                    
203200     EJECT                                                                
203300 S13-BEHAND-AVV-VID-UTSKRIFT             SECTION.                         
203400                                                                          
203500     IF SEQB-KVBEART > SEQB-KVAVBART + SEQB-KVANNANT                      
203600       MOVE SEQB-IDPURAD TO  4316-C-MID-IDRADNR (4316-4-INDX)             
203700       MOVE SEQB-KVAVBART TO 4316-C-MID-KVORAPP (4316-4-INDX)             
203800       ADD +1                TO  4316-4-INDX                              
203900     END-IF                                                               
204000     .                                                                    
204100     EJECT                                                                
205700 S15-TA-FRAM-MAX-4316-4-RADNR SECTION.                                    
205800******************************************************************        
205900*    TA FRAM HÖGSTA RADNR I 4316-C-MID                                    
206000******************************************************************        
206100                                                                          
206200     MOVE +1 TO 4316-4-INDX                                               
206300     PERFORM UNTIL 4316-C-MID-IDRADNR(4316-4-INDX) = ALL '+'              
206400         ADD +1                TO 4316-4-INDX                             
206500     END-PERFORM                                                          
206600     IF 4316-4-INDX            > +1                                       
206700         SUBTRACT +1 FROM 4316-4-INDX                                     
206800         MOVE 4316-C-MID-IDRADNR (4316-4-INDX)                            
206900                               TO MAX-4316-C-MID-IDRADNR                  
207000     END-IF                                                               
207100                                                                          
207200     IF SPAR-4316-4-INDX       < 14                                       
207300         MOVE +13              TO MAX2-4316-4-INDX                        
207400      ELSE                                                                
207500         MOVE +26              TO MAX2-4316-4-INDX                        
207600     END-IF                                                               
207700     .                                                                    
207800     EJECT                                                                
207900******************************************************************        
208000*    IMS SEKTIONER                                                        
208100******************************************************************        
208200                                                                          
208300 IMS-GET-MSG SECTION.                                                     
208400     MOVE '  QC' TO GODK-STATUSKODER                                      
208500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
208600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
208700     PERFORM IMS-STATUSKONTROLL                                           
208800     .                                                                    
208900     SKIP3                                                                
209000 IMS-INSERT-MSG SECTION.                                                  
209100     IF ENGLISH-TEXT                                                      
209200       MOVE NEJ TO MFS-KDHUVOMR                                           
209300     END-IF                                                               
209400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
209500     MOVE SPACE TO GODK-STATUSKODER                                       
209600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
209700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
209800     PERFORM IMS-STATUSKONTROLL                                           
209900     .                                                                    
210000     SKIP3                                                                
210100 IMS-ISRT-MSG-ALT-PCB SECTION.                                            
210200     IF ENGLISH-TEXT                                                      
210300       MOVE NEJ TO MFS-KDHUVOMR                                           
210400     END-IF                                                               
210500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
210600     MOVE SPACE TO GODK-STATUSKODER                                       
210700     CALL CBLTDLI USING ISRT ALT-PCB P-TO-P-SW                            
210800     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
210900     PERFORM IMS-STATUSKONTROLL                                           
211000     .                                                                    
211100     SKIP3                                                                
211200 IMS-INSERT-MSG-ALT1-PCB SECTION.                                         
211300     MOVE SPACE TO GODK-STATUSKODER                                       
211400     CALL CBLTDLI USING ISRT ALT1-PCB P-TO-P-SW1                          
211500     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
211600     PERFORM IMS-STATUSKONTROLL                                           
211700     .                                                                    
211800                                                                          
211900 IMS-INSERT2-MSG-ALT1-PCB SECTION.                                        
212000     MOVE SPACE TO GODK-STATUSKODER                                       
212100     CALL CBLTDLI USING ISRT ALT1-PCB P-TO-P-SW3                          
212200     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
212300     PERFORM IMS-STATUSKONTROLL                                           
212400     .                                                                    
212500     EJECT                                                                
212600                                                                          
212700 IMS-INSERT-MSG-ALT2-PCB SECTION.                                         
212800     MOVE SPACE TO GODK-STATUSKODER                                       
212900     CALL CBLTDLI USING ISRT ALT2-PCB P-TO-P-SW2                          
213000     MOVE ALT2-STATUS-CODE TO STATUS-WS                                   
213100     PERFORM IMS-STATUSKONTROLL                                           
213200     .                                                                    
213300                                                                          
213400                                                                          
213500 IMS-GU-4305 SECTION.                                                     
213600     STRING 'WLXXDJ01(WDGXKEY  =' W-WDGXKEY-4305-X ')'                    
213700            DELIMITED BY SIZE INTO SSA1                                   
213800     MOVE '  ' TO GODK-STATUSKODER                                        
213900     CALL CBLTDLI USING GU XXDJ-PCB DLI-IO-AREA-4 SSA1                    
214000     MOVE XXDJ-STATUS-CODE TO STATUS-WS                                   
214100     PERFORM IMS-STATUSKONTROLL                                           
214200     .                                                                    
214300     SKIP3                                                                
214400 IMS-GHNP-4306 SECTION.                                                   
214500     STRING 'WLXXDJ11(WDGXKEY  =' W-WDGXKEY-4306-X ')'                    
214600            DELIMITED BY SIZE INTO SSA1                                   
214700     MOVE '  ' TO GODK-STATUSKODER                                        
214800     CALL CBLTDLI USING GHNP XXDJ-PCB DLI-IO-AREA-4 SSA1                  
214900     MOVE XXDJ-STATUS-CODE TO STATUS-WS                                   
215000     PERFORM IMS-STATUSKONTROLL                                           
215100     .                                                                    
215200     SKIP3                                                                
215300 IMS-REPL-4306 SECTION.                                                   
215400     MOVE '  '   TO GODK-STATUSKODER                                      
215500     CALL CBLTDLI USING REPL XXDJ-PCB DLI-IO-AREA-4                       
215600     MOVE XXDJ-STATUS-CODE TO STATUS-WS                                   
215700     PERFORM IMS-STATUSKONTROLL                                           
215800     .                                                                    
215900     EJECT                                                                
216000 IMS-GNP-4308-F SECTION.                                                  
216100     STRING 'WLXXDJ11*F(WDGXKEY  =' W-WDGXKEY-4306-X ')'                  
216200            DELIMITED BY SIZE INTO SSA1                                   
216300     MOVE 'WLXXDJ21 '  TO SSA2                                            
216400     MOVE '  GE' TO GODK-STATUSKODER                                      
216500     CALL CBLTDLI USING GNP XXDJ-PCB DLI-IO-AREA-4 SSA1 SSA2              
216600     MOVE XXDJ-STATUS-CODE TO STATUS-WS                                   
216700     PERFORM IMS-STATUSKONTROLL                                           
216800     .                                                                    
216900     SKIP3                                                                
217000 IMS-GNP-4308 SECTION.                                                    
217100     STRING 'WLXXDJ11(WDGXKEY  =' W-WDGXKEY-4306-X ')'                    
217200            DELIMITED BY SIZE INTO SSA1                                   
217300     MOVE 'WLXXDJ21 '    TO SSA2                                          
217400     MOVE '  GE' TO GODK-STATUSKODER                                      
217500     CALL CBLTDLI USING GNP XXDJ-PCB DLI-IO-AREA-4 SSA1 SSA2              
217600     MOVE XXDJ-STATUS-CODE TO STATUS-WS                                   
217700     PERFORM IMS-STATUSKONTROLL                                           
217800     .                                                                    
217900     EJECT                                                                
218000 IMS-GU-4311 SECTION.                                                     
218100     STRING 'WLXXDK01(WDGXKEY  =' W-WDGXKEY-4311-X ')'                    
218200            DELIMITED BY SIZE INTO SSA1                                   
218300     MOVE '  '     TO GODK-STATUSKODER                                    
218400     CALL CBLTDLI USING GU XXDK-PCB DLI-IO-AREA SSA1                      
218500     MOVE XXDK-STATUS-CODE TO STATUS-WS                                   
218600     PERFORM IMS-STATUSKONTROLL                                           
218700     .                                                                    
218800     SKIP3                                                                
218900 IMS-GHNP-4312 SECTION.                                                   
219000     STRING 'WLXXDK11(WDGXKEY  =' W-WDGXKEY-4312-X ')'                    
219100            DELIMITED BY SIZE INTO SSA1                                   
219200     MOVE '  '     TO GODK-STATUSKODER                                    
219300     CALL CBLTDLI USING GHNP XXDK-PCB DLI-IO-AREA SSA1                    
219400     MOVE XXDK-STATUS-CODE TO STATUS-WS                                   
219500     PERFORM IMS-STATUSKONTROLL                                           
219600     .                                                                    
219700     SKIP3                                                                
219800 IMS-GET-XXDK-4312-STAT-GE SECTION.                                       
219900     STRING 'WLXXDK11(IDUSER   =' W-WDGXKEY-IDUSER-4312 ')'               
220000            DELIMITED BY SIZE INTO SSA1                                   
220100     MOVE '  GE'   TO GODK-STATUSKODER                                    
220200     CALL CBLTDLI USING GNP XXDK-PCB DLI-IO-AREA SSA1                     
220300     MOVE XXDK-STATUS-CODE TO STATUS-WS                                   
220400     PERFORM IMS-STATUSKONTROLL                                           
220500     .                                                                    
220600                                                                          
220700 IMS-GET-XXDK-4312-STAT-GE-F SECTION.                                     
220800     STRING 'WLXXDK11*F(IDUSER   =' W-WDGXKEY-IDUSER-4312 ')'             
220900            DELIMITED BY SIZE INTO SSA1                                   
221000     MOVE '  GE'   TO GODK-STATUSKODER                                    
221100     CALL CBLTDLI USING GNP XXDK-PCB DLI-IO-AREA SSA1                     
221200     MOVE XXDK-STATUS-CODE TO STATUS-WS                                   
221300     PERFORM IMS-STATUSKONTROLL                                           
221400     .                                                                    
221500                                                                          
221600                                                                          
221700 IMS-DLET-4312 SECTION.                                                   
221800     MOVE '  '   TO GODK-STATUSKODER                                      
221900     CALL CBLTDLI USING DLET XXDK-PCB DLI-IO-AREA                         
222000     MOVE XXDK-STATUS-CODE TO STATUS-WS                                   
222100     PERFORM IMS-STATUSKONTROLL                                           
222200     .                                                                    
222300     SKIP3                                                                
222400 IMS-REPL-4312 SECTION.                                                   
222500     MOVE '  '   TO GODK-STATUSKODER                                      
222600     CALL CBLTDLI USING REPL XXDK-PCB DLI-IO-AREA                         
222700     MOVE XXDK-STATUS-CODE TO STATUS-WS                                   
222800     PERFORM IMS-STATUSKONTROLL                                           
222900     .                                                                    
223000     EJECT                                                                
223100 IMS-GU-4315-TYP2 SECTION.                                                
223200     STRING 'WLXXDL01(WDGXKEY  =' W-WDGXKEY-4315-X ')'                    
223300            DELIMITED BY SIZE INTO SSA1                                   
223400     MOVE '  '     TO GODK-STATUSKODER                                    
223500     CALL CBLTDLI USING GU XXDL-PCB DLI-IO-AREA SSA1                      
223600     MOVE XXDL-STATUS-CODE TO STATUS-WS                                   
223700     PERFORM IMS-STATUSKONTROLL                                           
223800     .                                                                    
224000     SKIP3                                                                
224100 IMS-GHNP-4316-TYP2-L SECTION.                                            
224200     STRING 'WLXXDL11*L(WDGXKEY  >' W-WDGXKEY-4316-X-MIN                  
224300     '&WDGXKEY  <' W-WDGXKEY-4316-X-MAX ')'                               
224400            DELIMITED BY SIZE INTO SSA1                                   
224500     MOVE '  GE'   TO GODK-STATUSKODER                                    
224600     CALL CBLTDLI USING GHNP XXDL-PCB DLI-IO-AREA SSA1                    
224700     MOVE XXDL-STATUS-CODE TO STATUS-WS                                   
224800     PERFORM IMS-STATUSKONTROLL                                           
224900     .                                                                    
225000     SKIP3                                                                
225100 IMS-GHNP-4316-TYP2   SECTION.                                            
225200     STRING 'WLXXDL11(WDGXKEY  >' W-WDGXKEY-4316-X-MIN                    
225300     '&WDGXKEY  <' W-WDGXKEY-4316-X-MAX ')'                               
225400            DELIMITED BY SIZE INTO SSA1                                   
225500     MOVE '  '   TO GODK-STATUSKODER                                      
225600     CALL CBLTDLI USING GHNP XXDL-PCB DLI-IO-AREA SSA1                    
225700     MOVE XXDL-STATUS-CODE TO STATUS-WS                                   
225800     PERFORM IMS-STATUSKONTROLL                                           
225900     .                                                                    
226000     SKIP3                                                                
226100 IMS-GHNP-4316-TYP2-STAT-GE SECTION.                                      
226200     STRING 'WLXXDL11(WDGXKEY  >' W-WDGXKEY-4316-X-MIN                    
226300     '&WDGXKEY  <' W-WDGXKEY-4316-X-MAX ')'                               
226400            DELIMITED BY SIZE INTO SSA1                                   
226500     MOVE '  GE'     TO GODK-STATUSKODER                                  
226600     CALL CBLTDLI USING GHNP XXDL-PCB DLI-IO-AREA SSA1                    
226700     MOVE XXDL-STATUS-CODE TO STATUS-WS                                   
226800     PERFORM IMS-STATUSKONTROLL                                           
226900     .                                                                    
227000     SKIP3                                                                
227100 IMS-GHNP-4316-TYP3 SECTION.                                              
227200     STRING 'WLXXDL11(WDGXKEY  >' W-WDGXKEY-4316-X-MIN                    
227300     '&WDGXKEY  <' W-WDGXKEY-4316-X-MAX ')'                               
227400            DELIMITED BY SIZE INTO SSA1                                   
227500     MOVE '  GE'   TO GODK-STATUSKODER                                    
227600     CALL CBLTDLI USING GHNP XXDL-PCB DLI-IO-AREA SSA1                    
227700     MOVE XXDL-STATUS-CODE TO STATUS-WS                                   
227800     PERFORM IMS-STATUSKONTROLL                                           
227900     .                                                                    
228000     EJECT                                                                
228100 IMS-ISRT-4316-TYP3 SECTION.                                              
228200     STRING 'WLXXDL01(WDGXKEY  =' W-WDGXKEY-4315-X ')'                    
228300            DELIMITED BY SIZE INTO SSA1                                   
228400     MOVE 'WLXXDL11 '   TO SSA2                                           
228500     MOVE '  '     TO GODK-STATUSKODER                                    
228600     CALL CBLTDLI USING ISRT XXDL-PCB DLI-IO-AREA SSA1 SSA2               
228700     MOVE XXDL-STATUS-CODE TO STATUS-WS                                   
228800     PERFORM IMS-STATUSKONTROLL                                           
228900     .                                                                    
229000     SKIP3                                                                
229100 IMS-REPL-4316-TYP2-3 SECTION.                                            
229200     MOVE '  '   TO GODK-STATUSKODER                                      
229300     CALL CBLTDLI USING REPL XXDL-PCB DLI-IO-AREA                         
229400     MOVE XXDL-STATUS-CODE TO STATUS-WS                                   
229500     PERFORM IMS-STATUSKONTROLL                                           
229600     .                                                                    
229700     SKIP3                                                                
229800 IMS-DLET-4316 SECTION.                                                   
229900     MOVE '  '   TO GODK-STATUSKODER                                      
230000     CALL CBLTDLI USING DLET XXDL-PCB DLI-IO-AREA                         
230100     MOVE XXDL-STATUS-CODE TO STATUS-WS                                   
230200     PERFORM IMS-STATUSKONTROLL                                           
230300     .                                                                    
230400     EJECT                                                                
230500 IMS-GU-4315-TYP4 SECTION.                                                
230600     STRING 'WLXXDL01(WDGXKEY  =' W-WDGXKEY-4315-X ')'                    
230700            DELIMITED BY SIZE INTO SSA1                                   
230800     MOVE '  '     TO GODK-STATUSKODER                                    
230900     CALL CBLTDLI USING GU XXDL2-PCB DLI-IO-AREA-2 SSA1                   
231000     MOVE XXDL2-STATUS-CODE TO STATUS-WS                                  
231100     PERFORM IMS-STATUSKONTROLL                                           
231200     .                                                                    
231300     SKIP3                                                                
231400 IMS-GHNP-4316-TYP4-F SECTION.                                            
231500     STRING 'WLXXDL11*F(WDGXKEY  =' W-WDGXKEY-4316-X ')'                  
231600            DELIMITED BY SIZE INTO SSA1                                   
231700     MOVE '  '     TO GODK-STATUSKODER                                    
231800     CALL CBLTDLI USING GHNP XXDL2-PCB DLI-IO-AREA-2 SSA1                 
231900     MOVE XXDL2-STATUS-CODE TO STATUS-WS                                  
232000     PERFORM IMS-STATUSKONTROLL                                           
232100     .                                                                    
232200     SKIP3                                                                
232300 IMS-GHNP-4316-TYP4-L SECTION.                                            
232400     STRING 'WLXXDL11*L(WDGXKEY  =' W-WDGXKEY-4316-X ')'                  
232500            DELIMITED BY SIZE INTO SSA1                                   
232600     MOVE '  '   TO GODK-STATUSKODER                                      
232700     CALL CBLTDLI USING GHNP XXDL2-PCB DLI-IO-AREA-2 SSA1                 
232800     MOVE XXDL2-STATUS-CODE TO STATUS-WS                                  
232900     PERFORM IMS-STATUSKONTROLL                                           
233000     .                                                                    
233100     EJECT                                                                
233200 IMS-GHNP-4316-TYP4 SECTION.                                              
233300     STRING 'WLXXDL11(WDGXKEY  =' W-WDGXKEY-4316-X ')'                    
233400            DELIMITED BY SIZE INTO SSA1                                   
233500     MOVE '  GE'     TO GODK-STATUSKODER                                  
233600     CALL CBLTDLI USING GHNP XXDL2-PCB DLI-IO-AREA-2 SSA1                 
233700     MOVE XXDL2-STATUS-CODE TO STATUS-WS                                  
233800     PERFORM IMS-STATUSKONTROLL                                           
233900     .                                                                    
234000     SKIP3                                                                
234100 IMS-ISRT-4316-TYP4 SECTION.                                              
234200     STRING 'WLXXDL01(WDGXKEY  =' W-WDGXKEY-4315-X ')'                    
234300            DELIMITED BY SIZE INTO SSA1                                   
234400     MOVE 'WLXXDL11 '   TO SSA2                                           
234500     MOVE '  '     TO GODK-STATUSKODER                                    
234600     CALL CBLTDLI USING ISRT XXDL2-PCB DLI-IO-AREA-2 SSA1 SSA2            
234700     MOVE XXDL2-STATUS-CODE TO STATUS-WS                                  
234800     PERFORM IMS-STATUSKONTROLL                                           
234900     .                                                                    
235000     SKIP3                                                                
235100 IMS-REPL-4316-TYP4 SECTION.                                              
235200     MOVE '  '   TO GODK-STATUSKODER                                      
235300     CALL CBLTDLI USING REPL XXDL2-PCB DLI-IO-AREA-2                      
235400     MOVE XXDL2-STATUS-CODE TO STATUS-WS                                  
235500     PERFORM IMS-STATUSKONTROLL                                           
235600     .                                                                    
235700     EJECT                                                                
235800 IMS-GU-WDE4B1        SECTION.                                            
235900     STRING 'WDE4B1  (WDE4B1KY =' W-WDE4B1KY-X ')'                        
236000            DELIMITED BY SIZE INTO SSA1                                   
236100     MOVE '    ' TO GODK-STATUSKODER                                      
236200     CALL CBLTDLI USING GU WDE4B-PCB DLI-IO-AREA-5 SSA1                   
236300     MOVE WDE4B-STATUS-CODE TO STATUS-WS                                  
236400     PERFORM IMS-STATUSKONTROLL                                           
236500     .                                                                    
236600     SKIP2                                                                
236700 IMS-GU-WDE4B1-STAT-GE SECTION.                                           
236800     STRING 'WDE4B1  (WDE4B1KY =' W-WDE4B1KY-X ')'                        
236900            DELIMITED BY SIZE INTO SSA1                                   
237000     MOVE '  GE' TO GODK-STATUSKODER                                      
237100     CALL CBLTDLI USING GU WDE4B-PCB DLI-IO-AREA-5 SSA1                   
237200     MOVE WDE4B-STATUS-CODE TO STATUS-WS                                  
237300     PERFORM IMS-STATUSKONTROLL                                           
237400     .                                                                    
237500     SKIP2                                                                
237600 IMS-GN-WDE4B1        SECTION.                                            
237700     STRING 'WDE4B1  (WDE4B1KY>=' W-WDE4B1KY-MIN-X                        
237800                    '&WDE4B1KY<=' W-WDE4B1KY-MAX-X ')'                    
237900            DELIMITED BY SIZE INTO SSA1                                   
238000     MOVE '  GE' TO GODK-STATUSKODER                                      
238100     CALL CBLTDLI USING GN WDE4B-PCB DLI-IO-AREA-5 SSA1                   
238200     MOVE WDE4B-STATUS-CODE TO STATUS-WS                                  
238300     PERFORM IMS-STATUSKONTROLL                                           
238400     .                                                                    
238500     EJECT                                                                
238600 IMS-STATUSKONTROLL SECTION.                                              
238700     SET STATUS-IX TO 1                                                   
238800     SEARCH GODK-STATUS AT END CALL FELLOG                                
238900     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
238910        CONTINUE                                                          
239000     END-SEARCH                                                           
239200     .                                                                    
