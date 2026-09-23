000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL012700.                                                
000300 AUTHOR.         SUBBARAO PARUCHURI V.                                    
000400 DATE-WRITTEN.   04/09/28.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000610*2667214 - ADJUST CASE INFO                                               
000620*2523176 - PRINT PICKING ROUND                                            
000700*    NAME:       'CARPARTS.LDC.CASEADJ'                                   
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        PROGRAMMET VISAR OCH ÄNDRAR INNEHÅLLET I PACKADE                 
001100*        KOLLIN.                                                          
001200*        ÄNDRINGEN ÄR ALLTID ÅTERFÖRANDE AV DELAR AV ELLER                
001300*        ALLA RADERNA I KOLLIT TILL OPACKAT STATUS.                       
001400*                                                                         
001500*    WL012700 PROGRAM IS A REPLICA OF W4031600 PROGRAM                    
001600*    AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS.                             
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSACTION: WL0127U                                             
002000*        REQUEST:     WL0127I1                                            
002100*                                                                         
002200*    OUTDATA.                                                             
002300*        RESPONSE:    WL0127O1                                            
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     SKIP2                                                                
002800 INPUT-OUTPUT SECTION.                                                    
002900                                                                          
003000 FILE-CONTROL.                                                            
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP3                                                                
003400 FILE SECTION.                                                            
003500     EJECT                                                                
003600 WORKING-STORAGE SECTION.                                                 
003700 77  IDPGM                       PIC X(08)   VALUE 'WL012700'.            
003800 77    YES                       PIC X       VALUE 'Y'.                   
003900 77    JA                        PIC X       VALUE 'J'.                   
004000 77    NEJ                       PIC X       VALUE 'N'.                   
004100 77    RAETT                     PIC X       VALUE 'R'.                   
004200 77    FEL                       PIC X       VALUE 'F'.                   
004300 77    TOM-BILD                  PIC X       VALUE 'J'.                   
004400 77    KOLLI-BORTTAGET           PIC X       VALUE 'N'.                   
004500 77    ARB-RAD-HELT-ORAPP        PIC X       VALUE 'N'.                   
004600 77    KOLLI-KDKOLSTA-1-BORT     PIC X       VALUE 'N'.                   
004700 77    NY-NYCKEL                 PIC X       VALUE 'N'.                   
004800 77    SOEK-VIA-PRODNR           PIC X       VALUE 'N'.                   
004900 77    IND1                      PIC S9(9)   VALUE +0   COMP SYNC.        
005000 77    IND2                      PIC S9(9)   VALUE +0   COMP SYNC.        
005100 77    INDX                      PIC S9(9)   VALUE +0   COMP SYNC.        
005200 77    INX                       PIC S9(9)   VALUE +0   COMP SYNC.        
005300 77    RAD-INX                   PIC S9(9)   VALUE +0   COMP SYNC.        
005400 77    MAX-RAD-INX               PIC S9(9)   VALUE +500 COMP SYNC.        
005410 77    MSG-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
005500 77    FG-INDX                   PIC S9(9)   VALUE +0   COMP SYNC.        
005600 77    TAB-INDX                  PIC S9(9)   VALUE +0   COMP SYNC.        
005700 77    MAX-FG-INDX               PIC S9(9)   VALUE +10  COMP SYNC.        
005800 77    WS-KVORDRAD               PIC S9(5)   VALUE +0    COMP-3.          
005900 77    WS-KVORDRAD-SPAR          PIC S9(5)   VALUE +0    COMP-3.          
006000 77    WS-KVORDRAD-LEVPL-SPAR    PIC S9(5)   VALUE +0    COMP-3.          
006100 77    WS-KVFALRAD               PIC S9(5)   VALUE +0    COMP-3.          
006200 77    WS-KVKOLLI-BORT           PIC S9(5)   VALUE +0    COMP-3.          
006300 77    WS-KDMFSFOR               PIC 9(1)   VALUE ZERO.                   
006400 77    WS-KOLLI-ADFLGEO          PIC X(3)   VALUE SPACE.                  
006500 77    WS-KOLLI-ADFLOMR          PIC 9(3)   VALUE ZERO.                   
006600 01    WS-IDUSER.                                                         
006700*  03  FILLER                    PIC X(3)   VALUE ZERO.                   
006800   03  FILLER                    PIC X(3)   VALUE SPACE.                  
006900   03  WS-IDANSTNR               PIC X(5)   VALUE SPACE.                  
007000 77    WS-IDDISTR-N              PIC 9(4)   VALUE ZERO.                   
007100 77    WS-IDDISTR                PIC X(4)   VALUE SPACE.                  
007200 77    WS-IDKUNDNR-N             PIC 9(6)   VALUE ZERO.                   
007300 77    WS-IDKUNDNR               PIC X(6)   VALUE SPACE.                  
007400 77    WS-IDKOLLI                PIC X(5)   VALUE SPACE.                  
007500 77    WS-IDKOLLI-NUM            PIC 9(5).                                
007600 77    WS-IDPRODNR               PIC X(7)   VALUE SPACE.                  
007700 77    WS-JFR-IDPRODNR           PIC X(7).                                
007800 77    WS-IDPRODNR-N             PIC 9(7).                                
007900 77    WS-KORD-IDPRODNR          PIC 9(7).                                
008000 77    WS-JFR-IDKOLLI            PIC X(5).                                
008100 77    WS-IDRADNR                PIC X(4)   VALUE ZERO.                   
008200 77    WS-IDRADNR-START          PIC 9(4)   VALUE ZERO.                   
008300 77    WS-SPAR-RAD               PIC 9(4)   VALUE ZERO.                   
008400 77    WS-START-RAD              PIC 9(4)   VALUE ZERO.                   
008500 77    WS-SISTA-RAD              PIC 9(4)   VALUE ZERO.                   
008600 77    WS-KVLEVART               PIC 9(6)   VALUE ZERO.                   
008700 77    WS-KOLLI-VKORDBTO-KOLLI   PIC S9(6)V9(1) VALUE ZERO.               
008800 77    WS-KOLLI-VLORDBTO-KOLLI   PIC S9(4)V9(3) VALUE ZERO.               
008900 77    WS-TOT-ANT-RADER          PIC S9(3)  VALUE +0   COMP-3.            
009000 77    MAX-ANTAL-RADER           PIC S9(3)  VALUE +500 COMP-3.            
009100 77    MAX-ANTAL-RADER-PLUS-1    PIC S9(3)  VALUE +501 COMP-3.            
009200 77    WS-IDPLKLST               PIC S9(3)  VALUE ZERO COMP-3.            
009300 77    FILLER                    PIC  X(08) VALUE 'DN WS   '.             
009400 77    WS-DNOT-IDORDER           PIC S9(07) VALUE ZERO COMP-3.            
009500 77    WS-DNOT-IDARTNR           PIC S9(09) VALUE ZERO COMP-3.            
009600 77    WS-DNOT-IDDC              PIC  X(02) VALUE ZERO.                   
009700 77    WS-DNOT-IDKOLLI           PIC S9(05) VALUE ZERO COMP-3.            
009800 77    WS-DNOT-IDPURAD           PIC S9(05) VALUE ZERO COMP-3.            
009900                                                                          
010000 77    WS-SPAR-IDPURAD           PIC S9(5)  COMP-3.                       
010100 77    WS-KVPTID-MIN             PIC S9(3)      COMP-3 VALUE ZERO.        
010200 77    WS-SUPTID-PRAPP-MIN-TOT   PIC S9(7)      COMP-3 VALUE ZERO.        
010300 77    WS-TRAEFF-KOLLI           PIC X(1).                                
010400 77    WS-RADER-OK               PIC X(1)   VALUE 'N'.                    
010500 77    WS-RADER-SAKNAS           PIC X(1).                                
010600 77    WS-RADER-RAPPORTERADE     PIC X(1).                                
010700 77    SW-FEL-PRODNR             PIC X(1)   VALUE 'N'.                    
010800 77    SW-FEL-PACKARE            PIC X(1)   VALUE 'N'.                    
010900 77    SW-ORDER-AVSLUTAD         PIC X(1)   VALUE 'N'.                    
011000 77    SW-TIKLAR-JUSTERAD        PIC X(1)   VALUE 'N'.                    
011400*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
011500 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
011600 77  KDRC-DISPLAY                PIC Z(5).                                
011700                                                                          
011800 77  WS-IDELMT-ERROR             PIC X(16).                               
011900 77  WS-IDMSG-ERROR              PIC X(03).                               
012000 77  WS-IDMSG-INFO               PIC X(03).                               
012100 77  WS-COUNT                    PIC 9(3)   VALUE ZERO.                   
012200 77  WS-REC-LIMIT                PIC X       VALUE 'N'.                   
012300     88  REC-LIMIT                           VALUE 'J'.                   
012400                                                                          
012500 77  WS-INDX-REC                 PIC S9(3)  VALUE +0   COMP-3.            
012600     EJECT                                                                
012700*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
012800 01  GENERAL-SUBPROGRAMS.                                                 
012900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
013000     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
013100     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
013200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
013210     03  WZ01AUTH                PIC X(8)    VALUE 'WZ01AUTH'.            
013220     03  WMSGCONV                PIC X(8)    VALUE 'WMSGCONV'.            
013300     SKIP3                                                                
013400*    --- PARAMETERS TO ABEND                                              
013500                                                                          
013600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
013700 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
013800 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
013900     SKIP3                                                                
014000 01  MESSAGE-CODES.                                                       
014100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
014200     EJECT                                                                
014300*                                                                         
014400 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
014500     SKIP3                                                                
014600*01  -COPY WZ01SUB                                                        
014700     EJECT                                                                
014710 01  FILLER                      PIC X(16)   VALUE 'WZ01AUTH   '.         
014720*01  -COPY WZ01AUTH                                                       
014730*                                                                         
014740 01  FILLER                      PIC X(16)   VALUE 'WMSGCONV'.            
014750*01  -COPY WMSGCONV                                                       
014760     EJECT                                                                
014800 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
014900     SKIP3                                                                
015000 01  REQU-AREA.                                                           
015100*    03  -COPY WZ01REQ2                                                   
015200*    03  -COPY WL0127I1                                                   
015300     EJECT                                                                
015400 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
015500     SKIP3                                                                
015600 01  RESP-AREA.                                                           
015700*    03  -COPY WZ01RES2                                                   
015800*    03  -COPY WL0127O1                                                   
015900     EJECT                                                                
016000*                                                                         
016100*      --- VALID IDDD CODES                                               
016200*                                                                         
016300*01    -COPY WWDC99                                                       
016400       EJECT                                                              
016500 01  GEMENSAMMA-SUBPROGRAM.                                               
016600                                                                          
016700     03  W411DNOT               PIC X(8)    VALUE 'W411DNOT'.             
016800*                                                                         
016900*    --- AREOR TILL GEMENSAMMA SUBPROGRAM                                 
017000*                                                                         
017100 01  FILLER                     PIC X(16)   VALUE 'W411DNOT '.            
017200*   -COPY W411DNOT                                                        
017300     EJECT                                                                
017400                                                                          
017500 77    FL-SLINGA-KLAR            PIC X(1).                                
017600   88  SLINGA-KLAR                          VALUE 'J'.                    
017700     SKIP2                                                                
017800 77    WS-INDATA-TEST            PIC X(01).                               
017900   88  WS-INDATA-FEL                        VALUE 'F'.                    
018000   88  WS-INDATA-RATT                       VALUE 'R'.                    
018100     SKIP2                                                                
018200 77    WS-KDKOLSTA               PIC 9(01).                               
018300     SKIP2                                                                
018400 77    WS-BEHANDLING-TEST        PIC X(01).                               
018500   88  WS-BEHANDLING-FEL                    VALUE 'F'.                    
018600   88  WS-BEHANDLING-RATT                   VALUE 'R'.                    
018700     SKIP2                                                                
018800     SKIP2                                                                
018900 77    FL-RADSTA-BACKAD          PIC X(01)  VALUE 'N'.                    
019000     EJECT                                                                
019100     SKIP2                                                                
019200 01     WS-IDRADNR-X                              PIC X(4).               
019300 01     WS-IDRADNR-NUM  REDEFINES WS-IDRADNR-X    PIC 9(4).               
019400     SKIP2                                                                
019500 01     WS-IDKUNDRF.                                                      
019600   03   WS-IDORDNR               PIC X(5)   VALUE SPACE.                  
019700   03   FILLER                   PIC X(5)   VALUE SPACE.                  
019800 01     WS-JFR-IDANSTNR.                                                  
019900   03   FILLER                   PIC X(3).                                
020000   03   WS-JFR-IDANSTNR-5        PIC X(5).                                
020100*                                                                         
020200 01     WS-SUPTID-PRAPP         PIC 9(3)V99.                              
020300 01     FILLER REDEFINES WS-SUPTID-PRAPP.                                 
020400   03   WS-SUPTID-TIM           PIC 9(3).                                 
020500   03   WS-SUPTID-MIN           PIC 9(2).                                 
020600*                                                                         
020700 01     FILLER                  PIC X(11)   VALUE 'ARBETSAREOR'.          
020800 01     ARBETSAREOR.                                                      
020900   03   ARB-AREA-RAD.                                                     
021000     05 ARB-RAD                 PIC  9(4)         VALUE ZERO.             
021100     05 ARB-KVLEVART            PIC  9(6)         VALUE ZERO.             
021200*                                                                         
021300 01 WS-WDE44-ORAD-IDPURAD       PIC  9(4)         VALUE ZERO.             
021400*                                                                         
021500 01     FILLER                  PIC X(10)   VALUE 'HJÄLPAREOR'.           
021600 01     HJALPAREOR.                                                       
021700   03   HJALP-ODEL-DARFS        PIC 9(12).                                
021800   03   FILLER                  REDEFINES HJALP-ODEL-DARFS.               
021900     05 FILLER                  PIC  9(2).                                
022000     05 HJALP-ODEL-DARFS-6      PIC  9(6).                                
022100     05 FILLER                  PIC  9(4).                                
022200   03   HJALP-4472-TIRFS        PIC 9(11).                                
022300   03   FILLER                  REDEFINES HJALP-4472-TIRFS.               
022400     05 FILLER                  PIC  9(1).                                
022500     05 HJALP-4472-TIRFS-6      PIC  9(6).                                
022600     05 FILLER                  PIC  9(4).                                
022700*                                                                         
022800   03   ARB-KOLLI-UPPG-AREA.                                              
022900     05 ARB-KOLLI-VKORDNTO       PIC  9(6)V9(1)    VALUE ZERO.            
023000     05 ARB-KOLLI-VKORDBTO       PIC  9(6)V9(1)    VALUE ZERO.            
023100     05 ARB-KOLLI-KVFLAMP        PIC  S9(2)V9(1)   VALUE ZERO.            
023200     05 ARB-KOLLI-KDFARLIG       PIC  S9           VALUE ZERO.            
023300     05 ARB-KOLLI-KVORDRAD       PIC  S9(5)        VALUE ZERO.            
023400     05 ARB-KOLLI-SUORDV         PIC  S9(9)V9(2)   VALUE ZERO.            
023500     05 ARB-KOLLI-SUORDV-LOC     PIC  S9(9)V9(2)   VALUE ZERO.            
023600     05 ARB-KOLLI-SUORDV-LOCPREL PIC  S9(9)V9(2)   VALUE ZERO.            
023700     SKIP2                                                                
023800 01     FILLER                  PIC X(10)   VALUE 'SPAR-AREOR'.           
023900 01     SPAR-AREA.                                                        
024000   03   SPAR-PRAD-UPPG-AREA.                                              
024100     05 SPAR-PRAD-VKARTNTO      PIC  9(6)V9(3)    VALUE ZERO.             
024200     05 SPAR-PRAD-KVFLAMP       PIC  S9(2)V9(1)   VALUE ZERO.             
024300     05 SPAR-PRAD-KDFARLIG      PIC  S9           VALUE ZERO.             
024400     05 SPAR-PRAD-KVLEVART      PIC  S9(7)        VALUE ZERO.             
024500     05 SPAR-PRAD-PRARTNTO      PIC  S9(9)V9(2)   VALUE ZERO.             
024600     05 SPAR-PRAD-PRARTNTO-LOC  PIC  S9(9)V9(2)   VALUE ZERO.             
024700     05 SPAR-PRAD-PRARTNTO-LOCPREL  PIC  S9(9)V9(2)   VALUE ZERO.         
024800     EJECT                                                                
024900 01     FILLER                  PIC X(12)   VALUE 'SPAR-FG-AREA'.         
025000 01     SPAR-FG-AREA.                                                     
025100   03   SPAR-ORAD-UPPG-AREA.                                              
025200     05 SPAR-ORAD-IDPSN         PIC  9(3)              VALUE 0.           
025300     05 SPAR-ORAD-VKART-FG      PIC  S9(7)      COMP-3 VALUE 0.           
025400     05 SPAR-ORAD-VLFG          PIC  S9(4)V9(3) COMP-3 VALUE 0.           
025500     05 SPAR-ORAD-SUEQFG        PIC  S9(3)V9(4) COMP-3 VALUE 0.           
025600*                                                                         
025700 01     TOTAL-SUEQFG            PIC  S9(3)V9(4) COMP-3 VALUE 0.           
025800     EJECT                                                                
025900 01     FILLER                  PIC X(16)   VALUE 'FG-TABELL'.            
026000 01     FARLIGT-GODS-TABELL.                                              
026100   03   TABELL-POST OCCURS 10.                                            
026200     05 TAB-IDPSN               PIC  9(3)              VALUE 0.           
026300     05 TAB-VKART-FG            PIC  S9(7)      COMP-3 VALUE 0.           
026400     05 TAB-VLFG                PIC  S9(4)V9(3) COMP-3 VALUE 0.           
026500     EJECT                                                                
026600*------- SWITCHAR                                                         
026700*                                                                         
026800 01     FILLER                  PIC X(16)   VALUE 'SW-SWITCHAR'.          
026900 01     SW-SWITCHAR.                                                      
027000*                                                                         
027100*       * BILDSIDA RÄCKER INTE TILL FÖR ALLA RADER                        
027200  03    SW-SIDA-OVERFULL     PIC X(1)    VALUE 'N'.                       
027300*       * KORD UPPFYLLER VILLKOR FÖR VISNING                              
027400  03    SW-VISA-KORD         PIC X(1)    VALUE 'N'.                       
027500*       * SLUT PÅ RADNR PÅ BILDEN                                         
027600  03    SW-SLUT-RADNR        PIC X(1)    VALUE 'N'.                       
027700*       * ORAD HAR KKOLLI-SEGM FÖR INMATAT KOLLI-NR                       
027800  03    SW-KKOLLI-FINNS      PIC X(1)    VALUE 'N'.                       
027900     EJECT                                                                
028000 01    TEST-IDDISTR              PIC  9(5)               COMP-3.          
028100     SKIP3                                                                
028200*01    FILLER -COPY WWDIST07     -RED TEST-IDDISTR.                       
028300                                                                          
028400*01    FILLER -COPY WWDIST19     -RED TEST-IDDISTR.                       
028500*    ----DISTR-DEALER-PRICE-----                                          
028600*01    FILLER -COPY WWDIST79     -RED TEST-IDDISTR.                       
028700     EJECT                                                                
028800 01    NYCKLAR-TILL-DLI.                                                  
028900*                                                                         
029000   03    W-WDE4A1-KUNDORDER-X.                                            
029100     05    W-4A1-IDDISTR         PIC S9(5)   VALUE ZERO  COMP-3.          
029200     05    W-4A1-IDKUNDNR        PIC S9(7)   VALUE ZERO  COMP-3.          
029300     05    W-4A1-IDKUNDRF.                                                
029400       07  W-4A1-IDORDNR         PIC  9(5)   VALUE ZERO.                  
029500       07  FILLER                PIC  X(5)   VALUE SPACE.                 
029600*                                                                         
029700   03    W-WDE401-KUNDORDER-X.                                            
029800     05    W-401-IDDISTR         PIC S9(5)   VALUE ZERO  COMP-3.          
029900     05    W-401-IDKUNDNR        PIC S9(7)   VALUE ZERO  COMP-3.          
030000     05    W-401-IDKUNDRF.                                                
030100       07  W-401-IDORDNR         PIC  9(5)   VALUE ZERO.                  
030200       07  FILLER                PIC  X(5)   VALUE SPACE.                 
030300     05    W-401-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
030400     05    W-401-IDPLKLST        PIC S9(3)   VALUE ZERO  COMP-3.          
030500*                                                                         
030600   03    W-WDE411-IDPURAD-X.                                              
030700     05    W-411-IDPURAD2        PIC S9(5)   VALUE ZERO  COMP-3.          
030800*                                                                         
030900   03    W-WDE4B-KEYSEQ-MIN-X.                                            
031000     05    W-411-IDPRODNR-MIN    PIC S9(7)   VALUE ZERO  COMP-3.          
031100     05    W-411-IDPURAD-MIN     PIC S9(5)   VALUE ZERO  COMP-3.          
031200*                                                                         
031300   03    W-WDE4F1KY-MAX-X.                                                
031400     05    W-IDPRODNR-WDE4F-MAX  PIC S9(7)   VALUE ZERO  COMP-3.          
031500     05    W-IDKOLLI-WDE4F-MAX   PIC S9(5)   VALUE ZERO  COMP-3.          
031600     05    FILLER                PIC X(22)   VALUE HIGH-VALUE.            
031700                                                                          
031800   03    W-WDE4F1KY-MIN-X.                                                
031900     05    W-IDPRODNR-WDE4F-MIN  PIC S9(7)   VALUE ZERO  COMP-3.          
032000     05    W-IDKOLLI-WDE4F-MIN   PIC S9(5)   VALUE ZERO  COMP-3.          
032100     05    FILLER                PIC X(22)   VALUE LOW-VALUE.             
032200                                                                          
032300     EJECT                                                                
032400   03    W-WDE4B-KEYSEQ-MAX-X.                                            
032500     05    W-411-IDPRODNR-MAX    PIC S9(7)   VALUE ZERO  COMP-3.          
032600     05    W-411-IDPURAD-MAX     PIC S9(5)   VALUE ZERO  COMP-3.          
032700*                                                                         
032800   03    W-WDE4FSEQ-X.                                                    
032900     05    W-IDPRODNR-F          PIC S9(7)   VALUE ZERO  COMP-3.          
033000     05    W-IDKOLLI-F           PIC S9(5)   VALUE ZERO  COMP-3.          
033100*                                                                         
033200   03    W-WDE4B-KEYSEQ-X.                                                
033300     05    W-411-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
033400     05    W-411-IDPURAD         PIC S9(5)   VALUE ZERO  COMP-3.          
033500*                                                                         
033600   03    W-WDE421-IDKOLLI-X.                                              
033700     05    W-421-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
033800     05    W-421-IDKOLLI         PIC S9(5)   VALUE ZERO  COMP-3.          
033900*                                                                         
034000   03    W-WDE601-IDPRODNR-X.                                             
034100     05    W-601-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
034200*                                                                         
034300   03    W-WDE611-IDKOLLI-X.                                              
034400     05    W-611-IDKOLLI         PIC S9(5)   VALUE ZERO  COMP-3.          
034500*                                                                         
034600   03    W-WDEE4F-IDPLKST-X.                                              
034700     05    W-E4F-IDPLKST         PIC S9(3)   VALUE ZERO  COMP-3.          
034800*                                                                         
034900*                                                                         
035000   03    W-WDGXKEY-4321-X.                                                
035100     05    W-IDHTYP-4321         PIC  9(4)   VALUE 4321.                  
035200     05    FILLER                PIC X(26)   VALUE LOW-VALUE.             
035300*                                                                         
035400   03    W-WDQ301-ORDERDEL-X.                                             
035500     05    W-301-IDORDER         PIC S9(7)   COMP-3.                      
035600     05    W-301-IDDC            PIC X(2).                                
035700     05    W-301-IDPRODNR        PIC S9(7)   COMP-3.                      
035800     05    W-301-IDPLKLST        PIC S9(3)   COMP-3.                      
035900*                                                                         
036000   03    W-WDGXKEY-4471-X.                                                
036100     05    W-IDHTYP-4471         PIC  9(4)   VALUE 4471.                  
036200     05    W-IDDC-4471           PIC  X(2).                               
036300     05    W-IDPRC.                                                       
036400       07  W-IDPRCBAS-4471       PIC  X(3).                               
036500       07  W-IDPRCVAR-4471       PIC  X(1).                               
036600     05    FILLER                PIC  X(20)  VALUE LOW-VALUE.             
036700*                                                                         
036800   03    W-KDSEGKEY-4472-X.                                               
036900     05    W-KDSEGKEY-4472       PIC  X(1)   VALUE '1'.                   
037000*                                                                         
037100   03    W-WDGXKEY-4477-X.                                                
037200     05    W-IDHTYP-4477         PIC  9(4)   VALUE 4477.                  
037300     05    W-IDDC-4477           PIC  X(2).                               
037400     05    FILLER                PIC  X(24)  VALUE LOW-VALUE.             
037500*                                                                         
037600   03    W-WDGXKEY-4478-X.                                                
037700     05    W-IDSHIFT-4478        PIC  X(1).                               
037800     05    W-IDUSER-4478         PIC  X(8).                               
037900     05    FILLER                PIC  X(1)   VALUE LOW-VALUE.             
038000*                                                                         
038100     03  W-4301-WDGXKEY-X.                                                
038200         05 W-4301-IDHTYP        PIC X(4)  VALUE '4301'.                  
038300         05 W-4301-IDPRODNR      PIC S9(7) VALUE ZERO COMP-3.             
038400         05 W-4301-NYCKEL-VALFRI PIC X(22) VALUE LOW-VALUE.               
038500                                                                          
038600     03  W-4302-WDGXKEY-X.                                                
038700         05 W-4302-IDKOLLI-X.                                             
038800            07 W-4302-IDKOLLI    PIC S9(5) VALUE ZERO COMP-3.             
038900         05 W-4302-IDPLKLST-X.                                            
039000            07 W-4302-IDPLKLST   PIC S9(3) VALUE ZERO COMP-3.             
039100                                                                          
039200   03    W-WDA601KY-MIN-X.                                                
039300     05    W-A601KY-MIN-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
039400     05    W-A601KY-MIN-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
039500     05    W-A601KY-MIN-IDORDNR      PIC 9(07) VALUE ZERO.                
039600     05    FILLER                    PIC X(03) VALUE SPACE.               
039700     05    W-A601KY-MIN-TIREGDAT     PIC S9(7) VALUE ZERO COMP-3.         
039800     05    W-A601KY-MIN-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
039900     05    W-A601KY-MIN-TIREGTID     PIC S9(9) VALUE ZERO COMP-3.         
040000     05    W-A601KY-MIN-TIREGDAT-AVV PIC S9(7) VALUE ZERO COMP-3.         
040100     05    W-A601KY-MIN-TIREGTID-AVV PIC S9(9) VALUE ZERO COMP-3.         
040200     SKIP2                                                                
040300   03    W-WDA601KY-MAX-X.                                                
040400     05    W-A601KY-MAX-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
040500     05    W-A601KY-MAX-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
040600     05    W-A601KY-MAX-IDORDNR      PIC 9(07) VALUE ZERO.                
040700     05    FILLER                    PIC X(03) VALUE SPACE.               
040800     05    W-A601KY-MAX-TIREGDAT     PIC S9(7) VALUE ZERO COMP-3.         
040900     05    W-A601KY-MAX-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
041000     05    W-A601KY-MAX-TIREGTID     PIC S9(9) VALUE ZERO COMP-3.         
041100     05    W-A601KY-MAX-TIREGDAT-AVV PIC S9(7) VALUE ZERO COMP-3.         
041200     05    W-A601KY-MAX-TIREGTID-AVV PIC S9(9) VALUE ZERO COMP-3.         
041300*                                                                         
041400     EJECT                                                                
041500     SKIP3                                                                
041600 01    MEDDELANDE.                                                        
041700   03    INFO-UPDATE-OK           PIC X(03)   VALUE '001'.                
041800   03    FEL-XX-NOT-FOUND         PIC X(03)   VALUE '025'.                
041900   03    FEL-TOO-MANY-LINES       PIC X(03)   VALUE '028'.                
042000   03    FEL-SYSTEM-ERROR         PIC X(03)   VALUE '099'.                
042100   03    FEL-PACKER-ORDER-NOMATCH PIC X(03)   VALUE '111'.                
042200   03    FEL-ORDERPART-READY      PIC X(03)   VALUE '151'.                
042300   03    FEL-DEVIATION-CONTROL    PIC X(03)   VALUE '152'.                
042400   03    FEL-INTERVAL-PACKER      PIC X(03)   VALUE '160'.                
042500   03    FEL-CASE-ORDER-MISSING   PIC X(03)   VALUE '169'.                
042600   03    FEL-NO-UPDATE            PIC X(03)   VALUE '254'.                
042700   03    FEL-XX-ALREADY-INV       PIC X(03)   VALUE '255'.                
042800   03    FEL-MAY-NOT-BE-EMPTY     PIC X(03)   VALUE '256'.                
042900   03    FEL-CONSOLIDATED-CASE    PIC X(03)   VALUE '256'.                
042910   03    ERR-UNAUTHORIZED         PIC X(03)   VALUE '00A'.                
043000     EJECT                                                                
043100******************************************************************        
043200*                                                                         
043300*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
043400*                                                                         
043500 01    IMS-WS.                                                            
043600   03    FILLER                  PIC X(16)   VALUE 'IMS-WS*****'.         
043700     SKIP3                                                                
043800*                        **** STATUS-KOD FRÅN IMS                         
043900   03    STATUS-RAD-WS           PIC XX.                                  
044000     88    RAD-FINNS                         VALUE '  '.                  
044100     88    RAD-SAKNAS                        VALUE 'GE'.                  
044200   03    STATUS-KUNDORDER-SEK-WS PIC XX.                                  
044300     88    KUNDORDER-SEK-FINNS               VALUE '  '.                  
044400     88    KUNDORDER-SEK-SAKNAS              VALUE 'GE' 'GB'.             
044500   03    STATUS-WS               PIC XX.                                  
044600     88    SEGMENT-FINNS                     VALUE '  '.                  
044700     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
044800     88    BASEN-SLUT                        VALUE 'GB'.                  
044900     88    SEGMENT-FINNS-REDAN               VALUE 'II'.                  
045000     SKIP3                                                                
045100   03    GODK-STATUSKODER.                                                
045200     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
045300     SKIP3                                                                
045400 01    SSA1                      PIC X(128).                              
045500 01    SSA2                      PIC X(128).                              
045600     EJECT                                                                
045700*                            IMS FUNKTIONSKODER                           
045800*01    -COPY W0003                                                        
045900     EJECT                                                                
046000*                            DLI INPUT-OUTPUT AREA                        
046100 01  FILLER                  PIC X(16)   VALUE 'WDE401-IO'.               
046200 01  DLI-IO-E401.                                                         
046300*  03    WDE401 -COPY WDE401                                              
046400     EJECT                                                                
046500 01  FILLER                  PIC X(16)   VALUE 'WDE411-IO'.               
046600 01  DLI-IO-E411.                                                         
046700*  03    WDE411 -COPY WDE411                                              
046800     EJECT                                                                
046900 01  FILLER                  PIC X(16)   VALUE 'WDE421-IO'.               
047000 01  DLI-IO-E421.                                                         
047100*  03    WDE421 -COPY WDE421                                              
047200     EJECT                                                                
047300 01    DLI-IO-AREA3.                                                      
047400   03    IO-AREA3                PIC X(512)  VALUE SPACE.                 
047500*  03    WDE601 -COPY WDE601             -RED IO-AREA3.                   
047600     EJECT                                                                
047700*  03    WDE611 -COPY WDE611             -RED IO-AREA3.                   
047800     EJECT                                                                
047900*  03    WDE411 -COPY WDE411 -PRE KOPPL- -RED IO-AREA3.                   
048000     EJECT                                                                
048100 01    DLI-IO-AREA4.                                                      
048200   03    IO-AREA4                PIC X(100)  VALUE SPACE.                 
048300*  03    WDGX01   -COPY WDGX01             -RED IO-AREA4.                 
048400     EJECT                                                                
048500*  03    WDGX4322 -COPY WDGX4322           -RED IO-AREA4.                 
048600     EJECT                                                                
048700 01    DLI-IO-AREA5.                                                      
048800   03    IO-AREA5                PIC X(320)  VALUE SPACE.                 
048900*  03    WLORQA01 -COPY WDQ301             -RED IO-AREA5.                 
049000     EJECT                                                                
049100 01    DLI-IO-AREA6.                                                      
049200   03    IO-AREA6                PIC X(1000) VALUE SPACE.                 
049300*  03    WDGX4472 -COPY WDGX4472           -RED IO-AREA6.                 
049400     EJECT                                                                
049500*  03    WDGX4478 -COPY WDGX4478           -RED IO-AREA6.                 
049600     EJECT                                                                
049700 01    FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA8'.        
049800 01    DLI-IO-AREA8.                                                      
049900   03    WDE411 -COPY WDE411   -PRE WDE43-                                
050000     EJECT                                                                
050100 01    FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA9'.        
050200 01    DLI-IO-AREA9.                                                      
050300   03    IO-AREA9                PIC X(512)  VALUE SPACE.                 
050400*  03    WDE601 -COPY WDE601   -PRE WDE6-   -RED IO-AREA9.                
050500     EJECT                                                                
050600*  03    WDE611 -COPY WDE611   -PRE WDE6-   -RED IO-AREA9.                
050700     EJECT                                                                
050800 01    FILLER                    PIC X(16)   VALUE 'DLI-IO-E421'.         
050900 01    DLI-IO-WDE411-21.                                                  
051000     SKIP2                                                                
051100   03    WDE411 -COPY WDE411   -PRE WDE44-.                               
051200   03    WDE421 -COPY WDE421   -PRE WDE44-.                               
051300                                                                          
051400 01  FILLER                      PIC X(16)   VALUE 'A601-AREA'.           
051500 01  DLI-IO-AREA-WDA6.                                                    
051600*  03    -COPY WDA601                                                     
051700     EJECT                                                                
051710*                                                                         
051720 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDR501'.              
051730 01  DLI-IO-WDR501.                                                       
051740*    03   -COPY WDGX01                                                    
051750 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDGX0102'.            
051760 01  DLI-IO-WDGX0102.                                                     
051770*  03   -COPY WDGX0102                                                    
051780     EJECT                                                                
051800*                                                                         
051900 LINKAGE SECTION.                                                         
052000*01    -COPY W0009     -PRE MSG-                                          
052100     EJECT                                                                
052110*01    -COPY W0008     -PRE WDR5-                                         
052120     05  FILLER                  PIC X.                                   
052130     EJECT                                                                
052200*01    -COPY W0008     -PRE WDE41-                                        
052300     05  FILLER                  PIC X.                                   
052400     EJECT                                                                
052500*01    -COPY W0008     -PRE WDE4A-                                        
052600     05  FILLER                  PIC X.                                   
052700     EJECT                                                                
052800*01    -COPY W0008     -PRE WDE64-                                        
052900     05  FILLER                  PIC X.                                   
053000     EJECT                                                                
053100*01    -COPY W0008     -PRE WDE4-                                         
053200     05  FILLER                  PIC X.                                   
053300     EJECT                                                                
053400*01    -COPY W0008     -PRE WDE42-                                        
053500     05  FILLER                  PIC X.                                   
053600     EJECT                                                                
053700*01    -COPY W0008     -PRE WDE6-                                         
053800     05  FILLER                  PIC X.                                   
053900     EJECT                                                                
054000*01    -COPY W0008     -PRE XXJK-                                         
054100     05  FILLER                  PIC X.                                   
054200     EJECT                                                                
054300*01    -COPY W0008     -PRE ORQA-                                         
054400     05  FILLER                  PIC X.                                   
054500     EJECT                                                                
054600*01    -COPY W0008     -PRE XXKW-                                         
054700     05  FILLER                  PIC X.                                   
054800     EJECT                                                                
054900*01    -COPY W0008     -PRE XXLB-                                         
055000     05  FILLER                  PIC X.                                   
055100     EJECT                                                                
055200*01    -COPY W0008     -PRE WDE44-                                        
055300     05  FILLER                  PIC X.                                   
055400     EJECT                                                                
055500*01    -COPY W0008     -PRE WDE4F-                                        
055600     05  FILLER                  PIC X.                                   
055700     EJECT                                                                
055800*01    -COPY W0008     -PRE WDE43-                                        
055900     05  FILLER                  PIC X.                                   
056000     EJECT                                                                
056100*01    -COPY W0008     -PRE WDE62-                                        
056200     05  FILLER                  PIC X.                                   
056300     EJECT                                                                
056400*01    -COPY W0008     -PRE XXDU-                                         
056500     05  FILLER                  PIC X.                                   
056600     EJECT                                                                
056700*01    -COPY W0008     -PRE WDA6B-                                        
056800     05  FILLER                  PIC X.                                   
056900     EJECT                                                                
057000 01  DNOT-ORQP-PCB               PIC X.                                   
057100 01  DNOT-ORQP2-PCB              PIC X.                                   
057200 01  DNOT-ORQP3-PCB              PIC X.                                   
057300 01  DNOT-4013-PCB               PIC X.                                   
057400 01  DNOT-BENA-PCB               PIC X.                                   
057500     EJECT                                                                
057600 PROCEDURE DIVISION USING  MSG-PCB WDR5-PCB WDE41-PCB                     
057700      WDE4A-PCB WDE64-PCB WDE4-PCB WDE42-PCB WDE6-PCB XXJK-PCB            
057800      ORQA-PCB XXKW-PCB XXLB-PCB WDE44-PCB WDE4F-PCB                      
057900      WDE43-PCB WDE62-PCB XXDU-PCB WDA6B-PCB                              
058000      DNOT-ORQP-PCB                                                       
058100      DNOT-ORQP2-PCB                                                      
058200      DNOT-ORQP3-PCB                                                      
058300      DNOT-4013-PCB                                                       
058400      DNOT-BENA-PCB.                                                      
058500                                                                          
058600 MAIN SECTION.                                                            
058700     ENTRY 'DLITCBL' USING MSG-PCB WDR5-PCB WDE41-PCB                     
058800      WDE4A-PCB WDE64-PCB WDE4-PCB WDE42-PCB WDE6-PCB XXJK-PCB            
058900      ORQA-PCB XXKW-PCB XXLB-PCB WDE44-PCB WDE4F-PCB                      
059000      WDE43-PCB WDE62-PCB XXDU-PCB WDA6B-PCB                              
059100      DNOT-ORQP-PCB                                                       
059200      DNOT-ORQP2-PCB                                                      
059300      DNOT-ORQP3-PCB                                                      
059400      DNOT-4013-PCB                                                       
059500      DNOT-BENA-PCB.                                                      
059600                                                                          
059700     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
059800     IF SUB-KDRC = 0                                                      
059900        IF REQU-KDPGMACT = 'E' OR 'S'                                     
060000           PERFORM A-INIT                                                 
060100                                                                          
060200           PERFORM B-GENERELL-KONTROLL                                    
060300                                                                          
060400           IF WS-INDATA-RATT                                              
060500              PERFORM C-RELATIONSKONTROLL                                 
060600                                                                          
060700              IF WS-INDATA-RATT                                           
060800                                                                          
060900                 IF REQU-KDPGMACT =  'E'                                  
061000                    IF REQU-KVRADER  NUMERIC AND                          
061100                       REQU-KVRADER > 0                                   
061200                       MOVE REQU-KVRADER TO WS-INDX-REC                   
061300                       MOVE NEJ          TO WS-REC-LIMIT                  
061400                       MOVE +1           TO RAD-INX                       
061500                       PERFORM UNTIL (RAD-INX NOT <                       
061600                               MAX-ANTAL-RADER-PLUS-1) OR                 
061700                               REC-LIMIT                                  
061800                          IF REQU-FLBACKA (RAD-INX) = JA                  
061900                          OR REQU-FLBACKA (RAD-INX) = YES                 
062000                          OR REQU-FLBACKA-ALLA     = JA                   
062100                          OR REQU-FLBACKA-ALLA     = YES                  
062200                             PERFORM E-BEH-ATERBOKNING                    
062300                          END-IF                                          
062400                                                                          
062500                          IF RAD-INX = WS-INDX-REC                        
062600                             MOVE JA TO WS-REC-LIMIT                      
062700                          ELSE                                            
062800                             ADD +1  TO RAD-INX                           
062900                          END-IF                                          
063000                       END-PERFORM                                        
063100                                                                          
063200                       IF WS-BEHANDLING-RATT                              
063300                          PERFORM F-UPPDATERA-KOLLIREG                    
063400                       END-IF                                             
063500                                                                          
063600                       IF REQU-IDRADNR (1) NUMERIC                        
063700                          MOVE REQU-IDRADNR (1) TO                        
063800                               WS-IDRADNR-START                           
063900                       ELSE                                               
064000                          MOVE ZERO TO WS-IDRADNR-START                   
064100                       END-IF                                             
064200                                                                          
064300                    ELSE                                                  
064400                       MOVE FEL           TO WS-INDATA-TEST               
064500                       IF REQU-KVRADER = 0                                
064600                          MOVE 'KVRADER' TO RESP-IDELMT-ERROR             
064700                          MOVE '126'     TO RESP-IDMSG-ERROR              
064800                       ELSE                                               
064900                          MOVE 'KVRADER' TO RESP-IDELMT-ERROR             
065000                          MOVE '024'     TO RESP-IDMSG-ERROR              
065100                       END-IF                                             
065200                    END-IF                                                
065300                 END-IF                                                   
065400                 PERFORM D-LAGG-UT-RADER                                  
065500              END-IF                                                      
065600           END-IF                                                         
065700                                                                          
065800           IF WS-INDATA-RATT                                              
065900              PERFORM H-AVSLUT                                            
066000           END-IF                                                         
066100        ELSE                                                              
066200           MOVE FEL-SYSTEM-ERROR TO RESP-IDMSG-ERROR                      
066300        END-IF                                                            
066400        MOVE RESP-IDMSG-INFO    TO WS-IDMSG-INFO                          
066500        MOVE RESP-IDMSG-ERROR   TO WS-IDMSG-ERROR                         
066600        MOVE RESP-IDELMT-ERROR  TO WS-IDELMT-ERROR                        
066700        IF WS-IDMSG-ERROR NOT = SPACE                                     
066800           MOVE ALL '+' TO RESP-WL0127O1(1:34)                            
066900           MOVE WS-IDMSG-ERROR   TO RESP-IDMSG-ERROR                      
067000           MOVE WS-IDELMT-ERROR  TO RESP-IDELMT-ERROR                     
067100           MOVE WS-IDMSG-INFO    TO RESP-IDMSG-INFO                       
067200           MOVE 001              TO RESP-IDRESVER                         
067300           MOVE ZERO             TO RESP-KVRADER                          
067400        END-IF                                                            
067410        IF SUB-KDTRANS(1:6) = 'WLA127'                                    
067420          PERFORM S03-MSG-CONV                                            
067430        END-IF                                                            
067500        PERFORM S02-RETURN-RESPONSE                                       
067600     END-IF                                                               
067800     MOVE ZERO TO RETURN-CODE                                             
067900     GOBACK                                                               
068000     .                                                                    
068100     EJECT                                                                
068200 A-INIT             SECTION.                                              
068300                                                                          
068400       MOVE +2 TO INDX                                                    
068500                                                                          
068600     MOVE RAETT                           TO WS-INDATA-TEST               
068700                                             WS-BEHANDLING-TEST           
068800     MOVE ZERO                            TO WS-IDRADNR-START             
068900                                             ARB-AREA-RAD                 
069000     MOVE 'N'                             TO SW-FEL-PRODNR                
069100                                             SW-FEL-PACKARE               
069200                                             SW-ORDER-AVSLUTAD            
069300                                                                          
069400     MOVE ALL '+'        TO RESP-AREA                                     
069500     MOVE SPACE          TO RESP-IDMSG-ERROR                              
069600                            RESP-IDMSG-INFO                               
069700                            RESP-IDELMT-ERROR                             
069800     MOVE 001            TO RESP-IDRESVER                                 
069900     MOVE ZERO           TO RESP-KVRADER                                  
070000     MOVE ZERO           TO WS-COUNT                                      
070010     IF SUB-KDTRANS(1:7) = 'WL0127T' OR 'WL0127U'                         
070020        CONTINUE                                                          
070030     ELSE                                                                 
070040                                                                          
070050        MOVE 001                  TO AUTH-KDCALL                          
070060        CALL WZ01AUTH          USING AUTH-WZ01AUTH                        
070070                                     REQU-WZ01REQ2                        
070080        IF AUTH-KDRC > 0                                                  
070090          MOVE ERR-UNAUTHORIZED   TO RESP-IDMSG-ERROR                     
070091          MOVE NEJ                TO NY-NYCKEL                            
070092        END-IF                                                            
070093                                                                          
070094        MOVE FUNCTION UPPER-CASE (REQU-KDPGMACT)  TO                      
070095                                 REQU-KDPGMACT                            
070096        MOVE FUNCTION UPPER-CASE (REQU-IDDC-KEY)  TO                      
070100                                 REQU-IDDC-KEY                            
070200     END-IF                                                               
070201                                                                          
070210     PERFORM AA-FLYTTA-NYCKLAR                                            
070220     .                                                                    
070230     EJECT                                                                
070300                                                                          
070500 AA-FLYTTA-NYCKLAR  SECTION.                                              
070600                                                                          
070700     MOVE NEJ                             TO NY-NYCKEL                    
070800     IF REQU-IDANSTNR-KEY = ALL '+'                                       
070900         MOVE ZERO                        TO   WS-IDANSTNR                
071000     ELSE                                                                 
071100         MOVE REQU-IDANSTNR-KEY           TO   WS-IDANSTNR                
071200         MOVE JA                          TO NY-NYCKEL                    
071300     END-IF                                                               
071400                                                                          
071500     IF REQU-IDPRODNR-KEY = ALL '+'                                       
071600         MOVE ZERO                        TO   WS-IDPRODNR                
071700     ELSE                                                                 
071800         MOVE REQU-IDPRODNR-KEY           TO   WS-IDPRODNR                
071900         MOVE JA                          TO NY-NYCKEL                    
072000     END-IF                                                               
072100                                                                          
072200     IF REQU-IDDISTR-KEY = ALL '+' OR                                     
072300        REQU-IDDISTR-KEY = SPACES OR                                      
072400        REQU-IDDISTR-KEY = LOW-VALUES                                     
072500         MOVE ZERO                        TO   WS-IDDISTR                 
072600     ELSE                                                                 
072700         MOVE REQU-IDDISTR-KEY            TO   WS-IDDISTR                 
072800         MOVE JA                          TO NY-NYCKEL                    
072900     END-IF                                                               
073000     IF REQU-IDKUNDNR-KEY = ALL '+' OR                                    
073100        REQU-IDKUNDNR-KEY = SPACES OR                                     
073200        REQU-IDKUNDNR-KEY = LOW-VALUES                                    
073300         MOVE ZERO                        TO   WS-IDKUNDNR                
073400     ELSE                                                                 
073500         MOVE REQU-IDKUNDNR-KEY           TO   WS-IDKUNDNR                
073510         MOVE JA                          TO NY-NYCKEL                    
073520     END-IF                                                               
073530                                                                          
073600*    IF REQU-IDORDNR-KEY = ALL '+'                                        
073700*        MOVE ZERO                        TO   WS-IDORDNR                 
073800         MOVE '00000'                     TO   WS-IDORDNR                 
073900*    ELSE                                                                 
074000*        MOVE REQU-IDORDNR-KEY            TO   WS-IDORDNR                 
074100*        MOVE JA                          TO NY-NYCKEL                    
074200*    END-IF                                                               
074300                                                                          
074400     IF REQU-IDKOLLI-KEY = ALL '+'                                        
074500         MOVE ZERO                        TO   WS-IDKOLLI                 
074600     ELSE                                                                 
074700         MOVE REQU-IDKOLLI-KEY            TO   WS-IDKOLLI                 
074800         MOVE JA                          TO NY-NYCKEL                    
074900     END-IF                                                               
075000                                                                          
075100     MOVE REQU-IDDC-KEY                   TO RESP-IDDC-KEY                
075200                                                                          
075300     IF WS-IDANSTNR  NUMERIC                                              
075400      MOVE WS-IDANSTNR                     TO   RESP-IDANSTNR-KEY         
075500      INSPECT RESP-IDANSTNR-KEY REPLACING LEADING ZERO BY SPACE           
075600     END-IF                                                               
075700                                                                          
075800     IF WS-IDDISTR NUMERIC                                                
075900      MOVE WS-IDDISTR                      TO   RESP-IDDISTR-KEY          
076000      INSPECT RESP-IDDISTR-KEY REPLACING LEADING ZERO BY SPACE            
076100     END-IF                                                               
076200                                                                          
076300     IF WS-IDKUNDNR NUMERIC                                               
076400      MOVE WS-IDKUNDNR                     TO   RESP-IDKUNDNR-KEY         
076500      INSPECT RESP-IDKUNDNR-KEY REPLACING LEADING ZERO BY SPACE           
076600     END-IF                                                               
076700                                                                          
076800*    IF WS-IDORDNR  NUMERIC                                               
076900*     MOVE WS-IDORDNR                      TO   RESP-IDORDNR-KEY          
077000*     INSPECT RESP-IDORDNR-KEY REPLACING LEADING ZERO BY SPACE            
077100*    END-IF                                                               
077200                                                                          
077300     IF  WS-IDKOLLI NUMERIC                                               
077400      MOVE WS-IDKOLLI                      TO   RESP-IDKOLLI-KEY          
077500      INSPECT RESP-IDKOLLI-KEY REPLACING LEADING ZERO BY SPACE            
077600     END-IF                                                               
077700                                                                          
077800     IF WS-IDPRODNR  NUMERIC                                              
077900      MOVE WS-IDPRODNR                     TO   RESP-IDPRODNR-KEY         
078000      INSPECT RESP-IDPRODNR-KEY REPLACING LEADING ZERO BY SPACE           
078100     END-IF                                                               
078200     .                                                                    
078300                                                                          
078400     EJECT                                                                
078500 B-GENERELL-KONTROLL  SECTION.                                            
078600     SKIP3                                                                
078700     IF REQU-IDANSTNR-KEY = ALL '+'                                       
078800         MOVE FEL                       TO   WS-INDATA-TEST               
078900         MOVE '024'                     TO   RESP-IDMSG-ERROR             
079000         MOVE 'IDANSTNR'                TO   RESP-IDELMT-ERROR            
079100     END-IF                                                               
079200     IF WS-IDANSTNR NOT NUMERIC                                           
079300         MOVE FEL                       TO   WS-INDATA-TEST               
079400         MOVE '024'                     TO   RESP-IDMSG-ERROR             
079500         MOVE 'IDANSTNR'                TO   RESP-IDELMT-ERROR            
079600     END-IF                                                               
079700                                                                          
079800     IF WS-IDDISTR  NOT NUMERIC                                           
079900         MOVE FEL                       TO   WS-INDATA-TEST               
080000         MOVE '024'                     TO   RESP-IDMSG-ERROR             
080100         MOVE 'IDDISTR'                 TO   RESP-IDELMT-ERROR            
080200     ELSE                                                                 
080300         MOVE WS-IDDISTR                TO   W-401-IDDISTR                
080400                                             W-4A1-IDDISTR                
080500     END-IF                                                               
080600     SKIP2                                                                
080700     IF WS-IDKUNDNR NOT NUMERIC                                           
080800         MOVE FEL                       TO   WS-INDATA-TEST               
080900         MOVE '024'                     TO   RESP-IDMSG-ERROR             
081000         MOVE 'IDKUNDNR'                TO   RESP-IDELMT-ERROR            
081100     ELSE                                                                 
081200         MOVE WS-IDKUNDNR               TO   W-401-IDKUNDNR               
081300                                             W-4A1-IDKUNDNR               
081400     END-IF                                                               
081500     SKIP2                                                                
081600*    IF WS-IDORDNR  NOT NUMERIC                                           
081700*        MOVE FEL                       TO   WS-INDATA-TEST               
081800*        MOVE '024'                     TO   RESP-IDMSG-ERROR             
081900*        MOVE 'IDORDNR'                 TO   RESP-IDELMT-ERROR            
082000*    ELSE                                                                 
082100         MOVE WS-IDORDNR                TO   W-401-IDORDNR                
082200                                             W-4A1-IDORDNR                
082300*    END-IF                                                               
082400     SKIP2                                                                
082500     IF REQU-IDKOLLI-KEY = ALL '+'                                        
082600         MOVE FEL                       TO   WS-INDATA-TEST               
082700         MOVE '024'                     TO   RESP-IDMSG-ERROR             
082800         MOVE 'IDKOLLI'                 TO   RESP-IDELMT-ERROR            
082900     END-IF                                                               
083000     IF WS-IDKOLLI NOT NUMERIC                                            
083100         MOVE FEL                       TO   WS-INDATA-TEST               
083200         MOVE '024'                     TO   RESP-IDMSG-ERROR             
083300         MOVE 'IDKOLLI'                 TO   RESP-IDELMT-ERROR            
083400     END-IF                                                               
083500     SKIP2                                                                
083600     IF REQU-IDPRODNR-KEY = ALL '+'                                       
083700         MOVE FEL                       TO   WS-INDATA-TEST               
083800         MOVE '024'                     TO   RESP-IDMSG-ERROR             
083900         MOVE 'IDPRODNR'                TO   RESP-IDELMT-ERROR            
084000     END-IF                                                               
084100     IF WS-IDPRODNR NOT NUMERIC                                           
084200         MOVE FEL                       TO   WS-INDATA-TEST               
084300         MOVE '024'                     TO   RESP-IDMSG-ERROR             
084400         MOVE 'IDPRODNR'                TO   RESP-IDELMT-ERROR            
084500     ELSE                                                                 
084600         MOVE WS-IDPRODNR                TO  WS-IDPRODNR-N                
084700     END-IF                                                               
084800                                                                          
084900         IF REQU-IDRADNR-START-KEY = ALL '+'                              
085000            CONTINUE                                                      
085100         ELSE                                                             
085200             IF   REQU-IDRADNR-START-KEY NOT NUMERIC                      
085300                  MOVE FEL                TO   WS-INDATA-TEST             
085400                  MOVE '258'              TO   RESP-IDMSG-ERROR           
085500             ELSE                                                         
085600                  MOVE REQU-IDRADNR-START-KEY TO WS-IDRADNR-START         
085700             END-IF                                                       
085800         END-IF                                                           
085900                                                                          
086000     IF   REQU-FLBACKA-ALLA NOT = JA                                      
086100     AND  REQU-FLBACKA-ALLA NOT = YES                                     
086200     AND  REQU-FLBACKA-ALLA NOT = NEJ                                     
086300     AND  REQU-FLBACKA-ALLA NOT = '+'                                     
086400          MOVE FEL                  TO WS-INDATA-TEST                     
086500          MOVE '258'                TO RESP-IDMSG-ERROR                   
086600     END-IF                                                               
086700                                                                          
086800     IF  (REQU-FLBACKA-ALLA = JA                                          
086900     OR   REQU-FLBACKA-ALLA = YES)                                        
087000     AND  REQU-KDPGMACT = 'S'                                             
087100          MOVE FEL                  TO WS-INDATA-TEST                     
087200          MOVE '013'                TO RESP-IDMSG-ERROR                   
087300     END-IF                                                               
087400                                                                          
087500     MOVE +1                        TO RAD-INX                            
087600     PERFORM UNTIL RAD-INX NOT < MAX-ANTAL-RADER-PLUS-1 OR                
087700                   RAD-INX > WS-INDX-REC                                  
087800         IF   REQU-FLBACKA (RAD-INX) NOT = JA                             
087900         AND  REQU-FLBACKA (RAD-INX) NOT = YES                            
088000         AND  REQU-FLBACKA (RAD-INX) NOT = NEJ                            
088100         AND  REQU-FLBACKA (RAD-INX) NOT = '+'                            
088200              MOVE FEL                TO WS-INDATA-TEST                   
088300              MOVE '259'              TO RESP-IDMSG-ERROR                 
088400                               RESP-IDMSG-ERROR-LINE (RAD-INX)            
088500         ELSE                                                             
088600              IF  (REQU-FLBACKA (RAD-INX) = JA OR                         
088700                   REQU-FLBACKA (RAD-INX) = YES)                          
088800              AND (REQU-FLBACKA-ALLA     = JA  OR                         
088900                   REQU-FLBACKA-ALLA     = YES)                           
089000                  MOVE FEL         TO WS-INDATA-TEST                      
089100                  MOVE '258'          TO RESP-IDMSG-ERROR                 
089200                            RESP-IDMSG-ERROR-LINE (RAD-INX)               
089300              END-IF                                                      
089400         END-IF                                                           
089500                                                                          
089600         IF  (REQU-FLBACKA (RAD-INX) = JA OR                              
089700              REQU-FLBACKA (RAD-INX) = YES)                               
089800         AND  NOT REQU-KDPGMACT = 'E'                                     
089900              MOVE FEL              TO WS-INDATA-TEST                     
090000              MOVE '013'            TO RESP-IDMSG-ERROR                   
090100                         RESP-IDMSG-ERROR-LINE (RAD-INX)                  
090200         END-IF                                                           
090300         ADD +1                     TO RAD-INX                            
090400     END-PERFORM                                                          
090500     .                                                                    
090600     EJECT                                                                
090700 C-RELATIONSKONTROLL  SECTION.                                            
090800                                                                          
090900     MOVE WS-IDDISTR                  TO WS-IDDISTR-N                     
091000     MOVE WS-IDKUNDNR                 TO WS-IDKUNDNR-N                    
091100                                                                          
091200     PERFORM CA-KONTROLLERA-KUNDORDNR                                     
091300                                                                          
091400     IF WS-INDATA-RATT                                                    
091500                                                                          
091600         PERFORM CB-KONTROLLERA-KOLLI                                     
091700         IF WS-INDATA-RATT                                                
091800           IF REQU-KDPGMACT = 'E'                                         
091900             PERFORM CC-KONTROLLERA-PACKARE                               
092000           END-IF                                                         
092100         END-IF                                                           
092200     END-IF                                                               
092300     .                                                                    
092400     EJECT                                                                
092500 CA-KONTROLLERA-KUNDORDNR SECTION.                                        
092600                                                                          
092700     IF REQU-IDPRODNR-KEY = ALL '+'                                       
092800         IF REQU-IDDISTR-KEY  = ALL '+'                                   
092900           AND REQU-IDKUNDNR-KEY = ALL '+'                                
093000           AND REQU-IDORDNR-KEY  = ALL '+'                                
093100             IF WS-IDPRODNR > ZERO                                        
093200*--------------------------ANVÄNDS GAMLA PRODNR: MID-IDPRODNR-UT          
093300                 PERFORM CAB-HAMTA-IDGMTREF-I-WDE4                        
093400             ELSE                                                         
093500                 PERFORM CAA-HAMTA-PRODNR-I-WDE4-6                        
093600             END-IF                                                       
093700         ELSE                                                             
093800             PERFORM CAA-HAMTA-PRODNR-I-WDE4-6                            
093900         END-IF                                                           
094000     ELSE                                                                 
094100         PERFORM CAB-HAMTA-IDGMTREF-I-WDE4                                
094200     END-IF                                                               
094300     .                                                                    
094400     EJECT                                                                
094500 CAA-HAMTA-PRODNR-I-WDE4-6  SECTION.                                      
094600                                                                          
094700     MOVE WS-IDDISTR-N                   TO W-4A1-IDDISTR                 
094800     MOVE WS-IDKUNDNR-N                  TO W-4A1-IDKUNDNR                
094900     MOVE WS-IDORDNR                     TO W-4A1-IDORDNR                 
095000                                                                          
095100     PERFORM IMS-GU-KUNDORDER-SEK                                         
095200                                                                          
095300     IF KUNDORDER-SEK-FINNS                                               
095400       PERFORM UNTIL (KORD-IDDC = REQU-IDDC-KEY AND                       
095500         KORD-KVORDRAD-LEVPL = ZERO) OR KUNDORDER-SEK-SAKNAS              
095600          PERFORM IMS-GN-KUNDORDER-SEK                                    
095700       END-PERFORM                                                        
095800                                                                          
095900       IF KUNDORDER-SEK-FINNS                                             
096000         MOVE KORD-IDPRODNR TO W-601-IDPRODNR                             
096100         PERFORM IMS-GU-WDE601                                            
096200                                                                          
096300         IF SEGMENT-FINNS                                                 
096500           MOVE VORD-IDPRODNR TO WS-IDPRODNR                              
096600                                 WS-IDPRODNR-N                            
096700         ELSE                                                             
096800           MOVE FEL                       TO   WS-INDATA-TEST             
096900           MOVE 'IDORDNR'                 TO  RESP-IDELMT-ERROR           
097000           MOVE '041'                     TO  RESP-IDMSG-ERROR            
097100         END-IF                                                           
097200       ELSE                                                               
097300         MOVE FEL                         TO   WS-INDATA-TEST             
097400         MOVE 'IDORDNR'                   TO  RESP-IDELMT-ERROR           
097500         MOVE '041'                       TO   RESP-IDMSG-ERROR           
097600       END-IF                                                             
097700     ELSE                                                                 
097800       MOVE FEL                          TO   WS-INDATA-TEST              
097900       MOVE 'IDORDNR'                    TO  RESP-IDELMT-ERROR            
098000       MOVE '041'                        TO   RESP-IDMSG-ERROR            
098100     END-IF                                                               
098200     .                                                                    
098300     EJECT                                                                
098400 CAB-HAMTA-IDGMTREF-I-WDE4 SECTION.                                       
098500     SKIP3                                                                
098600     MOVE WS-IDPRODNR-N                 TO W-601-IDPRODNR                 
098700                                           W-411-IDPRODNR-MIN             
098800                                           W-411-IDPRODNR-MAX             
098900                                           W-411-IDPRODNR                 
099000     MOVE 1                             TO W-411-IDPURAD-MIN              
099100                                           W-411-IDPURAD                  
099200     MOVE 99999                         TO W-411-IDPURAD-MAX              
099300                                                                          
099400     PERFORM IMS-GU-KUNDORDER-SEK-INV-GE                                  
099500     IF SEGMENT-FINNS                                                     
099600         MOVE KORD-IDDISTR              TO WS-IDDISTR                     
099700                                           WS-IDDISTR-N                   
099800         MOVE KORD-IDKUNDNR             TO WS-IDKUNDNR                    
099900                                           WS-IDKUNDNR-N                  
100000         MOVE KORD-IDKUNDRF             TO WS-IDKUNDRF                    
100100     ELSE                                                                 
100200         MOVE FEL                       TO WS-INDATA-TEST                 
100300         MOVE 'IDORDNR'                 TO  RESP-IDELMT-ERROR             
100400         MOVE '041'                     TO RESP-IDMSG-ERROR               
100500     END-IF                                                               
100600     .                                                                    
100700     EJECT                                                                
100800 CB-KONTROLLERA-KOLLI   SECTION.                                          
100900                                                                          
101000     MOVE WS-IDPRODNR-N                  TO   W-601-IDPRODNR              
101100     PERFORM IMS-GHU-KOLLIREG                                             
101200                                                                          
101300     IF SEGMENT-FINNS                                                     
101600        IF VORD-IDDC = REQU-IDDC-KEY                                      
101700           MOVE WS-IDKOLLI               TO  W-611-IDKOLLI                
101800           PERFORM IMS-GHU-KOLLI                                          
101900           IF SEGMENT-FINNS                                               
102000              IF KOLLI-IDKOLLI-SAMP > ZERO                                
102100              AND KOLLI-KDSTASKLI   > SPACE                               
102200*---------------------------------------------INGÅR KOLLIT I ETT          
102300*---------------------------------------------SAMLINGSKOLLI               
102400                MOVE FEL                    TO  WS-INDATA-TEST            
102500                MOVE FEL-CONSOLIDATED-CASE  TO  RESP-IDMSG-ERROR          
102600              ELSE                                                        
102700                IF KOLLI-KDKOLSTA       <  6                              
102800                  IF REQU-KDPGMACT = 'E'                                  
102900                    MOVE KOLLI-KVORDRAD     TO   WS-KVORDRAD              
103000                    MOVE KOLLI-KDKOLSTA     TO   WS-KDKOLSTA              
103100                    MOVE KOLLI-ADFLGEO      TO   WS-KOLLI-ADFLGEO         
103200                    MOVE KOLLI-ADFLOMR      TO   WS-KOLLI-ADFLOMR         
103300*      SPAR UNDAN ANTAL RADER MED KDFARLIG = +4, +7 FÖR ATT KUNNA         
103400*      SÄTTA KDFARLIG FÖR HELA KOLLIT. VID BORTTAG AV KOLLI-              
103500*      KOPPLING FÖR DESSA RADER RÄKNAR MAN NED KVFALRAD                   
103600                    MOVE KOLLI-KVFALRAD     TO   WS-KVFALRAD              
103700                  END-IF                                                  
103800                ELSE                                                      
103900*---------------------------------------------ÄR KOLLIT FAKTURERAT        
104000*---------------------------------------------EL. FAKTURA-RELEASAT        
104100                  MOVE FEL             TO   WS-INDATA-TEST                
104200                  MOVE FEL-XX-ALREADY-INV                                 
104300                                       TO   RESP-IDMSG-ERROR              
104400                  MOVE 'IDKOLLI'       TO   RESP-IDELMT-ERROR             
104500                END-IF                                                    
104600              END-IF                                                      
104700           ELSE                                                           
104800              MOVE FEL                    TO  WS-INDATA-TEST              
104900              MOVE FEL-CASE-ORDER-MISSING TO  RESP-IDMSG-ERROR            
105000           END-IF                                                         
105100        ELSE                                                              
105200           MOVE FEL                      TO   WS-INDATA-TEST              
105300           MOVE FEL-CASE-ORDER-MISSING   TO   RESP-IDMSG-ERROR            
105400        END-IF                                                            
105500     ELSE                                                                 
105600        MOVE FEL                         TO   WS-INDATA-TEST              
105700        MOVE FEL-CASE-ORDER-MISSING      TO   RESP-IDMSG-ERROR            
105800     END-IF                                                               
105900     .                                                                    
106000     EJECT                                                                
106100 CC-KONTROLLERA-PACKARE SECTION.                                          
106200                                                                          
106300     MOVE NEJ                           TO SW-SLUT-RADNR                  
106400     MOVE 1                             TO RAD-INX                        
106500     MOVE WS-IDPRODNR-N                 TO W-411-IDPRODNR                 
106600                                                                          
106700     PERFORM UNTIL (RAD-INX > MAX-ANTAL-RADER                             
106800                OR  WS-INDATA-FEL                                         
106900                OR  SW-SLUT-RADNR = JA)                                   
107000                                                                          
107100       MOVE REQU-IDRADNR (RAD-INX)       TO WS-IDRADNR                    
107200       INSPECT WS-IDRADNR REPLACING LEADING SPACE BY ZERO                 
107300                                                                          
107400       IF  WS-IDRADNR > ZERO                                              
107500                                                                          
107600         IF  REQU-FLBACKA-ALLA        = JA                                
107700         OR  REQU-FLBACKA-ALLA        = YES                               
107800         OR  REQU-FLBACKA (RAD-INX)   = JA                                
107900         OR  REQU-FLBACKA (RAD-INX)   = YES                               
108000           MOVE WS-IDRADNR               TO W-411-IDPURAD                 
108100           PERFORM IMS-GU-WDE411-01-BSEQ                                  
108200           IF SEGMENT-FINNS                                               
108300             PERFORM CCB-KONTROLLERA-PLOCKLISTA                           
108400           ELSE                                                           
108500             MOVE FEL               TO   WS-INDATA-TEST                   
108600             MOVE 'IDRADNR'         TO   RESP-IDELMT-ERROR                
108700             MOVE FEL-XX-NOT-FOUND  TO   RESP-IDMSG-ERROR                 
108800                                  RESP-IDMSG-ERROR-LINE (RAD-INX)         
108900           END-IF                                                         
109000         END-IF                                                           
109100       ELSE                                                               
109200         MOVE JA                         TO SW-SLUT-RADNR                 
109300       END-IF                                                             
109400                                                                          
109500       ADD +1                            TO RAD-INX                       
109600     END-PERFORM                                                          
109700     .                                                                    
109800     EJECT                                                                
109900 CCB-KONTROLLERA-PLOCKLISTA              SECTION.                         
110000                                                                          
110100     EVALUATE TRUE                                                        
110200*      WHEN KORD-IDUSER NOT = WS-IDUSER                                   
110300       WHEN KORD-IDUSER(4:5) NOT = WS-IDUSER(4:5)                         
110400*----------------------------------------------PACKARE STÄMMER EJ         
110500         MOVE FEL                   TO   WS-INDATA-TEST                   
110600         MOVE FEL-PACKER-ORDER-NOMATCH                                    
110700                                    TO RESP-IDMSG-ERROR                   
110800       WHEN KORD-KVORDRAD-PACK = KORD-KVORDRAD                            
110900*----------------------------------------------ÄR ANGIVEN PACKARES        
111000*----------------------------------------------ORDERDEL REDAN KLAR        
111100         MOVE FEL                   TO   WS-INDATA-TEST                   
111200         MOVE FEL-ORDERPART-READY   TO   RESP-IDMSG-ERROR                 
111300       WHEN KORD-KDPAKOLL NOT = ZERO                                      
111400*--------------------------------------------FÅR MAN EJ RÄTTA DÅ          
111500*--------------------------------------------AVVIK.KONTROLL PÅGÅR         
111600         MOVE FEL                   TO   WS-INDATA-TEST                   
111700         MOVE FEL-DEVIATION-CONTROL TO   RESP-IDMSG-ERROR                 
111800       WHEN OTHER                                                         
111900         CONTINUE                                                         
112000     END-EVALUATE                                                         
112100     .                                                                    
112200     EJECT                                                                
112300 D-LAGG-UT-RADER      SECTION.                                            
112400                                                                          
112500*    BILDSIDAN FYLLS MED INFO FRÅN ORDERRADER TILLHÖRANDE                 
112600*    AKTUELLT KOLLI.                                                      
112700*    AVBRYTS DÅ EN DISTR-KUND-ORDER'S ALLA PLOCKLISTOR ÄR                 
112800*    BEHANDLADE ELLER DÅ BILDSIDAN BLIR ÖVERFULL.                         
112900                                                                          
113000                                                                          
113100     MOVE +1                            TO   RAD-INX                      
113200     MOVE NEJ                           TO   WS-TRAEFF-KOLLI              
113300     MOVE NEJ                           TO   SW-SIDA-OVERFULL             
113400     MOVE WS-IDDISTR-N                  TO W-4A1-IDDISTR                  
113500     MOVE WS-IDKUNDNR-N                 TO W-4A1-IDKUNDNR                 
113600     MOVE WS-IDORDNR                    TO W-4A1-IDORDNR                  
113700     PERFORM IMS-GU-KUNDORDER-SEK                                         
113800     PERFORM UNTIL SW-SIDA-OVERFULL = JA                                  
113900                OR KUNDORDER-SEK-SAKNAS                                   
114000                                                                          
114100       MOVE KORD-IDDISTR             TO W-401-IDDISTR                     
114200       MOVE KORD-IDKUNDNR            TO W-401-IDKUNDNR                    
114300       MOVE KORD-IDORDNR5            TO W-401-IDORDNR                     
114400       MOVE KORD-IDPRODNR            TO W-401-IDPRODNR                    
114500       MOVE KORD-IDPLKLST            TO W-401-IDPLKLST                    
114600       PERFORM IMS-GU-KUNDORDER                                           
114700                                                                          
114800       PERFORM DB-KTRL-KORD-VISNING                                       
114900                                                                          
115000       IF SW-VISA-KORD = JA                                               
115100         PERFORM DC-BEH-GODK-PLKLST                                       
115200       END-IF                                                             
115300                                                                          
115400       IF  SW-SIDA-OVERFULL = NEJ                                         
115500         PERFORM IMS-GN-KUNDORDER-SEK                                     
115600       END-IF                                                             
115700                                                                          
115800     END-PERFORM                                                          
115900                                                                          
116000     IF WS-TRAEFF-KOLLI = NEJ                                             
116100      IF  REQU-KDPGMACT = 'E'                                             
116200      OR  REQU-KDPGMACT = 'S'                                             
116300*       * VID VISNING EFTER KORREKT UPPDATERING ÄR DET INGET FEL          
116400*       * ATT PACKAREN SAKNAR RADER I KOLLIT.                             
116500*       * FALLET UPPSTÅR DÅ NÅGON ANNAN PACKARE HAR RAD(ER)               
116600*       * I KOLLIT.                                                       
116700        CONTINUE                                                          
116800      ELSE                                                                
116900        EVALUATE TRUE                                                     
117000          WHEN SW-FEL-PRODNR = 'J'                                        
117100            MOVE FEL               TO   WS-BEHANDLING-TEST                
117200            MOVE '023'             TO RESP-IDMSG-ERROR                    
117300            MOVE 'IDPRODNR'        TO RESP-IDELMT-ERROR                   
117400          WHEN SW-FEL-PACKARE = 'J'                                       
117500            MOVE FEL               TO   WS-BEHANDLING-TEST                
117600            MOVE '023'             TO RESP-IDMSG-ERROR                    
117700            MOVE 'IDPICKER'        TO RESP-IDELMT-ERROR                   
117800          WHEN SW-ORDER-AVSLUTAD = 'J'                                    
117900            MOVE FEL               TO   WS-BEHANDLING-TEST                
118000            MOVE '146'             TO RESP-IDMSG-ERROR                    
118100          WHEN OTHER                                                      
118200            MOVE FEL               TO   WS-BEHANDLING-TEST                
118300            MOVE FEL-CASE-ORDER-MISSING                                   
118400                                   TO RESP-IDMSG-ERROR                    
118500          END-EVALUATE                                                    
118600      END-IF                                                              
118700     END-IF                                                               
118800     .                                                                    
118900     EJECT                                                                
119000 DB-KTRL-KORD-VISNING SECTION.                                            
119100                                                                          
119200*    KONTROLLERAR VILLKOR FÖR ATT GODKÄNNA VISNING AV EN KORD.            
119300                                                                          
119400                                                                          
119500     MOVE JA  TO SW-VISA-KORD                                             
119600                                                                          
119700     EVALUATE TRUE                                                        
119800                                                                          
119900       WHEN KORD-IDPRODNR NOT = WS-IDPRODNR-N                             
120000           MOVE JA      TO SW-FEL-PRODNR                                  
120100                                                                          
120200       WHEN KORD-IDUSER NOT = WS-IDUSER                                   
120300         MOVE JA        TO SW-FEL-PACKARE                                 
120400                                                                          
120410       WHEN CDC                                                           
120420       AND  KORD-KVORDRAD-PACK = KORD-KVORDRAD                            
120430         MOVE JA        TO SW-ORDER-AVSLUTAD                              
120440                                                                          
120450       WHEN SDC                                                           
120460       AND  KORD-KVORDRAD-PACK = KORD-KVORDRAD                            
120470         MOVE JA        TO SW-ORDER-AVSLUTAD                              
120480                                                                          
120500     END-EVALUATE                                                         
120600     .                                                                    
120700     EJECT                                                                
120800 DC-BEH-GODK-PLKLST   SECTION.                                            
120900                                                                          
121000*    BEHANDLA EN FÖR VISNING GODKÄND PLOCKLISTA.                          
121100*    ORAD:ER LÄSES TILLS NÅGON ORAD BEFINNES TILLHÖRA AKTUELLT            
121200*    KOLLI; DÅ ANROPAS DCA-.                                              
121300                                                                          
121400     IF  WS-IDRADNR-START = ZERO                                          
121500       PERFORM IMS-GNP-RAD-OKVAL                                          
121600     ELSE                                                                 
121700       MOVE WS-IDRADNR-START TO W-411-IDPURAD2                            
121800       PERFORM IMS-GNP-RAD-FOM                                            
121900     END-IF                                                               
122000                                                                          
122100     MOVE NEJ                TO SW-KKOLLI-FINNS                           
122200                                                                          
122300     PERFORM UNTIL (SEGMENT-SAKNAS                                        
122400                OR  SW-KKOLLI-FINNS  = JA                                 
122500                OR  SW-SIDA-OVERFULL = JA)                                
122600                                                                          
122700       IF  ORAD-KVLEVART > ZERO                                           
122800         MOVE ORAD-IDPURAD        TO WS-SPAR-IDPURAD                      
122900         MOVE WS-IDPRODNR-N       TO W-421-IDPRODNR                       
123000         MOVE WS-IDKOLLI          TO W-421-IDKOLLI                        
123100         PERFORM IMS-GNP-KKOLLI-KVAL                                      
123200                                                                          
123300         IF  SEGMENT-FINNS                                                
123400           MOVE JA                TO SW-KKOLLI-FINNS                      
123500         ELSE                                                             
123600           PERFORM IMS-GNP-RAD-OKVAL                                      
123700         END-IF                                                           
123800       ELSE                                                               
123900         PERFORM IMS-GNP-RAD-OKVAL                                        
124000       END-IF                                                             
124100     END-PERFORM                                                          
124200                                                                          
124300     IF  SW-KKOLLI-FINNS = JA                                             
124400       PERFORM DCA-BEH-KOLLI-PLKLST                                       
124500     END-IF                                                               
124600     .                                                                    
124700     EJECT                                                                
124800 DCA-BEH-KOLLI-PLKLST SECTION.                                            
124900                                                                          
125000*    FÖR AKTUELL PLOCKLISTA BEHANDLAS ALLA RADER TILLHÖRANDE              
125100*    ANGIVET KOLLI.                                                       
125200                                                                          
125300     MOVE JA                     TO WS-TRAEFF-KOLLI                       
125400                                                                          
125500     PERFORM IMS-WDE43-GU-ORAD-LAST                                       
125600                                                                          
125700     MOVE WS-IDPRODNR-N          TO W-601-IDPRODNR                        
125800                                    W-421-IDPRODNR                        
125900                                    W-IDPRODNR-F                          
126000     MOVE WS-IDKOLLI             TO W-611-IDKOLLI                         
126100                                    W-421-IDKOLLI                         
126200                                    W-IDKOLLI-F                           
126300                                                                          
126400     IF  REQU-IDRADNR-START-KEY NOT = ALL '+'                             
126500       MOVE WS-IDRADNR-START    TO  W-411-IDPURAD2                        
126600       PERFORM IMS-GN-WDE411-21-FSEQ-SOK                                  
126700     ELSE                                                                 
126800       PERFORM IMS-GN-WDE411-21-FSEQ                                      
126900     END-IF                                                               
127000                                                                          
127100     PERFORM UNTIL (SEGMENT-SAKNAS                                        
127200                OR  SW-SIDA-OVERFULL = JA)                                
127300                                                                          
127400       MOVE WDE44-ORAD-IDPURAD  TO ARB-RAD                                
127500                             WS-WDE44-ORAD-IDPURAD                        
127600       MOVE WDE44-KKOLLI-KVLEVART TO ARB-KVLEVART                         
127700                                                                          
127800       MOVE WDE44-KKOLLI-IDKOLLI  TO W-421-IDKOLLI                        
127900                                                                          
128000                                                                          
128100       PERFORM DCAA-STYR-BILDRADER                                        
128200                                                                          
128300       IF SW-SIDA-OVERFULL = NEJ                                          
128400         PERFORM IMS-GN-WDE411-21-FSEQ                                    
128500       END-IF                                                             
128600                                                                          
128700     END-PERFORM                                                          
128800     .                                                                    
128900     EJECT                                                                
129000 DCAA-STYR-BILDRADER SECTION.                                             
129100                                                                          
129200*    REDIGERAR EN RAD I MOD.                                              
129300*    OM SIDAN ÖVERFULL, SPARAS SISTA MOD-RADENS NYCKEL                    
129400*      SAMT MEDDELANDE LÄGGS UT.                                          
129500                                                                          
129600     IF  RAD-INX <= MAX-RAD-INX                                           
129700*      * AKTUELL RAD FÅR PLATS PÅ SIDAN                                   
129800       PERFORM S05-SKRIV-MOD-RAD                                          
129900       ADD +1 TO RAD-INX                                                  
130000     ELSE                                                                 
130100*      * SIDAN ÖVERFULL                                                   
130200       MOVE JA                        TO SW-SIDA-OVERFULL                 
130300       MOVE RESP-IDRADNR (MAX-RAD-INX) TO WS-IDRADNR-X                    
130400       INSPECT WS-IDRADNR-X REPLACING LEADING SPACE BY ZERO               
130500                                                                          
130600     END-IF                                                               
130700     .                                                                    
130800     EJECT                                                                
130900 E-BEH-ATERBOKNING    SECTION.                                            
131000     SKIP3                                                                
131100     IF REQU-IDRADNR    (RAD-INX) = ZERO                                  
131200         MOVE MAX-ANTAL-RADER-PLUS-1    TO   RAD-INX                      
131300     ELSE                                                                 
131400         INSPECT REQU-IDRADNR    (RAD-INX) REPLACING LEADING              
131500                     SPACE BY ZERO                                        
131600         MOVE REQU-IDRADNR    (RAD-INX) TO   ARB-RAD                      
131700         INSPECT REQU-KVLEVART (RAD-INX) REPLACING LEADING                
131800                     SPACE BY ZERO                                        
131900         MOVE REQU-KVLEVART (RAD-INX)   TO   ARB-KVLEVART                 
132000                                                                          
132100     SKIP2                                                                
132200         MOVE 'N'                       TO  WS-RADER-OK                   
132300         MOVE WS-IDDISTR-N              TO W-4A1-IDDISTR                  
132400         MOVE WS-IDKUNDNR-N             TO W-4A1-IDKUNDNR                 
132500         MOVE WS-IDORDNR                TO W-4A1-IDORDNR                  
132600         PERFORM IMS-GU-KUNDORDER-SEK                                     
132700         IF KUNDORDER-SEK-FINNS                                           
132800            PERFORM UNTIL KUNDORDER-SEK-SAKNAS OR                         
132900                          WS-RADER-OK = 'J'                               
133000            MOVE KORD-IDDISTR           TO W-401-IDDISTR                  
133100            MOVE KORD-IDKUNDNR          TO W-401-IDKUNDNR                 
133200            MOVE KORD-IDORDNR5          TO W-401-IDORDNR                  
133300            MOVE KORD-IDPRODNR          TO W-401-IDPRODNR                 
133400            MOVE KORD-IDPLKLST          TO W-401-IDPLKLST                 
133500            PERFORM IMS-GU-KUNDORDER                                      
133600            MOVE KORD-IDUSER            TO WS-JFR-IDANSTNR                
133700                                                                          
133800            MOVE KORD-IDORDER           TO WS-DNOT-IDORDER                
133900            MOVE KORD-IDDC              TO WS-DNOT-IDDC                   
134000                                                                          
134100            IF WS-IDPRODNR-N = KORD-IDPRODNR AND                          
134200               WS-IDANSTNR = WS-JFR-IDANSTNR-5                            
134300               MOVE ARB-RAD TO W-411-IDPURAD2                             
134400               PERFORM IMS-GNP-RAD                                        
134500               IF SEGMENT-FINNS                                           
134600                  MOVE 'J' TO WS-RADER-OK                                 
134700                                                                          
134800                  MOVE ORAD-IDARTNR     TO WS-DNOT-IDARTNR                
134900                  MOVE ARB-RAD          TO WS-DNOT-IDPURAD                
135000                                                                          
135100                                                                          
135200               END-IF                                                     
135300            END-IF                                                        
135400                                                                          
135500            PERFORM IMS-GN-KUNDORDER-SEK                                  
135600            END-PERFORM                                                   
135700         ELSE                                                             
135800            MOVE FEL             TO WS-BEHANDLING-TEST                    
135900            MOVE 'IDORDNR'       TO  RESP-IDELMT-ERROR                    
136000            MOVE '041'           TO RESP-IDMSG-ERROR                      
136100         END-IF                                                           
136200                                                                          
136300         IF WS-RADER-OK = 'N'                                             
136400            MOVE FEL             TO WS-BEHANDLING-TEST                    
136500            MOVE FEL-INTERVAL-PACKER                                      
136600                                 TO RESP-IDMSG-ERROR                      
136700         END-IF                                                           
136800                                                                          
136900         IF WS-BEHANDLING-RATT                                            
137000            PERFORM EC-BEHANDLA-ORDERRAD                                  
137100            IF WS-BEHANDLING-RATT                                         
137200               MOVE WS-IDDISTR-N TO TEST-IDDISTR                          
137300               IF NOT DIST19-SATS                                         
137400                  PERFORM ED-UPPDATERA-PRODTAB                            
137500               END-IF                                                     
137600            END-IF                                                        
137700         END-IF                                                           
137800     END-IF                                                               
137900     .                                                                    
138000     EJECT                                                                
138100 EC-BEHANDLA-ORDERRAD       SECTION.                                      
138200     SKIP3                                                                
138300     MOVE NEJ              TO FL-RADSTA-BACKAD                            
138400                                                                          
138500     MOVE ARB-RAD          TO W-411-IDPURAD                               
138600     MOVE WS-IDPRODNR-N    TO W-421-IDPRODNR                              
138700     MOVE WS-IDKOLLI       TO W-421-IDKOLLI  W-611-IDKOLLI                
138800     MOVE W-401-IDPLKLST   TO W-E4F-IDPLKST                               
138900     SKIP2                                                                
139000     PERFORM IMS-GHN-KOLLI-KOPPL-SEK                                      
139100     IF SEGMENT-FINNS                                                     
139200         SUBTRACT +1 FROM WS-KVORDRAD                                     
139300         IF WS-KVORDRAD > ZERO                                            
139400         OR WS-KDKOLSTA = ZERO                                            
139500         OR (WS-KOLLI-ADFLGEO NOT = 'RAC'                                 
139600         AND WS-KDKOLSTA < 2)                                             
139700         OR (WS-KOLLI-ADFLGEO = 'RAC'                                     
139800         AND WS-KOLLI-ADFLOMR = 999                                       
139900         AND WS-KDKOLSTA < 2)                                             
140000             MOVE KKOLLI-KVLEVART TO WS-KVLEVART                          
140100             MOVE REQU-IDDC-KEY   TO WS-IDDC                              
140200             IF NDC-NA                                                    
140300                MOVE KKOLLI-IDKOLLI  TO WS-DNOT-IDKOLLI                   
140400                PERFORM S12-DATA-TILL-DEL-NOTE                            
140500             END-IF                                                       
140600             PERFORM IMS-DLET-KOLLI-KOPPL                                 
140700**  BORTTAG AV LAASNINGS SEGMENT(WDR4)                                    
140800             PERFORM IMS-WDE611-GU-KOLLI                                  
140900             MOVE W-601-IDPRODNR TO W-IDPRODNR-WDE4F-MAX                  
141000             MOVE W-611-IDKOLLI TO W-IDKOLLI-WDE4F-MAX                    
141100             MOVE W-601-IDPRODNR TO W-IDPRODNR-WDE4F-MIN                  
141200             MOVE W-611-IDKOLLI TO W-IDKOLLI-WDE4F-MIN                    
141300             PERFORM IMS-GET-WDE4F                                        
141400                                                                          
141500             IF SEGMENT-SAKNAS                                            
141600                MOVE WS-IDPRODNR   TO W-4301-IDPRODNR                     
141700                PERFORM IMS-GHU-XXDU01                                    
141800                                                                          
141900                IF SEGMENT-FINNS                                          
142000                   MOVE WS-IDKOLLI       TO W-4302-IDKOLLI                
142100                   MOVE W-401-IDPLKLST   TO W-4302-IDPLKLST               
142200                                                                          
142300                   PERFORM IMS-GHNP-XXDU11                                
142400                   IF SEGMENT-FINNS                                       
142500                     PERFORM IMS-DLET-XXDU                                
142600                                                                          
142700                                                                          
142800                     PERFORM IMS-GNP-XXDU11-FIRST                         
142900                     IF SEGMENT-SAKNAS                                    
143000                        PERFORM IMS-GHU-XXDU01                            
143100                        PERFORM IMS-DLET-XXDU                             
143200                     END-IF                                               
143300                   END-IF                                                 
143400                END-IF                                                    
143500             END-IF                                                       
143600                                                                          
143700         ELSE                                                             
143800             MOVE FEL            TO WS-BEHANDLING-TEST                    
143900             MOVE FEL-MAY-NOT-BE-EMPTY                                    
144000                                 TO RESP-IDMSG-ERROR                      
144100             MOVE 'IDKOLLI'      TO RESP-IDELMT-ERROR                     
144200             MOVE +1             TO WS-KVORDRAD                           
144300         END-IF                                                           
144400     ELSE                                                                 
144500         MOVE FEL                TO WS-BEHANDLING-TEST                    
144600         MOVE FEL-NO-UPDATE      TO RESP-IDMSG-INFO                       
144700     END-IF                                                               
144800                                                                          
144900     IF  WS-BEHANDLING-RATT                                               
145000       PERFORM IMS-GHU-RAD-SEK                                            
145100       PERFORM ECB-KTRL-MID-MOT-DB                                        
145200       IF  WS-BEHANDLING-RATT                                             
145300         IF ORAD-KDFARLIG = +4 OR ORAD-KDFARLIG = +7                      
145400             SUBTRACT +1 FROM WS-KVFALRAD                                 
145500         END-IF                                                           
145600                                                                          
145700         COMPUTE ORAD-KVLEVART                                            
145800                          = ORAD-KVLEVART - WS-KVLEVART                   
145900                                                                          
146000         IF ORAD-KDRADSTA = 4 OR 5                                        
146100            MOVE +3 TO ORAD-KDRADSTA                                      
146200            ADD +1  TO WS-TOT-ANT-RADER                                   
146300            MOVE JA TO FL-RADSTA-BACKAD                                   
146400         END-IF                                                           
146500                                                                          
146600         PERFORM ECA-SPARA-RAD-INFO                                       
146700                                                                          
146800         IF  ORAD-KVLEVART = ZERO                                         
146900             MOVE JA  TO ARB-RAD-HELT-ORAPP                               
147000         ELSE                                                             
147100             MOVE NEJ TO ARB-RAD-HELT-ORAPP                               
147200         END-IF                                                           
147300                                                                          
147400         IF KORD-KDORDKL = +0                                             
147500            PERFORM ECC-BACKA-VOR-TIKLAR                                  
147600         END-IF                                                           
147700                                                                          
147800         PERFORM IMS-REPL-RAD                                             
147900       END-IF                                                             
148000     END-IF                                                               
148100     .                                                                    
148200     EJECT                                                                
148300 ECA-SPARA-RAD-INFO        SECTION.                                       
148400     MOVE ORAD-VKARTNTO         TO  SPAR-PRAD-VKARTNTO                    
148500     MOVE ORAD-KVFLAMP          TO  SPAR-PRAD-KVFLAMP                     
148600     MOVE ORAD-KDFARLIG         TO  SPAR-PRAD-KDFARLIG                    
148700     MOVE ORAD-PRARTNTO-LOC     TO SPAR-PRAD-PRARTNTO-LOC                 
148800     MOVE ORAD-PRARTNTO-LOCPREL TO SPAR-PRAD-PRARTNTO-LOCPREL             
148900     MOVE ORAD-PRARTNTO         TO SPAR-PRAD-PRARTNTO                     
149000                                                                          
149100     IF ORAD-IDPSN > ZERO                                                 
149200       MOVE ORAD-IDPSN          TO  SPAR-ORAD-IDPSN                       
149300       MOVE ORAD-VKART-FG       TO  SPAR-ORAD-VKART-FG                    
149400       MOVE ORAD-VLFG           TO  SPAR-ORAD-VLFG                        
149500       MOVE ORAD-SUEQFG         TO  SPAR-ORAD-SUEQFG                      
149600     END-IF                                                               
149700                                                                          
149800     SKIP2                                                                
149900     IF ARB-KVLEVART            >   ZERO                                  
150000         MOVE ARB-KVLEVART      TO  SPAR-PRAD-KVLEVART                    
150100     ELSE                                                                 
150200         MOVE ORAD-KVAVBART     TO  SPAR-PRAD-KVLEVART                    
150300     END-IF                                                               
150400     SKIP2                                                                
150500     PERFORM S10-UPPD-SPAR-KOLLI                                          
150600     .                                                                    
150700 ECB-KTRL-MID-MOT-DB SECTION.                                             
150800                                                                          
150900*    KKOLLI KONTROLLERAS MOT MID ELLER ORAD.                              
151000*    OÖVERENSSTÄMMELSE (PGA DATA ÄNDRAD EFTER VISNING) MEDDELAS.          
151100                                                                          
151200     IF  ARB-KVLEVART > ZERO                                              
151300*      * LEV. I MID (ARB-) SKALL VARA = KKOLLI (WS-)                      
151400       IF  ARB-KVLEVART NOT = WS-KVLEVART                                 
151500         MOVE FEL                TO WS-BEHANDLING-TEST                    
151600         MOVE FEL-NO-UPDATE      TO RESP-IDMSG-INFO                       
151700       END-IF                                                             
151800     ELSE                                                                 
151900*      * LEV. I KKOLLI (WS-) SKALL VARA = ORAD'S AVBOKAT                  
152000       IF  WS-KVLEVART NOT = ORAD-KVAVBART                                
152100         MOVE FEL                TO WS-BEHANDLING-TEST                    
152200         MOVE FEL-NO-UPDATE      TO RESP-IDMSG-INFO                       
152300       END-IF                                                             
152400     END-IF                                                               
152500     .                                                                    
152600     EJECT                                                                
152700 ECC-BACKA-VOR-TIKLAR      SECTION.                                       
152800     .                                                                    
152900                                                                          
153000     MOVE LOW-VALUE              TO W-WDA601KY-MIN-X.                     
153100     MOVE HIGH-VALUE             TO W-WDA601KY-MAX-X.                     
153200     MOVE KORD-IDDISTR           TO W-A601KY-MIN-IDDISTR                  
153300                                    W-A601KY-MAX-IDDISTR                  
153400     MOVE KORD-IDKUNDNR          TO W-A601KY-MIN-IDKUNDNR                 
153500                                    W-A601KY-MAX-IDKUNDNR                 
153600     MOVE KORD-IDORDNR5          TO W-A601KY-MIN-IDORDNR                  
153700                                    W-A601KY-MAX-IDORDNR                  
153800                                                                          
153900     MOVE NEJ                    TO SW-TIKLAR-JUSTERAD                    
154000                                                                          
154100     PERFORM IMS-GHN-WDA6B                                                
154200     PERFORM UNTIL SEGMENT-SAKNAS                                         
154300                OR BASEN-SLUT                                             
154400                OR SW-TIKLAR-JUSTERAD = JA                                
154500                                                                          
154600         IF  VOR-IDARTNR = ORAD-IDARTNR                                   
154700         AND VOR-TIKLAR > +0                                              
154800                                                                          
154900             MOVE ZERO           TO VOR-TIKLAR                            
155000                                    VOR-TIKLATID                          
155100             PERFORM IMS-REPL-WDA6B                                       
155200             MOVE JA             TO SW-TIKLAR-JUSTERAD                    
155300         END-IF                                                           
155400                                                                          
155500         PERFORM IMS-GHN-WDA6B                                            
155600     END-PERFORM                                                          
155700     .                                                                    
155800                                                                          
155900 ED-UPPDATERA-PRODTAB      SECTION.                                       
156000     SKIP3                                                                
156100     IF  FL-RADSTA-BACKAD = JA                                            
156200*      * RAD-INTERVALLET HAR FÄRDIGPACKADE ORADER                         
156300                                                                          
156400       MOVE WS-IDPRODNR-N        TO W-411-IDPRODNR                        
156500       MOVE ARB-RAD              TO W-411-IDPURAD                         
156600       PERFORM IMS-GU-WDE42-KORD-BSEQ                                     
156700                                                                          
156800       PERFORM EDA-LAES-SHIFTTAB                                          
156900                                                                          
157000       MOVE KORD-IDORDER         TO W-301-IDORDER                         
157100       MOVE KORD-IDDC            TO W-301-IDDC                            
157200       MOVE KORD-IDPRODNR        TO W-301-IDPRODNR                        
157300       MOVE KORD-IDPLKLST        TO W-301-IDPLKLST                        
157400       PERFORM IMS-GU-ORQA01                                              
157500                                                                          
157600*        PRODTAB UPPDATERAS FÖR ALLA PRODKL, FOM MARS 2013.               
157700       IF  ODEL-KDPRODKL = 'B'                                            
157800       OR  ODEL-KDPRODKL = 'C'                                            
157900*        * PRODTAB UPPDATERAS ENDAST FÖR PRODKL B OCH C.                  
158000                                                                          
158100         MOVE KORD-IDDC          TO W-IDDC-4471                           
158200         MOVE ODEL-IDPRCBAS      TO W-IDPRCBAS-4471                       
158300         MOVE ODEL-IDPRCVAR      TO W-IDPRCVAR-4471                       
158400         PERFORM IMS-GHU-XXKW11                                           
158500                                                                          
158600         IF  SEGMENT-FINNS                                                
158700*          * PRODTAB UPPDATERAS ENDAST OM ORDERDELENS PRC FINNS.          
158800                                                                          
158900           MOVE 1                TO IND1                                  
159000           MOVE W-IDSHIFT-4478   TO IND2                                  
159100           MOVE ODEL-DARFS       TO HJALP-ODEL-DARFS                      
159200           MOVE 4472-TIRFS (IND1) TO HJALP-4472-TIRFS                     
159300                                                                          
159400           PERFORM UNTIL IND1 = 30 OR                                     
159500                         4472-TIRFS (IND1) = ZERO OR                      
159600                         HJALP-ODEL-DARFS-6 = HJALP-4472-TIRFS-6          
159700             ADD 1                TO IND1                                 
159800             MOVE 4472-TIRFS (IND1) TO HJALP-4472-TIRFS                   
159900           END-PERFORM                                                    
160000                                                                          
160100           MOVE ODEL-DARFS (3:10) TO 4472-TIRFS (IND1)                    
160200           MOVE W-IDSHIFT-4478  TO 4472-IDSHIFT (IND1, IND2)              
160300           SUBTRACT 1         FROM 4472-KVRADER-PRAPP (IND1, IND2)        
160400           PERFORM EDB-SUBTR-TOTAL-PRODTID                                
160500           PERFORM IMS-REPL-XXKW11                                        
160600         END-IF                                                           
160700       END-IF                                                             
160800     END-IF                                                               
160900     .                                                                    
161000     EJECT                                                                
161100 EDA-LAES-SHIFTTAB         SECTION.                                       
161200                                                                          
161300     MOVE KORD-IDDC         TO W-IDDC-4477                                
161400     MOVE '1'               TO W-IDSHIFT-4478                             
161500     MOVE KORD-IDUSER       TO W-IDUSER-4478                              
161600     PERFORM IMS-GU-XXLB                                                  
161700                                                                          
161800     IF SEGMENT-SAKNAS                                                    
161900        MOVE '2'            TO W-IDSHIFT-4478                             
162000        PERFORM IMS-GU-XXLB                                               
162100                                                                          
162200        IF SEGMENT-SAKNAS                                                 
162300           MOVE '3'         TO W-IDSHIFT-4478                             
162400           PERFORM IMS-GU-XXLB                                            
162500                                                                          
162600           IF SEGMENT-SAKNAS                                              
162700              MOVE '1'      TO W-IDSHIFT-4478                             
162800           END-IF                                                         
162900        END-IF                                                            
163000     END-IF                                                               
163100     .                                                                    
163200     EJECT                                                                
163300 EDB-SUBTR-TOTAL-PRODTID             SECTION.                             
163400                                                                          
163500     MOVE ODEL-KVPTID                     TO WS-KVPTID-MIN                
163600                                                                          
163700     MOVE 4472-SUPTID-PRAPP (IND1, IND2)  TO WS-SUPTID-PRAPP              
163800                                                                          
163900     COMPUTE WS-SUPTID-PRAPP-MIN-TOT = WS-SUPTID-TIM * 60                 
164000                                                                          
164100     ADD WS-SUPTID-MIN        TO WS-SUPTID-PRAPP-MIN-TOT                  
164200     SUBTRACT WS-KVPTID-MIN FROM WS-SUPTID-PRAPP-MIN-TOT                  
164300                                                                          
164400     IF WS-SUPTID-PRAPP-MIN-TOT NEGATIVE                                  
164500        MOVE 0 TO WS-SUPTID-PRAPP-MIN-TOT                                 
164600     END-IF                                                               
164700                                                                          
164800     DIVIDE WS-SUPTID-PRAPP-MIN-TOT BY 60 GIVING WS-SUPTID-TIM            
164900     COMPUTE WS-SUPTID-PRAPP-MIN-TOT = WS-SUPTID-PRAPP-MIN-TOT -          
165000                                      (WS-SUPTID-TIM * 60)                
165100                                                                          
165200     MOVE WS-SUPTID-PRAPP-MIN-TOT TO WS-SUPTID-MIN                        
165300                                                                          
165400     MOVE WS-SUPTID-PRAPP TO 4472-SUPTID-PRAPP (IND1, IND2)               
165500     .                                                                    
165600     EJECT                                                                
165700 F-UPPDATERA-KOLLIREG      SECTION.                                       
165800     MOVE WS-IDKOLLI   TO  W-611-IDKOLLI                                  
165900     PERFORM IMS-GHU-KOLLI                                                
166000                                                                          
166100     IF SEGMENT-FINNS                                                     
166200                                                                          
166300         PERFORM S11-UPPD-KOLLI-FRAN-ARB                                  
166400         MOVE WS-KVORDRAD         TO   KOLLI-KVORDRAD                     
166500                                                                          
166600         IF  KOLLI-KVORDRAD = ZERO                                        
166700             IF  KOLLI-KDKOLSTA   = +1                                    
166900                 MOVE JA          TO KOLLI-KDKOLSTA-1-BORT                
167000                 MOVE KOLLI-VKORDBTO-KOLLI                                
167100                                  TO WS-KOLLI-VKORDBTO-KOLLI              
167200                 MOVE KOLLI-VLORDBTO-KOLLI                                
167300                                  TO WS-KOLLI-VLORDBTO-KOLLI              
167400             END-IF                                                       
167500             ADD +1               TO   WS-KVKOLLI-BORT                    
167600             MOVE JA              TO   KOLLI-BORTTAGET                    
167700             MOVE '035'           TO   RESP-IDMSG-INFO                    
167800             MOVE 'IDKOLLI'       TO   RESP-IDELMT-ERROR                  
167900             PERFORM IMS-DLET-KOLLI                                       
168000             PERFORM FA-TA-BORT-KOLLI-FOR-EMA                             
168100             PERFORM FB-TA-BORT-LAS-SEGM                                  
168200         ELSE                                                             
168300             MOVE INFO-UPDATE-OK  TO   RESP-IDMSG-INFO                    
168400             PERFORM IMS-REPL-KOLLI                                       
168500         END-IF                                                           
168600     END-IF                                                               
168700                                                                          
168800     PERFORM IMS-GHU-KOLLIREG                                             
168900     IF SEGMENT-FINNS                                                     
169000         COMPUTE VORD-KVORDRAD-PACK                                       
169100                         = VORD-KVORDRAD-PACK - WS-TOT-ANT-RADER          
169200         IF VORD-KVORDRAD-PACK = ZERO                                     
169300             MOVE 1        TO  VORD-KDORDSTA                              
169400         END-IF                                                           
169500                                                                          
169600         IF VORD-KVKOLPAC = ZERO                                          
169700           CONTINUE                                                       
169800         ELSE                                                             
169900           SUBTRACT WS-KVKOLLI-BORT FROM VORD-KVKOLPAC                    
170000         END-IF                                                           
170100                                                                          
170200         IF WS-KDKOLSTA > 0                                               
170400            COMPUTE VORD-SUORDV-PACK-LOC =                                
170500                  VORD-SUORDV-PACK-LOC - ARB-KOLLI-SUORDV-LOC             
170600            IF VORD-SUORDV-PACK-LOC  < ZERO                               
170700              MOVE ZERO         TO VORD-SUORDV-PACK-LOC                   
170800            END-IF                                                        
170900                                                                          
171000            COMPUTE VORD-SUORDV-PACK-LOCPREL =                            
171100                   VORD-SUORDV-PACK-LOCPREL -                             
171200                                     ARB-KOLLI-SUORDV-LOCPREL             
171300            IF VORD-SUORDV-PACK-LOCPREL < ZERO                            
171400              MOVE ZERO         TO VORD-SUORDV-PACK-LOCPREL               
171500            END-IF                                                        
171600                                                                          
171700            COMPUTE VORD-SUORDV-PACK =                                    
171800                 VORD-SUORDV-PACK - ARB-KOLLI-SUORDV                      
171900            IF VORD-SUORDV-PACK-LOCPREL < ZERO                            
172000              MOVE ZERO         TO VORD-SUORDV-PACK                       
172100            END-IF                                                        
172110*LK                                                                       
172140            SUBTRACT ARB-KOLLI-VKORDNTO FROM VORD-VKORDNTO                
172150            SUBTRACT ARB-KOLLI-VKORDBTO FROM VORD-VKORDBTO                
172160*LK                                                                       
172200         END-IF                                                           
172300                                                                          
172400                                                                          
172500         IF KOLLI-KDKOLSTA-1-BORT = JA                                    
172700            SUBTRACT WS-KOLLI-VKORDBTO-KOLLI                              
172800                                FROM VORD-VKORDBTO                        
172900            IF VORD-VKORDBTO < ZERO                                       
173000              MOVE ZERO         TO VORD-VKORDBTO                          
173100            END-IF                                                        
173200            SUBTRACT WS-KOLLI-VLORDBTO-KOLLI                              
173300                                FROM VORD-VLORDBTO                        
173400            IF VORD-VLORDBTO < ZERO                                       
173500              MOVE ZERO         TO VORD-VLORDBTO                          
173600            END-IF                                                        
173700            SUBTRACT 1          FROM VORD-KVKOLLI                         
173800         END-IF                                                           
173900                                                                          
174000         PERFORM IMS-REPL-KOLLIREG                                        
174100     ELSE                                                                 
174200         MOVE FEL          TO  WS-BEHANDLING-TEST                         
174300     END-IF                                                               
174400     .                                                                    
174500     EJECT                                                                
174600 FA-TA-BORT-KOLLI-FOR-EMA SECTION.                                        
174700     PERFORM IMS-GHN-XXJK11                                               
174800                                                                          
174900     PERFORM UNTIL SEGMENT-SAKNAS                                         
175000       MOVE WS-IDKOLLI  TO WS-IDKOLLI-NUM                                 
175100                                                                          
175200       IF 4322-IDPRODNR = WS-IDPRODNR-N     AND                           
175300          4322-IDKOLLI  = WS-IDKOLLI-NUM                                  
175400         PERFORM IMS-DLET-XXJK                                            
175500       END-IF                                                             
175600                                                                          
175700       PERFORM IMS-GHN-XXJK11                                             
175800     END-PERFORM                                                          
175900     .                                                                    
176000     SKIP2                                                                
176100 FB-TA-BORT-LAS-SEGM  SECTION.                                            
176200                                                                          
176300     MOVE WS-IDPRODNR   TO W-4301-IDPRODNR                                
176400     PERFORM IMS-GHU-XXDU01                                               
176500     IF SEGMENT-FINNS                                                     
176600        MOVE WS-IDKOLLI  TO W-4302-IDKOLLI                                
176700                                                                          
176800        PERFORM IMS-GHNP-XXDU11-IDKOLLI                                   
176900        PERFORM UNTIL SEGMENT-SAKNAS                                      
177000           PERFORM IMS-DLET-XXDU                                          
177100           PERFORM IMS-GHNP-XXDU11-IDKOLLI                                
177200        END-PERFORM                                                       
177300                                                                          
177400        PERFORM IMS-GNP-XXDU11-FIRST                                      
177500        IF SEGMENT-SAKNAS                                                 
177600           PERFORM IMS-GHU-XXDU01                                         
177700           PERFORM IMS-DLET-XXDU                                          
177800        END-IF                                                            
177900     END-IF                                                               
178000     .                                                                    
178100     EJECT                                                                
178200 H-AVSLUT             SECTION.                                            
178300     SKIP3                                                                
178400     IF WS-BEHANDLING-RATT                                                
178500        CONTINUE                                                          
178600     ELSE                                                                 
178700        PERFORM IMS-ROLLBACK                                              
178800        IF REQU-KDPGMACT = 'E'                                            
178900         MOVE '006'            TO RESP-IDMSG-INFO                         
179000        END-IF                                                            
179100     END-IF                                                               
179200     .                                                                    
179300     EJECT                                                                
179400 S05-SKRIV-MOD-RAD    SECTION.                                            
179500                                                                          
179600     MOVE ARB-RAD               TO  RESP-IDRADNR    (RAD-INX)             
179700     INSPECT RESP-IDRADNR    (RAD-INX) REPLACING LEADING ZERO             
179800                                       BY SPACE                           
179900     IF ARB-KVLEVART = ZERO                                               
180000         MOVE ZERO              TO  RESP-KVLEVART (RAD-INX)               
180100     ELSE                                                                 
180200         MOVE ARB-KVLEVART      TO  RESP-KVLEVART (RAD-INX)               
180300     END-IF                                                               
180400     INSPECT RESP-KVLEVART (RAD-INX) REPLACING LEADING ZERO               
180500                                       BY SPACE                           
180600     MOVE SPACE                 TO  RESP-FLBACKA     (RAD-INX)            
180700     ADD +1                     TO  WS-COUNT                              
180800                                                                          
180900     IF  WS-COUNT   <  501                                                
181000       CONTINUE                                                           
181100     ELSE                                                                 
181200       MOVE FEL-TOO-MANY-LINES TO RESP-IDMSG-ERROR                        
181300     END-IF                                                               
181400                                                                          
181500     MOVE WS-COUNT       TO  RESP-KVRADER                                 
181600     .                                                                    
181700     EJECT                                                                
181800 S10-UPPD-SPAR-KOLLI    SECTION.                                          
181900                                                                          
182000     COMPUTE ARB-KOLLI-VKORDNTO ROUNDED = ARB-KOLLI-VKORDNTO +            
182100                    SPAR-PRAD-VKARTNTO * SPAR-PRAD-KVLEVART               
182200                                                                          
182300     COMPUTE ARB-KOLLI-VKORDBTO ROUNDED = ARB-KOLLI-VKORDBTO +            
182400                    SPAR-PRAD-VKARTNTO * SPAR-PRAD-KVLEVART               
182500                                                                          
182600     MOVE  WS-IDDISTR-N      TO TEST-IDDISTR                              
182700                                                                          
182800     IF DIST79-DEALER-PRICE                                               
182900      IF  ORAD-PRARTNTO-LOCPREL > 0                                       
183000       COMPUTE ARB-KOLLI-SUORDV-LOCPREL = ARB-KOLLI-SUORDV-LOCPREL        
183100           + SPAR-PRAD-PRARTNTO-LOCPREL * SPAR-PRAD-KVLEVART              
183200      ELSE                                                                
183300       COMPUTE ARB-KOLLI-SUORDV-LOC = ARB-KOLLI-SUORDV-LOC                
183400           + SPAR-PRAD-PRARTNTO-LOC * SPAR-PRAD-KVLEVART                  
183500      END-IF                                                              
183600     ELSE                                                                 
183800       IF DIST79-ECOM-PRICE                                               
183900         COMPUTE ARB-KOLLI-SUORDV-LOC = ARB-KOLLI-SUORDV-LOC              
184000             + SPAR-PRAD-PRARTNTO-LOC * SPAR-PRAD-KVLEVART                
184100       ELSE                                                               
184200         COMPUTE ARB-KOLLI-SUORDV = ARB-KOLLI-SUORDV +                    
184300             SPAR-PRAD-PRARTNTO * SPAR-PRAD-KVLEVART                      
184400       END-IF                                                             
184500     END-IF                                                               
184600                                                                          
184700     IF SPAR-ORAD-IDPSN > ZERO                                            
184800       MOVE +1 TO TAB-INDX                                                
184900                                                                          
185000       PERFORM UNTIL TAB-INDX > MAX-FG-INDX                               
185100         IF TAB-IDPSN(TAB-INDX) = 0                                       
185200           MOVE SPAR-ORAD-IDPSN TO TAB-IDPSN(TAB-INDX)                    
185300           PERFORM S10A-BERAEKNA-FG-DATA                                  
185400           MOVE +10 TO TAB-INDX                                           
185500                                                                          
185600         ELSE                                                             
185700           IF SPAR-ORAD-IDPSN = TAB-IDPSN(TAB-INDX)                       
185800             PERFORM S10A-BERAEKNA-FG-DATA                                
185900             MOVE +10 TO TAB-INDX                                         
186000           END-IF                                                         
186100         END-IF                                                           
186200                                                                          
186300         ADD +1 TO TAB-INDX                                               
186400       END-PERFORM                                                        
186500     END-IF                                                               
186600     .                                                                    
186700     EJECT                                                                
186800 S10A-BERAEKNA-FG-DATA SECTION.                                           
186900     SKIP3                                                                
187000     COMPUTE TAB-VLFG(TAB-INDX) = TAB-VLFG(TAB-INDX)   +                  
187100                                  (SPAR-ORAD-VLFG      *                  
187200                                   SPAR-PRAD-KVLEVART)                    
187300     IF SPAR-ORAD-IDPSN = 10 OR 11                                        
187400       COMPUTE TAB-VKART-FG(TAB-INDX) = TAB-VKART-FG(TAB-INDX) +          
187500                                        (SPAR-ORAD-VKART-FG    *          
187600                                         SPAR-PRAD-KVLEVART)              
187700     ELSE                                                                 
187800       MOVE ZERO TO TAB-VKART-FG(TAB-INDX)                                
187900     END-IF                                                               
188000                                                                          
188100     COMPUTE TOTAL-SUEQFG = TOTAL-SUEQFG        +                         
188200                            (SPAR-ORAD-SUEQFG   *                         
188300                             SPAR-PRAD-KVLEVART)                          
188400     .                                                                    
188500     EJECT                                                                
188600 S11-UPPD-KOLLI-FRAN-ARB   SECTION.                                       
188700     SKIP3                                                                
188800     SUBTRACT ARB-KOLLI-KVORDRAD FROM KOLLI-KVORDRAD                      
188900     SUBTRACT ARB-KOLLI-VKORDNTO FROM KOLLI-VKORDNTO-KOLLI                
189000     IF WS-KVORDRAD > 0                                                   
189100       SUBTRACT ARB-KOLLI-VKORDBTO FROM KOLLI-VKORDBTO-KOLLI              
189200     END-IF                                                               
189300                                                                          
189400     SUBTRACT ARB-KOLLI-SUORDV-LOC FROM KOLLI-SUORDV-LOC                  
189500     SUBTRACT ARB-KOLLI-SUORDV-LOCPREL                                    
189600                                   FROM KOLLI-SUORDV-LOCPREL              
189700     SUBTRACT ARB-KOLLI-SUORDV     FROM KOLLI-SUORDV-KOLLI                
189800                                                                          
189900     IF WS-KVFALRAD = ZERO                                                
190000         MOVE ZERO               TO KOLLI-KDFARLIG-KOLLI                  
190100     END-IF                                                               
190200     MOVE WS-KVFALRAD            TO KOLLI-KVFALRAD                        
190300                                                                          
190400     MOVE +1 TO TAB-INDX                                                  
190500     PERFORM UNTIL TAB-INDX > MAX-FG-INDX                                 
190600       IF TAB-IDPSN(TAB-INDX) > ZERO                                      
190700         MOVE +1 TO FG-INDX                                               
190800                                                                          
190900         PERFORM UNTIL FG-INDX > MAX-FG-INDX                              
191000           IF TAB-IDPSN(TAB-INDX) = KOLLI-IDPSN(FG-INDX)                  
191100             SUBTRACT TAB-VLFG(TAB-INDX) FROM                             
191200                      KOLLI-VLFG(FG-INDX)                                 
191300                                                                          
191400             IF KOLLI-IDPSN(FG-INDX) = 10                                 
191500               SUBTRACT TAB-VKART-FG(TAB-INDX) FROM                       
191600                        KOLLI-VKART-FG(FG-INDX)                           
191700             END-IF                                                       
191800             IF KOLLI-VLFG(FG-INDX) = ZERO                                
191900               MOVE ZERO TO KOLLI-IDPSN(FG-INDX)                          
192000             END-IF                                                       
192100                                                                          
192200             MOVE +10 TO FG-INDX                                          
192300           END-IF                                                         
192400           ADD +1 TO FG-INDX                                              
192500         END-PERFORM                                                      
192600       END-IF                                                             
192700       ADD +1 TO TAB-INDX                                                 
192800     END-PERFORM                                                          
192900                                                                          
193000     IF TOTAL-SUEQFG > ZERO                                               
193100       SUBTRACT TOTAL-SUEQFG FROM KOLLI-SUEQFG                            
193200     END-IF                                                               
193300     .                                                                    
193400     EJECT                                                                
193500                                                                          
193600                                                                          
193700 S12-DATA-TILL-DEL-NOTE SECTION.                                          
193800                                                                          
193900     MOVE WS-IDDISTR-N               TO TEST-IDDISTR                      
194000     IF DIST07-USA-RETAILER-DNOTE                                         
194100     OR DIST07-CAN-RETAILER                                               
194200        INITIALIZE DNOT-ORDER-INFO                                        
194300                                                                          
194400        MOVE IDPGM                    TO DNOT-IDPGM                       
194500        MOVE WS-DNOT-IDORDER          TO DNOT-IDORDER                     
194600        MOVE WS-DNOT-IDARTNR          TO DNOT-IDARTNR                     
194700        MOVE WS-DNOT-IDDC             TO DNOT-IDDC                        
194800        MOVE WS-DNOT-IDKOLLI          TO DNOT-IDKOLLI-BORT                
194900        MOVE WS-DNOT-IDPURAD          TO DNOT-IDPURAD                     
195000                                                                          
195100        CALL W411DNOT USING DNOT-W411DNOT                                 
195200                            DNOT-ORQP-PCB                                 
195300                            DNOT-ORQP2-PCB                                
195400                            DNOT-ORQP3-PCB                                
195500                            DNOT-4013-PCB                                 
195600                            DNOT-BENA-PCB                                 
195700     END-IF                                                               
195800     .                                                                    
195900     EJECT                                                                
196000                                                                          
196100*    --- DISPATCHER SECTIONS                                              
196200 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
196300                                                                          
196400     MOVE 'GETARG'               TO SUB-KDFUNC                            
196500     MOVE 'CARPARTS.LDC.CASEADJ'            TO SUB-ADDISPABS              
196600     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
196700                                                                          
196800     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
196900                                                                          
197000     IF SUB-KDRC > 0                                                      
197100       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
197200       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
197300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
197400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
197500     END-IF                                                               
197600     .                                                                    
197700     SKIP3                                                                
197800 S02-RETURN-RESPONSE SECTION.                                             
197900                                                                          
198000     MOVE 'RETURN'                   TO SUB-KDFUNC                        
198100     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
198200                                                                          
198300     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
198400                                                                          
198500     IF SUB-KDRC > 0                                                      
198600       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
198700       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
198800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
198900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
199000     END-IF                                                               
199100     .                                                                    
199200     EJECT                                                                
199210 S03-MSG-CONV SECTION.                                                    
199220     MOVE SPACES                  TO RESP-MESSAGES (1)                    
199230                                     RESP-MESSAGES (2)                    
199240     MOVE 1                       TO MSG-IX                               
199250*    REQUEST OK                                                           
199260     MOVE 200                     TO RESP-KDSTATUS-API                    
199270     IF RESP-IDMSG-INFO > SPACE                                           
199280       MOVE SPACES                TO MSG-CONV-AREA                        
199290       MOVE RESP-IDMSG-INFO       TO MSG-CONV-IDMSG-IN                    
199291       CALL WMSGCONV           USING MSG-CONV-AREA                        
199292       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
199293       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
199294       ADD 1                      TO MSG-IX                               
199295     END-IF                                                               
199296     IF RESP-IDMSG-ERROR > SPACE                                          
199297*      BAD REQUEST                                                        
199298       MOVE 400                   TO RESP-KDSTATUS-API                    
199299       MOVE SPACES                TO MSG-CONV-AREA                        
199300       MOVE RESP-IDMSG-ERROR      TO MSG-CONV-IDMSG-IN                    
199301       MOVE RESP-IDELMT-ERROR     TO MSG-CONV-IDELMT                      
199302       CALL WMSGCONV           USING MSG-CONV-AREA                        
199303       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
199304       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
199305     END-IF                                                               
199306     .                                                                    
199307     EJECT                                                                
199310* IMS SEKTIONER                                                           
199400     SKIP3                                                                
199500 IMS-GU-KUNDORDER SECTION.                                                
199600     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
199700            DELIMITED BY SIZE INTO SSA1                                   
199800     MOVE '    ' TO GODK-STATUSKODER                                      
199900     CALL CBLTDLI USING GU     WDE41-PCB DLI-IO-E401 SSA1                 
200000     MOVE WDE41-STATUS-CODE TO STATUS-WS                                  
200100     PERFORM IMS-STATUSKONTROLL                                           
200200     SKIP2                                                                
200300     .                                                                    
200400 IMS-GNP-RAD       SECTION.                                               
200500     STRING 'WDE411  (IDPURAD  =' W-WDE411-IDPURAD-X ')'                  
200600            DELIMITED BY SIZE INTO SSA1                                   
200700     MOVE '  GE' TO GODK-STATUSKODER                                      
200800     CALL CBLTDLI USING GNP    WDE41-PCB DLI-IO-E411 SSA1                 
200900     MOVE WDE41-STATUS-CODE TO STATUS-WS                                  
201000                              STATUS-RAD-WS                               
201100     PERFORM IMS-STATUSKONTROLL                                           
201200     SKIP2                                                                
201300     .                                                                    
201400 IMS-GNP-RAD-OKVAL SECTION.                                               
201500     MOVE 'WDE411  ' TO SSA1                                              
201600     MOVE '  GE' TO GODK-STATUSKODER                                      
201700     CALL CBLTDLI USING GNP    WDE41-PCB DLI-IO-E411 SSA1                 
201800     MOVE WDE41-STATUS-CODE TO STATUS-WS                                  
201900                              STATUS-RAD-WS                               
202000     PERFORM IMS-STATUSKONTROLL                                           
202100     .                                                                    
202200     SKIP2                                                                
202300 IMS-GNP-KKOLLI-KVAL SECTION.                                             
202400     STRING 'WDE421  (WDE421KY =' W-WDE421-IDKOLLI-X ')'                  
202500            DELIMITED BY SIZE INTO SSA1                                   
202600     MOVE '  GE' TO GODK-STATUSKODER                                      
202700     CALL CBLTDLI USING GNP    WDE41-PCB DLI-IO-E421 SSA1                 
202800     MOVE WDE41-STATUS-CODE TO STATUS-WS                                  
202900     PERFORM IMS-STATUSKONTROLL                                           
203000     .                                                                    
203100     SKIP2                                                                
203200 IMS-GNP-RAD-FOM   SECTION.                                               
203300     STRING 'WDE411  (IDPURAD >=' W-WDE411-IDPURAD-X ')'                  
203400            DELIMITED BY SIZE INTO SSA1                                   
203500     MOVE '  GE' TO GODK-STATUSKODER                                      
203600     CALL CBLTDLI USING GNP    WDE41-PCB DLI-IO-E411 SSA1                 
203700     MOVE WDE41-STATUS-CODE TO STATUS-WS                                  
203800                              STATUS-RAD-WS                               
203900     PERFORM IMS-STATUSKONTROLL                                           
204000     .                                                                    
204100     SKIP2                                                                
204200 IMS-GU-WDE601    SECTION.                                                
204300                                                                          
204400     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
204500            DELIMITED BY SIZE INTO SSA1                                   
204600     MOVE '  GE' TO GODK-STATUSKODER                                      
204700     CALL CBLTDLI USING GU    WDE6-PCB DLI-IO-AREA3 SSA1                  
204800     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
204900     PERFORM IMS-STATUSKONTROLL                                           
205000     SKIP3                                                                
205100     .                                                                    
205200     EJECT                                                                
205300 IMS-GU-KUNDORDER-SEK-INV-GE SECTION.                                     
205400     STRING 'WDE411  (WDE4BSEQ>=' W-WDE4B-KEYSEQ-MIN-X                    
205500                    '&WDE4BSEQ<=' W-WDE4B-KEYSEQ-MAX-X ')'                
205600            DELIMITED BY SIZE INTO SSA1                                   
205700     MOVE 'WDE401  ' TO SSA2                                              
205800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
205900     CALL CBLTDLI USING GU   WDE42-PCB DLI-IO-E401 SSA1 SSA2              
206000     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
206100     PERFORM IMS-STATUSKONTROLL                                           
206200     SKIP2                                                                
206300     .                                                                    
206400 IMS-GU-WDE42-KORD-BSEQ SECTION.                                          
206500     STRING 'WDE411  (WDE4BSEQ =' W-WDE4B-KEYSEQ-X ')'                    
206600            DELIMITED BY SIZE INTO SSA1                                   
206700     MOVE 'WDE401  ' TO SSA2                                              
206800     MOVE '  ' TO GODK-STATUSKODER                                        
206900     CALL CBLTDLI USING GU   WDE42-PCB DLI-IO-E401 SSA1 SSA2              
207000     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
207100     PERFORM IMS-STATUSKONTROLL                                           
207200     SKIP2                                                                
207300     .                                                                    
207400 IMS-GHU-RAD-SEK    SECTION.                                              
207500     STRING 'WDE411  (WDE4BSEQ =' W-WDE4B-KEYSEQ-X ')'                    
207600            DELIMITED BY SIZE INTO SSA1                                   
207700     MOVE '    ' TO GODK-STATUSKODER                                      
207800     CALL CBLTDLI USING GHU    WDE4-PCB DLI-IO-E411 SSA1                  
207900     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
208000     PERFORM IMS-STATUSKONTROLL                                           
208100     SKIP2                                                                
208200     .                                                                    
208300 IMS-REPL-RAD           SECTION.                                          
208400     MOVE '    ' TO GODK-STATUSKODER                                      
208500     CALL CBLTDLI USING REPL WDE4-PCB DLI-IO-E411                         
208600     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
208700     PERFORM IMS-STATUSKONTROLL                                           
208800     SKIP2                                                                
208900     .                                                                    
209000 IMS-GHN-KOLLI-KOPPL-SEK SECTION.                                         
209100     STRING 'WDE411  (WDE4BSEQ =' W-WDE4B-KEYSEQ-X ')'                    
209200            DELIMITED BY SIZE INTO SSA1                                   
209300     STRING 'WDE421  (WDE421KY =' W-WDE421-IDKOLLI-X ')'                  
209400            DELIMITED BY SIZE INTO SSA2                                   
209500     MOVE '  GE' TO GODK-STATUSKODER                                      
209600     CALL CBLTDLI USING GHN    WDE4-PCB DLI-IO-E421 SSA1 SSA2             
209700     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
209800     PERFORM IMS-STATUSKONTROLL                                           
209900     SKIP2                                                                
210000     .                                                                    
210100 IMS-DLET-KOLLI-KOPPL  SECTION.                                           
210200     MOVE 'WDE421   ' TO SSA1                                             
210300     MOVE '  '   TO GODK-STATUSKODER                                      
210400     CALL CBLTDLI USING DLET WDE4-PCB DLI-IO-E421 SSA1                    
210500     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
210600     PERFORM IMS-STATUSKONTROLL                                           
210700     SKIP2                                                                
210800     .                                                                    
210900 IMS-GHU-KOLLIREG SECTION.                                                
211000     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
211100            DELIMITED BY SIZE INTO SSA1                                   
211200     MOVE '  GE' TO GODK-STATUSKODER                                      
211300     CALL CBLTDLI USING GHU    WDE6-PCB DLI-IO-AREA3 SSA1                 
211400     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
211500     PERFORM IMS-STATUSKONTROLL                                           
211600     SKIP2                                                                
211700     .                                                                    
211800 IMS-REPL-KOLLIREG SECTION.                                               
211900     MOVE '    ' TO GODK-STATUSKODER                                      
212000     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-AREA3                        
212100     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
212200     PERFORM IMS-STATUSKONTROLL                                           
212300     .                                                                    
212400     EJECT                                                                
212500 IMS-GHU-KOLLI    SECTION.                                                
212600     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
212700            DELIMITED BY SIZE INTO SSA1                                   
212800     STRING 'WDE611  (IDKOLLI  =' W-WDE611-IDKOLLI-X ')'                  
212900            DELIMITED BY SIZE INTO SSA2                                   
213000     MOVE '  GE' TO GODK-STATUSKODER                                      
213100     CALL CBLTDLI USING GHU    WDE64-PCB DLI-IO-AREA3 SSA1 SSA2           
213200     MOVE WDE64-STATUS-CODE TO STATUS-WS                                  
213300     PERFORM IMS-STATUSKONTROLL                                           
213400     SKIP2                                                                
213500     .                                                                    
213600 IMS-REPL-KOLLI    SECTION.                                               
213700     MOVE '    ' TO GODK-STATUSKODER                                      
213800     CALL CBLTDLI USING REPL WDE64-PCB DLI-IO-AREA3                       
213900     MOVE WDE64-STATUS-CODE TO STATUS-WS                                  
214000     PERFORM IMS-STATUSKONTROLL                                           
214100     SKIP2                                                                
214200     .                                                                    
214300 IMS-DLET-KOLLI    SECTION.                                               
214400     MOVE '    ' TO GODK-STATUSKODER                                      
214500     CALL CBLTDLI USING DLET WDE64-PCB DLI-IO-AREA3                       
214600     MOVE WDE64-STATUS-CODE TO STATUS-WS                                  
214700     PERFORM IMS-STATUSKONTROLL                                           
214800     SKIP2                                                                
214900     .                                                                    
215000     EJECT                                                                
215100 IMS-GU-KUNDORDER-SEK SECTION.                                            
215200     STRING 'WDE401  (WDE4ASEQ =' W-WDE4A1-KUNDORDER-X ')'                
215300            DELIMITED BY SIZE INTO SSA1                                   
215400     MOVE '  GEGB' TO GODK-STATUSKODER                                    
215500     CALL CBLTDLI USING GU     WDE4A-PCB DLI-IO-E401 SSA1                 
215600     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
215700                               STATUS-KUNDORDER-SEK-WS                    
215800     PERFORM IMS-STATUSKONTROLL                                           
215900     SKIP2                                                                
216000     .                                                                    
216100 IMS-GN-KUNDORDER-SEK SECTION.                                            
216200     STRING 'WDE401  (WDE4ASEQ =' W-WDE4A1-KUNDORDER-X ')'                
216300            DELIMITED BY SIZE INTO SSA1                                   
216400     MOVE '  GEGB' TO GODK-STATUSKODER                                    
216500     CALL CBLTDLI USING GN     WDE4A-PCB DLI-IO-E401 SSA1                 
216600     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
216700                               STATUS-KUNDORDER-SEK-WS                    
216800     PERFORM IMS-STATUSKONTROLL                                           
216900     .                                                                    
217000     EJECT                                                                
217100 IMS-GHN-XXJK11   SECTION.                                                
217200     STRING 'WLXXJK01(WDGXKEY  =' W-WDGXKEY-4321-X ')'                    
217300            DELIMITED BY SIZE INTO SSA1                                   
217400     MOVE 'WLXXJK11'         TO SSA2                                      
217500     MOVE '  GEGB'           TO GODK-STATUSKODER                          
217600     CALL CBLTDLI USING GHN    XXJK-PCB DLI-IO-AREA4 SSA1 SSA2            
217700     MOVE XXJK-STATUS-CODE   TO STATUS-WS                                 
217800     PERFORM IMS-STATUSKONTROLL                                           
217900     SKIP2                                                                
218000                                                                          
218100     .                                                                    
218200 IMS-DLET-XXJK     SECTION.                                               
218300     MOVE '  ' TO GODK-STATUSKODER                                        
218400     CALL CBLTDLI USING DLET XXJK-PCB DLI-IO-AREA4                        
218500     MOVE XXJK-STATUS-CODE TO STATUS-WS                                   
218600     PERFORM IMS-STATUSKONTROLL                                           
218700     .                                                                    
218800     EJECT                                                                
218900 IMS-GU-ORQA01    SECTION.                                                
219000     STRING 'WLORQA01(WDQ301KY =' W-WDQ301-ORDERDEL-X ')'                 
219100            DELIMITED BY SIZE INTO SSA1                                   
219200     MOVE '    ' TO GODK-STATUSKODER                                      
219300     CALL CBLTDLI USING GU     ORQA-PCB DLI-IO-AREA5 SSA1                 
219400     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
219500     PERFORM IMS-STATUSKONTROLL                                           
219600     .                                                                    
219700     EJECT                                                                
219800 IMS-GHU-XXKW11 SECTION.                                                  
219900     STRING 'WLXXKW01(WDGXKEY  =' W-WDGXKEY-4471-X ')'                    
220000            DELIMITED BY SIZE INTO SSA1                                   
220100     STRING 'WLXXKW11(KDSEGKEY =' W-KDSEGKEY-4472-X ')'                   
220200            DELIMITED BY SIZE INTO SSA2                                   
220300     MOVE '    GE'           TO GODK-STATUSKODER                          
220400     CALL CBLTDLI USING GHU    XXKW-PCB DLI-IO-AREA6 SSA1 SSA2            
220500     MOVE XXKW-STATUS-CODE   TO STATUS-WS                                 
220600     PERFORM IMS-STATUSKONTROLL                                           
220700     .                                                                    
220800     SKIP2                                                                
220900 IMS-REPL-XXKW11   SECTION.                                               
221000     MOVE '    ' TO GODK-STATUSKODER                                      
221100     CALL CBLTDLI USING REPL XXKW-PCB DLI-IO-AREA6                        
221200     MOVE XXKW-STATUS-CODE TO STATUS-WS                                   
221300     PERFORM IMS-STATUSKONTROLL                                           
221400     .                                                                    
221500     EJECT                                                                
221600 IMS-GU-XXLB      SECTION.                                                
221700     STRING 'WLXXLB01(WDGXKEY  =' W-WDGXKEY-4477-X ')'                    
221800            DELIMITED BY SIZE INTO SSA1                                   
221900     STRING 'WLXXLB11(WDGXKEY  =' W-WDGXKEY-4478-X ')'                    
222000            DELIMITED BY SIZE INTO SSA2                                   
222100     MOVE '  GE'             TO GODK-STATUSKODER                          
222200     CALL CBLTDLI USING GU     XXLB-PCB DLI-IO-AREA6 SSA1 SSA2            
222300     MOVE XXLB-STATUS-CODE   TO STATUS-WS                                 
222400     PERFORM IMS-STATUSKONTROLL                                           
222500     .                                                                    
222600     EJECT                                                                
222700 IMS-GU-WDE411-01-BSEQ SECTION.                                           
222800     STRING 'WDE411  (WDE4BSEQ =' W-WDE4B-KEYSEQ-X ')'                    
222900            DELIMITED BY SIZE INTO SSA1                                   
223000     MOVE 'WDE401  ' TO SSA2                                              
223100     MOVE '  GE' TO GODK-STATUSKODER                                      
223200     CALL CBLTDLI USING GU   WDE42-PCB DLI-IO-E401 SSA1 SSA2              
223300     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
223400     PERFORM IMS-STATUSKONTROLL                                           
223500     .                                                                    
223600     SKIP2                                                                
223700 IMS-WDE611-GU-KOLLI SECTION.                                             
223800     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
223900            DELIMITED BY SIZE INTO SSA1                                   
224000     STRING 'WDE611  (IDKOLLI  =' W-WDE611-IDKOLLI-X ')'                  
224100            DELIMITED BY SIZE INTO SSA2                                   
224200     MOVE '  ' TO GODK-STATUSKODER                                        
224300     CALL CBLTDLI USING GU WDE62-PCB DLI-IO-AREA9 SSA1 SSA2               
224400     MOVE WDE62-STATUS-CODE TO STATUS-WS                                  
224500     PERFORM IMS-STATUSKONTROLL                                           
224600     .                                                                    
224700     SKIP2                                                                
224800* LAESN. FÖR KONTROLL OM LAASSEGMENT SKALL TAS BORT                       
224900 IMS-GET-WDE4F SECTION.                                                   
225000     STRING 'WDE4F1  (WDE4F1KY>=' W-WDE4F1KY-MIN-X                        
225100                    '&WDE4F1KY<=' W-WDE4F1KY-MAX-X                        
225200                    '&IDPLKLST =' W-WDEE4F-IDPLKST-X ')'                  
225300            DELIMITED BY SIZE INTO SSA1                                   
225400     MOVE '  GE' TO GODK-STATUSKODER                                      
225500     CALL CBLTDLI USING GN WDE4F-PCB DLI-IO-AREA9 SSA1                    
225600     MOVE WDE4F-STATUS-CODE TO STATUS-WS                                  
225700     PERFORM IMS-STATUSKONTROLL                                           
225800     .                                                                    
225900     EJECT                                                                
226000 IMS-WDE43-GU-ORAD-LAST SECTION.                                          
226100     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
226200            DELIMITED BY SIZE INTO SSA1                                   
226300     MOVE 'WDE411  *L ' TO SSA2                                           
226400     MOVE '  ' TO GODK-STATUSKODER                                        
226500     CALL CBLTDLI USING GU   WDE43-PCB DLI-IO-AREA8 SSA1 SSA2             
226600     MOVE WDE43-STATUS-CODE TO STATUS-WS                                  
226700     PERFORM IMS-STATUSKONTROLL                                           
226800     .                                                                    
226900     EJECT                                                                
227000 IMS-GN-WDE411-21-FSEQ  SECTION.                                          
227100     STRING 'WDE411  *D(WDE4FSEQ =' W-WDE4FSEQ-X ')'                      
227200            DELIMITED BY SIZE INTO SSA1                                   
227300     STRING 'WDE421  (WDE421KY =' W-WDE421-IDKOLLI-X ')'                  
227400            DELIMITED BY SIZE INTO SSA2                                   
227500     MOVE '  GE' TO GODK-STATUSKODER                                      
227600     CALL CBLTDLI USING GN WDE44-PCB DLI-IO-WDE411-21 SSA1 SSA2           
227700     MOVE WDE44-STATUS-CODE TO STATUS-WS                                  
227800     PERFORM IMS-STATUSKONTROLL                                           
227900     .                                                                    
228000     EJECT                                                                
228100 IMS-GN-WDE411-21-FSEQ-SOK  SECTION.                                      
228200     STRING 'WDE411  *D(WDE4FSEQ =' W-WDE4FSEQ-X                          
228300                      '&IDPURAD =>' W-WDE411-IDPURAD-X ')'                
228400            DELIMITED BY SIZE INTO SSA1                                   
228500     STRING 'WDE421  (WDE421KY =' W-WDE421-IDKOLLI-X ')'                  
228600            DELIMITED BY SIZE INTO SSA2                                   
228700     MOVE '  GE' TO GODK-STATUSKODER                                      
228800     CALL CBLTDLI USING GN WDE44-PCB DLI-IO-WDE411-21 SSA1 SSA2           
228900     MOVE WDE44-STATUS-CODE TO STATUS-WS                                  
229000     PERFORM IMS-STATUSKONTROLL                                           
229100     .                                                                    
229200     EJECT                                                                
229300 IMS-GHU-XXDU01      SECTION.                                             
229400                                                                          
229500     STRING 'WLXXDU01(WDGXKEY  =' W-4301-WDGXKEY-X ')'                    
229600            DELIMITED BY SIZE INTO SSA1                                   
229700     MOVE '  GE' TO GODK-STATUSKODER                                      
229800     CALL CBLTDLI USING GHU XXDU-PCB DLI-IO-AREA9 SSA1                    
229900     MOVE XXDU-STATUS-CODE TO STATUS-WS                                   
230000     PERFORM IMS-STATUSKONTROLL                                           
230100     .                                                                    
230200 IMS-GHNP-XXDU11-IDKOLLI         SECTION.                                 
230300*    LÄS MED SAMMA IDKOLLI OCH NÄSTA PLOCKLISTA                           
230400*                                                                         
230500     STRING 'WLXXDU11(IDKOLLI  =' W-4302-IDKOLLI-X ')'                    
230600            DELIMITED BY SIZE INTO SSA1                                   
230700     MOVE '  GE' TO GODK-STATUSKODER                                      
230800     CALL CBLTDLI USING GHNP XXDU-PCB DLI-IO-AREA9 SSA1                   
230900     MOVE XXDU-STATUS-CODE TO STATUS-WS                                   
231000     PERFORM IMS-STATUSKONTROLL                                           
231100     .                                                                    
231200 IMS-GHNP-XXDU11      SECTION.                                            
231300                                                                          
231400     STRING 'WLXXDU11(WDGXKEY  =' W-4302-WDGXKEY-X ')'                    
231500            DELIMITED BY SIZE INTO SSA1                                   
231600     MOVE '  GE' TO GODK-STATUSKODER                                      
231700     CALL CBLTDLI USING GHNP XXDU-PCB DLI-IO-AREA9 SSA1                   
231800     MOVE XXDU-STATUS-CODE TO STATUS-WS                                   
231900     PERFORM IMS-STATUSKONTROLL                                           
232000     .                                                                    
232100 IMS-GNP-XXDU11-FIRST SECTION.                                            
232200                                                                          
232300     MOVE 'WLXXDU11*F '  TO SSA1                                          
232400     MOVE '  GE' TO GODK-STATUSKODER                                      
232500     CALL CBLTDLI USING GNP XXDU-PCB DLI-IO-AREA9 SSA1                    
232600     MOVE XXDU-STATUS-CODE TO STATUS-WS                                   
232700     PERFORM IMS-STATUSKONTROLL                                           
232800     .                                                                    
232900                                                                          
233000 IMS-DLET-XXDU       SECTION.                                             
233100                                                                          
233200     MOVE '  ' TO GODK-STATUSKODER                                        
233300     CALL CBLTDLI USING DLET XXDU-PCB DLI-IO-AREA9                        
233400     MOVE XXDU-STATUS-CODE TO STATUS-WS                                   
233500     PERFORM IMS-STATUSKONTROLL                                           
233600     .                                                                    
233700                                                                          
233800 IMS-GHN-WDA6B SECTION.                                                   
233900     STRING 'WDA601  (WDA6BSEQ>=' W-WDA601KY-MIN-X                        
234000                    '&WDA6BSEQ<=' W-WDA601KY-MAX-X ')'                    
234100            DELIMITED BY SIZE INTO SSA1                                   
234200     MOVE '  GEGB'               TO GODK-STATUSKODER                      
234300     CALL  CBLTDLI  USING GHN   WDA6B-PCB DLI-IO-AREA-WDA6 SSA1           
234400     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
234500     PERFORM IMS-STATUSKONTROLL                                           
234600     .                                                                    
234700                                                                          
234800 IMS-REPL-WDA6B SECTION.                                                  
234900     MOVE 'WDA601  '           TO SSA1                                    
235000     MOVE '    '               TO GODK-STATUSKODER                        
235100     CALL  CBLTDLI  USING REPL WDA6B-PCB DLI-IO-AREA-WDA6 SSA1            
235200     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
235300     PERFORM IMS-STATUSKONTROLL                                           
235400     .                                                                    
235500                                                                          
235600 IMS-ROLLBACK    SECTION.                                                 
235700     CALL CBLTDLI USING ROLB    MSG-PCB                                   
235800     .                                                                    
235900     SKIP3                                                                
236000 IMS-STATUSKONTROLL SECTION.                                              
236100     SET STATUS-IX TO 1                                                   
236200     SEARCH GODK-STATUS AT END CALL FELLOG                                
236300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
236400     END-SEARCH                                                           
236500     .                                                                    
