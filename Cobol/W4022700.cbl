000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4022700.                                                
000300 AUTHOR.         BODIL LINDAHL.                                           
000400 DATE-WRITTEN.   APRIL 2003                                               
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        KÖBILD VOR                                                       
000900*                                                                         
001000*        PROGRAMMET LÄSER WDA6                                            
001100*                   LÄSER WDP5                                            
001200*                   STARTAR W40277                                        
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSAKTION: W4T227                                              
001600*        MID:         W4I22701                                            
001700*                                                                         
001800*    UTDATA.                                                              
001900*        MOD:         W4O22701                                            
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300                                                                          
002400 DATA DIVISION.                                                           
002500     EJECT                                                                
002600 WORKING-STORAGE SECTION.                                                 
002700 77  IDPGM                       PIC X(08)   VALUE 'W4022700'.            
002800 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
002900 77  JA                          PIC X       VALUE 'J'.                   
003000 77  NEJ                         PIC X       VALUE 'N'.                   
003100 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
003200 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
003300 77  WS-IDANSK-SOEKN             PIC X       VALUE 'N'.                   
003400 77  INDX                        PIC 9(3)    VALUE ZERO.                  
003500 77  MAX-INDX                    PIC 9(3)    VALUE 14 .                   
003600 77  WS-INDEX                    PIC X       VALUE SPACE.                 
003700 77  WS-IDANSK                   PIC 9(3)    VALUE ZERO.                  
003800 77  WS-IDANSK-MIN               PIC 9(3)    VALUE ZERO.                  
003900 77  WS-IDANSK-MAX               PIC 9(3)    VALUE ZERO.                  
004000 77  WS-IDARTNR                  PIC 9(8)    VALUE ZERO.                  
004100 77  WS-IDARTNR-NUM              PIC 9(9)    VALUE ZERO.                  
004200 77  WS-IDARTNR-ALPHA            PIC X(9)    VALUE SPACE.                 
004300 77  WS-KVBEART                  PIC 9(7)    VALUE ZERO.                  
004400 77  SW-HOPP                     PIC X       VALUE 'N'.                   
004500 77  WS-HOPP-INDX                PIC 9(3)    VALUE ZERO.                  
004600 77  WS-KDCMDVAL                 PIC X       VALUE SPACE.                 
004700 77  WS-IDDISTR                  PIC 9(4)    VALUE ZERO.                  
004800 77  WS-IDKUNDNR                 PIC 9(6)    VALUE ZERO.                  
004900                                                                          
005000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005100     88  NYCKLAR-OK                          VALUE 'J'.                   
005200     88  NYCKLAR-FEL                         VALUE 'N'.                   
005300                                                                          
005400 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005500     88  INDATA-OK                           VALUE 'J'.                   
005600     88  INDATA-FEL                          VALUE 'N'.                   
005700     EJECT                                                                
005800 01  -COPY WWDCKONS                                                       
005900 01  WS-IDANSK-KOLL              PIC 9(3).                                
006000 01  FILLER REDEFINES WS-IDANSK-KOLL.                                     
006100     03 WS-IDANSK-POS1           PIC 9.                                   
006200     03 WS-IDANSK-POS2           PIC 9.                                   
006300     03 WS-IDANSK-POS3           PIC 9.                                   
006400                                                                          
006500 01  WS-IDORDNR-KOLL             PIC X(7).                                
006600 01  FILLER REDEFINES WS-IDORDNR-KOLL.                                    
006700     03 FILLER                   PIC X(2).                                
006800     03 WS-IDORDNR5              PIC X(5).                                
006900                                                                          
007000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007100     88  EGEN-MID                            VALUE '4227'.                
007200     88  HOPP-MID                            VALUE '4277'.                
007300     88  GODK-MID                            VALUE '4277'.                
007400                                                                          
007500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007600 01  GENERELLA-SUBPROGRAM.                                                
007700     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007800     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008100     EJECT                                                                
008200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
008300*01 -COPY WMEDAREA                                                        
008400                                                                          
008500 01  MESSAGE-CODES.                                                       
008600     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
008700     03  INF-SISTA-SIDAN         PIC X(3)    VALUE '115'.                 
008800     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
008900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
009000     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
009100     03  INF-URVAL-SAKNAS        PIC X(3)    VALUE '005'.                 
009200     03  INF-BYT-BILD-TRYCK-PF9  PIC X(3)    VALUE '127'.                 
009300     03  ERR-PF9-AND-NO-DATA     PIC X(3)    VALUE '???'.                 
009400     EJECT                                                                
009500 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
009600     SKIP3                                                                
009700*01 -COPY WMSGINIT                                                        
009800     EJECT                                                                
009900 01  SPAR-AREA.                                                           
010000     03  SPAR-IDTRANS                 PIC X(4)  VALUE '4227'.             
010100     03  SPAR-INDEX                   PIC X     VALUE SPACE.              
010200     03  SPAR-SISTA-SIDAN             PIC X     VALUE SPACE.              
010300                                                                          
010400     03  SPAR-TIREGDAT.                                                   
010500         05  SPAR-TIREGDAT-AVV9       PIC S9(7) COMP-3 VALUE ZERO         
010600                                               OCCURS 14.                 
010700     03  SPAR-TIREGTID.                                                   
010800         05  SPAR-TIREGTID-AVV9       PIC S9(9) COMP-3 VALUE ZERO         
010900                                               OCCURS 14.                 
011000     03  SPAR-ANSK.                                                       
011100         05  SPAR-IDANSK              PIC 9(3) VALUE ZERO                 
011200                                               OCCURS 14.                 
011300                                                                          
011400     03  SPAR-NYCKLAR-ENTER           PIC X(16) VALUE SPACE.              
011500     03  SPAR-WDA6ISEQ-ENTER REDEFINES SPAR-NYCKLAR-ENTER.                
011600         05  SEQI-IDANSK-ENTER        PIC S9(3) COMP-3.                   
011700         05  SEQI-TIREGDAT-AVV9-ENTER PIC S9(7) COMP-3.                   
011800         05  SEQI-TIREGTID-AVV9-ENTER PIC S9(9) COMP-3.                   
011900     03  SPAR-WDA6JSEQ-ENTER REDEFINES SPAR-NYCKLAR-ENTER.                
012000         05  SEQJ-IDARTNR-ENTER       PIC S9(9) COMP-3.                   
012100         05  SEQJ-TIREGDAT-AVV9-ENTER PIC S9(7) COMP-3.                   
012200         05  SEQJ-TIREGTID-AVV9-ENTER PIC S9(9) COMP-3.                   
012300     03  SPAR-WDA6KSEQ-ENTER REDEFINES SPAR-NYCKLAR-ENTER.                
012400         05  SEQK-IDANSK-ENTER        PIC S9(3) COMP-3.                   
012500         05  SEQK-IDLEVNR-ENTER       PIC X(5).                           
012600         05  SEQK-TIREGDAT-AVV9-ENTER PIC S9(7) COMP-3.                   
012700         05  SEQK-TIREGTID-AVV9-ENTER PIC S9(9) COMP-3.                   
012800     03  SPAR-WDA6LSEQ-ENTER REDEFINES SPAR-NYCKLAR-ENTER.                
012900         05  SEQL-IDLEVNR-ENTER       PIC X(5).                           
013000         05  SEQL-TIREGDAT-AVV9-ENTER PIC S9(7) COMP-3.                   
013100         05  SEQL-TIREGTID-AVV9-ENTER PIC S9(9) COMP-3.                   
013200                                                                          
013300     03  SPAR-NYCKLAR-NEXT              PIC X(16)   VALUE SPACE.          
013400     03  SPAR-WDA6ISEQ-NEXT  REDEFINES SPAR-NYCKLAR-NEXT.                 
013500         05  SEQI-IDANSK-NEXT         PIC S9(3) COMP-3.                   
013600         05  SEQI-TIREGDAT-AVV9-NEXT  PIC S9(7) COMP-3.                   
013700         05  SEQI-TIREGTID-AVV9-NEXT  PIC S9(9) COMP-3.                   
013800     03  SPAR-WDA6JSEQ-NEXT  REDEFINES SPAR-NYCKLAR-NEXT.                 
013900         05  SEQJ-IDARTNR-NEXT        PIC S9(9) COMP-3.                   
014000         05  SEQJ-TIREGDAT-AVV9-NEXT  PIC S9(7) COMP-3.                   
014100         05  SEQJ-TIREGTID-AVV9-NEXT  PIC S9(9) COMP-3.                   
014200     03  SPAR-WDA6KSEQ-NEXT  REDEFINES SPAR-NYCKLAR-NEXT.                 
014300         05  SEQK-IDANSK-NEXT         PIC S9(3) COMP-3.                   
014400         05  SEQK-IDLEVNR-NEXT        PIC X(5).                           
014500         05  SEQK-TIREGDAT-AVV9-NEXT  PIC S9(7) COMP-3.                   
014600         05  SEQK-TIREGTID-AVV9-NEXT  PIC S9(9) COMP-3.                   
014700     03  SPAR-WDA6LSEQ-NEXT  REDEFINES SPAR-NYCKLAR-NEXT.                 
014800         05  SEQL-IDLEVNR-NEXT        PIC X(5).                           
014900         05  SEQL-TIREGDAT-AVV9-NEXT  PIC S9(7) COMP-3.                   
015000         05  SEQL-TIREGTID-AVV9-NEXT  PIC S9(9) COMP-3.                   
015100     EJECT                                                                
015200*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
015300 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
015400*01  MID -COPY W4I22701                                                   
015500     EJECT                                                                
015600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
015700*01  -COPY WMSGAREA                                                       
015800     EJECT                                                                
015900*    --- FÖR HOPP TILL 4277                                               
016000       05  4277-MID REDEFINES MSG-MID-OUT.                                
016100*          07  -COPY W4I27701 -PRE 4277-                                  
016200     03  MOD REDEFINES MSG-AREA.                                          
016300*      05  -COPY W4O22701                                                 
016400     EJECT                                                                
016500 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
016600*01  -COPY WMFSAREA                                                       
016700     EJECT                                                                
016800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
016900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017000     SKIP3                                                                
017100 01  NYCKLAR-TILL-DLI.                                                    
017200     03  W-WDA601KY-X.                                                    
017300         05  W-WDA601-IDDISTR        PIC S9(5) VALUE ZERO COMP-3.         
017400         05  W-WDA601-IDKUNDNR       PIC S9(7) VALUE ZERO COMP-3.         
017500         05  W-WDA601-IDKUNDRF       PIC X(10) VALUE SPACE.               
017600         05  W-WDA601-TIREGDAT-URSP  PIC S9(7) VALUE ZERO COMP-3.         
017700         05  W-WDA601-IDARTNR        PIC S9(9) VALUE ZERO COMP-3.         
017800         05  W-WDA601-TIREGTID-URSP  PIC S9(9) VALUE ZERO COMP-3.         
017900         05  W-WDA601-TIREGDAT-AVV   PIC S9(7) VALUE ZERO COMP-3.         
018000         05  W-WDA601-TIREGTID-AVV   PIC S9(9) VALUE ZERO COMP-3.         
018100                                                                          
018200     03  W-WDA6ISEQ-MIN-X.                                                
018300         05  SEQI-IDANSK-MIN         PIC S9(3) VALUE ZERO COMP-3.         
018400         05  SEQI-TIREGDAT-AVV9-MIN  PIC S9(7) VALUE ZERO COMP-3.         
018500         05  SEQI-TIREGTID-AVV9-MIN  PIC S9(9) VALUE ZERO COMP-3.         
018600     03  W-WDA6ISEQ-MAX-X.                                                
018700         05  SEQI-IDANSK-MAX         PIC S9(3) VALUE ZERO COMP-3.         
018800         05  SEQI-TIREGDAT-AVV9-MAX  PIC S9(7) VALUE ZERO COMP-3.         
018900         05  SEQI-TIREGTID-AVV9-MAX  PIC S9(9) VALUE ZERO COMP-3.         
019000     EJECT                                                                
019100     03  W-WDA6JSEQ-MIN-X.                                                
019200         05  SEQJ-IDARTNR-MIN        PIC S9(9) VALUE ZERO COMP-3.         
019300         05  SEQJ-TIREGDAT-AVV9-MIN  PIC S9(7) VALUE ZERO COMP-3.         
019400         05  SEQJ-TIREGTID-AVV9-MIN  PIC S9(9) VALUE ZERO COMP-3.         
019500     03  W-WDA6JSEQ-MAX-X.                                                
019600         05  SEQJ-IDARTNR-MAX        PIC S9(9) VALUE ZERO COMP-3.         
019700         05  SEQJ-TIREGDAT-AVV9-MAX  PIC S9(7) VALUE ZERO COMP-3.         
019800         05  SEQJ-TIREGTID-AVV9-MAX  PIC S9(9) VALUE ZERO COMP-3.         
019900                                                                          
020000     03  W-WDA6KSEQ-MIN-X.                                                
020100         05  SEQK-IDANSK-MIN         PIC S9(3) VALUE ZERO COMP-3.         
020200         05  SEQK-IDLEVNR-MIN        PIC X(5)  VALUE SPACE.               
020300         05  SEQK-TIREGDAT-AVV9-MIN  PIC S9(7) VALUE ZERO COMP-3.         
020400         05  SEQK-TIREGTID-AVV9-MIN  PIC S9(9) VALUE ZERO COMP-3.         
020500     03  W-WDA6KSEQ-MAX-X.                                                
020600         05  SEQK-IDANSK-MAX         PIC S9(3) VALUE ZERO COMP-3.         
020700         05  SEQK-IDLEVNR-MAX        PIC X(5)  VALUE SPACE.               
020800         05  SEQK-TIREGDAT-AVV9-MAX  PIC S9(7) VALUE ZERO COMP-3.         
020900         05  SEQK-TIREGTID-AVV9-MAX  PIC S9(9) VALUE ZERO COMP-3.         
021000                                                                          
021100     03  W-WDA6LSEQ-MIN-X.                                                
021200         05  SEQL-IDLEVNR-MIN        PIC X(5)  VALUE SPACE.               
021300         05  SEQL-TIREGDAT-AVV9-MIN  PIC S9(7) VALUE ZERO COMP-3.         
021400         05  SEQL-TIREGTID-AVV9-MIN  PIC S9(9) VALUE ZERO COMP-3.         
021500     03  W-WDA6LSEQ-MAX-X.                                                
021600         05  SEQL-IDLEVNR-MAX        PIC X(5)  VALUE SPACE.               
021700         05  SEQL-TIREGDAT-AVV9-MAX  PIC S9(7) VALUE ZERO COMP-3.         
021800         05  SEQL-TIREGTID-AVV9-MAX  PIC S9(9) VALUE ZERO COMP-3.         
021900                                                                          
022000     03  W-WDA6ISEQ-MINA-X.                                               
022100         05  SEQI-IDANSK-MINA         PIC S9(3) VALUE ZERO COMP-3.        
022200         05  SEQI-TIREGDAT-AVV9-MINA  PIC S9(7) VALUE ZERO COMP-3.        
022300         05  SEQI-TIREGTID-AVV9-MINA  PIC S9(9) VALUE ZERO COMP-3.        
022400     03  W-WDA6ISEQ-MAXA-X.                                               
022500         05  SEQI-IDANSK-MAXA         PIC S9(3) VALUE ZERO COMP-3.        
022600         05  SEQI-TIREGDAT-AVV9-MAXA  PIC S9(7) VALUE ZERO COMP-3.        
022700         05  SEQI-TIREGTID-AVV9-MAXA  PIC S9(9) VALUE ZERO COMP-3.        
022800     EJECT                                                                
022900     03  W-WDA6JSEQ-MINA-X.                                               
023000         05  SEQJ-IDARTNR-MINA        PIC S9(9) VALUE ZERO COMP-3.        
023100         05  SEQJ-TIREGDAT-AVV9-MINA  PIC S9(7) VALUE ZERO COMP-3.        
023200         05  SEQJ-TIREGTID-AVV9-MINA  PIC S9(9) VALUE ZERO COMP-3.        
023300     03  W-WDA6JSEQ-MAXA-X.                                               
023400         05  SEQJ-IDARTNR-MAXA        PIC S9(9) VALUE ZERO COMP-3.        
023500         05  SEQJ-TIREGDAT-AVV9-MAXA  PIC S9(7) VALUE ZERO COMP-3.        
023600         05  SEQJ-TIREGTID-AVV9-MAXA  PIC S9(9) VALUE ZERO COMP-3.        
023700                                                                          
023800     03  W-WDA6KSEQ-MINA-X.                                               
023900         05  SEQK-IDANSK-MINA         PIC S9(3) VALUE ZERO COMP-3.        
024000         05  SEQK-IDLEVNR-MINA        PIC X(5)  VALUE SPACE.              
024100         05  SEQK-TIREGDAT-AVV9-MINA  PIC S9(7) VALUE ZERO COMP-3.        
024200         05  SEQK-TIREGTID-AVV9-MINA  PIC S9(9) VALUE ZERO COMP-3.        
024300     03  W-WDA6KSEQ-MAXA-X.                                               
024400         05  SEQK-IDANSK-MAXA         PIC S9(3) VALUE ZERO COMP-3.        
024500         05  SEQK-IDLEVNR-MAXA        PIC X(5)  VALUE SPACE.              
024600         05  SEQK-TIREGDAT-AVV9-MAXA  PIC S9(7) VALUE ZERO COMP-3.        
024700         05  SEQK-TIREGTID-AVV9-MAXA  PIC S9(9) VALUE ZERO COMP-3.        
024800                                                                          
024900     03  W-WDA6LSEQ-MINA-X.                                               
025000         05  SEQL-IDLEVNR-MINA        PIC X(5)  VALUE SPACE.              
025100         05  SEQL-TIREGDAT-AVV9-MINA  PIC S9(7) VALUE ZERO COMP-3.        
025200         05  SEQL-TIREGTID-AVV9-MINA  PIC S9(9) VALUE ZERO COMP-3.        
025300     03  W-WDA6LSEQ-MAXA-X.                                               
025400         05  SEQL-IDLEVNR-MAXA        PIC X(5)  VALUE SPACE.              
025500         05  SEQL-TIREGDAT-AVV9-MAXA  PIC S9(7) VALUE ZERO COMP-3.        
025600         05  SEQL-TIREGTID-AVV9-MAXA  PIC S9(9) VALUE ZERO COMP-3.        
025700                                                                          
025800     03  W-WDP501KY-X.                                                    
025900         05  W-IDSKYLT           PIC X(3)   VALUE SPACE.                  
026000         05  W-IDDOKTYP          PIC X(8)   VALUE SPACE.                  
026100         05  W-IDDOK             PIC X(8)   VALUE SPACE.                  
026200     03  W-IDSID-X.                                                       
026300         05  W-IDSID             PIC S9(3)  COMP-3 VALUE ZERO.            
026400                                                                          
026500     03  W-WDD901KY-X.                                                    
026600         05  W-IDARTNR-D9            PIC S9(9) VALUE ZERO COMP-3.         
026700         05  W-IDDC-D9               PIC X(2) VALUE SPACE.                
026800     03  W-IDLEVNR-X.                                                     
026900         05  W-IDLEVNR               PIC  X(5) VALUE SPACE.               
027000     EJECT                                                                
027100*    --- STATUS-KOD FRÅN IMS                                              
027200 01  STATUS-WS                   PIC XX.                                  
027300     88  SEGMENT-FINNS                       VALUE '  '.                  
027400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
027500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
027600     SKIP2                                                                
027700 01  GODK-STATUSKODER.                                                    
027800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
027900     SKIP3                                                                
028000 01  ALL-SSA.                                                             
028100     03 SSA1                     PIC X(96).                               
028200     03 SSA2                     PIC X(96).                               
028300     03 SSA3                     PIC X(96).                               
028400     EJECT                                                                
028500*    --- IMS FUNKTIONSKODER                                               
028600*01  -COPY W0003                                                          
028700     EJECT                                                                
028800*    ---  DLI INPUT-OUTPUT AREA                                           
028900                                                                          
029000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA601'.                      
029100 01  DLI-IO-WDA601.                                                       
029200*    03  -COPY WDA601                                                     
029300     EJECT                                                                
029400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA611'.                      
029500 01  DLI-IO-WDA611.                                                       
029600*    03  -COPY WDA611                                                     
029700     EJECT                                                                
029800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA612'.                      
029900 01  DLI-IO-WDA612.                                                       
030000*    03  -COPY WDA612                                                     
030100     EJECT                                                                
030200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA613'.                      
030300 01  DLI-IO-WDA613.                                                       
030400*    03  -COPY WDA613                                                     
030500     EJECT                                                                
030600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP501'.                      
030700 01  DLI-IO-WDP501.                                                       
030800*    03  -COPY WDP501                                                     
030900     EJECT                                                                
031000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP512'.                      
031100 01  DLI-IO-WDP512.                                                       
031200*    03  -COPY WDP512                                                     
031300     EJECT                                                                
031400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD924'.                      
031500 01  DLI-IO-WDD924.                                                       
031600*    03  -COPY WDD924 -PRE D9-                                            
031700     EJECT                                                                
031800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD925'.                      
031900 01  DLI-IO-WDD925.                                                       
032000*    03  -COPY WDD925 -PRE D9-                                            
032100     EJECT                                                                
032200 LINKAGE SECTION.                                                         
032300*01  -COPY W0009   -PRE MSG-                                              
032400     EJECT                                                                
032500*01  -COPY W0009   -PRE ALT-                                              
032600     EJECT                                                                
032700*01  -COPY W0008   -PRE WDP7-                                             
032800     05  FILLER                  PIC X.                                   
032900     EJECT                                                                
033000*01  -COPY W0008   -PRE WDA6-                                             
033100     05  FILLER                  PIC X.                                   
033200     EJECT                                                                
033300*01  -COPY W0008   -PRE WDA6I-                                            
033400     05  FILLER                  PIC X.                                   
033500     EJECT                                                                
033600*01  -COPY W0008   -PRE WDA6J-                                            
033700     05  FILLER                  PIC X.                                   
033800     EJECT                                                                
033900*01  -COPY W0008   -PRE WDA6K-                                            
034000     05  FILLER                  PIC X.                                   
034100     EJECT                                                                
034200*01  -COPY W0008   -PRE WDA6L-                                            
034300     05  FILLER                  PIC X.                                   
034400     EJECT                                                                
034500*01  -COPY W0008   -PRE WDP5-                                             
034600     05  FILLER                  PIC X.                                   
034700     EJECT                                                                
034800*01  -COPY W0008   -PRE WDD9-                                             
034900     05  FILLER                  PIC X.                                   
035000     EJECT                                                                
035100 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB WDP7-PCB WDA6-PCB              
035200                           WDA6I-PCB WDA6J-PCB WDA6K-PCB                  
035300                           WDA6L-PCB WDP5-PCB  WDD9-PCB.                  
035400 MAIN SECTION.                                                            
035500     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB WDP7-PCB WDA6-PCB              
035600                           WDA6I-PCB WDA6J-PCB WDA6K-PCB                  
035700                           WDA6L-PCB WDP5-PCB  WDD9-PCB.                  
035800                                                                          
035900     PERFORM IMS-GET-MSG                                                  
036000     IF SEGMENT-FINNS                                                     
036100       PERFORM A-INIT                                                     
036200       PERFORM B-KOLLA-NYCKLAR                                            
036300       IF NYCKLAR-OK                                                      
036400          IF MFS-FIRST                                                    
036500             PERFORM C-FOERSTA-SIDA                                       
036600          ELSE                                                            
036700             IF MFS-NEXT                                                  
036800                PERFORM D-NAESTA-SIDA                                     
036900             ELSE                                                         
037000                IF MFS-SPLIT                                              
037100                   PERFORM G-KOLLA-HOPP                                   
037200                   IF INDATA-OK                                           
037300                      MOVE JA TO SW-HOPP                                  
037400                   END-IF                                                 
037500                ELSE                                                      
037600                   PERFORM E-SAMMA-SIDA                                   
037700                END-IF                                                    
037800             END-IF                                                       
037900          END-IF                                                          
038000          IF SW-HOPP = NEJ                                                
038100             PERFORM F-LAES-VISA-INFO                                     
038200          END-IF                                                          
038300       END-IF                                                             
038400       IF SW-HOPP = JA                                                    
038500          PERFORM IMS-INSERT-ALT-MSG                                      
038600       ELSE                                                               
038700          COMPUTE MSG-KVLL = LENGTH OF MOD-W4O22701 + 4                   
038800          PERFORM IMS-INSERT-MSG                                          
038900       END-IF                                                             
039000     END-IF                                                               
039100                                                                          
039200     MOVE ZERO TO RETURN-CODE                                             
039300     GOBACK                                                               
039400     .                                                                    
039500     EJECT                                                                
039600 A-INIT SECTION.                                                          
039700     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
039800                                                                          
039900     IF MSG-DUBBLA-TRANSKODER                                             
040000        MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I22701                
040100        MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                
040200        MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                               
040300     ELSE                                                                 
040400        MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I22701                 
040500        MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                
040600        MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                               
040700     END-IF                                                               
040800                                                                          
040900     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
041000     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
041100     MOVE MFS-IDTRANS TO W-IDTRANS                                        
041200                                                                          
041300     MOVE LOW-VALUE TO MSG-AREA                                           
041400     MOVE 'W4O227N1' TO MFS-IDMOD                                         
041500     MOVE '4227' TO MOD-IDTRANS                                           
041600     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
041700                                                                          
041800     IF EGEN-MID                                                          
041900        CONTINUE                                                          
042000     ELSE                                                                 
042100        IF HOPP-MID                                                       
042200           MOVE ALL '+' TO MID-IDANSK-IN                                  
042300                           MID-IDLEVNR-IN                                 
042400                           MID-IDARTNR-IN                                 
042500                           MID-INPUT                                      
042600           MOVE SPACE TO MFS-KDTRTYP                                      
042700                         MFS-IDPFK                                        
042800        ELSE                                                              
042900           MOVE ALL '+' TO MID-IDANSK-IN                                  
043000                           MID-IDARTNR-IN                                 
043100                           MID-IDLEVNR-IN                                 
043200           MOVE SPACE TO MFS-KDTRTYP                                      
043300           MOVE '7' TO MFS-IDPFK                                          
043400           PERFORM MFS-RENSA-SPAR-FAELT                                   
043500        END-IF                                                            
043600     END-IF                                                               
043700                                                                          
043800     MOVE NEJ TO SW-HOPP                                                  
043900     .                                                                    
044000     EJECT                                                                
044100 B-KOLLA-NYCKLAR SECTION.                                                 
044200     MOVE 'B-KOLLA-NYCKLAR ' TO CURRENT-SECTION                           
044300                                                                          
044400     MOVE ALL '+'           TO MSGI-WMSGINIT                              
044500     MOVE '001'             TO MSGI-KDCALL                                
044600     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
044700     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
044800     MOVE '4227'            TO MSGI-IDTRANS                               
044900     IF EGEN-MID                                                          
045000        IF MID-IDANSK-IN NOT = ALL '+'                                    
045100           MOVE MID-IDANSK-IN  TO MSGI-IDANSK                             
045200        END-IF                                                            
045300        IF MID-IDLEVNR-IN NOT = ALL '+'                                   
045400           MOVE MID-IDLEVNR-IN TO MSGI-IDLEVNR                            
045500        END-IF                                                            
045600        IF MID-IDARTNR-IN NOT = ALL '+'                                   
045700           MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                            
045800        END-IF                                                            
045900     END-IF                                                               
046000     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
046100     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
046200     MOVE SPAR-INDEX TO WS-INDEX                                          
046300     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
046400     MOVE JA TO NYCKLAR-SW                                                
046500                                                                          
046600     MOVE NEJ TO WS-IDANSK-SOEKN                                          
046700     INSPECT MSGI-IDANSK REPLACING LEADING SPACE BY ZERO                  
046800     IF MSGI-IDANSK NUMERIC                                               
046900        MOVE MSGI-IDANSK TO WS-IDANSK                                     
047000        MOVE WS-IDANSK TO WS-IDANSK-KOLL                                  
047100        IF WS-IDANSK-POS1 = ZERO                                          
047200           MOVE WS-IDANSK-POS2 TO WS-IDANSK-POS1                          
047300           MOVE WS-IDANSK-POS3 TO WS-IDANSK-POS2                          
047400           MOVE ZERO TO WS-IDANSK-POS3                                    
047500           MOVE WS-IDANSK-KOLL TO WS-IDANSK-MIN                           
047600           MOVE 9 TO WS-IDANSK-POS3                                       
047700           MOVE WS-IDANSK-KOLL TO WS-IDANSK-MAX                           
047800           MOVE JA TO WS-IDANSK-SOEKN                                     
047900        ELSE                                                              
048000           MOVE WS-IDANSK TO WS-IDANSK-MIN                                
048100                             WS-IDANSK-MAX                                
048200        END-IF                                                            
048300     ELSE                                                                 
048400        MOVE NEJ TO NYCKLAR-SW                                            
048500     END-IF                                                               
048600                                                                          
048700     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
048800     IF MSGI-IDARTNR NUMERIC                                              
048900        MOVE MSGI-IDARTNR TO WS-IDARTNR                                   
049000     ELSE                                                                 
049100        MOVE NEJ TO NYCKLAR-SW                                            
049200     END-IF                                                               
049300                                                                          
049400     MOVE MFS-RENSA-FAELT TO MOD-IDANSK-IN                                
049500     IF MID-IDANSK-IN NOT = ALL '+'                                       
049600       MOVE '7'         TO MFS-IDPFK                                      
049700       MOVE SPACE       TO MFS-KDTRTYP                                    
049800       PERFORM MFS-RENSA-SPAR-FAELT                                       
049900     END-IF                                                               
050000                                                                          
050100     MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-IN                               
050200     IF MID-IDLEVNR-IN NOT = ALL '+'                                      
050300       MOVE '7'         TO MFS-IDPFK                                      
050400       MOVE SPACE       TO MFS-KDTRTYP                                    
050500       PERFORM MFS-RENSA-SPAR-FAELT                                       
050600     END-IF                                                               
050700                                                                          
050800     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
050900     IF MID-IDARTNR-IN NOT = ALL '+'                                      
051000       MOVE '7'         TO MFS-IDPFK                                      
051100       MOVE SPACE       TO MFS-KDTRTYP                                    
051200       PERFORM MFS-RENSA-SPAR-FAELT                                       
051300     END-IF                                                               
051400                                                                          
051500     MOVE LOW-VALUE  TO W-WDA6ISEQ-MIN-X                                  
051600                        W-WDA6JSEQ-MIN-X                                  
051700                        W-WDA6KSEQ-MIN-X                                  
051800                        W-WDA6LSEQ-MIN-X                                  
051900     MOVE HIGH-VALUE TO W-WDA6ISEQ-MAX-X                                  
052000                        W-WDA6JSEQ-MAX-X                                  
052100                        W-WDA6KSEQ-MAX-X                                  
052200                        W-WDA6LSEQ-MAX-X                                  
052300                                                                          
052400     IF NYCKLAR-OK                                                        
052500        IF EGEN-MID OR HOPP-MID                                           
052600           IF MFS-FIRST                                                   
052700              IF (MID-IDANSK-IN = ALL '+')                                
052800              AND (MID-IDLEVNR-IN = ALL '+')                              
052900              AND (MID-IDARTNR-IN = ALL '+')                              
053000                 IF WS-INDEX = 'I' OR 'J' OR 'K' OR 'L'                   
053100                    IF WS-INDEX = 'I'                                     
053200                      MOVE WS-IDANSK-MIN TO SEQI-IDANSK-MIN               
053300                      MOVE WS-IDANSK-MAX TO SEQI-IDANSK-MAX               
053400                    END-IF                                                
053500                    IF WS-INDEX = 'J'                                     
053600                      MOVE WS-IDARTNR TO SEQJ-IDARTNR-MIN                 
053700                                         SEQJ-IDARTNR-MAX                 
053800                    END-IF                                                
053900                    IF WS-INDEX = 'K'                                     
054000                      MOVE WS-IDANSK TO SEQK-IDANSK-MIN                   
054100                                        SEQK-IDANSK-MAX                   
054200                      MOVE SEQK-IDLEVNR-ENTER TO SEQK-IDLEVNR-MIN         
054300                                                 SEQK-IDLEVNR-MAX         
054400                    END-IF                                                
054500                    IF WS-INDEX = 'L'                                     
054600                      MOVE SEQL-IDLEVNR-ENTER TO SEQL-IDLEVNR-MIN         
054700                                                 SEQL-IDLEVNR-MAX         
054800                    END-IF                                                
054900                 ELSE                                                     
055000                    PERFORM BA-VALJ-INDEX                                 
055100                 END-IF                                                   
055200              ELSE                                                        
055300                 PERFORM BA-VALJ-INDEX                                    
055400              END-IF                                                      
055500           END-IF                                                         
055600        ELSE                                                              
055700           PERFORM BA-VALJ-INDEX                                          
055800        END-IF                                                            
055900     END-IF                                                               
056000                                                                          
056100     IF NYCKLAR-OK                                                        
056200        IF WS-INDEX = 'I' OR 'J' OR 'K' OR 'L'                            
056300           CONTINUE                                                       
056400        ELSE                                                              
056500           MOVE NEJ TO NYCKLAR-SW                                         
056600           MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                             
056700           CALL WMEDKONV USING MED-WMEDAREA                               
056800           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
056900        END-IF                                                            
057000     END-IF                                                               
057100                                                                          
057200     IF EGEN-MID OR NYCKLAR-OK                                            
057300        MOVE MSGI-IDANSK TO MOD-IDANSK-UT                                 
057400        INSPECT MOD-IDANSK-UT REPLACING LEADING ZERO BY SPACE             
057500        MOVE MSGI-IDLEVNR TO MOD-IDLEVNR-UT                               
057600        INSPECT MOD-IDANSK-UT REPLACING LEADING ZERO BY SPACE             
057700        MOVE MSGI-IDARTNR TO MOD-IDARTNR-UT                               
057800        INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE            
057900        IF WS-INDEX = 'I'                                                 
058000           MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-UT                         
058100                                   MOD-IDARTNR-UT                         
058200        ELSE                                                              
058300           IF WS-INDEX = 'J'                                              
058400              MOVE MFS-RENSA-FAELT TO MOD-IDANSK-UT                       
058500                                      MOD-IDLEVNR-UT                      
058600           ELSE                                                           
058700              IF WS-INDEX = 'K'                                           
058800                 MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                   
058900              ELSE                                                        
059000                 IF WS-INDEX = 'L'                                        
059100                    MOVE MFS-RENSA-FAELT TO MOD-IDANSK-UT                 
059200                                            MOD-IDARTNR-UT                
059300                 END-IF                                                   
059400              END-IF                                                      
059500           END-IF                                                         
059600        END-IF                                                            
059700     ELSE                                                                 
059800        MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                
059900        CALL WMEDKONV USING MED-WMEDAREA                                  
060000        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
060100        PERFORM MFS-RENSA-FAELT-IN                                        
060200        PERFORM MFS-RENSA-FAELT-UT                                        
060300        PERFORM MFS-RENSA-SPAR-FAELT                                      
060400     END-IF                                                               
060500     .                                                                    
060600     EJECT                                                                
060700 BA-VALJ-INDEX SECTION.                                                   
060800     MOVE 'BA-VALJ-INDEX   ' TO CURRENT-SECTION                           
060900                                                                          
061000     IF MID-IDANSK-IN = ALL '+'                                           
061100        IF MID-IDARTNR-IN = ALL '+'                                       
061200           IF MID-IDLEVNR-IN = ALL '+'                                    
061300              MOVE 'I' TO WS-INDEX                                        
061400              MOVE MSGI-IDANSK TO WS-IDANSK                               
061500              INSPECT WS-IDANSK REPLACING LEADING SPACE BY ZERO           
061600              MOVE WS-IDANSK-MIN TO SEQI-IDANSK-MIN                       
061700              MOVE WS-IDANSK-MAX TO SEQI-IDANSK-MAX                       
061800           ELSE                                                           
061900              MOVE 'L' TO WS-INDEX                                        
062000              MOVE MID-IDLEVNR-IN TO SEQL-IDLEVNR-MIN                     
062100                                     SEQL-IDLEVNR-MAX                     
062200           END-IF                                                         
062300        ELSE                                                              
062400           MOVE 'J' TO WS-INDEX                                           
062500           MOVE MID-IDARTNR-IN TO WS-IDARTNR-NUM                          
062600           INSPECT WS-IDARTNR-NUM REPLACING LEADING SPACE                 
062700                                BY ZERO                                   
062800           MOVE WS-IDARTNR-NUM TO SEQJ-IDARTNR-MIN                        
062900                                  SEQJ-IDARTNR-MAX                        
063000        END-IF                                                            
063100     ELSE                                                                 
063200        IF MID-IDLEVNR-IN = ALL '+'                                       
063300           MOVE 'I' TO WS-INDEX                                           
063400           MOVE MID-IDANSK-IN TO WS-IDANSK                                
063500           INSPECT WS-IDANSK REPLACING LEADING SPACE BY ZERO              
063600           MOVE WS-IDANSK-MIN TO SEQI-IDANSK-MIN                          
063700           MOVE WS-IDANSK-MAX TO SEQI-IDANSK-MAX                          
063800        ELSE                                                              
063900           MOVE 'K' TO WS-INDEX                                           
064000           MOVE MID-IDANSK-IN TO WS-IDANSK                                
064100           INSPECT WS-IDANSK REPLACING LEADING SPACE BY ZERO              
064200           MOVE WS-IDANSK TO SEQK-IDANSK-MIN                              
064300                             SEQK-IDANSK-MAX                              
064400           MOVE MID-IDLEVNR-IN TO SEQK-IDLEVNR-MIN                        
064500                                  SEQK-IDLEVNR-MAX                        
064600        END-IF                                                            
064700     END-IF                                                               
064800     .                                                                    
064900     EJECT                                                                
065000 C-FOERSTA-SIDA SECTION.                                                  
065100     MOVE 'C-FOERSTA-SIDA  ' TO CURRENT-SECTION                           
065200                                                                          
065300     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
065400     CALL WMEDKONV USING MED-WMEDAREA                                     
065500     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
065600     PERFORM MFS-RENSA-FAELT-IN                                           
065700     PERFORM MFS-RENSA-SPAR-FAELT                                         
065800     .                                                                    
065900     EJECT                                                                
066000 D-NAESTA-SIDA SECTION.                                                   
066100     MOVE 'D-NAESTA-SIDA   ' TO CURRENT-SECTION                           
066200                                                                          
066300     IF SPAR-IDTRANS = '4227'                                             
066400       IF SPAR-SISTA-SIDAN = 'J'                                          
066500          MOVE INF-SISTA-SIDAN TO MED-IDMFSFEL                            
066600          CALL WMEDKONV USING MED-WMEDAREA                                
066700          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
066800          IF WS-INDEX = 'I' OR 'J' OR 'K' OR 'L'                          
066900             IF WS-INDEX = 'I'                                            
067000                MOVE HIGH-VALUE           TO W-WDA6ISEQ-MAX-X             
067100                IF SEQI-IDANSK-ENTER NOT NUMERIC                          
067200                   MOVE WS-IDANSK-MIN     TO SEQI-IDANSK-ENTER            
067300                   MOVE WS-IDANSK-MAX     TO SEQI-IDANSK-MAX              
067400                ELSE                                                      
067500                   MOVE SEQI-IDANSK-ENTER TO SEQI-IDANSK-MAX              
067600                END-IF                                                    
067700                MOVE SPAR-WDA6ISEQ-ENTER  TO W-WDA6ISEQ-MIN-X             
067800                IF WS-IDANSK-SOEKN = JA                                   
067900                   MOVE WS-IDANSK-MAX     TO SEQI-IDANSK-MAX              
068000                END-IF                                                    
068100             END-IF                                                       
068200             IF WS-INDEX = 'J'                                            
068300                MOVE HIGH-VALUE            TO W-WDA6JSEQ-MAX-X            
068400                IF SEQJ-IDARTNR-ENTER NOT NUMERIC                         
068500                   MOVE WS-IDARTNR         TO SEQJ-IDARTNR-ENTER          
068600                                              SEQJ-IDARTNR-MAX            
068700                ELSE                                                      
068800                   MOVE SEQJ-IDARTNR-ENTER TO SEQJ-IDARTNR-MAX            
068900                END-IF                                                    
069000                MOVE SPAR-WDA6JSEQ-ENTER   TO W-WDA6JSEQ-MIN-X            
069100             END-IF                                                       
069200             IF WS-INDEX = 'K'                                            
069300                MOVE HIGH-VALUE            TO W-WDA6KSEQ-MAX-X            
069400                IF SEQK-IDANSK-ENTER NOT NUMERIC                          
069500                   MOVE WS-IDANSK         TO SEQK-IDANSK-ENTER            
069600                                             SEQK-IDANSK-MAX              
069700                ELSE                                                      
069800                   MOVE SEQK-IDANSK-ENTER TO SEQK-IDANSK-MAX              
069900                END-IF                                                    
070000                MOVE SPAR-WDA6KSEQ-ENTER  TO W-WDA6KSEQ-MIN-X             
070100                MOVE SEQK-IDLEVNR-ENTER   TO SEQK-IDLEVNR-MAX             
070200             END-IF                                                       
070300             IF WS-INDEX = 'L'                                            
070400                MOVE SPAR-WDA6LSEQ-ENTER TO W-WDA6LSEQ-MIN-X              
070500                MOVE HIGH-VALUE          TO W-WDA6LSEQ-MAX-X              
070600                MOVE SEQL-IDLEVNR-ENTER  TO SEQL-IDLEVNR-MIN              
070700                                            SEQL-IDLEVNR-MAX              
070800             END-IF                                                       
070900          ELSE                                                            
071000             MOVE 'I' TO WS-INDEX                                         
071100             MOVE LOW-VALUE           TO W-WDA6ISEQ-MIN-X                 
071200             MOVE HIGH-VALUE          TO W-WDA6ISEQ-MAX-X                 
071300             MOVE WS-IDANSK           TO SEQI-IDANSK-MIN                  
071400                                         SEQI-IDANSK-MAX                  
071500          END-IF                                                          
071600       ELSE                                                               
071700          IF WS-INDEX = 'I' OR 'J' OR 'K' OR 'L'                          
071800             IF WS-INDEX = 'I'                                            
071900                MOVE HIGH-VALUE          TO W-WDA6ISEQ-MAX-X              
072000                IF SEQI-IDANSK-NEXT NOT NUMERIC                           
072100                   MOVE WS-IDANSK-MIN    TO SEQI-IDANSK-NEXT              
072200                   MOVE WS-IDANSK-MAX    TO SEQI-IDANSK-MAX               
072300                ELSE                                                      
072400                   MOVE SEQI-IDANSK-NEXT TO SEQI-IDANSK-MAX               
072500                END-IF                                                    
072600                MOVE SPAR-WDA6ISEQ-NEXT  TO W-WDA6ISEQ-MIN-X              
072700                IF WS-IDANSK-SOEKN = JA                                   
072800                   MOVE WS-IDANSK-MAX     TO SEQI-IDANSK-MAX              
072900                END-IF                                                    
073000             END-IF                                                       
073100             IF WS-INDEX = 'J'                                            
073200                MOVE HIGH-VALUE          TO W-WDA6JSEQ-MAX-X              
073300                IF SEQJ-IDARTNR-NEXT NOT NUMERIC                          
073400                   MOVE WS-IDARTNR       TO SEQJ-IDARTNR-NEXT             
073500                                            SEQJ-IDARTNR-MAX              
073600                ELSE                                                      
073700                   MOVE SEQJ-IDARTNR-NEXT TO SEQJ-IDARTNR-MAX             
073800                END-IF                                                    
073900                MOVE SPAR-WDA6JSEQ-NEXT  TO W-WDA6JSEQ-MIN-X              
074000             END-IF                                                       
074100             IF WS-INDEX = 'K'                                            
074200                MOVE HIGH-VALUE          TO W-WDA6KSEQ-MAX-X              
074300                IF SEQK-IDANSK-NEXT NOT NUMERIC                           
074400                   MOVE WS-IDANSK        TO SEQK-IDANSK-NEXT              
074500                                            SEQK-IDANSK-MAX               
074600                ELSE                                                      
074700                   MOVE SEQK-IDANSK-NEXT   TO SEQK-IDANSK-MAX             
074800                END-IF                                                    
074900                MOVE SPAR-WDA6KSEQ-NEXT  TO W-WDA6KSEQ-MIN-X              
075000                MOVE SEQK-IDLEVNR-NEXT   TO SEQK-IDLEVNR-MAX              
075100             END-IF                                                       
075200             IF WS-INDEX = 'L'                                            
075300                MOVE SPAR-WDA6LSEQ-NEXT TO W-WDA6LSEQ-MIN-X               
075400                MOVE HIGH-VALUE         TO W-WDA6LSEQ-MAX-X               
075500                MOVE SEQL-IDLEVNR-NEXT   TO SEQL-IDLEVNR-MIN              
075600                                            SEQL-IDLEVNR-MAX              
075700             END-IF                                                       
075800          ELSE                                                            
075900             MOVE 'I' TO WS-INDEX                                         
076000             MOVE LOW-VALUE          TO W-WDA6ISEQ-MIN-X                  
076100             MOVE HIGH-VALUE         TO W-WDA6ISEQ-MAX-X                  
076200             MOVE WS-IDANSK           TO SEQI-IDANSK-MIN                  
076300                                         SEQI-IDANSK-MAX                  
076400          END-IF                                                          
076500        END-IF                                                            
076600     ELSE                                                                 
076700        PERFORM MFS-RENSA-FAELT-IN                                        
076800        PERFORM MFS-RENSA-SPAR-FAELT                                      
076900     END-IF                                                               
077000     .                                                                    
077100     EJECT                                                                
077200 E-SAMMA-SIDA SECTION.                                                    
077300     MOVE 'E-SAMMA-SIDA    ' TO CURRENT-SECTION                           
077400                                                                          
077500     IF SPAR-IDTRANS = '4227'                                             
077600        IF WS-INDEX = 'I' OR 'J' OR 'K' OR 'L'                            
077700           IF WS-INDEX = 'I'                                              
077800              MOVE HIGH-VALUE          TO W-WDA6ISEQ-MAX-X                
077900              IF SEQI-IDANSK-ENTER NOT NUMERIC                            
078000                 MOVE WS-IDANSK-MIN    TO SEQI-IDANSK-ENTER               
078100                 MOVE WS-IDANSK-MAX    TO SEQI-IDANSK-MAX                 
078200              ELSE                                                        
078300                 MOVE SEQI-IDANSK-ENTER TO SEQI-IDANSK-MAX                
078400              END-IF                                                      
078500              MOVE SPAR-WDA6ISEQ-ENTER TO W-WDA6ISEQ-MIN-X                
078600              IF WS-IDANSK-SOEKN = JA                                     
078700                 MOVE WS-IDANSK-MAX     TO SEQI-IDANSK-MAX                
078800              END-IF                                                      
078900           END-IF                                                         
079000           IF WS-INDEX = 'J'                                              
079100              MOVE HIGH-VALUE          TO W-WDA6JSEQ-MAX-X                
079200              IF SEQJ-IDARTNR-ENTER NOT NUMERIC                           
079300                 MOVE WS-IDARTNR       TO SEQJ-IDARTNR-ENTER              
079400                                          SEQJ-IDARTNR-MAX                
079500              ELSE                                                        
079600                 MOVE SEQJ-IDARTNR-ENTER TO SEQJ-IDARTNR-MAX              
079700              END-IF                                                      
079800              MOVE SPAR-WDA6JSEQ-ENTER TO W-WDA6JSEQ-MIN-X                
079900           END-IF                                                         
080000           IF WS-INDEX = 'K'                                              
080100              MOVE HIGH-VALUE          TO W-WDA6KSEQ-MAX-X                
080200              IF SEQK-IDANSK-ENTER NOT NUMERIC                            
080300                 MOVE WS-IDANSK        TO SEQK-IDANSK-ENTER               
080400                                          SEQK-IDANSK-MAX                 
080500              ELSE                                                        
080600                 MOVE SEQK-IDANSK-ENTER TO SEQK-IDANSK-MAX                
080700              END-IF                                                      
080800              MOVE SPAR-WDA6KSEQ-ENTER TO W-WDA6KSEQ-MIN-X                
080900           END-IF                                                         
081000           IF WS-INDEX = 'L'                                              
081100              MOVE HIGH-VALUE          TO W-WDA6LSEQ-MAX-X                
081200              MOVE SPAR-WDA6LSEQ-ENTER TO W-WDA6LSEQ-MIN-X                
081300              MOVE SEQL-IDLEVNR-ENTER  TO SEQL-IDLEVNR-MAX                
081400                                          SEQL-IDLEVNR-MIN                
081500           END-IF                                                         
081600        ELSE                                                              
081700           MOVE 'I' TO WS-INDEX                                           
081800           MOVE LOW-VALUE           TO W-WDA6ISEQ-MIN-X                   
081900           MOVE HIGH-VALUE          TO W-WDA6ISEQ-MAX-X                   
082000           MOVE WS-IDANSK           TO SEQI-IDANSK-MIN                    
082100                                       SEQI-IDANSK-MAX                    
082200        END-IF                                                            
082300                                                                          
082400        IF MID-INPUT = ALL '+'                                            
082500           PERFORM MFS-RENSA-FAELT-IN                                     
082600        ELSE                                                              
082700           MOVE INF-BYT-BILD-TRYCK-PF9 TO MED-IDMFSINF                    
082800           CALL WMEDKONV USING MED-WMEDAREA                               
082900           MOVE MED-MFSINF TO MOD-TEMFSFEL                                
083000           PERFORM EA-MID-INDATA-TILL-MOD                                 
083100        END-IF                                                            
083200     ELSE                                                                 
083300        PERFORM MFS-RENSA-FAELT-IN                                        
083400        PERFORM MFS-RENSA-SPAR-FAELT                                      
083500     END-IF                                                               
083600     .                                                                    
083700     EJECT                                                                
083800 EA-MID-INDATA-TILL-MOD SECTION.                                          
083900     MOVE 'EA-MID-TILL-MOD ' TO CURRENT-SECTION                           
084000                                                                          
084100     MOVE +1 TO INDX                                                      
084200     PERFORM UNTIL INDX > MAX-INDX                                        
084300        IF MID-KDCMDVAL(INDX) NOT = ALL '+'                               
084400           MOVE MID-KDCMDVAL(INDX) TO MOD-KDCMDVAL(INDX)                  
084500           MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMDVAL-ATTR(INDX)          
084600        ELSE                                                              
084700           MOVE MFS-RENSA-FAELT TO MOD-KDCMDVAL(INDX)                     
084800        END-IF                                                            
084900        ADD +1 TO INDX                                                    
085000     END-PERFORM                                                          
085100     .                                                                    
085200     EJECT                                                                
085300 F-LAES-VISA-INFO SECTION.                                                
085400     MOVE 'F-LAES-VISA-INFO' TO CURRENT-SECTION                           
085500                                                                          
085600     PERFORM FA-LAES-GRUNDDATA                                            
085700                                                                          
085800     IF SEGMENT-SAKNAS                                                    
085900        MOVE INF-URVAL-SAKNAS TO MED-IDMFSFEL                             
086000        CALL WMEDKONV USING MED-WMEDAREA                                  
086100        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
086200        PERFORM MFS-RENSA-FAELT-UT                                        
086300        PERFORM MFS-RENSA-SPAR-FAELT                                      
086400        MOVE '002'      TO MSGI-KDCALL                                    
086500        MOVE '4227'     TO SPAR-IDTRANS                                   
086600        MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                 
086700        CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                        
086800     ELSE                                                                 
086900        MOVE +1 TO INDX                                                   
087000        PERFORM FC-SPARA-ENTER-NYCKLAR                                    
087100                                                                          
087200        PERFORM UNTIL INDX > MAX-INDX                                     
087300        OR SEGMENT-SAKNAS                                                 
087400                                                                          
087500              PERFORM FB-VISA-RADDATA                                     
087600                                                                          
087700              IF WS-INDEX = 'I'                                           
087800                 PERFORM IMS-GN-WDA6ISEQ                                  
087900              ELSE                                                        
088000                 IF WS-INDEX = 'J'                                        
088100                    PERFORM IMS-GN-WDA6JSEQ                               
088200                 ELSE                                                     
088300                    IF WS-INDEX = 'K'                                     
088400                       PERFORM IMS-GN-WDA6KSEQ                            
088500                    ELSE                                                  
088600                       IF WS-INDEX = 'L'                                  
088700                          PERFORM IMS-GN-WDA6LSEQ                         
088800                       END-IF                                             
088900                    END-IF                                                
089000                 END-IF                                                   
089100              END-IF                                                      
089200                                                                          
089300        END-PERFORM                                                       
089400                                                                          
089500        PERFORM UNTIL INDX > MAX-INDX                                     
089600          PERFORM MFS-RENSA-RAD-FAELT-UT                                  
089700          ADD 1 TO INDX                                                   
089800        END-PERFORM                                                       
089900                                                                          
090000        IF SEGMENT-FINNS                                                  
090100           PERFORM FD-SPARA-NEXT-NYCKLAR                                  
090200           IF SPAR-SISTA-SIDAN = 'J'                                      
090300              CONTINUE                                                    
090400           ELSE                                                           
090500              MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                   
090600              CALL WMEDKONV USING MED-WMEDAREA                            
090700              MOVE MED-TEMFSINF TO MOD-TEMFSINF                           
090800           END-IF                                                         
090900        ELSE                                                              
091000           MOVE 'J' TO SPAR-SISTA-SIDAN                                   
091100        END-IF                                                            
091200                                                                          
091300        MOVE '002'      TO MSGI-KDCALL                                    
091400        MOVE '4227'     TO SPAR-IDTRANS                                   
091500        MOVE WS-INDEX   TO SPAR-INDEX                                     
091600        MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                 
091700        CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                        
091800     END-IF                                                               
091900     .                                                                    
092000     EJECT                                                                
092100 FA-LAES-GRUNDDATA SECTION.                                               
092200     MOVE 'FA-LAES-GRUNDDAT' TO CURRENT-SECTION                           
092300                                                                          
092400     IF WS-INDEX = 'I'                                                    
092500        PERFORM IMS-GU-WDA6ISEQ                                           
092600     ELSE                                                                 
092700        IF WS-INDEX = 'J'                                                 
092800           PERFORM IMS-GU-WDA6JSEQ                                        
092900        ELSE                                                              
093000           IF WS-INDEX = 'K'                                              
093100              PERFORM IMS-GU-WDA6KSEQ                                     
093200           ELSE                                                           
093300              IF WS-INDEX = 'L'                                           
093400                 PERFORM IMS-GU-WDA6LSEQ                                  
093500              END-IF                                                      
093600           END-IF                                                         
093700        END-IF                                                            
093800     END-IF                                                               
093900     .                                                                    
094000     EJECT                                                                
094100 FB-VISA-RADDATA SECTION.                                                 
094200     MOVE 'FB-VISA-RADDATA ' TO CURRENT-SECTION                           
094300                                                                          
094400     IF MSGI-KDARBTYP-SEC-IDLEV = VOR-IDLEVNR                             
094500     OR MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE                      
094600       MOVE VOR-TIREGDAT-URSP TO MOD-TIREGDAT-URSP(INDX)                  
094700       MOVE VOR-TEVORMRK    TO MOD-TEVORMRK(INDX)                         
094800       MOVE VOR-IDDISTR     TO MOD-IDDISTR (INDX)                         
094900       MOVE VOR-IDKUNDNR    TO MOD-IDKUNDNR(INDX)                         
095000       MOVE VOR-IDORDNR7    TO WS-IDORDNR-KOLL                            
095100       MOVE WS-IDORDNR5     TO MOD-IDORDNR5(INDX)                         
095200***    MOVE VOR-IDANSK      TO MOD-IDANSK(INDX)                           
095300       MOVE VOR-IDLEVNR     TO MOD-IDLEVNR(INDX)                          
095400       MOVE VOR-IDARTNR     TO MOD-IDARTNR(INDX)                          
095500       MOVE VOR-KVBEART-URSP TO MOD-KVBEART-URSP(INDX)                    
095600       MOVE VOR-KDORDBEK    TO MOD-KDORDBEK(INDX)                         
095700       MOVE VOR-IDDC        TO MOD-IDDC(INDX)                             
095800       COMPUTE WS-KVBEART = VOR-KVBEART-Q - VOR-KVPREAVB                  
095900       END-COMPUTE                                                        
096000       MOVE WS-KVBEART      TO MOD-KVBEART(INDX)                          
096100                                                                          
096200       MOVE VOR-IDDISTR     TO W-WDA601-IDDISTR                           
096300       MOVE VOR-IDKUNDNR    TO W-WDA601-IDKUNDNR                          
096400       MOVE VOR-IDKUNDRF    TO W-WDA601-IDKUNDRF                          
096500       MOVE VOR-TIREGDAT-URSP TO W-WDA601-TIREGDAT-URSP                   
096600       MOVE VOR-IDARTNR     TO W-WDA601-IDARTNR                           
096700       MOVE VOR-TIREGTID-URSP TO W-WDA601-TIREGTID-URSP                   
096800       MOVE VOR-TIREGDAT-AVV TO W-WDA601-TIREGDAT-AVV                     
096900       MOVE VOR-TIREGTID-AVV TO W-WDA601-TIREGTID-AVV                     
097000                                                                          
097100       MOVE VOR-TIREGDAT-AVV9 TO SPAR-TIREGDAT-AVV9(INDX)                 
097200       MOVE VOR-TIREGTID-AVV9 TO SPAR-TIREGTID-AVV9(INDX)                 
097300       MOVE VOR-IDANSK      TO SPAR-IDANSK(INDX)                          
097400                                                                          
097500       PERFORM IMS-GET-WDA601                                             
097600       IF SEGMENT-FINNS                                                   
097700          PERFORM IMS-GET-WDA611                                          
097800          IF SEGMENT-FINNS                                                
097900             MOVE 'T'           TO MOD-FLAGGA-VOR(INDX)                   
098000          ELSE                                                            
098100             PERFORM IMS-GET-WDA612                                       
098200             IF SEGMENT-FINNS                                             
098300                MOVE 'T'        TO MOD-FLAGGA-VOR(INDX)                   
098400             ELSE                                                         
098500                MOVE MFS-RENSA-FAELT TO MOD-FLAGGA-VOR(INDX)              
098600             END-IF                                                       
098700          END-IF                                                          
098800          PERFORM IMS-GET-WDA613                                          
098900          IF SEGMENT-FINNS                                                
099000             MOVE 'T'           TO MOD-FLAGGA-SC(INDX)                    
099100          ELSE                                                            
099200             MOVE MFS-RENSA-FAELT TO MOD-FLAGGA-SC(INDX)                  
099300          END-IF                                                          
099400       ELSE                                                               
099500          MOVE MFS-RENSA-FAELT TO MOD-FLAGGA-VOR(INDX)                    
099600                                  MOD-FLAGGA-SC(INDX)                     
099700       END-IF                                                             
099800                                                                          
099900       MOVE VOR-IDARTNR TO W-IDARTNR-D9                                   
100000       MOVE WC-CDC-SE   TO W-IDDC-D9                                      
100100       MOVE VOR-IDLEVNR TO W-IDLEVNR                                      
100200                                                                          
100300       PERFORM IMS-GU-WDD924                                              
100400       IF SEGMENT-FINNS                                                   
100500          MOVE 'S'               TO MOD-FLAGGA-ANSK(INDX)                 
100600       ELSE                                                               
100700         PERFORM IMS-GU-WDD925                                            
100800         IF SEGMENT-FINNS                                                 
100900            MOVE 'T'             TO MOD-FLAGGA-ANSK(INDX)                 
101000         ELSE                                                             
101100            MOVE MFS-RENSA-FAELT TO MOD-FLAGGA-ANSK(INDX)                 
101200         END-IF                                                           
101300       END-IF                                                             
101400                                                                          
101500       MOVE 'S  '     TO W-IDSKYLT                                        
101600       MOVE +1        TO W-IDSID                                          
101700       MOVE VOR-IDARTNR TO WS-IDARTNR-NUM                                 
101800       MOVE WS-IDARTNR-NUM(2:8) TO WS-IDARTNR-ALPHA                       
101900       INSPECT WS-IDARTNR-ALPHA REPLACING LEADING ZERO BY SPACE           
102000       MOVE WS-IDARTNR-ALPHA TO W-IDDOK                                   
102100       MOVE 'LOSNOT'  TO W-IDDOKTYP                                       
102200       PERFORM IMS-GU-WDP512                                              
102300       IF SEGMENT-FINNS                                                   
102400          MOVE 'T'           TO MOD-FLAGGA-LOSN(INDX)                     
102500       ELSE                                                               
102600          MOVE MFS-RENSA-FAELT TO MOD-FLAGGA-LOSN(INDX)                   
102700       END-IF                                                             
102800       ADD 1 TO INDX                                                      
102900     END-IF                                                               
103000     .                                                                    
103100     EJECT                                                                
103200 FC-SPARA-ENTER-NYCKLAR SECTION.                                          
103300     MOVE 'FC-SPARA-ENTER-N' TO CURRENT-SECTION                           
103400                                                                          
103500     IF WS-INDEX = 'I'                                                    
103600        MOVE VOR-IDANSK           TO SEQI-IDANSK-ENTER                    
103700        MOVE VOR-TIREGDAT-AVV9    TO SEQI-TIREGDAT-AVV9-ENTER             
103800        MOVE VOR-TIREGTID-AVV9    TO SEQI-TIREGTID-AVV9-ENTER             
103900     END-IF                                                               
104000     IF WS-INDEX = 'J'                                                    
104100        MOVE VOR-IDARTNR          TO SEQJ-IDARTNR-ENTER                   
104200        MOVE VOR-TIREGDAT-AVV9    TO SEQJ-TIREGDAT-AVV9-ENTER             
104300        MOVE VOR-TIREGTID-AVV9    TO SEQJ-TIREGTID-AVV9-ENTER             
104400     END-IF                                                               
104500     IF WS-INDEX = 'K'                                                    
104600        MOVE VOR-IDANSK           TO SEQK-IDANSK-ENTER                    
104700        MOVE VOR-IDLEVNR          TO SEQK-IDLEVNR-ENTER                   
104800        MOVE VOR-TIREGDAT-AVV9    TO SEQK-TIREGDAT-AVV9-ENTER             
104900        MOVE VOR-TIREGTID-AVV9    TO SEQK-TIREGTID-AVV9-ENTER             
105000     END-IF                                                               
105100     IF WS-INDEX = 'L'                                                    
105200        MOVE VOR-IDLEVNR          TO SEQL-IDLEVNR-ENTER                   
105300        MOVE VOR-TIREGDAT-AVV9    TO SEQL-TIREGDAT-AVV9-ENTER             
105400        MOVE VOR-TIREGTID-AVV9    TO SEQL-TIREGTID-AVV9-ENTER             
105500     END-IF                                                               
105600     .                                                                    
105700     EJECT                                                                
105800 FD-SPARA-NEXT-NYCKLAR SECTION.                                           
105900     MOVE 'FD-SPARA-NEXT-NY' TO CURRENT-SECTION                           
106000                                                                          
106100     IF WS-INDEX = 'I'                                                    
106200        MOVE VOR-IDANSK           TO SEQI-IDANSK-NEXT                     
106300        MOVE VOR-TIREGDAT-AVV9    TO SEQI-TIREGDAT-AVV9-NEXT              
106400        MOVE VOR-TIREGTID-AVV9    TO SEQI-TIREGTID-AVV9-NEXT              
106500     END-IF                                                               
106600     IF WS-INDEX = 'J'                                                    
106700        MOVE VOR-IDARTNR          TO SEQJ-IDARTNR-NEXT                    
106800        MOVE VOR-TIREGDAT-AVV9    TO SEQJ-TIREGDAT-AVV9-NEXT              
106900        MOVE VOR-TIREGTID-AVV9    TO SEQJ-TIREGTID-AVV9-NEXT              
107000     END-IF                                                               
107100     IF WS-INDEX = 'K'                                                    
107200        MOVE VOR-IDANSK           TO SEQK-IDANSK-NEXT                     
107300        MOVE VOR-IDLEVNR          TO SEQK-IDLEVNR-NEXT                    
107400        MOVE VOR-TIREGDAT-AVV9    TO SEQK-TIREGDAT-AVV9-NEXT              
107500        MOVE VOR-TIREGTID-AVV9    TO SEQK-TIREGTID-AVV9-NEXT              
107600     END-IF                                                               
107700     IF WS-INDEX = 'L'                                                    
107800        MOVE VOR-IDLEVNR          TO SEQL-IDLEVNR-NEXT                    
107900        MOVE VOR-TIREGDAT-AVV9    TO SEQL-TIREGDAT-AVV9-NEXT              
108000        MOVE VOR-TIREGTID-AVV9    TO SEQL-TIREGTID-AVV9-NEXT              
108100     END-IF                                                               
108200     .                                                                    
108300     EJECT                                                                
108400 G-KOLLA-HOPP SECTION.                                                    
108500     MOVE 'G-KOLLA-HOPP    ' TO CURRENT-SECTION                           
108600                                                                          
108700     IF WS-INDEX = 'I' OR 'J' OR 'K' OR 'L'                               
108800        IF WS-INDEX = 'I'                                                 
108900           MOVE HIGH-VALUE          TO W-WDA6ISEQ-MAX-X                   
109000           IF SEQI-IDANSK-ENTER NOT NUMERIC                               
109100              MOVE WS-IDANSK-MIN    TO SEQI-IDANSK-ENTER                  
109200              MOVE WS-IDANSK-MAX    TO SEQI-IDANSK-MAX                    
109300           ELSE                                                           
109400              MOVE SEQI-IDANSK-ENTER TO SEQI-IDANSK-MAX                   
109500           END-IF                                                         
109600           MOVE SPAR-WDA6ISEQ-ENTER TO W-WDA6ISEQ-MIN-X                   
109700           IF WS-IDANSK-SOEKN = JA                                        
109800              MOVE WS-IDANSK-MAX     TO SEQI-IDANSK-MAX                   
109900           END-IF                                                         
110000        END-IF                                                            
110100        IF WS-INDEX = 'J'                                                 
110200           MOVE HIGH-VALUE          TO W-WDA6JSEQ-MAX-X                   
110300           IF SEQJ-IDARTNR-ENTER NOT NUMERIC                              
110400              MOVE WS-IDARTNR       TO SEQJ-IDARTNR-ENTER                 
110500                                       SEQJ-IDARTNR-MAX                   
110600           ELSE                                                           
110700              MOVE SEQJ-IDARTNR-ENTER TO SEQJ-IDARTNR-MAX                 
110800           END-IF                                                         
110900           MOVE SPAR-WDA6JSEQ-ENTER TO W-WDA6JSEQ-MIN-X                   
111000        END-IF                                                            
111100        IF WS-INDEX = 'K'                                                 
111200           MOVE HIGH-VALUE          TO W-WDA6KSEQ-MAX-X                   
111300           IF SEQK-IDANSK-ENTER NOT NUMERIC                               
111400              MOVE WS-IDANSK        TO SEQK-IDANSK-ENTER                  
111500                                       SEQK-IDANSK-MAX                    
111600           ELSE                                                           
111700              MOVE SEQK-IDANSK-ENTER TO SEQK-IDANSK-MAX                   
111800           END-IF                                                         
111900           MOVE SPAR-WDA6KSEQ-ENTER TO W-WDA6KSEQ-MIN-X                   
112000        END-IF                                                            
112100        IF WS-INDEX = 'L'                                                 
112200           MOVE HIGH-VALUE          TO W-WDA6LSEQ-MAX-X                   
112300           MOVE SPAR-WDA6LSEQ-ENTER TO W-WDA6LSEQ-MIN-X                   
112400           MOVE SEQL-IDLEVNR-ENTER  TO SEQL-IDLEVNR-MAX                   
112500                                       SEQL-IDLEVNR-MIN                   
112600        END-IF                                                            
112700     ELSE                                                                 
112800        MOVE 'I' TO WS-INDEX                                              
112900        MOVE LOW-VALUE           TO W-WDA6ISEQ-MIN-X                      
113000        MOVE HIGH-VALUE          TO W-WDA6ISEQ-MAX-X                      
113100        MOVE WS-IDANSK           TO SEQI-IDANSK-MIN                       
113200                                    SEQI-IDANSK-MAX                       
113300     END-IF                                                               
113400                                                                          
113500     MOVE JA TO INDATA-SW                                                 
113600     MOVE SPACE TO WS-KDCMDVAL                                            
113700                                                                          
113800     MOVE +1 TO INDX                                                      
113900     PERFORM UNTIL INDX > MAX-INDX                                        
114000        IF MID-KDCMDVAL(INDX) = ALL '+' OR SPACE OR LOW-VALUE             
114100           CONTINUE                                                       
114200        ELSE                                                              
114300           IF MID-KDCMDVAL(INDX) = 'T'                                    
114400              IF WS-KDCMDVAL = SPACE                                      
114500                 MOVE MID-KDCMDVAL(INDX) TO WS-KDCMDVAL                   
114600                 MOVE MFS-ALFA-FAELT-RAETT                                
114700                                        TO MOD-KDCMDVAL-ATTR(INDX)        
114800                 MOVE INDX TO WS-HOPP-INDX                                
114900              ELSE                                                        
115000                 MOVE MFS-ALFA-FAELT-FEL                                  
115100                                  TO MOD-KDCMDVAL-ATTR(INDX)              
115200                 MOVE NEJ TO INDATA-SW                                    
115300              END-IF                                                      
115400           ELSE                                                           
115500              MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMDVAL-ATTR(INDX)        
115600              MOVE NEJ TO INDATA-SW                                       
115700           END-IF                                                         
115800        END-IF                                                            
115900        ADD +1 TO INDX                                                    
116000     END-PERFORM                                                          
116100                                                                          
116200     IF INDATA-OK                                                         
116300        IF WS-HOPP-INDX > ZERO                                            
116400           PERFORM GA-KOLLA-SPAR                                          
116500           IF INDATA-OK                                                   
116600              PERFORM GB-LAS-WDA6                                         
116700              IF INDATA-OK                                                
116800                 CONTINUE                                                 
116900              ELSE                                                        
117000                 PERFORM MFS-ROER-EJ-FAELT-IN                             
117100                 PERFORM MFS-ADD-LAES-IN-FAELT-IN                         
117200                 MOVE INF-URVAL-SAKNAS TO MED-IDMFSFEL                    
117300              END-IF                                                      
117400           ELSE                                                           
117500              PERFORM MFS-ROER-EJ-FAELT-IN                                
117600              PERFORM MFS-ADD-LAES-IN-FAELT-IN                            
117700              MOVE INF-URVAL-SAKNAS TO MED-IDMFSFEL                       
117800           END-IF                                                         
117900        ELSE                                                              
118000           PERFORM MFS-ROER-EJ-FAELT-IN                                   
118100           PERFORM MFS-ADD-LAES-IN-FAELT-IN                               
118200           MOVE ERR-PF9-AND-NO-DATA TO MED-IDMFSFEL                       
118300           MOVE NEJ TO INDATA-SW                                          
118400        END-IF                                                            
118500     ELSE                                                                 
118600        PERFORM MFS-ROER-EJ-FAELT-IN                                      
118700        MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                         
118800     END-IF                                                               
118900                                                                          
119000     IF INDATA-OK                                                         
119100        MOVE VOR-IDDISTR       TO WS-IDDISTR                              
119200        MOVE WS-IDDISTR        TO 4277-MID-IDDISTR                        
119300        MOVE VOR-IDKUNDNR      TO WS-IDKUNDNR                             
119400        MOVE WS-IDKUNDNR       TO 4277-MID-IDKUNDNR                       
119500        MOVE VOR-IDORDNR7      TO 4277-MID-IDORDNR7                       
119600        MOVE VOR-TIREGDAT-URSP TO 4277-MID-TIREGDAT-URSP                  
119700        MOVE VOR-TIREGTID-URSP TO 4277-MID-TIREGTID-URSP                  
119800        MOVE VOR-TIREGDAT-AVV  TO 4277-MID-TIREGDAT-AVV                   
119900        MOVE VOR-TIREGTID-AVV  TO 4277-MID-TIREGTID-AVV                   
120000        MOVE VOR-IDARTNR       TO WS-IDARTNR-NUM                          
120100        MOVE WS-IDARTNR-NUM    TO 4277-MID-IDARTNR                        
120200        MOVE ALL '+'           TO 4277-MID-TELOSNOT                       
120300                                                                          
120400        COMPUTE MSG-KVLL = LENGTH OF 4277-MID-W4I27701-CTX + 17           
120500        MOVE 'W4T277  ' TO MSG-KDTRANS-1                                  
120600        MOVE '4227'     TO MSG-IDTRANS-1                                  
120700        MOVE '1'        TO MSG-KDMFSFOR-1                                 
120800     ELSE                                                                 
120900        IF MED-IDMFSFEL = ERR-PF9-AND-NO-DATA                             
121000           MOVE 'PF9 AND NO DATA' TO MOD-TEMFSFEL                         
121100        ELSE                                                              
121200           CALL WMEDKONV USING MED-WMEDAREA                               
121300           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
121400        END-IF                                                            
121500     END-IF                                                               
121600     .                                                                    
121700     EJECT                                                                
121800 GA-KOLLA-SPAR SECTION.                                                   
121900     MOVE 'GA-KOLLA-SPAR   ' TO CURRENT-SECTION                           
122000                                                                          
122100     IF SPAR-TIREGDAT-AVV9(WS-HOPP-INDX) NUMERIC                          
122200     AND SPAR-TIREGTID-AVV9(WS-HOPP-INDX) NUMERIC                         
122300        IF WS-INDEX = 'I'                                                 
122400           IF SEQI-IDANSK-ENTER NUMERIC                                   
122500              MOVE SEQI-IDANSK-ENTER TO SEQI-IDANSK-MINA                  
122600                                        SEQI-IDANSK-MAXA                  
122700              MOVE SPAR-TIREGDAT-AVV9(WS-HOPP-INDX)                       
122800                                     TO SEQI-TIREGDAT-AVV9-MINA           
122900                                        SEQI-TIREGDAT-AVV9-MAXA           
123000              MOVE SPAR-TIREGTID-AVV9(WS-HOPP-INDX)                       
123100                                     TO SEQI-TIREGTID-AVV9-MINA           
123200                                        SEQI-TIREGTID-AVV9-MAXA           
123300              IF WS-IDANSK-SOEKN = JA                                     
123400                MOVE SPAR-IDANSK(WS-HOPP-INDX) TO SEQI-IDANSK-MINA        
123500                                                 SEQI-IDANSK-MAXA         
123600              END-IF                                                      
123700              PERFORM IMS-GET-WDA6ISEQ                                    
123800              IF SEGMENT-SAKNAS                                           
123900                 MOVE NEJ TO INDATA-SW                                    
124000              END-IF                                                      
124100           ELSE                                                           
124200              MOVE NEJ TO INDATA-SW                                       
124300           END-IF                                                         
124400        END-IF                                                            
124500        IF WS-INDEX = 'J'                                                 
124600           IF SEQJ-IDARTNR-ENTER NUMERIC                                  
124700              MOVE SEQJ-IDARTNR-ENTER TO SEQJ-IDARTNR-MINA                
124800                                         SEQJ-IDARTNR-MAXA                
124900              MOVE SPAR-TIREGDAT-AVV9(WS-HOPP-INDX)                       
125000                                      TO SEQJ-TIREGDAT-AVV9-MINA          
125100                                         SEQJ-TIREGDAT-AVV9-MAXA          
125200              MOVE SPAR-TIREGTID-AVV9(WS-HOPP-INDX)                       
125300                                      TO SEQJ-TIREGTID-AVV9-MINA          
125400                                         SEQJ-TIREGTID-AVV9-MAXA          
125500              PERFORM IMS-GET-WDA6JSEQ                                    
125600              IF SEGMENT-SAKNAS                                           
125700                 MOVE NEJ TO INDATA-SW                                    
125800              END-IF                                                      
125900           ELSE                                                           
126000              MOVE NEJ TO INDATA-SW                                       
126100           END-IF                                                         
126200        END-IF                                                            
126300        IF WS-INDEX = 'K'                                                 
126400           IF SEQK-IDANSK-ENTER NUMERIC                                   
126500              MOVE SEQK-IDANSK-ENTER  TO SEQK-IDANSK-MINA                 
126600                                         SEQK-IDANSK-MAXA                 
126700              MOVE SEQK-IDLEVNR-ENTER TO SEQK-IDLEVNR-MINA                
126800                                         SEQK-IDLEVNR-MAXA                
126900              MOVE SPAR-TIREGDAT-AVV9(WS-HOPP-INDX)                       
127000                                     TO SEQK-TIREGDAT-AVV9-MINA           
127100                                        SEQK-TIREGDAT-AVV9-MAXA           
127200              MOVE SPAR-TIREGTID-AVV9(WS-HOPP-INDX)                       
127300                                     TO SEQK-TIREGTID-AVV9-MINA           
127400                                        SEQK-TIREGTID-AVV9-MAXA           
127500              PERFORM IMS-GET-WDA6KSEQ                                    
127600              IF SEGMENT-SAKNAS                                           
127700                 MOVE NEJ TO INDATA-SW                                    
127800              END-IF                                                      
127900           ELSE                                                           
128000              MOVE NEJ TO INDATA-SW                                       
128100           END-IF                                                         
128200        END-IF                                                            
128300        IF WS-INDEX = 'L'                                                 
128400           MOVE SEQL-IDLEVNR-ENTER TO SEQL-IDLEVNR-MINA                   
128500                                      SEQL-IDLEVNR-MAXA                   
128600           MOVE SPAR-TIREGDAT-AVV9(WS-HOPP-INDX)                          
128700                                   TO SEQL-TIREGDAT-AVV9-MINA             
128800                                      SEQL-TIREGDAT-AVV9-MAXA             
128900           MOVE SPAR-TIREGTID-AVV9(WS-HOPP-INDX)                          
129000                                   TO SEQL-TIREGTID-AVV9-MINA             
129100                                      SEQL-TIREGTID-AVV9-MAXA             
129200           PERFORM IMS-GET-WDA6LSEQ                                       
129300           IF SEGMENT-SAKNAS                                              
129400              MOVE NEJ TO INDATA-SW                                       
129500           END-IF                                                         
129600        END-IF                                                            
129700     ELSE                                                                 
129800        MOVE NEJ TO INDATA-SW                                             
129900        MOVE 'SPAR-EJ-NUM' TO MOD-TEMFSINF                                
130000                              MOD-TEMFSFEL                                
130100     END-IF                                                               
130200     .                                                                    
130300     EJECT                                                                
130400                                                                          
130500 GB-LAS-WDA6 SECTION.                                                     
130600     MOVE 'GB-LAS-WDA6     ' TO CURRENT-SECTION                           
130700                                                                          
130800     MOVE VOR-IDDISTR       TO W-WDA601-IDDISTR                           
130900     MOVE VOR-IDKUNDNR      TO W-WDA601-IDKUNDNR                          
131000     MOVE VOR-IDKUNDRF      TO W-WDA601-IDKUNDRF                          
131100     MOVE VOR-TIREGDAT-URSP TO W-WDA601-TIREGDAT-URSP                     
131200     MOVE VOR-TIREGTID-URSP TO W-WDA601-TIREGTID-URSP                     
131300     MOVE VOR-TIREGDAT-AVV  TO W-WDA601-TIREGDAT-AVV                      
131400     MOVE VOR-TIREGTID-AVV  TO W-WDA601-TIREGTID-AVV                      
131500     MOVE VOR-IDARTNR       TO W-WDA601-IDARTNR                           
131600                                                                          
131700     PERFORM IMS-GET-WDA601                                               
131800     IF SEGMENT-FINNS                                                     
131900        CONTINUE                                                          
132000***     PERFORM IMS-GET-WDA611                                            
132100***     IF SEGMENT-SAKNAS                                                 
132200***        PERFORM IMS-GET-WDA612                                         
132300***        IF SEGMENT-SAKNAS                                              
132400***           PERFORM IMS-GET-WDA613                                      
132500***           IF SEGMENT-SAKNAS                                           
132600***             MOVE 'S  '       TO W-IDSKYLT                             
132700***             MOVE +1          TO W-IDSID                               
132800***             MOVE VOR-IDARTNR TO WS-IDARTNR-NUM                        
132900***             MOVE WS-IDARTNR-NUM(2:8) TO WS-IDARTNR-ALPHA              
133000***             INSPECT WS-IDARTNR-ALPHA                                  
133100***                          REPLACING LEADING ZERO BY SPACE              
133200***             MOVE WS-IDARTNR-ALPHA TO W-IDDOK                          
133300***             MOVE 'VORNOT'    TO W-IDDOKTYP                            
133400***             PERFORM IMS-GU-WDP512                                     
133500***             IF SEGMENT-SAKNAS                                         
133600***                MOVE 'LOSNOT'    TO W-IDDOKTYP                         
133700***                PERFORM IMS-GU-WDP512                                  
133800***                IF SEGMENT-SAKNAS                                      
133900***                   MOVE NEJ TO INDATA-SW                               
134000***                END-IF                                                 
134100***             END-IF                                                    
134200***           END-IF                                                      
134300***        END-IF                                                         
134400***     END-IF                                                            
134500     ELSE                                                                 
134600        MOVE NEJ TO INDATA-SW                                             
134700     END-IF                                                               
134800     .                                                                    
134900     EJECT                                                                
135000 MFS-RENSA-SPAR-FAELT SECTION.                                            
135100                                                                          
135200     MOVE MFS-RENSA-FAELT TO SPAR-INDEX                                   
135300                             SPAR-SISTA-SIDAN                             
135400                             SPAR-NYCKLAR-ENTER                           
135500                             SPAR-NYCKLAR-NEXT                            
135600                             SPAR-TIREGDAT                                
135700                             SPAR-TIREGTID                                
135800                             SPAR-ANSK                                    
135900     .                                                                    
136000     SKIP3                                                                
136100 MFS-RENSA-FAELT-UT SECTION.                                              
136200                                                                          
136300     MOVE +1 TO INDX                                                      
136400     PERFORM UNTIL INDX > MAX-INDX                                        
136500        MOVE MFS-RENSA-FAELT TO MOD-KDCMDVAL(INDX)                        
136600                                MOD-TIREGDAT-URSP(INDX)                   
136700                                MOD-TEVORMRK(INDX)                        
136800                                MOD-IDDISTR(INDX)                         
136900                                MOD-IDKUNDNR(INDX)                        
137000                                MOD-IDORDNR5(INDX)                        
137100                                MOD-IDLEVNR(INDX)                         
137200                                MOD-IDARTNR(INDX)                         
137300                                MOD-KVBEART-URSP(INDX)                    
137400                                MOD-KVBEART(INDX)                         
137500                                MOD-KDORDBEK(INDX)                        
137600                                MOD-IDDC(INDX)                            
137700                                MOD-FLAGGA-VOR(INDX)                      
137800                                MOD-FLAGGA-SC(INDX)                       
137900                                MOD-FLAGGA-ANSK(INDX)                     
138000                                MOD-FLAGGA-LOSN(INDX)                     
138100        MOVE MFS-STAENG-FAELT TO  MOD-KDCMDVAL-ATTR(INDX)                 
138200        ADD +1 TO INDX                                                    
138300     END-PERFORM                                                          
138400     .                                                                    
138500     EJECT                                                                
138600 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
138700                                                                          
138800     MOVE MFS-RENSA-FAELT TO MOD-KDCMDVAL(INDX)                           
138900                             MOD-TIREGDAT-URSP(INDX)                      
139000                             MOD-TEVORMRK(INDX)                           
139100                             MOD-IDDISTR(INDX)                            
139200                             MOD-IDKUNDNR(INDX)                           
139300                             MOD-IDORDNR5(INDX)                           
139400                             MOD-IDLEVNR(INDX)                            
139500                             MOD-IDARTNR(INDX)                            
139600                             MOD-KVBEART-URSP(INDX)                       
139700                             MOD-KVBEART(INDX)                            
139800                             MOD-KDORDBEK(INDX)                           
139900                             MOD-IDDC(INDX)                               
140000                             MOD-FLAGGA-VOR(INDX)                         
140100                             MOD-FLAGGA-SC(INDX)                          
140200                             MOD-FLAGGA-ANSK(INDX)                        
140300                             MOD-FLAGGA-LOSN(INDX)                        
140400     MOVE MFS-STAENG-FAELT TO MOD-KDCMDVAL-ATTR(INDX)                     
140500     .                                                                    
140600     EJECT                                                                
140700 MFS-RENSA-FAELT-IN SECTION.                                              
140800                                                                          
140900     MOVE +1 TO INDX                                                      
141000     PERFORM UNTIL INDX > MAX-INDX                                        
141100        MOVE MFS-RENSA-FAELT TO MOD-KDCMDVAL(INDX)                        
141200        ADD +1 TO INDX                                                    
141300     END-PERFORM                                                          
141400     .                                                                    
141500     SKIP3                                                                
141600 MFS-ADD-LAES-IN-FAELT-IN SECTION.                                        
141700                                                                          
141800     MOVE +1 TO INDX                                                      
141900     PERFORM UNTIL INDX > MAX-INDX                                        
142000        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMDVAL-ATTR(INDX)             
142100        ADD +1 TO INDX                                                    
142200     END-PERFORM                                                          
142300     .                                                                    
142400     SKIP3                                                                
142500 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
142600                                                                          
142700     MOVE +1 TO INDX                                                      
142800     PERFORM UNTIL INDX > MAX-INDX                                        
142900        MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMDVAL(INDX)                      
143000        ADD +1 TO INDX                                                    
143100     END-PERFORM                                                          
143200     .                                                                    
143300     EJECT                                                                
143400* --- IMS SEKTIONER ---                                                   
143500     SKIP3                                                                
143600 IMS-GET-MSG SECTION.                                                     
143700     MOVE '  QC' TO GODK-STATUSKODER                                      
143800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
143900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
144000     PERFORM IMS-STATUSKONTROLL                                           
144100     .                                                                    
144200     SKIP3                                                                
144300 IMS-INSERT-MSG SECTION.                                                  
144400     IF MSGI-IDLAND-SPR = 'SE'                                            
144500       MOVE '0' TO MFS-KDHUVOMR                                           
144600     END-IF                                                               
144700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
144800     MOVE SPACE TO GODK-STATUSKODER                                       
144900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
145000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
145100     PERFORM IMS-STATUSKONTROLL                                           
145200     .                                                                    
145300     SKIP3                                                                
145400 IMS-INSERT-ALT-MSG SECTION.                                              
145500     MOVE SPACE TO GODK-STATUSKODER                                       
145600     CALL CBLTDLI USING ISRT ALT-PCB MSG-IO-AREA                          
145700     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
145800     PERFORM IMS-STATUSKONTROLL                                           
145900     .                                                                    
146000     EJECT                                                                
146100 IMS-GET-WDA601 SECTION.                                                  
146200     MOVE 'GET-WDA601      ' TO CURRENT-IMS-SECTION                       
146300                                                                          
146400     MOVE SPACE               TO ALL-SSA                                  
146500     STRING 'WDA601  (WDA601KY =' W-WDA601KY-X ')'                        
146600          DELIMITED BY SIZE INTO SSA1                                     
146700     MOVE '  GE' TO GODK-STATUSKODER                                      
146800     CALL CBLTDLI USING GU WDA6-PCB DLI-IO-WDA601 SSA1                    
146900     MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
147000     PERFORM IMS-STATUSKONTROLL                                           
147100     .                                                                    
147200     SKIP3                                                                
147300 IMS-GET-WDA611 SECTION.                                                  
147400     MOVE 'GET-WDA611      ' TO CURRENT-IMS-SECTION                       
147500                                                                          
147600     MOVE SPACE               TO ALL-SSA                                  
147700     MOVE 'WDA611 ' TO SSA1                                               
147800     MOVE '  GE' TO GODK-STATUSKODER                                      
147900     CALL CBLTDLI USING GNP WDA6-PCB DLI-IO-WDA611 SSA1                   
148000     MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
148100     PERFORM IMS-STATUSKONTROLL                                           
148200     .                                                                    
148300     SKIP3                                                                
148400 IMS-GET-WDA612 SECTION.                                                  
148500     MOVE 'GET-WDA612      ' TO CURRENT-IMS-SECTION                       
148600                                                                          
148700     MOVE SPACE               TO ALL-SSA                                  
148800     MOVE 'WDA612 ' TO SSA1                                               
148900     MOVE '  GE' TO GODK-STATUSKODER                                      
149000     CALL CBLTDLI USING GNP WDA6-PCB DLI-IO-WDA612 SSA1                   
149100     MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
149200     PERFORM IMS-STATUSKONTROLL                                           
149300     .                                                                    
149400     EJECT                                                                
149500 IMS-GET-WDA613 SECTION.                                                  
149600     MOVE 'GET-WDA613      ' TO CURRENT-IMS-SECTION                       
149700                                                                          
149800     MOVE SPACE               TO ALL-SSA                                  
149900     MOVE 'WDA613 ' TO SSA1                                               
150000     MOVE '  GE' TO GODK-STATUSKODER                                      
150100     CALL CBLTDLI USING GNP WDA6-PCB DLI-IO-WDA613 SSA1                   
150200     MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
150300     PERFORM IMS-STATUSKONTROLL                                           
150400     .                                                                    
150500     SKIP3                                                                
150600 IMS-GU-WDA6ISEQ SECTION.                                                 
150700     MOVE 'GU-WDA6ISEQ     ' TO CURRENT-IMS-SECTION                       
150800                                                                          
150900     MOVE SPACE               TO ALL-SSA                                  
151000     STRING 'WDA601  (WDA6ISEQ>=' W-WDA6ISEQ-MIN-X                        
151100                    '&WDA6ISEQ<=' W-WDA6ISEQ-MAX-X ')'                    
151200          DELIMITED BY SIZE INTO SSA1                                     
151300     MOVE '  GE' TO GODK-STATUSKODER                                      
151400     CALL CBLTDLI USING GU WDA6I-PCB DLI-IO-WDA601 SSA1                   
151500     MOVE WDA6I-STATUS-CODE TO STATUS-WS                                  
151600     PERFORM IMS-STATUSKONTROLL                                           
151700     .                                                                    
151800     SKIP3                                                                
151900 IMS-GET-WDA6ISEQ SECTION.                                                
152000     MOVE 'GET-WDA6ISEQ    ' TO CURRENT-IMS-SECTION                       
152100                                                                          
152200     MOVE SPACE               TO ALL-SSA                                  
152300     STRING 'WDA601  (WDA6ISEQ>=' W-WDA6ISEQ-MINA-X                       
152400                    '&WDA6ISEQ<=' W-WDA6ISEQ-MAXA-X ')'                   
152500          DELIMITED BY SIZE INTO SSA1                                     
152600     MOVE '  GE' TO GODK-STATUSKODER                                      
152700     CALL CBLTDLI USING GU WDA6I-PCB DLI-IO-WDA601 SSA1                   
152800     MOVE WDA6I-STATUS-CODE TO STATUS-WS                                  
152900     PERFORM IMS-STATUSKONTROLL                                           
153000     .                                                                    
153100     SKIP3                                                                
153200 IMS-GN-WDA6ISEQ SECTION.                                                 
153300     MOVE 'GN-WDA6ISEQ     ' TO CURRENT-IMS-SECTION                       
153400                                                                          
153500     MOVE SPACE               TO ALL-SSA                                  
153600     STRING 'WDA601  (WDA6ISEQ>=' W-WDA6ISEQ-MIN-X                        
153700                    '&WDA6ISEQ<=' W-WDA6ISEQ-MAX-X ')'                    
153800          DELIMITED BY SIZE INTO SSA1                                     
153900     MOVE '  GE' TO GODK-STATUSKODER                                      
154000     CALL CBLTDLI USING GN WDA6I-PCB DLI-IO-WDA601 SSA1                   
154100     MOVE WDA6I-STATUS-CODE TO STATUS-WS                                  
154200     PERFORM IMS-STATUSKONTROLL                                           
154300     .                                                                    
154400     EJECT                                                                
154500 IMS-GU-WDA6JSEQ SECTION.                                                 
154600     MOVE 'GU-WDA6JSEQ     ' TO CURRENT-IMS-SECTION                       
154700                                                                          
154800     MOVE SPACE               TO ALL-SSA                                  
154900     STRING 'WDA601  (WDA6JSEQ>=' W-WDA6JSEQ-MIN-X                        
155000                    '&WDA6JSEQ<=' W-WDA6JSEQ-MAX-X ')'                    
155100          DELIMITED BY SIZE INTO SSA1                                     
155200     MOVE '  GE' TO GODK-STATUSKODER                                      
155300     CALL CBLTDLI USING GU WDA6J-PCB DLI-IO-WDA601 SSA1                   
155400     MOVE WDA6J-STATUS-CODE TO STATUS-WS                                  
155500     PERFORM IMS-STATUSKONTROLL                                           
155600     .                                                                    
155700     SKIP3                                                                
155800 IMS-GET-WDA6JSEQ SECTION.                                                
155900     MOVE 'GET-WDA6JSEQ    ' TO CURRENT-IMS-SECTION                       
156000                                                                          
156100     MOVE SPACE               TO ALL-SSA                                  
156200     STRING 'WDA601  (WDA6JSEQ>=' W-WDA6JSEQ-MINA-X                       
156300                    '&WDA6JSEQ<=' W-WDA6JSEQ-MAXA-X ')'                   
156400          DELIMITED BY SIZE INTO SSA1                                     
156500     MOVE '  GE' TO GODK-STATUSKODER                                      
156600     CALL CBLTDLI USING GU WDA6J-PCB DLI-IO-WDA601 SSA1                   
156700     MOVE WDA6J-STATUS-CODE TO STATUS-WS                                  
156800     PERFORM IMS-STATUSKONTROLL                                           
156900     .                                                                    
157000     SKIP3                                                                
157100 IMS-GN-WDA6JSEQ SECTION.                                                 
157200     MOVE 'GN-WDA6JSEQ     ' TO CURRENT-IMS-SECTION                       
157300                                                                          
157400     MOVE SPACE               TO ALL-SSA                                  
157500     STRING 'WDA601  (WDA6JSEQ>=' W-WDA6JSEQ-MIN-X                        
157600                    '&WDA6JSEQ<=' W-WDA6JSEQ-MAX-X ')'                    
157700          DELIMITED BY SIZE INTO SSA1                                     
157800     MOVE '  GE' TO GODK-STATUSKODER                                      
157900     CALL CBLTDLI USING GN WDA6J-PCB DLI-IO-WDA601 SSA1                   
158000     MOVE WDA6J-STATUS-CODE TO STATUS-WS                                  
158100     PERFORM IMS-STATUSKONTROLL                                           
158200     .                                                                    
158300     EJECT                                                                
158400 IMS-GU-WDA6KSEQ SECTION.                                                 
158500     MOVE 'GU-WDA6KSEQ     ' TO CURRENT-IMS-SECTION                       
158600                                                                          
158700     MOVE SPACE               TO ALL-SSA                                  
158800     STRING 'WDA601  (WDA6KSEQ>=' W-WDA6KSEQ-MIN-X                        
158900                    '&WDA6KSEQ<=' W-WDA6KSEQ-MAX-X ')'                    
159000          DELIMITED BY SIZE INTO SSA1                                     
159100     MOVE '  GE' TO GODK-STATUSKODER                                      
159200     CALL CBLTDLI USING GU WDA6K-PCB DLI-IO-WDA601 SSA1                   
159300     MOVE WDA6K-STATUS-CODE TO STATUS-WS                                  
159400     PERFORM IMS-STATUSKONTROLL                                           
159500     .                                                                    
159600     SKIP3                                                                
159700 IMS-GET-WDA6KSEQ SECTION.                                                
159800     MOVE 'GET-WDA6KSEQ    ' TO CURRENT-IMS-SECTION                       
159900                                                                          
160000     MOVE SPACE               TO ALL-SSA                                  
160100     STRING 'WDA601  (WDA6KSEQ>=' W-WDA6KSEQ-MINA-X                       
160200                    '&WDA6KSEQ<=' W-WDA6KSEQ-MAXA-X ')'                   
160300          DELIMITED BY SIZE INTO SSA1                                     
160400     MOVE '  GE' TO GODK-STATUSKODER                                      
160500     CALL CBLTDLI USING GU WDA6K-PCB DLI-IO-WDA601 SSA1                   
160600     MOVE WDA6K-STATUS-CODE TO STATUS-WS                                  
160700     PERFORM IMS-STATUSKONTROLL                                           
160800     .                                                                    
160900     SKIP3                                                                
161000 IMS-GN-WDA6KSEQ SECTION.                                                 
161100     MOVE 'GN-WDA6KSEQ     ' TO CURRENT-IMS-SECTION                       
161200                                                                          
161300     MOVE SPACE               TO ALL-SSA                                  
161400     STRING 'WDA601  (WDA6KSEQ>=' W-WDA6KSEQ-MIN-X                        
161500                    '&WDA6KSEQ<=' W-WDA6KSEQ-MAX-X ')'                    
161600          DELIMITED BY SIZE INTO SSA1                                     
161700     MOVE '  GE' TO GODK-STATUSKODER                                      
161800     CALL CBLTDLI USING GN WDA6K-PCB DLI-IO-WDA601 SSA1                   
161900     MOVE WDA6K-STATUS-CODE TO STATUS-WS                                  
162000     PERFORM IMS-STATUSKONTROLL                                           
162100     .                                                                    
162200     EJECT                                                                
162300 IMS-GU-WDA6LSEQ SECTION.                                                 
162400     MOVE 'GU-WDA6LSEQ     ' TO CURRENT-IMS-SECTION                       
162500                                                                          
162600     MOVE SPACE               TO ALL-SSA                                  
162700     STRING 'WDA601  (WDA6LSEQ>=' W-WDA6LSEQ-MIN-X                        
162800                    '&WDA6LSEQ<=' W-WDA6LSEQ-MAX-X ')'                    
162900          DELIMITED BY SIZE INTO SSA1                                     
163000     MOVE '  GE' TO GODK-STATUSKODER                                      
163100     CALL CBLTDLI USING GU WDA6L-PCB DLI-IO-WDA601 SSA1                   
163200     MOVE WDA6L-STATUS-CODE TO STATUS-WS                                  
163300     PERFORM IMS-STATUSKONTROLL                                           
163400     .                                                                    
163500     SKIP3                                                                
163600 IMS-GET-WDA6LSEQ SECTION.                                                
163700     MOVE 'GET-WDA6LSEQ    ' TO CURRENT-IMS-SECTION                       
163800                                                                          
163900     MOVE SPACE               TO ALL-SSA                                  
164000     STRING 'WDA601  (WDA6LSEQ>=' W-WDA6LSEQ-MINA-X                       
164100                    '&WDA6LSEQ<=' W-WDA6LSEQ-MAXA-X ')'                   
164200          DELIMITED BY SIZE INTO SSA1                                     
164300     MOVE '  GE' TO GODK-STATUSKODER                                      
164400     CALL CBLTDLI USING GU WDA6L-PCB DLI-IO-WDA601 SSA1                   
164500     MOVE WDA6L-STATUS-CODE TO STATUS-WS                                  
164600     PERFORM IMS-STATUSKONTROLL                                           
164700     .                                                                    
164800     SKIP3                                                                
164900 IMS-GN-WDA6LSEQ SECTION.                                                 
165000     MOVE 'GN-WDA6LSEQ     ' TO CURRENT-IMS-SECTION                       
165100                                                                          
165200     MOVE SPACE               TO ALL-SSA                                  
165300     STRING 'WDA601  (WDA6LSEQ>=' W-WDA6LSEQ-MIN-X                        
165400                    '&WDA6LSEQ<=' W-WDA6LSEQ-MAX-X ')'                    
165500          DELIMITED BY SIZE INTO SSA1                                     
165600     MOVE '  GE' TO GODK-STATUSKODER                                      
165700     CALL CBLTDLI USING GN WDA6L-PCB DLI-IO-WDA601 SSA1                   
165800     MOVE WDA6L-STATUS-CODE TO STATUS-WS                                  
165900     PERFORM IMS-STATUSKONTROLL                                           
166000     .                                                                    
166100     SKIP3                                                                
166200 IMS-GU-WDP512 SECTION.                                                   
166300     MOVE 'GU-WDP512       ' TO CURRENT-IMS-SECTION                       
166400                                                                          
166500     MOVE SPACE               TO ALL-SSA                                  
166600     STRING 'WDP501  (WDP501KY =' W-WDP501KY-X ')'                        
166700          DELIMITED BY SIZE INTO SSA1                                     
166800     STRING 'WDP512  (IDSID    =' W-IDSID-X ')'                           
166900          DELIMITED BY SIZE INTO SSA2                                     
167000     MOVE '  GE' TO GODK-STATUSKODER                                      
167100     CALL CBLTDLI USING GU WDP5-PCB DLI-IO-WDP512 SSA1 SSA2               
167200     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
167300     PERFORM IMS-STATUSKONTROLL                                           
167400     .                                                                    
167500     SKIP3                                                                
167600 IMS-GU-WDD924  SECTION.                                                  
167700     MOVE 'GU-WDD924       ' TO CURRENT-IMS-SECTION                       
167800                                                                          
167900     MOVE SPACE                 TO ALL-SSA                                
168000     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
168100            DELIMITED BY SIZE INTO SSA1                                   
168200     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
168300            DELIMITED BY SIZE INTO SSA2                                   
168400     MOVE 'WDD924 '             TO SSA3                                   
168500     MOVE '  GE'                TO GODK-STATUSKODER                       
168600     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD924 SSA1 SSA2 SSA3          
168700     MOVE WDD9-STATUS-CODE      TO STATUS-WS                              
168800     PERFORM IMS-STATUSKONTROLL                                           
168900     .                                                                    
169000 IMS-GU-WDD925  SECTION.                                                  
169100     MOVE 'GU-WDD925       ' TO CURRENT-IMS-SECTION                       
169200                                                                          
169300     MOVE SPACE                 TO ALL-SSA                                
169400     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
169500            DELIMITED BY SIZE INTO SSA1                                   
169600     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
169700            DELIMITED BY SIZE INTO SSA2                                   
169800     MOVE 'WDD925 '             TO SSA3                                   
169900     MOVE '  GE'                TO GODK-STATUSKODER                       
170000     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD925 SSA1 SSA2 SSA3          
170100     MOVE WDD9-STATUS-CODE      TO STATUS-WS                              
170200     PERFORM IMS-STATUSKONTROLL                                           
170300     .                                                                    
170400 IMS-STATUSKONTROLL SECTION.                                              
170500     SET STATUS-IX TO 1                                                   
170600     SEARCH GODK-STATUS                                                   
170700       AT END                                                             
170800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
170900         DELIMITED BY SIZE INTO FELTEXT                                   
171000         CALL FELLOG                                                      
171100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
171200         CONTINUE                                                         
171300     END-SEARCH                                                           
171400     .                                                                    
