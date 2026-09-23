000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4022600.                                                
000300 AUTHOR.         BODIL LINDAHL.                                           
000400 DATE-WRITTEN.   APRIL 2003                                               
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        KÖBILD VOR                                                       
000900*                                                                         
001000*        PROGRAMMET LÄSER WDA6                                            
001100*                   STARTAR W40276                                        
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSAKTION: W4T226                                              
001500*        MID:         W4I22601                                            
001600*                                                                         
001700*    UTDATA.                                                              
001800*        MOD:         W4O22601                                            
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200                                                                          
002300 DATA DIVISION.                                                           
002400     EJECT                                                                
002500 WORKING-STORAGE SECTION.                                                 
002600 77  IDPGM                       PIC X(08)   VALUE 'W4022600'.            
002700 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
002800 77  OBEHORIG                    PIC X       VALUE 'F'.                   
002900 77  SPAR-BEHORIGHETS-KONTR      PIC X       VALUE SPACE.                 
003000 77  JA                          PIC X       VALUE 'J'.                   
003100 77  NEJ                         PIC X       VALUE 'N'.                   
003200 77  INDX                        PIC 9(3)    VALUE ZERO.                  
003300 77  MAX-INDX                    PIC 9(3)    VALUE 14 .                   
003400 77  WS-INDEX                    PIC X       VALUE SPACE.                 
003500 77  WS-IDARTNR                  PIC 9(9)    VALUE ZERO.                  
003600 77  WS-KVBEART                  PIC 9(7)    VALUE ZERO.                  
003700 77  SW-HOPP                     PIC X       VALUE 'N'.                   
003800 77  WS-HOPP-INDX                PIC 9(3)    VALUE 001.                   
003900 77  WS-KDCMDVAL                 PIC X       VALUE SPACE.                 
004000 77  WS-IDDISTR                  PIC 9(4)    VALUE ZERO.                  
004100 77  WS-IDKUNDNR                 PIC 9(6)    VALUE ZERO.                  
004200                                                                          
004300 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004400     88  NYCKLAR-OK                          VALUE 'J'.                   
004500     88  NYCKLAR-FEL                         VALUE 'N'.                   
004600                                                                          
004700 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004800     88  INDATA-OK                           VALUE 'J'.                   
004900     88  INDATA-FEL                          VALUE 'N'.                   
005000     EJECT                                                                
005100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005200     88  EGEN-MID                            VALUE '4226'.                
005300     88  HOPP-MID                            VALUE '4276'.                
005400     88  GODK-MID                            VALUE '4276'.                
005500                                                                          
005600*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
005700 01  GENERELLA-SUBPROGRAM.                                                
005800     03  WSECURIT                PIC X(8)    VALUE 'WSECURIT'.            
005900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006300     EJECT                                                                
006400*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
006500*01 -COPY WMEDAREA                                                        
006600     EJECT                                                                
006700*    --- PARAMETRAR TILL SUBPROGRAM WSECURIT                              
006800*   -COPY WSECAREA                                                        
006900     EJECT                                                                
007000 01  MESSAGE-CODES.                                                       
007100     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
007200     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
007300     03  INF-SISTA-SIDAN         PIC X(3)    VALUE '115'.                 
007400     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
007500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
007600     03  ERR-OBEHORIG            PIC X(3)    VALUE '405'.                 
007700     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
007800     03  INF-URVAL-SAKNAS        PIC X(3)    VALUE '005'.                 
007900     03  INF-BYT-BILD-TRYCK-PF9  PIC X(3)    VALUE '127'.                 
008000     EJECT                                                                
008100 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008200     SKIP3                                                                
008300*01 -COPY WMSGINIT                                                        
008400     EJECT                                                                
008500 01  SPAR-AREA.                                                           
008600     03  SPAR-IDTRANS                 PIC X(4)  VALUE '4226'.             
008700     03  SPAR-INDEX                   PIC X     VALUE SPACE.              
008800     03  SPAR-SISTA-SIDAN             PIC X     VALUE SPACE.              
008900                                                                          
009000     03  SPAR-DAT-TID-RAD OCCURS 14.                                      
009100         05  SPAR-TIREGDAT-AVV-RAD         PIC S9(7) COMP-3.              
009200         05  SPAR-TIREGTID-AVV-RAD         PIC S9(9) COMP-3.              
009300         05  SPAR-TIREGDAT-AVV9-RAD        PIC S9(7) COMP-3.              
009400         05  SPAR-TIREGTID-AVV9-RAD        PIC S9(9) COMP-3.              
009500         05  SPAR-TIREGDAT-URSP-RAD        PIC S9(7) COMP-3.              
009600         05  SPAR-TIREGTID-URSP-RAD        PIC S9(9) COMP-3.              
009700                                                                          
009800     03  SPAR-NYCKLAR-ENTER           PIC X(25) VALUE SPACE.              
009900     03  SPAR-WDA6ESEQ-ENTER REDEFINES SPAR-NYCKLAR-ENTER.                
010000         05  SEQE-IDDISTR-ENTER       PIC S9(5) COMP-3.                   
010100         05  SEQE-TIREGDAT-AVV9-ENTER PIC S9(7) COMP-3.                   
010200         05  SEQE-TIREGTID-AVV9-ENTER PIC S9(9) COMP-3.                   
010300     03  SPAR-WDA6FSEQ-ENTER REDEFINES SPAR-NYCKLAR-ENTER.                
010400         05  SEQF-IDDISTR-ENTER       PIC S9(5) COMP-3.                   
010500         05  SEQF-IDKUNDNR-ENTER      PIC S9(7) COMP-3.                   
010600         05  SEQF-TIREGDAT-AVV9-ENTER PIC S9(7) COMP-3.                   
010700         05  SEQF-TIREGTID-AVV9-ENTER PIC S9(9) COMP-3.                   
010800     03  SPAR-WDA6GSEQ-ENTER REDEFINES SPAR-NYCKLAR-ENTER.                
010900         05  SEQG-IDDISTR-ENTER       PIC S9(5) COMP-3.                   
011000         05  SEQG-IDKUNDNR-ENTER      PIC S9(7) COMP-3.                   
011100         05  SEQG-IDARTNR-ENTER       PIC S9(9) COMP-3.                   
011200         05  SEQG-TIREGDAT-AVV9-ENTER PIC S9(7) COMP-3.                   
011300         05  SEQG-TIREGTID-AVV9-ENTER PIC S9(9) COMP-3.                   
011400     03  SPAR-WDA6HSEQ-ENTER REDEFINES SPAR-NYCKLAR-ENTER.                
011500         05  SEQH-IDDISTR-ENTER       PIC S9(5) COMP-3.                   
011600         05  SEQH-IDARTNR-ENTER       PIC S9(9) COMP-3.                   
011700         05  SEQH-TIREGDAT-AVV9-ENTER PIC S9(7) COMP-3.                   
011800         05  SEQH-TIREGTID-AVV9-ENTER PIC S9(9) COMP-3.                   
011900                                                                          
012000     03  SPAR-NYCKLAR-NEXT              PIC X(25)   VALUE SPACE.          
012100     03  SPAR-WDA6ESEQ-NEXT  REDEFINES SPAR-NYCKLAR-NEXT.                 
012200         05  SEQE-IDDISTR-NEXT        PIC S9(5) COMP-3.                   
012300         05  SEQE-TIREGDAT-AVV9-NEXT  PIC S9(7) COMP-3.                   
012400         05  SEQE-TIREGTID-AVV9-NEXT  PIC S9(9) COMP-3.                   
012500     03  SPAR-WDA6FSEQ-NEXT  REDEFINES SPAR-NYCKLAR-NEXT.                 
012600         05  SEQF-IDDISTR-NEXT        PIC S9(5) COMP-3.                   
012700         05  SEQF-IDKUNDNR-NEXT       PIC S9(7) COMP-3.                   
012800         05  SEQF-TIREGDAT-AVV9-NEXT  PIC S9(7) COMP-3.                   
012900         05  SEQF-TIREGTID-AVV9-NEXT  PIC S9(9) COMP-3.                   
013000     03  SPAR-WDA6GSEQ-NEXT  REDEFINES SPAR-NYCKLAR-NEXT.                 
013100         05  SEQG-IDDISTR-NEXT        PIC S9(5) COMP-3.                   
013200         05  SEQG-IDKUNDNR-NEXT       PIC S9(7) COMP-3.                   
013300         05  SEQG-IDARTNR-NEXT        PIC S9(9) COMP-3.                   
013400         05  SEQG-TIREGDAT-AVV9-NEXT  PIC S9(7) COMP-3.                   
013500         05  SEQG-TIREGTID-AVV9-NEXT  PIC S9(9) COMP-3.                   
013600     03  SPAR-WDA6HSEQ-NEXT  REDEFINES SPAR-NYCKLAR-NEXT.                 
013700         05  SEQH-IDDISTR-NEXT        PIC S9(5) COMP-3.                   
013800         05  SEQH-IDARTNR-NEXT        PIC S9(9) COMP-3.                   
013900         05  SEQH-TIREGDAT-AVV9-NEXT  PIC S9(7) COMP-3.                   
014000         05  SEQH-TIREGTID-AVV9-NEXT  PIC S9(9) COMP-3.                   
014100     EJECT                                                                
014200*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
014300 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
014400*01  MID -COPY W4I22601                                                   
014500     EJECT                                                                
014600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
014700*01  -COPY WMSGAREA                                                       
014800     EJECT                                                                
014900*    --- FÖR HOPP TILL 4276                                               
015000       05  4276-MID REDEFINES MSG-MID-OUT.                                
015100*          07  -COPY W4I27601 -PRE 4276-                                  
015200     EJECT                                                                
015300     03  MOD REDEFINES MSG-AREA.                                          
015400*      05  -COPY W4O22601                                                 
015500     EJECT                                                                
015600 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
015700*01  -COPY WMFSAREA                                                       
015800     EJECT                                                                
015900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
016000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016100     SKIP3                                                                
016200 01  NYCKLAR-TILL-DLI.                                                    
016300     03  W-WDA601KY-X.                                                    
016400         05  W-WDA601-IDDISTR        PIC S9(5) VALUE ZERO COMP-3.         
016500         05  W-WDA601-IDKUNDNR       PIC S9(7) VALUE ZERO COMP-3.         
016600         05  W-WDA601-IDKUNDRF       PIC X(10) VALUE SPACE.               
016700         05  W-WDA601-TIREGDAT-URSP  PIC S9(7) VALUE ZERO COMP-3.         
016800         05  W-WDA601-IDARTNR        PIC S9(9) VALUE ZERO COMP-3.         
016900         05  W-WDA601-TIREGTID-URSP  PIC S9(9) VALUE ZERO COMP-3.         
017000         05  W-WDA601-TIREGDAT-AVV   PIC S9(7) VALUE ZERO COMP-3.         
017100         05  W-WDA601-TIREGTID-AVV   PIC S9(9) VALUE ZERO COMP-3.         
017200                                                                          
017300     03  W-WDA6ESEQ-MIN-X.                                                
017400         05  SEQE-IDDISTR-MIN        PIC S9(5) VALUE ZERO COMP-3.         
017500         05  SEQE-TIREGDAT-AVV9-MIN  PIC S9(7) VALUE ZERO COMP-3.         
017600         05  SEQE-TIREGTID-AVV9-MIN  PIC S9(9) VALUE ZERO COMP-3.         
017700     03  W-WDA6ESEQ-MAX-X.                                                
017800         05  SEQE-IDDISTR-MAX        PIC S9(5) VALUE ZERO COMP-3.         
017900         05  SEQE-TIREGDAT-AVV9-MAX  PIC S9(7) VALUE ZERO COMP-3.         
018000         05  SEQE-TIREGTID-AVV9-MAX  PIC S9(9) VALUE ZERO COMP-3.         
018100     EJECT                                                                
018200     03  W-WDA6FSEQ-MIN-X.                                                
018300         05  SEQF-IDDISTR-MIN        PIC S9(5) VALUE ZERO COMP-3.         
018400         05  SEQF-IDKUNDNR-MIN       PIC S9(7) VALUE ZERO COMP-3.         
018500         05  SEQF-TIREGDAT-AVV9-MIN  PIC S9(7) VALUE ZERO COMP-3.         
018600         05  SEQF-TIREGTID-AVV9-MIN  PIC S9(9) VALUE ZERO COMP-3.         
018700     03  W-WDA6FSEQ-MAX-X.                                                
018800         05  SEQF-IDDISTR-MAX        PIC S9(5) VALUE ZERO COMP-3.         
018900         05  SEQF-IDKUNDNR-MAX       PIC S9(7) VALUE ZERO COMP-3.         
019000         05  SEQF-TIREGDAT-AVV9-MAX  PIC S9(7) VALUE ZERO COMP-3.         
019100         05  SEQF-TIREGTID-AVV9-MAX  PIC S9(9) VALUE ZERO COMP-3.         
019200                                                                          
019300     03  W-WDA6GSEQ-MIN-X.                                                
019400         05  SEQG-IDDISTR-MIN        PIC S9(5) VALUE ZERO COMP-3.         
019500         05  SEQG-IDKUNDNR-MIN       PIC S9(7) VALUE ZERO COMP-3.         
019600         05  SEQG-IDARTNR-MIN        PIC S9(9) VALUE ZERO COMP-3.         
019700         05  SEQG-TIREGDAT-AVV9-MIN  PIC S9(7) VALUE ZERO COMP-3.         
019800         05  SEQG-TIREGTID-AVV9-MIN  PIC S9(9) VALUE ZERO COMP-3.         
019900     EJECT                                                                
020000     03  W-WDA6GSEQ-MAX-X.                                                
020100         05  SEQG-IDDISTR-MAX        PIC S9(5) VALUE ZERO COMP-3.         
020200         05  SEQG-IDKUNDNR-MAX       PIC S9(7) VALUE ZERO COMP-3.         
020300         05  SEQG-IDARTNR-MAX        PIC S9(9) VALUE ZERO COMP-3.         
020400         05  SEQG-TIREGDAT-AVV9-MAX  PIC S9(7) VALUE ZERO COMP-3.         
020500         05  SEQG-TIREGTID-AVV9-MAX  PIC S9(9) VALUE ZERO COMP-3.         
020600                                                                          
020700     03  W-WDA6HSEQ-MIN-X.                                                
020800         05  SEQH-IDDISTR-MIN         PIC S9(5) COMP-3.                   
020900         05  SEQH-IDARTNR-MIN         PIC S9(9) COMP-3.                   
021000         05  SEQH-TIREGDAT-AVV9-MIN  PIC S9(7) VALUE ZERO COMP-3.         
021100         05  SEQH-TIREGTID-AVV9-MIN  PIC S9(9) VALUE ZERO COMP-3.         
021200     03  W-WDA6HSEQ-MAX-X.                                                
021300         05  SEQH-IDDISTR-MAX         PIC S9(5) COMP-3.                   
021400         05  SEQH-IDARTNR-MAX         PIC S9(9) COMP-3.                   
021500         05  SEQH-TIREGDAT-AVV9-MAX  PIC S9(7) VALUE ZERO COMP-3.         
021600         05  SEQH-TIREGTID-AVV9-MAX  PIC S9(9) VALUE ZERO COMP-3.         
021700                                                                          
021800     03  W-WDA6ESEQ-MINA-X.                                               
021900         05  SEQE-IDDISTR-MINA        PIC S9(5) VALUE ZERO COMP-3.        
022000         05  SEQE-TIREGDAT-AVV9-MINA  PIC S9(7) VALUE ZERO COMP-3.        
022100         05  SEQE-TIREGTID-AVV9-MINA  PIC S9(9) VALUE ZERO COMP-3.        
022200     03  W-WDA6ESEQ-MAXA-X.                                               
022300         05  SEQE-IDDISTR-MAXA        PIC S9(5) VALUE ZERO COMP-3.        
022400         05  SEQE-TIREGDAT-AVV9-MAXA  PIC S9(7) VALUE ZERO COMP-3.        
022500         05  SEQE-TIREGTID-AVV9-MAXA  PIC S9(9) VALUE ZERO COMP-3.        
022600     EJECT                                                                
022700     03  W-WDA6FSEQ-MINA-X.                                               
022800         05  SEQF-IDDISTR-MINA        PIC S9(5) VALUE ZERO COMP-3.        
022900         05  SEQF-IDKUNDNR-MINA       PIC S9(7) VALUE ZERO COMP-3.        
023000         05  SEQF-TIREGDAT-AVV9-MINA  PIC S9(7) VALUE ZERO COMP-3.        
023100         05  SEQF-TIREGTID-AVV9-MINA  PIC S9(9) VALUE ZERO COMP-3.        
023200     03  W-WDA6FSEQ-MAXA-X.                                               
023300         05  SEQF-IDDISTR-MAXA        PIC S9(5) VALUE ZERO COMP-3.        
023400         05  SEQF-IDKUNDNR-MAXA       PIC S9(7) VALUE ZERO COMP-3.        
023500         05  SEQF-TIREGDAT-AVV9-MAXA  PIC S9(7) VALUE ZERO COMP-3.        
023600         05  SEQF-TIREGTID-AVV9-MAXA  PIC S9(9) VALUE ZERO COMP-3.        
023700                                                                          
023800     03  W-WDA6GSEQ-MINA-X.                                               
023900         05  SEQG-IDDISTR-MINA        PIC S9(5) VALUE ZERO COMP-3.        
024000         05  SEQG-IDKUNDNR-MINA       PIC S9(7) VALUE ZERO COMP-3.        
024100         05  SEQG-IDARTNR-MINA        PIC S9(9) VALUE ZERO COMP-3.        
024200         05  SEQG-TIREGDAT-AVV9-MINA  PIC S9(7) VALUE ZERO COMP-3.        
024300         05  SEQG-TIREGTID-AVV9-MINA  PIC S9(9) VALUE ZERO COMP-3.        
024400                                                                          
024500     03  W-WDA6GSEQ-MAXA-X.                                               
024600         05  SEQG-IDDISTR-MAXA        PIC S9(5) VALUE ZERO COMP-3.        
024700         05  SEQG-IDKUNDNR-MAXA       PIC S9(7) VALUE ZERO COMP-3.        
024800         05  SEQG-IDARTNR-MAXA        PIC S9(9) VALUE ZERO COMP-3.        
024900         05  SEQG-TIREGDAT-AVV9-MAXA  PIC S9(7) VALUE ZERO COMP-3.        
025000         05  SEQG-TIREGTID-AVV9-MAXA  PIC S9(9) VALUE ZERO COMP-3.        
025100                                                                          
025200     03  W-WDA6HSEQ-MINA-X.                                               
025300         05  SEQH-IDDISTR-MINA        PIC S9(5) COMP-3.                   
025400         05  SEQH-IDARTNR-MINA        PIC S9(9) COMP-3.                   
025500         05  SEQH-TIREGDAT-AVV9-MINA  PIC S9(7) VALUE ZERO COMP-3.        
025600         05  SEQH-TIREGTID-AVV9-MINA  PIC S9(9) VALUE ZERO COMP-3.        
025700     03  W-WDA6HSEQ-MAXA-X.                                               
025800         05  SEQH-IDDISTR-MAXA        PIC S9(5) COMP-3.                   
025900         05  SEQH-IDARTNR-MAXA        PIC S9(9) COMP-3.                   
026000         05  SEQH-TIREGDAT-AVV9-MAXA  PIC S9(7) VALUE ZERO COMP-3.        
026100         05  SEQH-TIREGTID-AVV9-MAXA  PIC S9(9) VALUE ZERO COMP-3.        
026200     EJECT                                                                
026300*    --- STATUS-KOD FRÅN IMS                                              
026400 01  STATUS-WS                   PIC XX.                                  
026500     88  SEGMENT-FINNS                       VALUE '  '.                  
026600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
026700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
026800     SKIP2                                                                
026900 01  GODK-STATUSKODER.                                                    
027000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
027100     SKIP3                                                                
027200 01  SSA1                        PIC X(96).                               
027300 01  SSA2                        PIC X(96).                               
027400     EJECT                                                                
027500*    --- IMS FUNKTIONSKODER                                               
027600*01  -COPY W0003                                                          
027700     EJECT                                                                
027800*    ---  DLI INPUT-OUTPUT AREA                                           
027900                                                                          
028000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA601'.                      
028100 01  DLI-IO-WDA601.                                                       
028200*    03  -COPY WDA601                                                     
028300     EJECT                                                                
028400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA612'.                      
028500 01  DLI-IO-WDA612.                                                       
028600*    03  -COPY WDA612                                                     
028700     EJECT                                                                
028800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA613'.                      
028900 01  DLI-IO-WDA613.                                                       
029000*    03  -COPY WDA613                                                     
029100     EJECT                                                                
029200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA614'.                      
029300 01  DLI-IO-WDA614.                                                       
029400*    03  -COPY WDA614                                                     
029500     EJECT                                                                
029600 LINKAGE SECTION.                                                         
029700*01  -COPY W0009   -PRE MSG-                                              
029800     EJECT                                                                
029900*01  -COPY W0009   -PRE ALT-                                              
030000     EJECT                                                                
030100*01  -COPY W0008   -PRE WDP7-                                             
030200     05  FILLER                  PIC X.                                   
030300     EJECT                                                                
030400*01  -COPY W0008   -PRE WDA6-                                             
030500     05  FILLER                  PIC X.                                   
030600     EJECT                                                                
030700*01  -COPY W0008   -PRE WDA6E-                                            
030800     05  FILLER                  PIC X.                                   
030900     EJECT                                                                
031000*01  -COPY W0008   -PRE WDA6F-                                            
031100     05  FILLER                  PIC X.                                   
031200     EJECT                                                                
031300*01  -COPY W0008   -PRE WDA6G-                                            
031400     05  FILLER                  PIC X.                                   
031500     EJECT                                                                
031600*01  -COPY W0008   -PRE WDA6H-                                            
031700     05  FILLER                  PIC X.                                   
031800     EJECT                                                                
031900 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB WDP7-PCB WDA6-PCB              
032000                           WDA6E-PCB WDA6F-PCB WDA6G-PCB                  
032100                           WDA6H-PCB.                                     
032200 MAIN SECTION.                                                            
032300     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB WDP7-PCB WDA6-PCB              
032400                           WDA6E-PCB WDA6F-PCB WDA6G-PCB                  
032500                           WDA6H-PCB.                                     
032600                                                                          
032700     PERFORM IMS-GET-MSG                                                  
032800     IF SEGMENT-FINNS                                                     
032900       PERFORM A-INIT                                                     
033000       PERFORM B-KOLLA-NYCKLAR                                            
033100       IF NYCKLAR-OK                                                      
033200          PERFORM H-KOLLA-BEHORIGHET                                      
033300          IF SPAR-BEHORIGHETS-KONTR = OBEHORIG                            
033400             CALL WMEDKONV USING MED-WMEDAREA                             
033500             MOVE MED-MFSFEL TO MOD-TEMFSFEL                              
033600             PERFORM MFS-RENSA-SPAR-FAELT                                 
033700             PERFORM MFS-RENSA-FAELT-UT                                   
033800          ELSE                                                            
033900             IF MFS-UPDATE                                                
034000                PERFORM K-UPDATE                                          
034100                PERFORM E-SAMMA-SIDA                                      
034200             ELSE                                                         
034300                IF MFS-FIRST                                              
034400                   PERFORM C-FOERSTA-SIDA                                 
034500                ELSE                                                      
034600                   IF MFS-NEXT                                            
034700                      PERFORM D-NAESTA-SIDA                               
034800                   ELSE                                                   
034900                      IF MFS-SPLIT                                        
035000                         PERFORM G-KOLLA-HOPP                             
035100                         IF INDATA-OK                                     
035200                            MOVE JA TO SW-HOPP                            
035300                         END-IF                                           
035400                      ELSE                                                
035500                         PERFORM E-SAMMA-SIDA                             
035600                      END-IF                                              
035700                   END-IF                                                 
035800                END-IF                                                    
035900             END-IF                                                       
036000             IF SW-HOPP = NEJ                                             
036100                PERFORM F-LAES-VISA-INFO                                  
036200             END-IF                                                       
036300          END-IF                                                          
036400       END-IF                                                             
036500       IF SW-HOPP = JA                                                    
036600          PERFORM IMS-INSERT-ALT-MSG                                      
036700       ELSE                                                               
036800          COMPUTE MSG-KVLL = LENGTH OF MOD-W4O22601 + 4                   
036900          PERFORM IMS-INSERT-MSG                                          
037000       END-IF                                                             
037100     END-IF                                                               
037200                                                                          
037300     MOVE ZERO TO RETURN-CODE                                             
037400     GOBACK                                                               
037500     .                                                                    
037600     EJECT                                                                
037700 A-INIT SECTION.                                                          
037800                                                                          
037900     IF MSG-DUBBLA-TRANSKODER                                             
038000        MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I22601                
038100        MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                
038200        MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                               
038300     ELSE                                                                 
038400        MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I22601                 
038500        MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                
038600        MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                               
038700     END-IF                                                               
038800                                                                          
038900     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
039000     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
039100     MOVE MFS-IDTRANS TO W-IDTRANS                                        
039200                                                                          
039300     MOVE LOW-VALUE TO MSG-AREA                                           
039400     MOVE 'W4O226N1' TO MFS-IDMOD                                         
039500     MOVE '4226' TO MOD-IDTRANS                                           
039600     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
039700                                                                          
039800     IF EGEN-MID                                                          
039900        CONTINUE                                                          
040000     ELSE                                                                 
040100        IF HOPP-MID                                                       
040200           MOVE ALL '+' TO MID-IDDISTR-IN                                 
040300                           MID-IDKUNDNR-IN                                
040400                           MID-IDARTNR-IN                                 
040500                           MID-INPUT                                      
040600           MOVE SPACE TO MFS-KDTRTYP                                      
040700                         MFS-IDPFK                                        
040800        ELSE                                                              
040900           MOVE ALL '+' TO MID-IDDISTR-IN                                 
041000                           MID-IDKUNDNR-IN                                
041100                           MID-IDARTNR-IN                                 
041200           MOVE SPACE TO MFS-KDTRTYP                                      
041300           MOVE '7' TO MFS-IDPFK                                          
041400           PERFORM MFS-RENSA-SPAR-FAELT                                   
041500        END-IF                                                            
041600     END-IF                                                               
041700                                                                          
041800     MOVE NEJ TO SW-HOPP                                                  
041900     .                                                                    
042000     EJECT                                                                
042100 B-KOLLA-NYCKLAR SECTION.                                                 
042200                                                                          
042300     MOVE ALL '+'           TO MSGI-WMSGINIT                              
042400     MOVE '001'             TO MSGI-KDCALL                                
042500     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
042600     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
042700     MOVE '4226'            TO MSGI-IDTRANS                               
042800     IF EGEN-MID                                                          
042900        IF MID-IDDISTR-IN NOT = ALL '+'                                   
043000           MOVE MID-IDDISTR-IN  TO MSGI-IDDISTR                           
043100        END-IF                                                            
043200        IF MID-IDKUNDNR-IN NOT = ALL '+'                                  
043300           MOVE MID-IDKUNDNR-IN TO MSGI-IDKUNDNR                          
043400        END-IF                                                            
043500        IF MID-IDARTNR-IN NOT = ALL '+'                                   
043600           MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                            
043700        END-IF                                                            
043800     END-IF                                                               
043900     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
044000     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
044100     MOVE SPAR-INDEX TO WS-INDEX                                          
044200     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
044300     MOVE JA TO NYCKLAR-SW                                                
044400                                                                          
044500     INSPECT MSGI-IDDISTR REPLACING LEADING SPACE BY ZERO                 
044600     IF MSGI-IDDISTR NUMERIC                                              
044700        MOVE MSGI-IDDISTR TO WS-IDDISTR                                   
044800     ELSE                                                                 
044900        MOVE NEJ TO NYCKLAR-SW                                            
045000     END-IF                                                               
045100                                                                          
045200     INSPECT MSGI-IDKUNDNR REPLACING LEADING SPACE BY ZERO                
045300     IF MSGI-IDKUNDNR NUMERIC                                             
045400        MOVE MSGI-IDKUNDNR TO WS-IDKUNDNR                                 
045500     ELSE                                                                 
045600        MOVE NEJ TO NYCKLAR-SW                                            
045700     END-IF                                                               
045800                                                                          
045900     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
046000     IF MSGI-IDARTNR NUMERIC                                              
046100        MOVE MSGI-IDARTNR TO WS-IDARTNR                                   
046200     ELSE                                                                 
046300        MOVE NEJ TO NYCKLAR-SW                                            
046400     END-IF                                                               
046500                                                                          
046600     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
046700     IF MID-IDDISTR-IN NOT = ALL '+'                                      
046800       MOVE '7'         TO MFS-IDPFK                                      
046900       MOVE SPACE       TO MFS-KDTRTYP                                    
047000       PERFORM MFS-RENSA-SPAR-FAELT                                       
047100     END-IF                                                               
047200                                                                          
047300     MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-IN                              
047400     IF MID-IDKUNDNR-IN NOT = ALL '+'                                     
047500       MOVE '7'         TO MFS-IDPFK                                      
047600       MOVE SPACE       TO MFS-KDTRTYP                                    
047700       PERFORM MFS-RENSA-SPAR-FAELT                                       
047800     END-IF                                                               
047900                                                                          
048000     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
048100     IF MID-IDARTNR-IN NOT = ALL '+'                                      
048200       MOVE '7'         TO MFS-IDPFK                                      
048300       MOVE SPACE       TO MFS-KDTRTYP                                    
048400       PERFORM MFS-RENSA-SPAR-FAELT                                       
048500     END-IF                                                               
048600                                                                          
048700     MOVE LOW-VALUE  TO W-WDA6ESEQ-MIN-X                                  
048800                        W-WDA6FSEQ-MIN-X                                  
048900                        W-WDA6GSEQ-MIN-X                                  
049000                        W-WDA6HSEQ-MIN-X                                  
049100     MOVE HIGH-VALUE TO W-WDA6ESEQ-MAX-X                                  
049200                        W-WDA6FSEQ-MAX-X                                  
049300                        W-WDA6GSEQ-MAX-X                                  
049400                        W-WDA6HSEQ-MAX-X                                  
049500                                                                          
049600     IF NYCKLAR-OK                                                        
049700        IF EGEN-MID OR HOPP-MID                                           
049800           IF MFS-FIRST                                                   
049900              IF (MID-IDDISTR-IN = ALL '+')                               
050000              AND (MID-IDKUNDNR-IN = ALL '+')                             
050100              AND (MID-IDARTNR-IN = ALL '+')                              
050200                 IF WS-INDEX = 'E' OR 'F' OR 'G' OR 'H'                   
050300                    IF WS-INDEX = 'E'                                     
050400                      MOVE WS-IDDISTR  TO SEQE-IDDISTR-MIN                
050500                                          SEQE-IDDISTR-MAX                
050600                    END-IF                                                
050700                    IF WS-INDEX = 'F'                                     
050800                      MOVE WS-IDDISTR  TO SEQF-IDDISTR-MIN                
050900                                          SEQF-IDDISTR-MAX                
051000                      MOVE WS-IDKUNDNR TO SEQF-IDKUNDNR-MIN               
051100                                          SEQF-IDKUNDNR-MAX               
051200                    END-IF                                                
051300                    IF WS-INDEX = 'G'                                     
051400                      MOVE WS-IDDISTR  TO SEQG-IDDISTR-MIN                
051500                                          SEQG-IDDISTR-MAX                
051600                      MOVE WS-IDKUNDNR TO SEQG-IDKUNDNR-MIN               
051700                                          SEQG-IDKUNDNR-MAX               
051800                      MOVE WS-IDARTNR  TO SEQG-IDARTNR-MIN                
051900                                          SEQG-IDARTNR-MAX                
052000                    END-IF                                                
052100                    IF WS-INDEX = 'H'                                     
052200                       MOVE SEQH-IDDISTR-ENTER TO SEQH-IDDISTR-MIN        
052300                                                  SEQH-IDDISTR-MAX        
052400                       MOVE SEQH-IDARTNR-ENTER TO                         
052500                                               SEQH-IDARTNR-MIN           
052600                                               SEQH-IDARTNR-MAX           
052700                    END-IF                                                
052800                 ELSE                                                     
052900                    PERFORM BA-VALJ-INDEX                                 
053000                 END-IF                                                   
053100              ELSE                                                        
053200                 PERFORM BA-VALJ-INDEX                                    
053300              END-IF                                                      
053400           END-IF                                                         
053500        ELSE                                                              
053600           PERFORM BA-VALJ-INDEX                                          
053700        END-IF                                                            
053800     END-IF                                                               
053900                                                                          
054000                                                                          
054100     IF EGEN-MID OR NYCKLAR-OK                                            
054200        MOVE MSGI-IDDISTR TO MOD-IDDISTR-UT                               
054300        INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE            
054400        MOVE MSGI-IDKUNDNR TO MOD-IDKUNDNR-UT                             
054500        INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE           
054600        MOVE MSGI-IDARTNR TO MOD-IDARTNR-UT                               
054700        INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE            
054800        IF EGEN-MID AND NYCKLAR-FEL                                       
054900           MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                             
055000           CALL WMEDKONV USING MED-WMEDAREA                               
055100           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
055200        ELSE                                                              
055300          IF WS-INDEX = 'E'                                               
055400             MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-UT                      
055500                                     MOD-IDARTNR-UT                       
055600          ELSE                                                            
055700             IF WS-INDEX = 'F'                                            
055800                MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                    
055900             ELSE                                                         
056000                IF WS-INDEX = 'G'                                         
056100                   CONTINUE                                               
056200                ELSE                                                      
056300                   IF WS-INDEX = 'H'                                      
056400                      MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-UT             
056500                   END-IF                                                 
056600                END-IF                                                    
056700             END-IF                                                       
056800          END-IF                                                          
056900        END-IF                                                            
057000     ELSE                                                                 
057100        MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                
057200        CALL WMEDKONV USING MED-WMEDAREA                                  
057300        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
057400        PERFORM MFS-RENSA-FAELT-IN                                        
057500        PERFORM MFS-RENSA-FAELT-UT                                        
057600        PERFORM MFS-RENSA-SPAR-FAELT                                      
057700     END-IF                                                               
057800     .                                                                    
057900     EJECT                                                                
058000 BA-VALJ-INDEX SECTION.                                                   
058100                                                                          
058200     IF MID-IDDISTR-IN = ALL '+'                                          
058300        MOVE MSGI-IDDISTR TO WS-IDDISTR                                   
058400        INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO                
058500        IF MID-IDKUNDNR-IN = ALL '+'                                      
058600           IF MID-IDARTNR-IN = ALL '+'                                    
058700              MOVE 'E' TO WS-INDEX                                        
058800              MOVE WS-IDDISTR TO SEQE-IDDISTR-MIN                         
058900                                 SEQE-IDDISTR-MAX                         
059000           ELSE                                                           
059100              MOVE 'H' TO WS-INDEX                                        
059200              MOVE MID-IDARTNR-IN TO WS-IDARTNR                           
059300              INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO          
059400              MOVE WS-IDDISTR  TO SEQH-IDDISTR-MIN                        
059500                                  SEQH-IDDISTR-MAX                        
059600              MOVE WS-IDARTNR  TO SEQH-IDARTNR-MIN                        
059700                                  SEQH-IDARTNR-MAX                        
059800           END-IF                                                         
059900        ELSE                                                              
060000           MOVE MID-IDKUNDNR-IN TO WS-IDKUNDNR                            
060100           INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO            
060200           IF MID-IDARTNR-IN = ALL '+'                                    
060300              MOVE 'F' TO WS-INDEX                                        
060400              MOVE WS-IDDISTR  TO SEQF-IDDISTR-MIN                        
060500                                  SEQF-IDDISTR-MAX                        
060600              MOVE WS-IDKUNDNR TO SEQF-IDKUNDNR-MIN                       
060700                                  SEQF-IDKUNDNR-MAX                       
060800           ELSE                                                           
060900              MOVE 'G' TO WS-INDEX                                        
061000              MOVE MID-IDARTNR-IN TO WS-IDARTNR                           
061100              INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO          
061200              MOVE WS-IDDISTR  TO SEQG-IDDISTR-MIN                        
061300                                  SEQG-IDDISTR-MAX                        
061400              MOVE WS-IDKUNDNR TO SEQG-IDKUNDNR-MIN                       
061500                                  SEQG-IDKUNDNR-MAX                       
061600              MOVE WS-IDARTNR  TO SEQG-IDARTNR-MIN                        
061700                                  SEQG-IDARTNR-MAX                        
061800           END-IF                                                         
061900        END-IF                                                            
062000     ELSE                                                                 
062100        IF MID-IDKUNDNR-IN = ALL '+'                                      
062200           IF MID-IDARTNR-IN = ALL '+'                                    
062300              MOVE 'E' TO WS-INDEX                                        
062400              MOVE MID-IDDISTR-IN TO WS-IDDISTR                           
062500              INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO          
062600              MOVE WS-IDDISTR TO SEQE-IDDISTR-MIN                         
062700                                 SEQE-IDDISTR-MAX                         
062800           ELSE                                                           
062900              MOVE 'H' TO WS-INDEX                                        
063000              MOVE MID-IDDISTR-IN TO WS-IDDISTR                           
063100              INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO          
063200              MOVE MID-IDARTNR-IN TO WS-IDARTNR                           
063300              INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO          
063400              MOVE WS-IDDISTR  TO SEQH-IDDISTR-MIN                        
063500                                  SEQH-IDDISTR-MAX                        
063600              MOVE WS-IDARTNR  TO SEQH-IDARTNR-MIN                        
063700                                  SEQH-IDARTNR-MAX                        
063800           END-IF                                                         
063900        ELSE                                                              
064000           IF MID-IDARTNR-IN = ALL '+'                                    
064100              MOVE 'F' TO WS-INDEX                                        
064200              MOVE MID-IDDISTR-IN TO WS-IDDISTR                           
064300              INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO          
064400              MOVE MID-IDKUNDNR-IN TO WS-IDKUNDNR                         
064500              INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO         
064600              MOVE WS-IDDISTR  TO SEQF-IDDISTR-MIN                        
064700                                  SEQF-IDDISTR-MAX                        
064800              MOVE WS-IDKUNDNR TO SEQF-IDKUNDNR-MIN                       
064900                                  SEQF-IDKUNDNR-MAX                       
065000           ELSE                                                           
065100              MOVE 'G' TO WS-INDEX                                        
065200              MOVE MID-IDDISTR-IN TO WS-IDDISTR                           
065300              INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO          
065400              MOVE MID-IDKUNDNR-IN TO WS-IDKUNDNR                         
065500              INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO         
065600              MOVE MID-IDARTNR-IN TO WS-IDARTNR                           
065700              INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO          
065800              MOVE WS-IDDISTR  TO SEQG-IDDISTR-MIN                        
065900                                  SEQG-IDDISTR-MAX                        
066000              MOVE WS-IDKUNDNR TO SEQG-IDKUNDNR-MIN                       
066100                                  SEQG-IDKUNDNR-MAX                       
066200              MOVE WS-IDARTNR  TO SEQG-IDARTNR-MIN                        
066300                                  SEQG-IDARTNR-MAX                        
066400           END-IF                                                         
066500        END-IF                                                            
066600     END-IF                                                               
066700     .                                                                    
066800     EJECT                                                                
066900 C-FOERSTA-SIDA SECTION.                                                  
067000                                                                          
067100     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
067200     CALL WMEDKONV USING MED-WMEDAREA                                     
067300     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
067400     PERFORM MFS-RENSA-FAELT-IN                                           
067500     PERFORM MFS-RENSA-SPAR-FAELT                                         
067600     .                                                                    
067700     EJECT                                                                
067800 D-NAESTA-SIDA SECTION.                                                   
067900                                                                          
068000     IF SPAR-IDTRANS = '4226'                                             
068100       IF SPAR-SISTA-SIDAN = 'J'                                          
068200          MOVE INF-SISTA-SIDAN TO MED-IDMFSFEL                            
068300          CALL WMEDKONV USING MED-WMEDAREA                                
068400          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
068500          IF WS-INDEX = 'E' OR 'F' OR 'G' OR 'H'                          
068600             IF WS-INDEX = 'E'                                            
068700                MOVE HIGH-VALUE           TO W-WDA6ESEQ-MAX-X             
068800                IF SEQE-IDDISTR-ENTER NOT NUMERIC                         
068900                   MOVE WS-IDDISTR        TO SEQE-IDDISTR-ENTER           
069000                                             SEQE-IDDISTR-MAX             
069100                ELSE                                                      
069200                   MOVE SEQE-IDDISTR-ENTER TO SEQE-IDDISTR-MAX            
069300                END-IF                                                    
069400                MOVE SPAR-WDA6ESEQ-ENTER  TO W-WDA6ESEQ-MIN-X             
069500             END-IF                                                       
069600             IF WS-INDEX = 'F'                                            
069700                MOVE HIGH-VALUE            TO W-WDA6FSEQ-MAX-X            
069800                IF SEQF-IDDISTR-ENTER NOT NUMERIC                         
069900                   MOVE WS-IDDISTR         TO SEQF-IDDISTR-ENTER          
070000                                              SEQF-IDDISTR-MAX            
070100                   MOVE WS-IDKUNDNR        TO SEQF-IDKUNDNR-ENTER         
070200                                              SEQF-IDKUNDNR-MAX           
070300                ELSE                                                      
070400                   MOVE SEQF-IDDISTR-ENTER  TO SEQF-IDDISTR-MAX           
070500                   MOVE SEQF-IDKUNDNR-ENTER TO SEQF-IDKUNDNR-MAX          
070600                END-IF                                                    
070700                MOVE SPAR-WDA6FSEQ-ENTER   TO W-WDA6FSEQ-MIN-X            
070800             END-IF                                                       
070900             IF WS-INDEX = 'G'                                            
071000                MOVE HIGH-VALUE            TO W-WDA6GSEQ-MAX-X            
071100                IF SEQG-IDDISTR-ENTER NOT NUMERIC                         
071200                   MOVE WS-IDDISTR        TO SEQG-IDDISTR-ENTER           
071300                                             SEQG-IDDISTR-MAX             
071400                   MOVE WS-IDKUNDNR       TO SEQG-IDKUNDNR-ENTER          
071500                                             SEQG-IDKUNDNR-MAX            
071600                   MOVE WS-IDARTNR        TO SEQG-IDARTNR-ENTER           
071700                                             SEQG-IDARTNR-MAX             
071800                ELSE                                                      
071900                   MOVE SEQG-IDDISTR-ENTER  TO SEQG-IDDISTR-MAX           
072000                   MOVE SEQG-IDKUNDNR-ENTER TO SEQG-IDKUNDNR-MAX          
072100                   MOVE SEQG-IDARTNR-ENTER  TO SEQG-IDARTNR-MAX           
072200                END-IF                                                    
072300                MOVE SPAR-WDA6GSEQ-ENTER TO W-WDA6GSEQ-MIN-X              
072400             END-IF                                                       
072500             IF WS-INDEX = 'H'                                            
072600                IF SEQH-IDDISTR-ENTER NOT NUMERIC                         
072700                   MOVE WS-IDDISTR        TO SEQH-IDDISTR-ENTER           
072800                                             SEQH-IDDISTR-MAX             
072900                   MOVE WS-IDARTNR        TO SEQH-IDARTNR-ENTER           
073000                                             SEQH-IDARTNR-MAX             
073100                ELSE                                                      
073200                   MOVE SEQH-IDDISTR-ENTER  TO SEQH-IDDISTR-MAX           
073300                   MOVE SEQH-IDARTNR-ENTER  TO SEQH-IDARTNR-MAX           
073400                END-IF                                                    
073500                MOVE SPAR-WDA6HSEQ-ENTER TO W-WDA6HSEQ-MIN-X              
073600             END-IF                                                       
073700          ELSE                                                            
073800             MOVE 'E' TO WS-INDEX                                         
073900             MOVE LOW-VALUE           TO W-WDA6ESEQ-MIN-X                 
074000             MOVE HIGH-VALUE          TO W-WDA6ESEQ-MAX-X                 
074100             MOVE WS-IDDISTR          TO SEQE-IDDISTR-MIN                 
074200                                         SEQE-IDDISTR-MAX                 
074300          END-IF                                                          
074400       ELSE                                                               
074500          IF WS-INDEX = 'E' OR 'F' OR 'G' OR 'H'                          
074600             IF WS-INDEX = 'E'                                            
074700                MOVE HIGH-VALUE          TO W-WDA6ESEQ-MAX-X              
074800                IF SEQE-IDDISTR-NEXT NOT NUMERIC                          
074900                   MOVE WS-IDDISTR       TO SEQE-IDDISTR-NEXT             
075000                                            SEQE-IDDISTR-MAX              
075100                ELSE                                                      
075200                   MOVE SEQE-IDDISTR-NEXT TO SEQE-IDDISTR-MAX             
075300                END-IF                                                    
075400                MOVE SPAR-WDA6ESEQ-NEXT  TO W-WDA6ESEQ-MIN-X              
075500             END-IF                                                       
075600             IF WS-INDEX = 'F'                                            
075700                MOVE HIGH-VALUE          TO W-WDA6FSEQ-MAX-X              
075800                IF SEQF-IDDISTR-NEXT NOT NUMERIC                          
075900                   MOVE WS-IDDISTR       TO SEQF-IDDISTR-NEXT             
076000                                            SEQF-IDDISTR-MAX              
076100                   MOVE WS-IDKUNDNR      TO SEQF-IDKUNDNR-NEXT            
076200                                            SEQF-IDKUNDNR-MAX             
076300                ELSE                                                      
076400                   MOVE SEQF-IDDISTR-NEXT  TO SEQF-IDDISTR-MAX            
076500                   MOVE SEQF-IDKUNDNR-NEXT TO SEQF-IDKUNDNR-MAX           
076600                END-IF                                                    
076700                MOVE SPAR-WDA6FSEQ-NEXT  TO W-WDA6FSEQ-MIN-X              
076800             END-IF                                                       
076900             IF WS-INDEX = 'G'                                            
077000                MOVE HIGH-VALUE          TO W-WDA6GSEQ-MAX-X              
077100                IF SEQG-IDDISTR-NEXT NOT NUMERIC                          
077200                   MOVE WS-IDDISTR        TO SEQG-IDDISTR-NEXT            
077300                                             SEQG-IDDISTR-MAX             
077400                   MOVE WS-IDKUNDNR       TO SEQG-IDKUNDNR-NEXT           
077500                                             SEQG-IDKUNDNR-MAX            
077600                   MOVE WS-IDARTNR        TO SEQG-IDARTNR-NEXT            
077700                                             SEQG-IDARTNR-MAX             
077800                ELSE                                                      
077900                   MOVE SEQG-IDDISTR-NEXT  TO SEQG-IDDISTR-MAX            
078000                   MOVE SEQG-IDKUNDNR-NEXT TO SEQG-IDKUNDNR-MAX           
078100                   MOVE SEQG-IDARTNR-NEXT  TO SEQG-IDARTNR-MAX            
078200                END-IF                                                    
078300                MOVE SPAR-WDA6GSEQ-NEXT TO W-WDA6GSEQ-MIN-X               
078400             END-IF                                                       
078500             IF WS-INDEX = 'H'                                            
078600                IF SEQH-IDDISTR-NEXT NOT NUMERIC                          
078700                   MOVE WS-IDDISTR        TO SEQH-IDDISTR-NEXT            
078800                                             SEQH-IDDISTR-MAX             
078900                   MOVE WS-IDARTNR        TO SEQH-IDARTNR-NEXT            
079000                                             SEQH-IDARTNR-MAX             
079100                ELSE                                                      
079200                   MOVE SEQH-IDDISTR-NEXT  TO SEQH-IDDISTR-MAX            
079300                   MOVE SEQH-IDARTNR-NEXT  TO SEQH-IDARTNR-MAX            
079400                END-IF                                                    
079500                MOVE SPAR-WDA6HSEQ-NEXT TO W-WDA6HSEQ-MIN-X               
079600             END-IF                                                       
079700          ELSE                                                            
079800             MOVE 'E' TO WS-INDEX                                         
079900             MOVE LOW-VALUE          TO W-WDA6ESEQ-MIN-X                  
080000             MOVE HIGH-VALUE         TO W-WDA6ESEQ-MAX-X                  
080100             MOVE WS-IDDISTR          TO SEQE-IDDISTR-MIN                 
080200                                         SEQE-IDDISTR-MAX                 
080300          END-IF                                                          
080400        END-IF                                                            
080500        PERFORM MFS-RENSA-FAELT-IN                                        
080600     ELSE                                                                 
080700        PERFORM MFS-RENSA-FAELT-IN                                        
080800        PERFORM MFS-RENSA-SPAR-FAELT                                      
080900     END-IF                                                               
081000     .                                                                    
081100     EJECT                                                                
081200 E-SAMMA-SIDA SECTION.                                                    
081300                                                                          
081400     IF SPAR-IDTRANS = '4226'                                             
081500        IF WS-INDEX = 'E' OR 'F' OR 'G' OR 'H'                            
081600           IF WS-INDEX = 'E'                                              
081700              MOVE HIGH-VALUE          TO W-WDA6ESEQ-MAX-X                
081800              IF SEQE-IDDISTR-ENTER NOT NUMERIC                           
081900                 MOVE WS-IDDISTR       TO SEQE-IDDISTR-ENTER              
082000                                          SEQE-IDDISTR-MAX                
082100              ELSE                                                        
082200                 MOVE SEQE-IDDISTR-ENTER TO SEQE-IDDISTR-MAX              
082300              END-IF                                                      
082400              MOVE SPAR-WDA6ESEQ-ENTER TO W-WDA6ESEQ-MIN-X                
082500           END-IF                                                         
082600           IF WS-INDEX = 'F'                                              
082700              MOVE HIGH-VALUE          TO W-WDA6FSEQ-MAX-X                
082800              IF SEQF-IDDISTR-ENTER NOT NUMERIC                           
082900                 MOVE WS-IDDISTR       TO SEQF-IDDISTR-ENTER              
083000                                          SEQF-IDDISTR-MAX                
083100                 MOVE WS-IDKUNDNR      TO SEQF-IDKUNDNR-ENTER             
083200                                          SEQF-IDKUNDNR-MAX               
083300              ELSE                                                        
083400                 MOVE SEQF-IDDISTR-ENTER  TO SEQF-IDDISTR-MAX             
083500                 MOVE SEQF-IDKUNDNR-ENTER TO SEQF-IDKUNDNR-MAX            
083600              END-IF                                                      
083700              MOVE SPAR-WDA6FSEQ-ENTER TO W-WDA6FSEQ-MIN-X                
083800           END-IF                                                         
083900           IF WS-INDEX = 'G'                                              
084000              MOVE HIGH-VALUE          TO W-WDA6GSEQ-MAX-X                
084100              IF SEQG-IDDISTR-ENTER NOT NUMERIC                           
084200                 MOVE WS-IDDISTR       TO SEQG-IDDISTR-ENTER              
084300                                          SEQG-IDDISTR-MAX                
084400                 MOVE WS-IDKUNDNR      TO SEQG-IDKUNDNR-ENTER             
084500                                          SEQG-IDKUNDNR-MAX               
084600                 MOVE WS-IDARTNR       TO SEQG-IDARTNR-ENTER              
084700                                          SEQG-IDARTNR-MAX                
084800              ELSE                                                        
084900                 MOVE SEQG-IDDISTR-ENTER  TO SEQG-IDDISTR-MAX             
085000                 MOVE SEQG-IDKUNDNR-ENTER TO SEQG-IDKUNDNR-MAX            
085100                 MOVE SEQG-IDARTNR-ENTER  TO SEQG-IDARTNR-MAX             
085200              END-IF                                                      
085300              MOVE SPAR-WDA6GSEQ-ENTER TO W-WDA6GSEQ-MIN-X                
085400           END-IF                                                         
085500           IF WS-INDEX = 'H'                                              
085600              MOVE HIGH-VALUE          TO W-WDA6HSEQ-MAX-X                
085700              IF SEQH-IDDISTR-ENTER NOT NUMERIC                           
085800                 MOVE WS-IDDISTR       TO SEQH-IDDISTR-ENTER              
085900                                          SEQH-IDDISTR-MAX                
086000                 MOVE WS-IDARTNR       TO SEQH-IDARTNR-ENTER              
086100                                          SEQH-IDARTNR-MAX                
086200              ELSE                                                        
086300                 MOVE SEQH-IDDISTR-ENTER  TO SEQH-IDDISTR-MAX             
086400                 MOVE SEQH-IDARTNR-ENTER  TO SEQH-IDARTNR-MAX             
086500              END-IF                                                      
086600              MOVE SPAR-WDA6HSEQ-ENTER TO W-WDA6HSEQ-MIN-X                
086700           END-IF                                                         
086800        ELSE                                                              
086900           MOVE 'E' TO WS-INDEX                                           
087000           MOVE LOW-VALUE           TO W-WDA6ESEQ-MIN-X                   
087100           MOVE HIGH-VALUE          TO W-WDA6ESEQ-MAX-X                   
087200           MOVE WS-IDDISTR          TO SEQE-IDDISTR-MIN                   
087300                                       SEQE-IDDISTR-MAX                   
087400        END-IF                                                            
087500                                                                          
087600        IF MID-INPUT = ALL '+'                                            
087700           PERFORM MFS-RENSA-FAELT-IN                                     
087800        ELSE                                                              
087900           MOVE INF-BYT-BILD-TRYCK-PF9 TO MED-IDMFSINF                    
088000           CALL WMEDKONV USING MED-WMEDAREA                               
088100           MOVE MED-MFSINF TO MOD-TEMFSFEL                                
088200           PERFORM EA-MID-INDATA-TILL-MOD                                 
088300        END-IF                                                            
088400     ELSE                                                                 
088500        PERFORM MFS-RENSA-FAELT-IN                                        
088600        PERFORM MFS-RENSA-SPAR-FAELT                                      
088700     END-IF                                                               
088800     .                                                                    
088900     EJECT                                                                
089000 EA-MID-INDATA-TILL-MOD SECTION.                                          
089100                                                                          
089200     MOVE +1 TO INDX                                                      
089300     PERFORM UNTIL INDX > MAX-INDX                                        
089400        IF MID-KDCMDVAL(INDX) NOT = ALL '+'                               
089500           MOVE MID-KDCMDVAL(INDX) TO MOD-KDCMDVAL(INDX)                  
089600           MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMDVAL-ATTR(INDX)          
089700        ELSE                                                              
089800           MOVE MFS-RENSA-FAELT TO MOD-KDCMDVAL(INDX)                     
089900        END-IF                                                            
090000        ADD +1 TO INDX                                                    
090100     END-PERFORM                                                          
090200     .                                                                    
090300     EJECT                                                                
090400 F-LAES-VISA-INFO SECTION.                                                
090500                                                                          
090600     PERFORM FA-LAES-GRUNDDATA                                            
090700                                                                          
090800     IF SEGMENT-SAKNAS OR                                                 
090900        (NOT (WS-INDEX = 'E' OR 'F' OR 'G' OR 'H' ))                      
091100        MOVE INF-URVAL-SAKNAS TO MED-IDMFSFEL                             
091200        CALL WMEDKONV USING MED-WMEDAREA                                  
091300        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
091400        PERFORM MFS-RENSA-FAELT-UT                                        
091500        PERFORM MFS-RENSA-SPAR-FAELT                                      
091600        MOVE '002'      TO MSGI-KDCALL                                    
091700        MOVE '4226'     TO SPAR-IDTRANS                                   
091800        MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                 
091900        CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                        
092000     ELSE                                                                 
092100        MOVE +1 TO INDX                                                   
092200        PERFORM FC-SPARA-ENTER-NYCKLAR                                    
092300                                                                          
092400        PERFORM UNTIL INDX > MAX-INDX                                     
092500                                                                          
092600           IF SEGMENT-FINNS                                               
092700              PERFORM FB-VISA-RADDATA                                     
092800                                                                          
092900              IF WS-INDEX = 'E'                                           
093000                 PERFORM IMS-GN-WDA6ESEQ                                  
093100              ELSE                                                        
093200                 IF WS-INDEX = 'F'                                        
093300                    PERFORM IMS-GN-WDA6FSEQ                               
093400                 ELSE                                                     
093500                    IF WS-INDEX = 'G'                                     
093600                       PERFORM IMS-GN-WDA6GSEQ                            
093700                    ELSE                                                  
093800                       IF WS-INDEX = 'H'                                  
093900                          PERFORM IMS-GN-WDA6HSEQ                         
094000                       END-IF                                             
094100                    END-IF                                                
094200                 END-IF                                                   
094300              END-IF                                                      
094400           ELSE                                                           
094500              PERFORM MFS-RENSA-RAD-FAELT-UT                              
094600           END-IF                                                         
094700                                                                          
094800           ADD 1 TO INDX                                                  
094900        END-PERFORM                                                       
095000                                                                          
095100        IF SEGMENT-FINNS                                                  
095200           PERFORM FD-SPARA-NEXT-NYCKLAR                                  
095300           IF SPAR-SISTA-SIDAN = 'J'                                      
095400              CONTINUE                                                    
095500           ELSE                                                           
095600              MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                   
095700              CALL WMEDKONV USING MED-WMEDAREA                            
095800              MOVE MED-TEMFSINF TO MOD-TEMFSINF                           
095900           END-IF                                                         
096000        ELSE                                                              
096100           MOVE 'J' TO SPAR-SISTA-SIDAN                                   
096200        END-IF                                                            
096300                                                                          
096400        MOVE '002'      TO MSGI-KDCALL                                    
096500        MOVE '4226'     TO SPAR-IDTRANS                                   
096600        MOVE WS-INDEX   TO SPAR-INDEX                                     
096700        MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                 
096800        CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                        
096900     END-IF                                                               
097000     .                                                                    
097100     EJECT                                                                
097200 FA-LAES-GRUNDDATA SECTION.                                               
097300                                                                          
097400     IF WS-INDEX = 'E'                                                    
097500        PERFORM IMS-GU-WDA6ESEQ                                           
097600     ELSE                                                                 
097700        IF WS-INDEX = 'F'                                                 
097800           PERFORM IMS-GU-WDA6FSEQ                                        
097900        ELSE                                                              
098000           IF WS-INDEX = 'G'                                              
098100              PERFORM IMS-GU-WDA6GSEQ                                     
098200           ELSE                                                           
098300              IF WS-INDEX = 'H'                                           
098400                 PERFORM IMS-GU-WDA6HSEQ                                  
098500              END-IF                                                      
098600           END-IF                                                         
098700        END-IF                                                            
098800     END-IF                                                               
098900     .                                                                    
099000     EJECT                                                                
099100 FB-VISA-RADDATA SECTION.                                                 
099200                                                                          
099300     MOVE VOR-TIREGDAT-URSP TO MOD-TIREGDAT-URSP(INDX)                    
099400     MOVE VOR-TEVORMRK      TO MOD-TEVORMRK(INDX)                         
099500     MOVE VOR-TEVORMRK-SC   TO MOD-TEVORMRK-SC(INDX)                      
099600     MOVE VOR-IDDISTR       TO MOD-IDDISTR (INDX)                         
099700     MOVE VOR-IDKUNDNR      TO MOD-IDKUNDNR(INDX)                         
099800     MOVE VOR-IDORDNR7      TO MOD-IDORDNR7(INDX)                         
099900     MOVE VOR-IDARTNR       TO MOD-IDARTNR(INDX)                          
100000     MOVE VOR-KVBEART-URSP  TO MOD-KVBEART-URSP(INDX)                     
100100     MOVE VOR-KDORDBEK      TO MOD-KDORDBEK(INDX)                         
100200     MOVE VOR-IDDC          TO MOD-IDDC(INDX)                             
100300     COMPUTE WS-KVBEART = VOR-KVBEART-Q - VOR-KVPREAVB                    
100400     END-COMPUTE                                                          
100500     MOVE WS-KVBEART        TO MOD-KVBEART(INDX)                          
100600                                                                          
100700     MOVE VOR-IDDISTR       TO W-WDA601-IDDISTR                           
100800     MOVE VOR-IDKUNDNR      TO W-WDA601-IDKUNDNR                          
100900     MOVE VOR-IDKUNDRF      TO W-WDA601-IDKUNDRF                          
101000     MOVE VOR-TIREGDAT-URSP TO W-WDA601-TIREGDAT-URSP                     
101100     MOVE VOR-IDARTNR       TO W-WDA601-IDARTNR                           
101200     MOVE VOR-TIREGTID-URSP TO W-WDA601-TIREGTID-URSP                     
101300     MOVE VOR-TIREGDAT-AVV  TO W-WDA601-TIREGDAT-AVV                      
101400     MOVE VOR-TIREGTID-AVV  TO W-WDA601-TIREGTID-AVV                      
101500                                                                          
101600     MOVE VOR-TIREGDAT-AVV  TO SPAR-TIREGDAT-AVV-RAD(INDX)                
101700     MOVE VOR-TIREGTID-AVV  TO SPAR-TIREGTID-AVV-RAD(INDX)                
101800     MOVE VOR-TIREGDAT-AVV9 TO SPAR-TIREGDAT-AVV9-RAD(INDX)               
101900     MOVE VOR-TIREGTID-AVV9 TO SPAR-TIREGTID-AVV9-RAD(INDX)               
102000     MOVE VOR-TIREGDAT-URSP TO SPAR-TIREGDAT-URSP-RAD(INDX)               
102100     MOVE VOR-TIREGTID-URSP TO SPAR-TIREGTID-URSP-RAD(INDX)               
102200                                                                          
102300     PERFORM IMS-GET-WDA601                                               
102400     IF SEGMENT-FINNS                                                     
102500        PERFORM IMS-GET-WDA612                                            
102600        IF SEGMENT-FINNS                                                  
102700           MOVE 'T'          TO MOD-FLAGGA-VOR(INDX)                      
102800           IF VTE-FLLAEST = 'N'                                           
102900              MOVE MFS-ADD-LYS-UPP-FAELT                                  
103000                             TO MOD-FLAGGA-VOR-ATTR(INDX)                 
103100           END-IF                                                         
103200        ELSE                                                              
103300           MOVE MFS-RENSA-FAELT TO MOD-FLAGGA-VOR(INDX)                   
103400        END-IF                                                            
103500        PERFORM IMS-GET-WDA613                                            
103600        IF SEGMENT-FINNS                                                  
103700           MOVE 'T'             TO MOD-FLAGGA-SC(INDX)                    
103800        ELSE                                                              
103900           MOVE MFS-RENSA-FAELT TO MOD-FLAGGA-SC(INDX)                    
104000        END-IF                                                            
104100        PERFORM IMS-GET-WDA614                                            
104200        IF SEGMENT-FINNS                                                  
104300           MOVE 'T'             TO MOD-FLAGGA-DEAL(INDX)                  
104400        ELSE                                                              
104500           MOVE MFS-RENSA-FAELT TO MOD-FLAGGA-DEAL(INDX)                  
104600        END-IF                                                            
104700     ELSE                                                                 
104800        MOVE MFS-RENSA-FAELT TO MOD-FLAGGA-VOR(INDX)                      
104900                                MOD-FLAGGA-SC(INDX)                       
105000                                MOD-FLAGGA-DEAL(INDX)                     
105100     END-IF                                                               
105200                                                                          
105300     .                                                                    
105400     EJECT                                                                
105500 FC-SPARA-ENTER-NYCKLAR SECTION.                                          
105600                                                                          
105700     IF WS-INDEX = 'E'                                                    
105800        MOVE VOR-IDDISTR          TO SEQE-IDDISTR-ENTER                   
105900        MOVE VOR-TIREGDAT-AVV9    TO SEQE-TIREGDAT-AVV9-ENTER             
106000        MOVE VOR-TIREGTID-AVV9    TO SEQE-TIREGTID-AVV9-ENTER             
106100     END-IF                                                               
106200     IF WS-INDEX = 'F'                                                    
106300        MOVE VOR-IDDISTR          TO SEQF-IDDISTR-ENTER                   
106400        MOVE VOR-IDKUNDNR         TO SEQF-IDKUNDNR-ENTER                  
106500        MOVE VOR-TIREGDAT-AVV9    TO SEQF-TIREGDAT-AVV9-ENTER             
106600        MOVE VOR-TIREGTID-AVV9    TO SEQF-TIREGTID-AVV9-ENTER             
106700     END-IF                                                               
106800     IF WS-INDEX = 'G'                                                    
106900        MOVE VOR-IDDISTR          TO SEQG-IDDISTR-ENTER                   
107000        MOVE VOR-IDKUNDNR         TO SEQG-IDKUNDNR-ENTER                  
107100        MOVE VOR-IDARTNR          TO SEQG-IDARTNR-ENTER                   
107200        MOVE VOR-TIREGDAT-AVV9    TO SEQG-TIREGDAT-AVV9-ENTER             
107300        MOVE VOR-TIREGTID-AVV9    TO SEQG-TIREGTID-AVV9-ENTER             
107400     END-IF                                                               
107500     IF WS-INDEX = 'H'                                                    
107600        MOVE VOR-IDDISTR          TO SEQH-IDDISTR-ENTER                   
107700        MOVE VOR-IDARTNR          TO SEQH-IDARTNR-ENTER                   
107800        MOVE VOR-TIREGDAT-AVV9    TO SEQH-TIREGDAT-AVV9-ENTER             
107900        MOVE VOR-TIREGTID-AVV9    TO SEQH-TIREGTID-AVV9-ENTER             
108000     END-IF                                                               
108100     .                                                                    
108200     EJECT                                                                
108300 FD-SPARA-NEXT-NYCKLAR SECTION.                                           
108400                                                                          
108500     IF WS-INDEX = 'E'                                                    
108600        MOVE VOR-IDDISTR          TO SEQE-IDDISTR-NEXT                    
108700        MOVE VOR-TIREGDAT-AVV9    TO SEQE-TIREGDAT-AVV9-NEXT              
108800        MOVE VOR-TIREGTID-AVV9    TO SEQE-TIREGTID-AVV9-NEXT              
108900     END-IF                                                               
109000     IF WS-INDEX = 'F'                                                    
109100        MOVE VOR-IDDISTR          TO SEQF-IDDISTR-NEXT                    
109200        MOVE VOR-IDKUNDNR         TO SEQF-IDKUNDNR-NEXT                   
109300        MOVE VOR-TIREGDAT-AVV9    TO SEQF-TIREGDAT-AVV9-NEXT              
109400        MOVE VOR-TIREGTID-AVV9    TO SEQF-TIREGTID-AVV9-NEXT              
109500     END-IF                                                               
109600     IF WS-INDEX = 'G'                                                    
109700        MOVE VOR-IDDISTR          TO SEQG-IDDISTR-NEXT                    
109800        MOVE VOR-IDKUNDNR         TO SEQG-IDKUNDNR-NEXT                   
109900        MOVE VOR-IDARTNR          TO SEQG-IDARTNR-NEXT                    
110000        MOVE VOR-TIREGDAT-AVV9    TO SEQG-TIREGDAT-AVV9-NEXT              
110100        MOVE VOR-TIREGTID-AVV9    TO SEQG-TIREGTID-AVV9-NEXT              
110200     END-IF                                                               
110300     IF WS-INDEX = 'H'                                                    
110400        MOVE VOR-IDDISTR          TO SEQH-IDDISTR-NEXT                    
110500        MOVE VOR-IDARTNR          TO SEQH-IDARTNR-NEXT                    
110600        MOVE VOR-TIREGDAT-AVV9    TO SEQH-TIREGDAT-AVV9-NEXT              
110700        MOVE VOR-TIREGTID-AVV9    TO SEQH-TIREGTID-AVV9-NEXT              
110800     END-IF                                                               
110900     .                                                                    
111000     EJECT                                                                
111100 G-KOLLA-HOPP SECTION.                                                    
111200                                                                          
111300     IF WS-INDEX = 'E' OR 'F' OR 'G' OR 'H'                               
111400        IF WS-INDEX = 'E'                                                 
111500           MOVE HIGH-VALUE          TO W-WDA6ESEQ-MAX-X                   
111600           IF SEQE-IDDISTR-ENTER NOT NUMERIC                              
111700              MOVE WS-IDDISTR       TO SEQE-IDDISTR-ENTER                 
111800                                       SEQE-IDDISTR-MAX                   
111900           ELSE                                                           
112000              MOVE SEQE-IDDISTR-ENTER TO SEQE-IDDISTR-MAX                 
112100           END-IF                                                         
112200           MOVE SPAR-WDA6ESEQ-ENTER TO W-WDA6ESEQ-MIN-X                   
112300        END-IF                                                            
112400        IF WS-INDEX = 'F'                                                 
112500           MOVE HIGH-VALUE          TO W-WDA6FSEQ-MAX-X                   
112600           IF SEQF-IDDISTR-ENTER NOT NUMERIC                              
112700              MOVE WS-IDDISTR       TO SEQF-IDDISTR-ENTER                 
112800                                       SEQF-IDDISTR-MAX                   
112900              MOVE WS-IDKUNDNR      TO SEQF-IDKUNDNR-ENTER                
113000                                       SEQF-IDKUNDNR-MAX                  
113100           ELSE                                                           
113200              MOVE SEQF-IDDISTR-ENTER  TO SEQF-IDDISTR-MAX                
113300              MOVE SEQF-IDKUNDNR-ENTER TO SEQF-IDKUNDNR-MAX               
113400           END-IF                                                         
113500           MOVE SPAR-WDA6FSEQ-ENTER TO W-WDA6FSEQ-MIN-X                   
113600        END-IF                                                            
113700        IF WS-INDEX = 'G'                                                 
113800           MOVE HIGH-VALUE          TO W-WDA6GSEQ-MAX-X                   
113900           IF SEQG-IDDISTR-ENTER NOT NUMERIC                              
114000              MOVE WS-IDDISTR       TO SEQG-IDDISTR-ENTER                 
114100                                       SEQG-IDDISTR-MAX                   
114200              MOVE WS-IDKUNDNR      TO SEQG-IDKUNDNR-ENTER                
114300                                       SEQG-IDKUNDNR-MAX                  
114400              MOVE WS-IDARTNR       TO SEQG-IDARTNR-ENTER                 
114500                                       SEQG-IDARTNR-MAX                   
114600           ELSE                                                           
114700              MOVE SEQG-IDDISTR-ENTER  TO SEQG-IDDISTR-MAX                
114800              MOVE SEQG-IDKUNDNR-ENTER TO SEQG-IDKUNDNR-MAX               
114900              MOVE SEQG-IDARTNR-ENTER  TO SEQG-IDARTNR-MAX                
115000           END-IF                                                         
115100           MOVE SPAR-WDA6GSEQ-ENTER TO W-WDA6GSEQ-MIN-X                   
115200        END-IF                                                            
115300        IF WS-INDEX = 'H'                                                 
115400           MOVE HIGH-VALUE          TO W-WDA6HSEQ-MAX-X                   
115500           IF SEQH-IDDISTR-ENTER NOT NUMERIC                              
115600              MOVE WS-IDDISTR       TO SEQH-IDDISTR-ENTER                 
115700                                       SEQH-IDDISTR-MAX                   
115800              MOVE WS-IDARTNR       TO SEQH-IDARTNR-ENTER                 
115900                                       SEQH-IDARTNR-MAX                   
116000           ELSE                                                           
116100              MOVE SEQH-IDDISTR-ENTER  TO SEQH-IDDISTR-MAX                
116200              MOVE SEQH-IDARTNR-ENTER  TO SEQH-IDARTNR-MAX                
116300           END-IF                                                         
116400           MOVE SPAR-WDA6HSEQ-ENTER TO W-WDA6HSEQ-MIN-X                   
116500        END-IF                                                            
116600     ELSE                                                                 
116700        MOVE 'E' TO WS-INDEX                                              
116800        MOVE LOW-VALUE           TO W-WDA6ESEQ-MIN-X                      
116900        MOVE HIGH-VALUE          TO W-WDA6ESEQ-MAX-X                      
117000        MOVE WS-IDDISTR          TO SEQE-IDDISTR-MIN                      
117100                                    SEQE-IDDISTR-MAX                      
117200     END-IF                                                               
117300                                                                          
117400     MOVE JA TO INDATA-SW                                                 
117500     MOVE SPACE TO WS-KDCMDVAL                                            
117600                                                                          
117700     MOVE +1 TO INDX                                                      
117800     PERFORM UNTIL INDX > MAX-INDX                                        
117900        IF MID-KDCMDVAL(INDX) = ALL '+'                                   
118000           CONTINUE                                                       
118100        ELSE                                                              
118200           IF MID-KDCMDVAL(INDX) = 'T'                                    
118300              IF WS-KDCMDVAL = SPACE                                      
118400                 MOVE MID-KDCMDVAL(INDX) TO WS-KDCMDVAL                   
118500                 MOVE MFS-ALFA-FAELT-RAETT                                
118600                                        TO MOD-KDCMDVAL-ATTR(INDX)        
118700                 MOVE INDX TO WS-HOPP-INDX                                
118800              ELSE                                                        
118900                MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMDVAL-ATTR(INDX)        
119000                MOVE NEJ TO INDATA-SW                                     
119100              END-IF                                                      
119200           ELSE                                                           
119300              MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMDVAL-ATTR(INDX)        
119400              MOVE NEJ TO INDATA-SW                                       
119500           END-IF                                                         
119600        END-IF                                                            
119700        ADD +1 TO INDX                                                    
119800     END-PERFORM                                                          
119900                                                                          
120000     IF INDATA-OK                                                         
120100        PERFORM GA-KOLLA-SPAR                                             
120200        IF INDATA-OK                                                      
120300           PERFORM GB-LAS-WDA6                                            
120400           IF INDATA-OK                                                   
120500              CONTINUE                                                    
120600           ELSE                                                           
120700              PERFORM MFS-ROER-EJ-FAELT-IN                                
120800              PERFORM MFS-ADD-LAES-IN-FAELT-IN                            
120900              MOVE INF-URVAL-SAKNAS TO MED-IDMFSFEL                       
121000           END-IF                                                         
121100        ELSE                                                              
121200           PERFORM MFS-ROER-EJ-FAELT-IN                                   
121300           PERFORM MFS-ADD-LAES-IN-FAELT-IN                               
121400           MOVE INF-URVAL-SAKNAS TO MED-IDMFSFEL                          
121500        END-IF                                                            
121600     ELSE                                                                 
121700        PERFORM MFS-ROER-EJ-FAELT-IN                                      
121800        MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                         
121900     END-IF                                                               
122000                                                                          
122100     IF INDATA-OK                                                         
122200        MOVE VOR-IDDISTR       TO WS-IDDISTR                              
122300        MOVE WS-IDDISTR        TO 4276-MID-IDDISTR                        
122400        MOVE VOR-IDKUNDNR      TO WS-IDKUNDNR                             
122500        MOVE WS-IDKUNDNR       TO 4276-MID-IDKUNDNR                       
122600        MOVE VOR-IDORDNR7      TO 4276-MID-IDORDNR7                       
122700        MOVE VOR-TIREGDAT-URSP TO 4276-MID-TIREGDAT-URSP                  
122800        MOVE VOR-TIREGTID-URSP TO 4276-MID-TIREGTID-URSP                  
122900        MOVE VOR-TIREGDAT-AVV  TO 4276-MID-TIREGDAT-AVV                   
123000        MOVE VOR-TIREGTID-AVV  TO 4276-MID-TIREGTID-AVV                   
123100        MOVE VOR-IDARTNR       TO WS-IDARTNR                              
123200        MOVE WS-IDARTNR        TO 4276-MID-IDARTNR                        
123300        MOVE ALL '+'           TO 4276-MID-TEVORSC                        
123400                                  4276-MID-TEVORDEL                       
123500                                  4276-MID-FLLAEST                        
123600                                                                          
123700        COMPUTE MSG-KVLL = LENGTH OF 4276-MID-W4I27601-CTX + 17           
123800        MOVE 'W4T276  ' TO MSG-KDTRANS-1                                  
123900        MOVE '4226'     TO MSG-IDTRANS-1                                  
124000        MOVE '1'        TO MSG-KDMFSFOR-1                                 
124100     ELSE                                                                 
124200        CALL WMEDKONV USING MED-WMEDAREA                                  
124300        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
124400     END-IF                                                               
124500     .                                                                    
124600     EJECT                                                                
124700 GA-KOLLA-SPAR SECTION.                                                   
124800                                                                          
124900     IF SPAR-TIREGDAT-AVV9-RAD  (WS-HOPP-INDX) NUMERIC                    
125000     AND SPAR-TIREGTID-AVV9-RAD (WS-HOPP-INDX) NUMERIC                    
125100        IF WS-INDEX = 'E'                                                 
125200           IF SEQE-IDDISTR-ENTER NUMERIC                                  
125300              MOVE SEQE-IDDISTR-ENTER TO SEQE-IDDISTR-MINA                
125400                                        SEQE-IDDISTR-MAXA                 
125500              MOVE SPAR-TIREGDAT-AVV9-RAD(WS-HOPP-INDX)                   
125600                                     TO SEQE-TIREGDAT-AVV9-MINA           
125700                                        SEQE-TIREGDAT-AVV9-MAXA           
125800              MOVE SPAR-TIREGTID-AVV9-RAD(WS-HOPP-INDX)                   
125900                                     TO SEQE-TIREGTID-AVV9-MINA           
126000                                        SEQE-TIREGTID-AVV9-MAXA           
126100              PERFORM IMS-GET-WDA6ESEQ                                    
126200              IF SEGMENT-SAKNAS                                           
126300                 MOVE NEJ TO INDATA-SW                                    
126400              END-IF                                                      
126500           ELSE                                                           
126600              MOVE NEJ TO INDATA-SW                                       
126700           END-IF                                                         
126800        END-IF                                                            
126900        IF WS-INDEX = 'F'                                                 
127000           IF SEQF-IDDISTR-ENTER NUMERIC                                  
127100              MOVE SEQF-IDDISTR-ENTER TO SEQF-IDDISTR-MINA                
127200                                         SEQF-IDDISTR-MAXA                
127300              MOVE SEQF-IDKUNDNR-ENTER TO SEQF-IDKUNDNR-MINA              
127400                                          SEQF-IDKUNDNR-MAXA              
127500              MOVE SPAR-TIREGDAT-AVV9-RAD(WS-HOPP-INDX)                   
127600                                      TO SEQF-TIREGDAT-AVV9-MINA          
127700                                         SEQF-TIREGDAT-AVV9-MAXA          
127800              MOVE SPAR-TIREGTID-AVV9-RAD(WS-HOPP-INDX)                   
127900                                      TO SEQF-TIREGTID-AVV9-MINA          
128000                                         SEQF-TIREGTID-AVV9-MAXA          
128100              PERFORM IMS-GET-WDA6FSEQ                                    
128200              IF SEGMENT-SAKNAS                                           
128300                 MOVE NEJ TO INDATA-SW                                    
128400              END-IF                                                      
128500           ELSE                                                           
128600              MOVE NEJ TO INDATA-SW                                       
128700           END-IF                                                         
128800        END-IF                                                            
128900        IF WS-INDEX = 'G'                                                 
129000           IF SEQG-IDDISTR-ENTER NUMERIC                                  
129100              MOVE SEQG-IDDISTR-ENTER  TO SEQG-IDDISTR-MINA               
129200                                          SEQG-IDDISTR-MAXA               
129300              MOVE SEQG-IDKUNDNR-ENTER TO SEQG-IDKUNDNR-MINA              
129400                                          SEQG-IDKUNDNR-MAXA              
129500              MOVE SEQG-IDARTNR-ENTER  TO SEQG-IDARTNR-MINA               
129600                                          SEQG-IDARTNR-MAXA               
129700              MOVE SPAR-TIREGDAT-AVV9-RAD(WS-HOPP-INDX)                   
129800                                     TO SEQG-TIREGDAT-AVV9-MINA           
129900                                        SEQG-TIREGDAT-AVV9-MAXA           
130000              MOVE SPAR-TIREGTID-AVV9-RAD(WS-HOPP-INDX)                   
130100                                     TO SEQG-TIREGTID-AVV9-MINA           
130200                                        SEQG-TIREGTID-AVV9-MAXA           
130300              PERFORM IMS-GET-WDA6GSEQ                                    
130400              IF SEGMENT-SAKNAS                                           
130500                 MOVE NEJ TO INDATA-SW                                    
130600              END-IF                                                      
130700           ELSE                                                           
130800              MOVE NEJ TO INDATA-SW                                       
130900           END-IF                                                         
131000        END-IF                                                            
131100        IF WS-INDEX = 'H'                                                 
131200           MOVE SEQH-IDDISTR-ENTER TO SEQH-IDDISTR-MINA                   
131300                                      SEQH-IDDISTR-MAXA                   
131400           MOVE SEQH-IDARTNR-ENTER TO SEQH-IDARTNR-MINA                   
131500                                      SEQH-IDARTNR-MAXA                   
131600           MOVE SPAR-TIREGDAT-AVV9-RAD(WS-HOPP-INDX)                      
131700                                   TO SEQH-TIREGDAT-AVV9-MINA             
131800                                      SEQH-TIREGDAT-AVV9-MAXA             
131900           MOVE SPAR-TIREGTID-AVV9-RAD(WS-HOPP-INDX)                      
132000                                   TO SEQH-TIREGTID-AVV9-MINA             
132100                                      SEQH-TIREGTID-AVV9-MAXA             
132200           PERFORM IMS-GET-WDA6HSEQ                                       
132300           IF SEGMENT-SAKNAS                                              
132400              MOVE NEJ TO INDATA-SW                                       
132500           END-IF                                                         
132600        END-IF                                                            
132700     ELSE                                                                 
132800        MOVE NEJ TO INDATA-SW                                             
132900        MOVE 'SPAR-EJ-NUM' TO MOD-TEMFSINF                                
133000                              MOD-TEMFSFEL                                
133100     END-IF                                                               
133200     .                                                                    
133300     EJECT                                                                
133400                                                                          
133500 GB-LAS-WDA6 SECTION.                                                     
133600                                                                          
133700     MOVE VOR-IDDISTR       TO W-WDA601-IDDISTR                           
133800     MOVE VOR-IDKUNDNR      TO W-WDA601-IDKUNDNR                          
133900     MOVE VOR-IDKUNDRF      TO W-WDA601-IDKUNDRF                          
134000     MOVE VOR-TIREGDAT-URSP TO W-WDA601-TIREGDAT-URSP                     
134100     MOVE VOR-TIREGTID-URSP TO W-WDA601-TIREGTID-URSP                     
134200     MOVE VOR-TIREGDAT-AVV  TO W-WDA601-TIREGDAT-AVV                      
134300     MOVE VOR-TIREGTID-AVV  TO W-WDA601-TIREGTID-AVV                      
134400     MOVE VOR-IDARTNR       TO W-WDA601-IDARTNR                           
134500                                                                          
134600     PERFORM IMS-GET-WDA601                                               
134700     IF SEGMENT-FINNS                                                     
134800        CONTINUE                                                          
134900     ELSE                                                                 
135000        MOVE NEJ TO INDATA-SW                                             
135100     END-IF                                                               
135200     .                                                                    
135300     EJECT                                                                
135400 H-KOLLA-BEHORIGHET SECTION.                                              
135500                                                                          
135600     MOVE MSG-SIGNON-USERID TO    SEC-IDUSER                              
135700     MOVE '4226'            TO    SEC-IDTRANS                             
135800     MOVE WS-IDDISTR        TO    SEC-IDKEY                               
135900     CALL WSECURIT          USING SEC-IDUSER                              
136000                                  SEC-IDTRANS                             
136100                                  SEC-IDKEY                               
136200                                  SEC-KDSVAR                              
136300     IF SEC-KDSVAR = OBEHORIG                                             
136400        MOVE ERR-OBEHORIG TO MED-IDMFSFEL                                 
136500        MOVE OBEHORIG     TO SPAR-BEHORIGHETS-KONTR                       
136600     END-IF                                                               
136700     .                                                                    
136800     EJECT                                                                
136900 K-UPDATE SECTION.                                                        
137000                                                                          
137100     MOVE +1 TO INDX                                                      
137200     PERFORM UNTIL INDX > MAX-INDX                                        
137300       IF MID-TEVORMRK-SC(INDX) NOT = ALL '+'                             
137400         MOVE MID-IDDISTR(INDX)    TO W-WDA601-IDDISTR                    
137500         MOVE MID-IDKUNDNR(INDX)   TO W-WDA601-IDKUNDNR                   
137600         INSPECT MID-IDORDNR7(INDX)                                       
137700                               REPLACING LEADING SPACE BY ZERO            
137800         MOVE MID-IDORDNR7(INDX)   TO W-WDA601-IDKUNDRF                   
137900         MOVE SPAR-TIREGDAT-URSP-RAD(INDX)                                
138000                                   TO W-WDA601-TIREGDAT-URSP              
138100         INSPECT MID-IDARTNR(INDX)                                        
138200                               REPLACING LEADING SPACE BY ZERO            
138300         MOVE MID-IDARTNR(INDX)    TO W-WDA601-IDARTNR                    
138400         MOVE SPAR-TIREGTID-URSP-RAD(INDX)                                
138500                                   TO W-WDA601-TIREGTID-URSP              
138600         MOVE SPAR-TIREGDAT-AVV-RAD(INDX)                                 
138700                                   TO W-WDA601-TIREGDAT-AVV               
138800         MOVE SPAR-TIREGTID-AVV-RAD(INDX)                                 
138900                                   TO W-WDA601-TIREGTID-AVV               
139000                                                                          
139100         PERFORM IMS-GHU-WDA601                                           
139200         IF SEGMENT-FINNS                                                 
139300            MOVE MID-TEVORMRK-SC(INDX) TO VOR-TEVORMRK-SC                 
139400            PERFORM IMS-REPL-WDA601                                       
139500         END-IF                                                           
139600       END-IF                                                             
139700       ADD 1 TO INDX                                                      
139800     END-PERFORM                                                          
139900                                                                          
140000     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
140100     CALL WMEDKONV USING MED-WMEDAREA                                     
140200     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
140300*    PERFORM MFS-FORM-ATTR                                                
140400     .                                                                    
140500 MFS-RENSA-SPAR-FAELT SECTION.                                            
140600                                                                          
140700     MOVE MFS-RENSA-FAELT TO SPAR-INDEX                                   
140800                             SPAR-SISTA-SIDAN                             
140900                             SPAR-NYCKLAR-ENTER                           
141000                             SPAR-NYCKLAR-NEXT                            
141100*                            SPAR-TIREGDAT                                
141200*                            SPAR-TIREGTID                                
141300     .                                                                    
141400     SKIP3                                                                
141500 MFS-RENSA-FAELT-UT SECTION.                                              
141600                                                                          
141700     MOVE +1 TO INDX                                                      
141800     PERFORM UNTIL INDX > MAX-INDX                                        
141900        MOVE MFS-RENSA-FAELT TO MOD-KDCMDVAL(INDX)                        
142000                                MOD-TIREGDAT-URSP(INDX)                   
142100                                MOD-TEVORMRK(INDX)                        
142200                                MOD-TEVORMRK-SC(INDX)                     
142300                                MOD-IDDISTR(INDX)                         
142400                                MOD-IDKUNDNR(INDX)                        
142500                                MOD-IDORDNR7(INDX)                        
142600                                MOD-IDARTNR(INDX)                         
142700                                MOD-KVBEART-URSP(INDX)                    
142800                                MOD-KVBEART(INDX)                         
142900                                MOD-KDORDBEK(INDX)                        
143000                                MOD-IDDC(INDX)                            
143100                                MOD-FLAGGA-VOR(INDX)                      
143200                                MOD-FLAGGA-SC(INDX)                       
143300                                MOD-FLAGGA-DEAL(INDX)                     
143400        MOVE MFS-STAENG-FAELT TO  MOD-KDCMDVAL-ATTR(INDX)                 
143500        ADD +1 TO INDX                                                    
143600     END-PERFORM                                                          
143700     .                                                                    
143800     EJECT                                                                
143900 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
144000                                                                          
144100     MOVE MFS-RENSA-FAELT  TO MOD-KDCMDVAL(INDX)                          
144200                              MOD-TIREGDAT-URSP(INDX)                     
144300                              MOD-TEVORMRK(INDX)                          
144400                              MOD-TEVORMRK-SC(INDX)                       
144500                              MOD-IDDISTR(INDX)                           
144600                              MOD-IDKUNDNR(INDX)                          
144700                              MOD-IDORDNR7(INDX)                          
144800                              MOD-IDARTNR(INDX)                           
144900                              MOD-KVBEART-URSP(INDX)                      
145000                              MOD-KVBEART(INDX)                           
145100                              MOD-KDORDBEK(INDX)                          
145200                              MOD-IDDC(INDX)                              
145300                              MOD-FLAGGA-VOR(INDX)                        
145400                              MOD-FLAGGA-SC(INDX)                         
145500                              MOD-FLAGGA-DEAL(INDX)                       
145600     MOVE MFS-STAENG-FAELT TO MOD-KDCMDVAL-ATTR(INDX)                     
145700     MOVE ZERO             TO SPAR-TIREGDAT-AVV-RAD(INDX)                 
145800                              SPAR-TIREGTID-AVV-RAD(INDX)                 
145900                              SPAR-TIREGDAT-AVV9-RAD(INDX)                
146000                              SPAR-TIREGTID-AVV9-RAD(INDX)                
146100                              SPAR-TIREGDAT-URSP-RAD(INDX)                
146200                              SPAR-TIREGTID-URSP-RAD(INDX)                
146300     .                                                                    
146400     EJECT                                                                
146500 MFS-RENSA-FAELT-IN SECTION.                                              
146600                                                                          
146700     MOVE +1 TO INDX                                                      
146800     PERFORM UNTIL INDX > MAX-INDX                                        
146900        MOVE MFS-RENSA-FAELT TO MOD-KDCMDVAL(INDX)                        
147000        ADD +1 TO INDX                                                    
147100     END-PERFORM                                                          
147200     .                                                                    
147300     SKIP3                                                                
147400 MFS-ADD-LAES-IN-FAELT-IN SECTION.                                        
147500                                                                          
147600     MOVE +1 TO INDX                                                      
147700     PERFORM UNTIL INDX > MAX-INDX                                        
147800        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMDVAL-ATTR(INDX)             
147900        ADD +1 TO INDX                                                    
148000     END-PERFORM                                                          
148100     .                                                                    
148200     SKIP3                                                                
148300 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
148400                                                                          
148500     MOVE +1 TO INDX                                                      
148600     PERFORM UNTIL INDX > MAX-INDX                                        
148700        MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMDVAL(INDX)                      
148800                                  MOD-TEVORMRK-SC(INDX)                   
148900        ADD +1 TO INDX                                                    
149000     END-PERFORM                                                          
149100     .                                                                    
149200*MFS-FORM-ATTR SECTION.                                                   
149300                                                                          
149400*    --- ALLA INDATA-FÄLT                                                 
149500*    MOVE +1 TO INDX                                                      
149600*    PERFORM UNTIL INDX > MAX-INDX                                        
149700*      MOVE MFS-FORMATETS-ATTR TO MOD-KDCMDVAL-ATTR(INDX)                 
149800*                                 MOD-FLAGGA-VOR-ATTR(INDX)               
149900*    ADD 1 TO INDX                                                        
150000*    END-PERFORM                                                          
150100*    .                                                                    
150200*    EJECT                                                                
150300* --- IMS SEKTIONER ---                                                   
150400     SKIP3                                                                
150500 IMS-GET-MSG SECTION.                                                     
150600     MOVE '  QC' TO GODK-STATUSKODER                                      
150700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
150800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
150900     PERFORM IMS-STATUSKONTROLL                                           
151000     .                                                                    
151100     SKIP3                                                                
151200 IMS-INSERT-MSG SECTION.                                                  
151300     IF MSGI-IDLAND-SPR = 'SE'                                            
151400       MOVE '0' TO MFS-KDHUVOMR                                           
151500     END-IF                                                               
151600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
151700     MOVE SPACE TO GODK-STATUSKODER                                       
151800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
151900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
152000     PERFORM IMS-STATUSKONTROLL                                           
152100     .                                                                    
152200     SKIP3                                                                
152300 IMS-INSERT-ALT-MSG SECTION.                                              
152400     MOVE SPACE TO GODK-STATUSKODER                                       
152500     CALL CBLTDLI USING ISRT ALT-PCB MSG-IO-AREA                          
152600     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
152700     PERFORM IMS-STATUSKONTROLL                                           
152800     .                                                                    
152900     EJECT                                                                
153000 IMS-GET-WDA601 SECTION.                                                  
153100     STRING 'WDA601  (WDA601KY =' W-WDA601KY-X ')'                        
153200          DELIMITED BY SIZE INTO SSA1                                     
153300     MOVE '  GE' TO GODK-STATUSKODER                                      
153400     CALL CBLTDLI USING GU WDA6-PCB DLI-IO-WDA601 SSA1                    
153500     MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
153600     PERFORM IMS-STATUSKONTROLL                                           
153700     .                                                                    
153800     SKIP3                                                                
153900 IMS-GET-WDA612 SECTION.                                                  
154000     MOVE 'WDA612 ' TO SSA1                                               
154100     MOVE '  GE' TO GODK-STATUSKODER                                      
154200     CALL CBLTDLI USING GNP WDA6-PCB DLI-IO-WDA612 SSA1                   
154300     MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
154400     PERFORM IMS-STATUSKONTROLL                                           
154500     .                                                                    
154600     EJECT                                                                
154700 IMS-GET-WDA613 SECTION.                                                  
154800     MOVE 'WDA613 ' TO SSA1                                               
154900     MOVE '  GE' TO GODK-STATUSKODER                                      
155000     CALL CBLTDLI USING GNP WDA6-PCB DLI-IO-WDA613 SSA1                   
155100     MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
155200     PERFORM IMS-STATUSKONTROLL                                           
155300     .                                                                    
155400     SKIP3                                                                
155500 IMS-GET-WDA614 SECTION.                                                  
155600     MOVE 'WDA614 ' TO SSA1                                               
155700     MOVE '  GE' TO GODK-STATUSKODER                                      
155800     CALL CBLTDLI USING GNP WDA6-PCB DLI-IO-WDA614 SSA1                   
155900     MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
156000     PERFORM IMS-STATUSKONTROLL                                           
156100     .                                                                    
156200     SKIP3                                                                
156300 IMS-GU-WDA6ESEQ SECTION.                                                 
156400     STRING 'WDA601  (WDA6ESEQ>=' W-WDA6ESEQ-MIN-X                        
156500                    '&WDA6ESEQ<=' W-WDA6ESEQ-MAX-X ')'                    
156600          DELIMITED BY SIZE INTO SSA1                                     
156700     MOVE '  GE' TO GODK-STATUSKODER                                      
156800     CALL CBLTDLI USING GU WDA6E-PCB DLI-IO-WDA601 SSA1                   
156900     MOVE WDA6E-STATUS-CODE TO STATUS-WS                                  
157000     PERFORM IMS-STATUSKONTROLL                                           
157100     .                                                                    
157200     EJECT                                                                
157300 IMS-GET-WDA6ESEQ SECTION.                                                
157400     STRING 'WDA601  (WDA6ESEQ>=' W-WDA6ESEQ-MINA-X                       
157500                    '&WDA6ESEQ<=' W-WDA6ESEQ-MAXA-X ')'                   
157600          DELIMITED BY SIZE INTO SSA1                                     
157700     MOVE '  GE' TO GODK-STATUSKODER                                      
157800     CALL CBLTDLI USING GU WDA6E-PCB DLI-IO-WDA601 SSA1                   
157900     MOVE WDA6E-STATUS-CODE TO STATUS-WS                                  
158000     PERFORM IMS-STATUSKONTROLL                                           
158100     .                                                                    
158200     SKIP3                                                                
158300 IMS-GN-WDA6ESEQ SECTION.                                                 
158400     STRING 'WDA601  (WDA6ESEQ>=' W-WDA6ESEQ-MIN-X                        
158500                    '&WDA6ESEQ<=' W-WDA6ESEQ-MAX-X ')'                    
158600          DELIMITED BY SIZE INTO SSA1                                     
158700     MOVE '  GE' TO GODK-STATUSKODER                                      
158800     CALL CBLTDLI USING GN WDA6E-PCB DLI-IO-WDA601 SSA1                   
158900     MOVE WDA6E-STATUS-CODE TO STATUS-WS                                  
159000     PERFORM IMS-STATUSKONTROLL                                           
159100     .                                                                    
159200     SKIP3                                                                
159300 IMS-GU-WDA6FSEQ SECTION.                                                 
159400     STRING 'WDA601  (WDA6FSEQ>=' W-WDA6FSEQ-MIN-X                        
159500                    '&WDA6FSEQ<=' W-WDA6FSEQ-MAX-X ')'                    
159600          DELIMITED BY SIZE INTO SSA1                                     
159700     MOVE '  GE' TO GODK-STATUSKODER                                      
159800     CALL CBLTDLI USING GU WDA6F-PCB DLI-IO-WDA601 SSA1                   
159900     MOVE WDA6F-STATUS-CODE TO STATUS-WS                                  
160000     PERFORM IMS-STATUSKONTROLL                                           
160100     .                                                                    
160200     EJECT                                                                
160300 IMS-GET-WDA6FSEQ SECTION.                                                
160400     STRING 'WDA601  (WDA6FSEQ>=' W-WDA6FSEQ-MINA-X                       
160500                    '&WDA6FSEQ<=' W-WDA6FSEQ-MAXA-X ')'                   
160600          DELIMITED BY SIZE INTO SSA1                                     
160700     MOVE '  GE' TO GODK-STATUSKODER                                      
160800     CALL CBLTDLI USING GU WDA6F-PCB DLI-IO-WDA601 SSA1                   
160900     MOVE WDA6F-STATUS-CODE TO STATUS-WS                                  
161000     PERFORM IMS-STATUSKONTROLL                                           
161100     .                                                                    
161200     SKIP3                                                                
161300 IMS-GN-WDA6FSEQ SECTION.                                                 
161400     STRING 'WDA601  (WDA6FSEQ>=' W-WDA6FSEQ-MIN-X                        
161500                    '&WDA6FSEQ<=' W-WDA6FSEQ-MAX-X ')'                    
161600          DELIMITED BY SIZE INTO SSA1                                     
161700     MOVE '  GE' TO GODK-STATUSKODER                                      
161800     CALL CBLTDLI USING GN WDA6F-PCB DLI-IO-WDA601 SSA1                   
161900     MOVE WDA6F-STATUS-CODE TO STATUS-WS                                  
162000     PERFORM IMS-STATUSKONTROLL                                           
162100     .                                                                    
162200     SKIP3                                                                
162300 IMS-GU-WDA6GSEQ SECTION.                                                 
162400     STRING 'WDA601  (WDA6GSEQ>=' W-WDA6GSEQ-MIN-X                        
162500                    '&WDA6GSEQ<=' W-WDA6GSEQ-MAX-X ')'                    
162600          DELIMITED BY SIZE INTO SSA1                                     
162700     MOVE '  GE' TO GODK-STATUSKODER                                      
162800     CALL CBLTDLI USING GU WDA6G-PCB DLI-IO-WDA601 SSA1                   
162900     MOVE WDA6G-STATUS-CODE TO STATUS-WS                                  
163000     PERFORM IMS-STATUSKONTROLL                                           
163100     .                                                                    
163200     EJECT                                                                
163300 IMS-GET-WDA6GSEQ SECTION.                                                
163400     STRING 'WDA601  (WDA6GSEQ>=' W-WDA6GSEQ-MINA-X                       
163500                    '&WDA6GSEQ<=' W-WDA6GSEQ-MAXA-X ')'                   
163600          DELIMITED BY SIZE INTO SSA1                                     
163700     MOVE '  GE' TO GODK-STATUSKODER                                      
163800     CALL CBLTDLI USING GU WDA6G-PCB DLI-IO-WDA601 SSA1                   
163900     MOVE WDA6G-STATUS-CODE TO STATUS-WS                                  
164000     PERFORM IMS-STATUSKONTROLL                                           
164100     .                                                                    
164200     SKIP3                                                                
164300 IMS-GN-WDA6GSEQ SECTION.                                                 
164400     STRING 'WDA601  (WDA6GSEQ>=' W-WDA6GSEQ-MIN-X                        
164500                    '&WDA6GSEQ<=' W-WDA6GSEQ-MAX-X ')'                    
164600          DELIMITED BY SIZE INTO SSA1                                     
164700     MOVE '  GE' TO GODK-STATUSKODER                                      
164800     CALL CBLTDLI USING GN WDA6G-PCB DLI-IO-WDA601 SSA1                   
164900     MOVE WDA6G-STATUS-CODE TO STATUS-WS                                  
165000     PERFORM IMS-STATUSKONTROLL                                           
165100     .                                                                    
165200     SKIP3                                                                
165300 IMS-GU-WDA6HSEQ SECTION.                                                 
165400     STRING 'WDA601  (WDA6HSEQ>=' W-WDA6HSEQ-MIN-X                        
165500                    '&WDA6HSEQ<=' W-WDA6HSEQ-MAX-X ')'                    
165600          DELIMITED BY SIZE INTO SSA1                                     
165700     MOVE '  GE' TO GODK-STATUSKODER                                      
165800     CALL CBLTDLI USING GU WDA6H-PCB DLI-IO-WDA601 SSA1                   
165900     MOVE WDA6H-STATUS-CODE TO STATUS-WS                                  
166000     PERFORM IMS-STATUSKONTROLL                                           
166100     .                                                                    
166200     EJECT                                                                
166300 IMS-GET-WDA6HSEQ SECTION.                                                
166400     STRING 'WDA601  (WDA6HSEQ>=' W-WDA6HSEQ-MINA-X                       
166500                    '&WDA6HSEQ<=' W-WDA6HSEQ-MAXA-X ')'                   
166600          DELIMITED BY SIZE INTO SSA1                                     
166700     MOVE '  GE' TO GODK-STATUSKODER                                      
166800     CALL CBLTDLI USING GU WDA6H-PCB DLI-IO-WDA601 SSA1                   
166900     MOVE WDA6H-STATUS-CODE TO STATUS-WS                                  
167000     PERFORM IMS-STATUSKONTROLL                                           
167100     .                                                                    
167200     SKIP3                                                                
167300 IMS-GN-WDA6HSEQ SECTION.                                                 
167400     STRING 'WDA601  (WDA6HSEQ>=' W-WDA6HSEQ-MIN-X                        
167500                    '&WDA6HSEQ<=' W-WDA6HSEQ-MAX-X ')'                    
167600          DELIMITED BY SIZE INTO SSA1                                     
167700     MOVE '  GE' TO GODK-STATUSKODER                                      
167800     CALL CBLTDLI USING GN WDA6H-PCB DLI-IO-WDA601 SSA1                   
167900     MOVE WDA6H-STATUS-CODE TO STATUS-WS                                  
168000     PERFORM IMS-STATUSKONTROLL                                           
168100     .                                                                    
168200                                                                          
168300 IMS-GHU-WDA601 SECTION.                                                  
168400                                                                          
168500     STRING 'WDA601  (WDA601KY =' W-WDA601KY-X ')'                        
168600          DELIMITED BY SIZE INTO SSA1                                     
168700     MOVE '  GE' TO GODK-STATUSKODER                                      
168800     CALL CBLTDLI USING GHU WDA6-PCB DLI-IO-WDA601 SSA1                   
168900     MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
169000     PERFORM IMS-STATUSKONTROLL                                           
169100     .                                                                    
169200                                                                          
169300 IMS-REPL-WDA601 SECTION.                                                 
169400                                                                          
169500     MOVE '  ' TO GODK-STATUSKODER                                        
169600     CALL CBLTDLI USING REPL WDA6-PCB DLI-IO-WDA601                       
169700     MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
169800     PERFORM IMS-STATUSKONTROLL                                           
169900     .                                                                    
170000                                                                          
170100 IMS-STATUSKONTROLL SECTION.                                              
170200     SET STATUS-IX TO 1                                                   
170300     SEARCH GODK-STATUS                                                   
170400       AT END                                                             
170500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
170600         DELIMITED BY SIZE INTO FELTEXT                                   
170700         CALL FELLOG                                                      
170800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
170900         CONTINUE                                                         
171000     END-SEARCH                                                           
171100     .                                                                    
