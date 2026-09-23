000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6010910.                                                
000400*AUTHOR.         RAHUL JAIN.                                              
000500*DATE-WRITTEN.   JUN 2012.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        BILDEN VISAR INFORMATION OM PARTIET, MOTSVARANDE DEN             
001100*        UTSKRIVNA ARBETSRAPPORTEN/CLEARINGRAPPORTEN.                     
001200*        UTSKRIFT AV ARTBETSRAPPORT/CLEARINGRAPPORT KAN SKE               
001300*        FRÅN DENNA BILD.                                                 
001400*                                                                         
001500*        PROGRAMMET LÄSER      W6INLA (W6D1)                              
001600*        PROGRAMMET LÄSER      W6PLAA (W6G1)                              
001700*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001800*        PROGRAMMET LÄSER      WLARTD (WDD8)                              
001900*        PROGRAMMET LÄSER      WDF5                                       
002000*        PROGRAMMET LÄSER      ÄDB6                                       
002100*                                                                         
002200*    INDATA.                                                              
002300*        REQU:        W60109I1                                            
002400*                                                                         
002500*    UTDATA.                                                              
002600*        RESP:        W60109O1                                            
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003400 77  IDPGM                       PIC X(08)   VALUE 'W6010910'.            
003500                                                                          
003600*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003700 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003800                                                                          
003900 77  JA                          PIC X       VALUE 'J'.                   
003910 77  YES                         PIC X       VALUE 'Y'.                   
004000 77  NEJ                         PIC X       VALUE 'N'.                   
004100                                                                          
004400 77  INDX                        PIC S9(9)  VALUE +0    COMP SYNC.        
004500 77  MAX-INDX                    PIC S9(4)  VALUE +6    COMP SYNC.        
004600                                                                          
004700*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004800 77  WS-IDLEVNR-KOLLI            PIC X(5)    VALUE SPACE.                 
004900 77  WS-IDOKOLLI                 PIC X(9)    VALUE SPACE.                 
005000 77  WS-IDLOPNRM                 PIC X(9)    VALUE SPACE.                 
005100                                                                          
005200 77  W-ADLAGOMR                  PIC X(4)    VALUE SPACE.                 
005300 77  W-ADBUFFOMR                 PIC X(4)    VALUE SPACE.                 
005400 77  W-ADINLOMR                  PIC 9(2)    VALUE ZERO.                  
005500 77  WS-VKART                    PIC S9(7)   COMP-3 VALUE ZERO.           
005600 77  WS-VLARTNTO                 PIC S9(8)V9(1) COMP-3 VALUE ZERO.        
005700                                                                          
005800 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005900     88  NYCKLAR-OK                          VALUE 'J'.                   
006000     88  NYCKLAR-FEL                         VALUE 'N'.                   
006100                                                                          
006500 77  ARTD-SW                     PIC X       VALUE 'N'.                   
006600     88  ARTD-OK                             VALUE 'J'.                   
006700                                                                          
006800 77  INL-SEGMENT-SW              PIC X       VALUE 'J'.                   
006900     88  INL-SEGMENT-SAKNAS                  VALUE 'N'.                   
007000                                                                          
007100 77  FLDIVKLI-SW                 PIC X       VALUE 'N'.                   
007200     88  FLDIVKLI                            VALUE 'J'.                   
007300                                                                          
007400 01  ALL-SPACE.                                                           
007500     03 FILLER                   PIC X(80)   VALUE SPACE.                 
007600 01  ALL-PLUS.                                                            
007700     03 FILLER                   PIC X(80)   VALUE ALL '+'.               
007710 77  WS-CP-UNICODE               PIC X(4)    VALUE 'UTF8'.                
007720 77  WS-CP-EBCDIC                PIC X(3)    VALUE '278'.                 
007800     EJECT                                                                
007900*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008000 01  GENERELLA-SUBPROGRAM.                                                
008100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008300     03  W611ADR                 PIC X(8)    VALUE 'W611ADR '.            
008400*    03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
008410     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
008500     EJECT                                                                
008600*01 -COPY WWOMVAND                                                        
008700     EJECT                                                                
008800 01  MESSAGE-CODES.                                                       
008900     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '014'.                 
009000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
009100     03  ERR-MISS-REGISTER       PIC X(3)    VALUE '025'.                 
009200     03  ERR-PART-MISSING        PIC X(3)    VALUE '025'.                 
009300     03  ERR-MISC-CASE           PIC X(3)    VALUE '354'.                 
009400     03  ERR-PRESS-PF4           PIC X(3)    VALUE '379'.                 
009500     03  INF-QUARANTINE          PIC X(3)    VALUE '355'.                 
009600     EJECT                                                                
009700*    --- PARAMETRAR TILL SUBPROGRAM W611ADR                               
009800*01 -COPY W611ADR                                                         
009900     EJECT                                                                
010000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
010100*                                                                         
010200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010300     SKIP3                                                                
010400*01  -COPY WMFSAREA                                                       
010500     EJECT                                                                
010501 01  FILLER                      PIC X(16)   VALUE 'DC CODES   '.         
010502     SKIP3                                                                
010510*   -COPY WWDC99                                                          
010511*   -COPY WWLNDKON                                                        
010520 01  FILLER                      PIC X(16)  VALUE 'WTRAUTF8-AREA'.        
010530*01  -COPY WTRAUTF8                                                       
010600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010700*                                                                         
010800     EJECT                                                                
010900 01  FILLER                      PIC X(16)     VALUE 'IMS-WS'.            
011000     SKIP3                                                                
011100 01  NYCKLAR-TILL-DLI.                                                    
011200                                                                          
011300     03  W-W6D101KY-X.                                                    
011400         05  W-D101KY-IDDC       PIC X(2)      VALUE SPACE.               
011500         05  W-D101KY-IDLEVNR    PIC X(5)      VALUE SPACE.               
011600         05  W-D101KY-IDFS       PIC X(8)      VALUE SPACE.               
011700         05  W-D101KY-TIAVIDAT   PIC S9(7)     COMP-3 VALUE ZERO.         
011800                                                                          
011900     03  W-IDLOPNRM-X.                                                    
012000         05  W-IDLOPNRM          PIC S9(9) COMP-3 VALUE ZERO.             
012001                                                                          
012010     03  W-IDSKYLT-X.                                                     
012020         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
012021                                                                          
012030     03  W-IDLAND-X.                                                      
012040         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
012100                                                                          
012200     03  W-W6D1C1KY-MIN-X.                                                
012300      05  W-D1C1KY-IDLEVNR-KOLLI-MIN PIC X(5)  VALUE SPACE.               
012400      05  W-D1C1KY-IDOKOLLI-MIN      PIC 9(9)  VALUE ZERO.                
012500      05  W-D1C1KY-IDRADNR-INL-MIN   PIC S9(5) COMP-3 VALUE ZERO.         
012600      05  W-D1C1KY-IDDC-MIN          PIC X(2)  VALUE SPACE.               
012700      05  W-D1C1KY-IDLEVNR-MIN       PIC X(5)  VALUE SPACE.               
012800      05  W-D1C1KY-IDFS-MIN          PIC X(8)  VALUE SPACE.               
012900      05  W-D1C1KY-TIAVIDAT-MIN      PIC S9(7) COMP-3 VALUE ZERO.         
013000      05  W-D1C1KY-IDRADNR-MIN       PIC S9(5) COMP-3 VALUE ZERO.         
013100                                                                          
013200     03  W-W6D1C1KY-MAX-X.                                                
013300      05  W-D1C1KY-IDLEVNR-KOLLI-MAX PIC X(5)  VALUE SPACE.               
013400      05  W-D1C1KY-IDOKOLLI-MAX      PIC 9(9)  VALUE ZERO.                
013500      05  W-D1C1KY-IDRADNR-INL-MAX   PIC S9(5) COMP-3 VALUE ZERO.         
013600      05  W-D1C1KY-IDDC-MAX          PIC X(2)  VALUE SPACE.               
013700      05  W-D1C1KY-IDLEVNR-MAX       PIC X(5)  VALUE SPACE.               
013800      05  W-D1C1KY-IDFS-MAX          PIC X(8)  VALUE SPACE.               
013900      05  W-D1C1KY-TIAVIDAT-MAX      PIC S9(7) COMP-3 VALUE ZERO.         
014000      05  W-D1C1KY-IDRADNR-MAX       PIC S9(5) COMP-3 VALUE ZERO.         
014100                                                                          
014200     03  W-W6GXKEY-6005-X.                                                
014300         05  W-6005-IDHTYP           PIC X(4)  VALUE '6005'.              
014400         05  W-6005-IDDC             PIC X(2)  VALUE SPACE.               
014500         05  FILLER                  PIC X(23) VALUE LOW-VALUE.           
014600     03  W-W6GXKEY-6006-X.                                                
014700         05  W-6006-ADINLOMR         PIC X(4)  VALUE SPACE.               
014800         05  FILLER                  PIC X(1)  VALUE LOW-VALUE.           
014900     03  W-IDARTNR-X.                                                     
015000         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
015100     03  W-IDRADNR-INL-X.                                                 
015200         05  W-IDRADNR-INL       PIC S9(5)   VALUE ZERO COMP-3.           
015300     03  W-IDRADNR-X.                                                     
015400         05  W-IDRADNR           PIC S9(5)   VALUE ZERO COMP-3.           
015500     03  W-KDSEGKEY-X.                                                    
015600         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
015700     03  W-IDLEVNR-X.                                                     
015800         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
015900     03  W-IDDC-X.                                                        
016000         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
016100                                                                          
016200     03  W-IDDC-B6-X.                                                     
016300         05 W-IDDC-B6            PIC X(2).                                
016301                                                                          
016310     03  W-KDSEGKEY-K722-X.                                               
016320         05  W-KDSEGKEY-K722     PIC X(1)    VALUE '1'.                   
016400                                                                          
016500     SKIP2                                                                
016600*    --- STATUS-KOD FRÅN IMS                                              
016700 01  STATUS-WS                   PIC XX.                                  
016800     88  SEGMENT-FINNS                       VALUE '  '.                  
016900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
017000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017100     88  SEGMENT-SLUT                        VALUE 'GB'.                  
017200     SKIP2                                                                
017300 01  GODK-STATUSKODER.                                                    
017400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017500     SKIP3                                                                
017600 01  SSA1                        PIC X(128).                              
017700 01  SSA2                        PIC X(64).                               
017800 01  SSA3                        PIC X(64).                               
017900     EJECT                                                                
018000*    --- IMS FUNKTIONSKODER                                               
018100*01  -COPY W0003                                                          
018200     EJECT                                                                
018300*    ---  DLI INPUT-OUTPUT AREA                                           
018400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
018500     SKIP3                                                                
018600 01  DLI-IO-AREA.                                                         
018700     03  IO-AREA                 PIC X(900)  VALUE SPACE.                 
018800     SKIP3                                                                
018900     03  W6INLA11 REDEFINES IO-AREA.                                      
019000*        05  -COPY W6D111                                                 
019100     EJECT                                                                
019200     03  W6INLD01 REDEFINES IO-AREA.                                      
019300*        05  -COPY W6D1C1                                                 
019400     EJECT                                                                
019500     03  W6INLC01 REDEFINES IO-AREA.                                      
019600*        05  -COPY W6D1B1                                                 
019700     EJECT                                                                
019800     03  W6PLAA11 REDEFINES IO-AREA.                                      
019900*        05  -COPY W6GX6006 -PRE PLAA-                                    
020000     EJECT                                                                
020100     03  WLARTC11 REDEFINES IO-AREA.                                      
020200*        05  -COPY WDK611  -PRE ARTC-                                     
020300     EJECT                                                                
020400     03  WLARTD11 REDEFINES IO-AREA.                                      
020500*        05  -COPY WDD811                                                 
020600     EJECT                                                                
020700     03  WDF502   REDEFINES IO-AREA.                                      
020800*        05  -COPY WDF502                                                 
020900     EJECT                                                                
021000 01  FILLER                     PIC X(16)   VALUE 'DLI-IO-AREA2'.         
021100     SKIP3                                                                
021200 01  DLI-IO-AREA2.                                                        
021300     03  IO-AREA2               PIC X(150)  VALUE SPACE.                  
021400     SKIP3                                                                
021500     03  W6INLA01 REDEFINES IO-AREA2.                                     
021600*        05  -COPY W6D101                                                 
021700     EJECT                                                                
021800     03  W6INLA01 REDEFINES IO-AREA2.                                     
021900*        05  -COPY W6D121                                                 
022000     EJECT                                                                
022100 01  FILLER                     PIC X(16)   VALUE 'DLI-IO-WDK711'.        
022200 01  DLI-IO-AREA-WDK711.                                                  
022300*    03  -COPY WDK711  -PRE WDK7-                                         
022400                                                                          
022410 01  FILLER                     PIC X(16)   VALUE 'DLI-IO-WDK712'.        
022420 01  DLI-IO-AREA-WDK712.                                                  
022430*    03  -COPY WDK712                                                     
022440                                                                          
022450 01  FILLER                     PIC X(16)   VALUE 'DLI-IO-WDK722'.        
022460 01  DLI-IO-AREA-WDK722.                                                  
022470*    03  -COPY WDK722                                                     
022480                                                                          
022500 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
022600 01   DLI-IO-AREA-B601.                                                   
022700*     03  -COPY WDB601                                                    
022800                                                                          
022840  01  FILLER              PIC X(16)   VALUE 'BENA11 AREA'.                
022850  01  DLI-IO-AREA-BENA11.                                                 
022860*    03  -COPY WDD311  -PRE BENA-                                         
022900     EJECT                                                                
023000 LINKAGE SECTION.                                                         
023100                                                                          
023200 01  REQU-AREA.                                                           
023300*    03 -COPY WZ01REQU                                                    
023400*    03 -COPY W60109I1                                                    
023500     EJECT                                                                
023600 01  RESP-AREA.                                                           
023700*    03 -COPY WZ01RESP                                                    
023800*    03 -COPY W60109O1                                                    
023900     EJECT                                                                
024000*                                                                         
024100*01  -COPY W0008  -PRE INLA-                                              
024200     05  FILLER                  PIC X.                                   
024300     EJECT                                                                
024400*01  -COPY W0008  -PRE INLC-                                              
024500     05  FILLER                  PIC X.                                   
024600     EJECT                                                                
024700*01  -COPY W0008  -PRE INLD-                                              
024800     05  FILLER                  PIC X.                                   
024900     EJECT                                                                
025000*01  -COPY W0008  -PRE PLAA-                                              
025100     05  FILLER                  PIC X.                                   
025200     EJECT                                                                
025300*01  -COPY W0008  -PRE ARTC-                                              
025400     05  FILLER                  PIC X.                                   
025500     EJECT                                                                
025600*01  -COPY W0008  -PRE ARTD-                                              
025700     05  FILLER                  PIC X.                                   
025800     EJECT                                                                
025900*01  -COPY W0008  -PRE WDF5-                                              
026000     05  FILLER                  PIC X.                                   
026100     EJECT                                                                
026200*01  -COPY W0008  -PRE WDK7-                                              
026300     05  FILLER                  PIC X.                                   
026400     EJECT                                                                
026500*01  -COPY W0008  -PRE WDB6-                                              
026600     05  FILLER                  PIC X.                                   
026700     EJECT                                                                
026710*01  -COPY W0008  -PRE BENA-                                              
026720     05  FILLER                  PIC X.                                   
026730     EJECT                                                                
026800***  PCB'ER TILL SUBPGM W611ADR                                           
026900                                                                          
027000 01  ADR-INLA-PCB                PIC X.                                   
027100                                                                          
027200 01  ADR-INLC-PCB                PIC X.                                   
027300                                                                          
027400 01  ADR-PLAA-PCB                PIC X.                                   
027500                                                                          
027600 01  ADR-WDK6-PCB                PIC X.                                   
027700                                                                          
027800 01  STYR-HANA-PCB               PIC X.                                   
027900                                                                          
028000 01  STYR-PLAA-PCB               PIC X.                                   
028010                                                                          
028100                                                                          
028200     EJECT                                                                
028300 PROCEDURE DIVISION  USING REQU-AREA RESP-AREA                            
028400                           INLA-PCB INLC-PCB INLD-PCB                     
028500                           PLAA-PCB ARTC-PCB ARTD-PCB WDF5-PCB            
028600                           WDK7-PCB WDB6-PCB BENA-PCB                     
028700                           ADR-INLA-PCB ADR-INLC-PCB ADR-PLAA-PCB         
028800                           ADR-WDK6-PCB STYR-HANA-PCB                     
028810                           STYR-PLAA-PCB.                                 
028900                                                                          
029000     PERFORM A-INIT                                                       
029100     PERFORM B-KOLLA-NYCKLAR                                              
029200     IF NYCKLAR-OK                                                        
029300         PERFORM F-LAES-VISA-INFO                                         
029400     END-IF                                                               
029500     IF REQU-PRINT                                                        
029600       MOVE ERR-PRESS-PF4 TO RESP-IDMSG-ERROR                             
029700       IF REQU-ADINLOMR-PRT NOT = ALL '+'                                 
029800         MOVE REQU-ADINLOMR-PRT TO RESP-ADINLOMR-PRT                      
029900         PERFORM MFS-LAES-IN-IGEN                                         
030000       END-IF                                                             
030100     END-IF                                                               
030200                                                                          
030300     GOBACK                                                               
030400     .                                                                    
030500     EJECT                                                                
030600 A-INIT SECTION.                                                          
030700                                                                          
030800     MOVE ALL '+'                   TO RESP-W60109O1                      
030900                                                                          
031000     MOVE 001                       TO RESP-IDMSGVER                      
031100     MOVE SPACE                     TO RESP-IDMSG-ERROR                   
031200                                       RESP-IDMSG-INFO                    
031300                                       RESP-IDELMT-ERROR                  
031400     .                                                                    
031500     EJECT                                                                
031600 B-KOLLA-NYCKLAR SECTION.                                                 
031700                                                                          
031800     MOVE JA TO NYCKLAR-SW                                                
031801                                                                          
032000     PERFORM BA-KOLLA-IDLEVNR-KOLLI                                       
032100     PERFORM BB-KOLLA-IDOKOLLI                                            
032200     PERFORM BC-KOLLA-IDLOPNRM                                            
032300     PERFORM BD-KOLLA-IDDC                                                
032302*                                                                         
032303     EVALUATE REQU-IDSPRAK                                                
032304       WHEN 'ZH'                                                          
032305        MOVE 'RCN'           TO W-IDSKYLT                                 
032306        MOVE WS-CP-UNICODE   TO TRAUTF8-KDCP                              
032307       WHEN 'SV'                                                          
032308        MOVE 'S  '           TO W-IDSKYLT                                 
032309        MOVE WS-CP-EBCDIC    TO TRAUTF8-KDCP                              
032310       WHEN OTHER                                                         
032311        MOVE 'GB '           TO W-IDSKYLT                                 
032312        MOVE WS-CP-EBCDIC    TO TRAUTF8-KDCP                              
032313     END-EVALUATE                                                         
032314*                                                                         
032315                                                                          
032316     IF NDC-CN                                                            
032320        MOVE WC-LAND-CN      TO W-IDLAND                                  
032330     ELSE                                                                 
032331        IF NDC-US                                                         
032340           MOVE WC-LAND-US   TO W-IDLAND                                  
032341        ELSE                                                              
032342           MOVE WC-LAND-SE   TO W-IDLAND                                  
032350        END-IF                                                            
032360     END-IF                                                               
032400                                                                          
032500     IF REQU-ADINLOMR-PRT = ALL '+'                                       
032600         MOVE ALL-SPACE          TO RESP-ADINLOMR-PRT                     
032700     ELSE                                                                 
032800         MOVE REQU-ADINLOMR-PRT  TO RESP-ADINLOMR-PRT                     
032900     END-IF                                                               
033000                                                                          
033100     IF NYCKLAR-OK                                                        
033200         MOVE MFS-ADD-LAES-IN-FAELT  TO RESP-ADINLOMR-PRT-ATTR            
033300     ELSE                                                                 
033400         MOVE ALL-SPACE          TO RESP-ADINLOMR-PRT                     
033500         MOVE MFS-FORMATETS-ATTR TO RESP-ADINLOMR-PRT-ATTR                
033600     END-IF                                                               
033700                                                                          
033800     IF NYCKLAR-FEL                                                       
033900         MOVE ERR-WRONG-KEY      TO RESP-IDMSG-ERROR                      
034000         PERFORM MFS-RENSA-FAELT-UT                                       
034100     END-IF                                                               
034200     .                                                                    
034300     EJECT                                                                
034400 BA-KOLLA-IDLEVNR-KOLLI SECTION.                                          
034500                                                                          
034600     IF REQU-IDLEVNR-KOLLI-KEY = SPACE                                    
034700         CONTINUE                                                         
034800     ELSE                                                                 
034900         MOVE REQU-IDLEVNR-KOLLI-KEY TO WS-IDLEVNR-KOLLI                  
035000     END-IF                                                               
035100     .                                                                    
035200     EJECT                                                                
035300 BB-KOLLA-IDOKOLLI SECTION.                                               
035400                                                                          
035500     IF REQU-IDOKOLLI-KEY IS NUMERIC                                      
035600         MOVE REQU-IDOKOLLI-KEY  TO WS-IDOKOLLI                           
035700     ELSE                                                                 
035800         MOVE NEJ                TO NYCKLAR-SW                            
035900     END-IF                                                               
036000     .                                                                    
036100     EJECT                                                                
036200 BC-KOLLA-IDLOPNRM SECTION.                                               
036300                                                                          
036400     IF REQU-IDLOPNRM-KEY IS NUMERIC                                      
036500         MOVE REQU-IDLOPNRM-KEY  TO WS-IDLOPNRM                           
036600     ELSE                                                                 
036700         MOVE NEJ                TO NYCKLAR-SW                            
036800     END-IF                                                               
036900     .                                                                    
037000     EJECT                                                                
037100 BD-KOLLA-IDDC     SECTION.                                               
037200                                                                          
037300     MOVE REQU-IDDC-KEY        TO W-IDDC-B6                               
037310                                  WS-IDDC                                 
037400     PERFORM IMS-GU-WDB601                                                
037500                                                                          
037600     IF DCS-KDDC = SPACE OR DCS-DDC                                       
037700         MOVE NEJ              TO NYCKLAR-SW                              
037800     ELSE                                                                 
037900         MOVE DCS-IDDC         TO W-IDDC                                  
038000                                  W-6005-IDDC                             
038100     END-IF                                                               
038200     .                                                                    
038300     EJECT                                                                
038400 F-LAES-VISA-INFO SECTION.                                                
038500                                                                          
038600     PERFORM FA-LAES-GRUNDDATA                                            
038700                                                                          
038800     IF INL-SEGMENT-SAKNAS OR FLDIVKLI OR INL-IDDC NOT = W-IDDC           
038900        IF FLDIVKLI                                                       
039000            MOVE ERR-MISC-CASE   TO RESP-IDMSG-ERROR                      
039100            MOVE ALL-SPACE       TO RESP-IDLOPNRM-KEY                     
039200         ELSE                                                             
039300           IF SEGMENT-SAKNAS                                              
039400             MOVE ERR-MISS-REGISTER    TO RESP-IDMSG-ERROR                
039500           ELSE                                                           
039600             MOVE ERR-PF11-AND-NO-DATA TO RESP-IDMSG-ERROR                
039700           END-IF                                                         
039800        END-IF                                                            
039900        PERFORM MFS-RENSA-FAELT-UT                                        
040000     END-IF                                                               
040100     .                                                                    
040200     EJECT                                                                
040300 FA-LAES-GRUNDDATA SECTION.                                               
040400                                                                          
040500     MOVE NEJ                   TO FLDIVKLI-SW                            
040600                                                                          
040700     IF (REQU-IDLEVNR-KOLLI-KEY  NOT = SPACE AND                          
040800        (REQU-IDOKOLLI-KEY       NUMERIC AND                              
040900         REQU-IDOKOLLI-KEY       >  ZERO))                                
041000                    OR                                                    
041100        (REQU-IDLOPNRM-KEY       = ALL '+' AND                            
041200         WS-IDLEVNR-KOLLI       NOT = SPACE AND                           
041300         WS-IDOKOLLI            > ZERO)                                   
041400                                                                          
041500         PERFORM FAA-LAES-C-INDEX                                         
041600     ELSE                                                                 
041700         PERFORM FAB-LAES-B-INDEX                                         
041800     END-IF                                                               
041900                                                                          
042000     IF SEGMENT-FINNS AND NOT FLDIVKLI                                    
042100         PERFORM FAC-SKAPA-MOD                                            
042200         MOVE JA               TO INL-SEGMENT-SW                          
042300     ELSE                                                                 
042400         MOVE NEJ              TO INL-SEGMENT-SW                          
042500     END-IF                                                               
042600     .                                                                    
042700     EJECT                                                                
042800 FAA-LAES-C-INDEX   SECTION.                                              
042900                                                                          
043000     MOVE LOW-VALUE            TO W-W6D1C1KY-MIN-X                        
043100     MOVE HIGH-VALUE           TO W-W6D1C1KY-MAX-X                        
043200     MOVE WS-IDLEVNR-KOLLI     TO W-D1C1KY-IDLEVNR-KOLLI-MIN              
043300                                  W-D1C1KY-IDLEVNR-KOLLI-MAX              
043400     MOVE WS-IDOKOLLI          TO W-D1C1KY-IDOKOLLI-MIN                   
043500                                  W-D1C1KY-IDOKOLLI-MAX                   
043600     PERFORM IMS-GU-INLD-INLD01                                           
043700     IF SEGMENT-FINNS                                                     
043800         MOVE SEQC-IDDC        TO W-D101KY-IDDC                           
043900         MOVE SEQC-IDLEVNR     TO W-D101KY-IDLEVNR                        
044000                                  W-IDLEVNR                               
044100         MOVE SEQC-IDFS        TO W-D101KY-IDFS                           
044200         MOVE SEQC-TIAVIDAT    TO W-D101KY-TIAVIDAT                       
044300         MOVE SEQC-IDRADNR-INL TO W-IDRADNR-INL                           
044400         MOVE SEQC-IDRADNR     TO W-IDRADNR                               
044500         PERFORM IMS-GU-INLA-INLA21                                       
044600         IF RAD-FLDIVKLI       =  JA                                      
044700             MOVE JA           TO FLDIVKLI-SW                             
044800         END-IF                                                           
044900      ELSE                                                                
045000         MOVE ALL-SPACE         TO RESP-IDLOPNRM-KEY                      
045100     END-IF                                                               
045200     .                                                                    
045300     EJECT                                                                
045400 FAB-LAES-B-INDEX   SECTION.                                              
045500                                                                          
045600     MOVE WS-IDLOPNRM          TO W-IDLOPNRM                              
045700     PERFORM IMS-GU-INLC-INLC01                                           
045800     IF SEGMENT-FINNS                                                     
045900         MOVE SEQB-IDDC        TO W-D101KY-IDDC                           
046000         MOVE SEQB-IDLEVNR     TO W-D101KY-IDLEVNR                        
046100                                  W-IDLEVNR                               
046200         MOVE SEQB-IDFS        TO W-D101KY-IDFS                           
046300         MOVE SEQB-TIAVIDAT    TO W-D101KY-TIAVIDAT                       
046400         MOVE SEQB-IDRADNR-INL TO W-IDRADNR-INL                           
046500     END-IF                                                               
046600     .                                                                    
046700     EJECT                                                                
046800 FAC-SKAPA-MOD      SECTION.                                              
046900                                                                          
047000     PERFORM IMS-GU-INLA-INLA01                                           
047100     IF INL-IDDC = W-IDDC                                                 
047200       PERFORM FACA-FLYTTA-INLA01-INFO                                    
047300                                                                          
047400       PERFORM IMS-GNP-INLA-INLA11                                        
047500       PERFORM FACB-FLYTTA-INLA11-INFO                                    
047600       MOVE INL-IDLBBET      TO RESP-IDLBBET                              
047700                                                                          
047800       PERFORM FACC-HAEMTA-ADRESSER                                       
047900       PERFORM FACD-FLYTTA-ARTIKEL-INFO                                   
048000                                                                          
048100       MOVE W-ADINLOMR           TO W-6006-ADINLOMR                       
048200       PERFORM IMS-GU-PLAA-PLAA11                                         
048300       IF SEGMENT-FINNS                                                   
048400           IF PLAA-6006-KDLORAPP =  2                                     
048500               MOVE JA           TO RESP-FLAR                             
048600            ELSE                                                          
048700               MOVE ALL-SPACE        TO RESP-FLAR                         
048800           END-IF                                                         
048900        ELSE                                                              
049000           MOVE ALL-SPACE        TO RESP-FLAR                             
049100       END-IF                                                             
049200     END-IF                                                               
049300                                                                          
049400     .                                                                    
049500     EJECT                                                                
049600 FACA-FLYTTA-INLA01-INFO  SECTION.                                        
049700                                                                          
049800     MOVE INL-IDFS             TO RESP-IDFS                               
049900     MOVE INL-IDLEVNR          TO RESP-IDLEVNR                            
050000     MOVE INL-TIAVIDAT         TO RESP-TIAVIDAT                           
050100     MOVE INL-TIINLMOT         TO RESP-TIINLMOT                           
050200     .                                                                    
050300     EJECT                                                                
050400 FACB-FLYTTA-INLA11-INFO  SECTION.                                        
050500                                                                          
050600     MOVE ART-ADLAGOMR         TO RESP-ADLAGOMR                           
050700                                  W-ADINLOMR                              
050800     MOVE ART-ADGANG           TO RESP-ADGANG                             
050900     MOVE ART-ADPLATS          TO RESP-ADPLATS                            
051200     MOVE ART-IDARTNR          TO RESP-IDARTNR                            
051300                                  W-IDARTNR                               
051400     MOVE ART-KDLAGEMB         TO RESP-KDLAGEMB                           
051500     MOVE ART-KDRT             TO RESP-KDRT                               
051600     MOVE ART-KDSORT           TO RESP-KDSORT                             
051700     IF ART-KDKVAANT > ZERO                                               
051710       IF REQU-IDMSGVER = 001                                             
051720         MOVE YES              TO RESP-FLKVAANT                           
051730       ELSE                                                               
051800         MOVE JA               TO RESP-FLKVAANT                           
051810       END-IF                                                             
051900     ELSE                                                                 
052020       MOVE NEJ                TO RESP-FLKVAANT                           
052110     END-IF                                                               
052200     MOVE ART-KVAVIS           TO RESP-KVAVIS-HUV                         
052300     MOVE ART-ADTRDEST-KIT     TO RESP-ADTRDEST                           
052400     MOVE ART-KVAVIS-KIT       TO RESP-KVAVIS-KIT                         
052401     MOVE ART-BEART            TO RESP-BEART                              
052402     MOVE ART-BEFT             TO RESP-BEFT                               
052403     MOVE ART-KDARTURS         TO RESP-KDARTURS                           
052404     MOVE ART-KDKVAINL         TO RESP-KDKVAINL                           
052405                                                                          
052410     IF REQU-IDMSGVER = 001                                               
052420        PERFORM FACBA-GET-WEB-INFO                                        
052460     END-IF                                                               
052500*NDC                                                                      
052600     IF REQU-KDMATT = 'U'                                                 
052700       COMPUTE WS-VKART = ART-VKART * CONV-GR-TO-OZ                       
052800       MOVE WS-VKART           TO RESP-VKART                              
052900       MOVE 'OZ'               TO RESP-KDSORT-VIKT                        
053000       COMPUTE WS-VLARTNTO = ART-VLARTNTO * CONV-CM3-TO-IN3               
053100       MOVE WS-VLARTNTO        TO RESP-VLARTNTO                           
053200       MOVE 'CU.IN.'           TO RESP-BESORT-VOLYM                       
053300     ELSE                                                                 
053400       MOVE ART-VKART          TO RESP-VKART                              
053500       MOVE ' G'               TO RESP-KDSORT-VIKT                        
053600       MOVE ART-VLARTNTO       TO RESP-VLARTNTO                           
053700       MOVE '   CM3'           TO RESP-BESORT-VOLYM                       
053800     END-IF                                                               
053900     MOVE ART-KVAVIS           TO RESP-KVAVIS                             
054000     MOVE ART-KVAVIS-PRIO      TO RESP-KVAVIS-PRIO                        
054100     MOVE ART-KVKVAPRIM-BER    TO RESP-KVKVAPRIM                          
054200     MOVE ART-KVKVASEK-BER     TO RESP-KVKVASEK                           
054300                                                                          
054400     MOVE ART-IDLOPNRM         TO RESP-IDLOPNRM-KEY                       
054500     INSPECT RESP-IDLOPNRM-KEY REPLACING LEADING ZERO BY SPACE            
054600                                                                          
054700     EVALUATE ART-KDFARLIG                                                
054800       WHEN 4                                                             
054900         IF REQU-IDSPRAK = 'SV'                                           
055000           MOVE 'JA'             TO RESP-KDFARLIG-TEXT                    
055100         ELSE                                                             
055200           MOVE 'YES'            TO RESP-KDFARLIG-TEXT                    
055300         END-IF                                                           
055400       WHEN 5                                                             
055500         IF REQU-IDSPRAK = 'SV'                                           
055600           MOVE 'ASBEST'         TO RESP-KDFARLIG-TEXT                    
055700         ELSE                                                             
055800           MOVE 'ASBESTOS'       TO RESP-KDFARLIG-TEXT                    
055900         END-IF                                                           
056000       WHEN 6                                                             
056100         IF REQU-IDSPRAK = 'SV'                                           
056200           MOVE 'KEMIKALIER'     TO RESP-KDFARLIG-TEXT                    
056300         ELSE                                                             
056400           MOVE 'CHEMICALS'      TO RESP-KDFARLIG-TEXT                    
056500         END-IF                                                           
056600       WHEN 7                                                             
056700         IF REQU-IDSPRAK = 'SV'                                           
056800           MOVE 'JA'             TO RESP-KDFARLIG-TEXT                    
056900         ELSE                                                             
057000           MOVE 'YES'            TO RESP-KDFARLIG-TEXT                    
057100         END-IF                                                           
057200     END-EVALUATE                                                         
057300     IF ART-FLKVAKAR = JA OR                                              
057400        ART-FLKVAFEL = JA                                                 
057500       MOVE INF-QUARANTINE       TO RESP-IDMSG-INFO                       
057600     END-IF                                                               
057700     .                                                                    
057800     EJECT                                                                
057900 FACBA-GET-WEB-INFO      SECTION.                                         
057901                                                                          
           MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                              
           IF DCS-UNICODE-IDSKYLT                                               
              MOVE 'UTF8'             TO TRAUTF8-KDCP                           
           ELSE                                                                 
              MOVE '278 '             TO TRAUTF8-KDCP                           
           END-IF                                                               
                                                                                
057902     PERFORM IMS-GET-BENA-TEXT                                            
057903     IF SEGMENT-FINNS                                                     
057904        MOVE BENA-TEXT-BEART    TO TRAUTF8-TECONV-FROM                    
057905     ELSE                                                                 
057906        MOVE SPACE              TO TRAUTF8-TECONV-FROM                    
057907                                   BENA-TEXT-BEART                        
057908        MOVE WS-CP-EBCDIC       TO TRAUTF8-KDCP                           
057909     END-IF                                                               
057910                                                                          
           IF TRAUTF8-TECONV-FROM = SPACES                                      
            MOVE 'GB'  TO W-IDSKYLT                                             
            MOVE '278' TO TRAUTF8-KDCP                                          
            PERFORM IMS-GET-BENA-TEXT                                           
            MOVE BENA-TEXT-BEART    TO TRAUTF8-TECONV-FROM                      
           END-IF                                                               
                                                                                
057912*    CALL FROM WEB                                                        
057913*    CONVERT TO UNICODE IF NOT ALREADY SO, STRIP TRAILING SPACE           
057914     CALL WTRAUTF8 USING TRAUTF8-AREA                                     
057915     MOVE TRAUTF8-TECONV-TO  TO RESP-BEART                                
057927     .                                                                    
057928     EJECT                                                                
057930 FACC-HAEMTA-ADRESSER      SECTION.                                       
058000                                                                          
058100     IF ART-IDLOPNRM           >  ZERO                                    
058200         MOVE ART-IDLOPNRM     TO ADR-IDLOPNRM                            
058300                                                                          
058400         CALL W611ADR USING ADR-W611ADR ADR-INLA-PCB ADR-INLC-PCB         
058500                                        ADR-PLAA-PCB                      
058600                                        ADR-WDK6-PCB                      
058700                                        STYR-HANA-PCB                     
058800                                        STYR-PLAA-PCB                     
058900                                                                          
059000         MOVE ADR-ADINLOMR-NXT1 TO RESP-ADINLOMR (1)                      
059100         MOVE ADR-ADINLOMR-NXT2 TO RESP-ADINLOMR (2)                      
059200         MOVE ADR-ADINLOMR-NXT3 TO RESP-ADINLOMR (3)                      
059300         MOVE ADR-ADINLOMR-NXT4 TO RESP-ADINLOMR (4)                      
059400         MOVE ADR-ADINLOMR-NXT5 TO RESP-ADINLOMR (5)                      
059500         MOVE ADR-ADINLOMR-NXT6 TO RESP-ADINLOMR (6)                      
059600      ELSE                                                                
059700         MOVE +1                  TO INDX                                 
059800         PERFORM UNTIL INDX       >  MAX-INDX                             
059900             MOVE ALL-SPACE       TO RESP-ADINLOMR (INDX)                 
060000             ADD +1               TO INDX                                 
060100         END-PERFORM                                                      
060200     END-IF                                                               
060300     .                                                                    
060400     EJECT                                                                
060500 FACD-FLYTTA-ARTIKEL-INFO  SECTION.                                       
060600                                                                          
060700     PERFORM IMS-GU-ARTC-ARTC11                                           
060800*NDC                                                                      
060900     IF DCS-NDC-NA OR DCS-NDC-PF OR DCS-NDC-OTHERS                        
061000       PERFORM IMS-GU-WDK7-WDK711                                         
061100     END-IF                                                               
061200                                                                          
061300     IF SEGMENT-FINNS                                                     
061301       MOVE ARTC-CLAG-IDANSK TO RESP-IDANSK                               
061310       IF NDC-CN OR NDC-US                                                
061320          PERFORM IMS-GU-WDK7-WDK722                                      
061330          IF SEGMENT-FINNS AND XLAG-IDANSK > 0                            
061340             MOVE XLAG-IDANSK   TO RESP-IDANSK                            
061350          END-IF                                                          
061410       END-IF                                                             
061500       MOVE ZERO                TO RESP-KDKONTR                           
061700       MOVE ARTC-CLAG-KVQPACK-3 TO RESP-KVQPACK-3                         
061710                                                                          
061800       IF NDC-CN OR NDC-US                                                
061801          IF NDC-US                                                       
061802             MOVE WC-LAND-US             TO W-IDLAND                      
061803          ELSE                                                            
061804             MOVE WC-LAND-CN             TO W-IDLAND                      
061805          END-IF                                                          
061806          PERFORM IMS-GU-WDK712                                           
061807          IF SEGMENT-FINNS                                                
061808             IF LART-KVQPACK-3 > 0                                        
061809                MOVE LART-KVQPACK-3 TO RESP-KVQPACK-3                     
061810             END-IF                                                       
061840          END-IF                                                          
061850       END-IF                                                             
061900     ELSE                                                                 
062000       MOVE ERR-PART-MISSING TO RESP-IDMSG-ERROR                          
062100       MOVE 'IDARTNR'        TO RESP-IDELMT-ERROR                         
062200       MOVE ALL-SPACE        TO RESP-KVQPACK-3                            
062300                                RESP-IDANSK                               
062400                                RESP-KDKONTR                              
062500     END-IF                                                               
062600                                                                          
062700     PERFORM FACDB-BEHANDLA-ARTD                                          
062800                                                                          
062900     PERFORM IMS-GU-WDF502                                                
063000     IF SEGMENT-FINNS                                                     
063100         MOVE XLEV-BELEVART    TO RESP-BELEV1                             
063200         PERFORM IMS-GN-WDF502                                            
063300         IF SEGMENT-FINNS                                                 
063400           MOVE XLEV-BELEVART    TO RESP-BELEV2                           
063500         ELSE                                                             
063600           MOVE ALL-SPACE        TO RESP-BELEV2                           
063700         END-IF                                                           
063800      ELSE                                                                
063900         MOVE ALL-SPACE        TO RESP-BELEV1                             
064000                                  RESP-BELEV2                             
064100     END-IF                                                               
064200     .                                                                    
064300     EJECT                                                                
064400 FACDB-BEHANDLA-ARTD       SECTION.                                       
064500                                                                          
064600     MOVE NEJ                      TO ARTD-SW                             
064700     PERFORM IMS-GU-ARTD-ARTD11                                           
064800     IF SEGMENT-FINNS                                                     
064900         PERFORM UNTIL SEGMENT-SAKNAS OR ARTD-OK                          
065000             MOVE SALDO-ADBUFFOMR   TO W-ADBUFFOMR                        
065100             IF W-ADBUFFOMR         =  W-ADLAGOMR                         
065200                 PERFORM IMS-GNP-ARTD-ARTD11                              
065300              ELSE                                                        
065400                 MOVE SALDO-ADBUFFOMR  TO RESP-ADBUFFOMR                  
065500                 MOVE SALDO-ADBUFFGANG TO RESP-ADBUFFGANG                 
065600                 MOVE SALDO-ADBUFFPL   TO RESP-ADBUFFPL                   
065700                 MOVE JA               TO ARTD-SW                         
065800             END-IF                                                       
065900         END-PERFORM                                                      
066000      ELSE                                                                
066100         MOVE ALL-SPACE        TO RESP-ADBUFFOMR                          
066200                                  RESP-ADBUFFGANG                         
066300                                  RESP-ADBUFFPL                           
066400         MOVE JA               TO ARTD-SW                                 
066500     END-IF                                                               
066600                                                                          
066700     IF ARTD-OK                                                           
066800         CONTINUE                                                         
066900      ELSE                                                                
067000         PERFORM IMS-GNP-ARTD-ARTD11-FIRST                                
067100         IF SEGMENT-FINNS                                                 
067200             MOVE SALDO-ADBUFFOMR  TO RESP-ADBUFFOMR                      
067300             MOVE SALDO-ADBUFFGANG TO RESP-ADBUFFGANG                     
067400             MOVE SALDO-ADBUFFPL   TO RESP-ADBUFFPL                       
067500          ELSE                                                            
067600             MOVE ALL-SPACE        TO RESP-ADBUFFOMR                      
067700                                      RESP-ADBUFFGANG                     
067800                                      RESP-ADBUFFPL                       
067900         END-IF                                                           
068000     END-IF                                                               
068100     .                                                                    
068200     EJECT                                                                
068300 MFS-RENSA-FAELT-UT SECTION.                                              
068400                                                                          
068500     MOVE ALL-SPACE            TO RESP-IDARTNR                            
068600                                  RESP-KVAVIS-HUV                         
068700                                  RESP-BEART                              
068710                                  RESP-KDKVAINL                           
068800                                  RESP-IDFS                               
068900                                  RESP-TIAVIDAT                           
069000                                  RESP-IDLBBET                            
069100                                  RESP-IDLEVNR                            
069200                                  RESP-TIINLMOT                           
069300                                  RESP-KVAVIS                             
069400                                  RESP-VKART                              
069500                                  RESP-ADLAGOMR                           
069600                                  RESP-ADGANG                             
069700                                  RESP-ADPLATS                            
069800                                  RESP-VLARTNTO                           
069900                                  RESP-KVAVIS-PRIO                        
070000                                  RESP-KDLAGEMB                           
070100                                  RESP-ADBUFFOMR                          
070200                                  RESP-ADBUFFGANG                         
070300                                  RESP-ADBUFFPL                           
070400                                  RESP-KDARTURS                           
070500                                  RESP-KVKVAPRIM                          
070600                                  RESP-BEFT                               
070700                                  RESP-IDANSK                             
070800                                  RESP-KVKVASEK                           
070900                                  RESP-KDSORT                             
071000                                  RESP-KDFARLIG-TEXT                      
071100                                  RESP-ADTRDEST                           
071200                                  RESP-KVAVIS-KIT                         
071300                                  RESP-KVQPACK-3                          
071400                                  RESP-KDKONTR                            
071500                                  RESP-FLAR                               
071600                                  RESP-BELEV1                             
071700                                  RESP-BELEV2                             
071800     MOVE +1                   TO INDX                                    
071900     PERFORM UNTIL INDX        >  MAX-INDX                                
072000         MOVE ALL-SPACE        TO RESP-ADINLOMR (INDX)                    
072100         ADD +1                TO INDX                                    
072200     END-PERFORM                                                          
072300     .                                                                    
072400     EJECT                                                                
072500 MFS-LAES-IN-IGEN SECTION.                                                
072600                                                                          
072700     MOVE MFS-ADD-LAES-IN-FAELT TO RESP-ADINLOMR-PRT-ATTR                 
072800                                                                          
072900     .                                                                    
073000     EJECT                                                                
073100* --- IMS SEKTIONER ---                                                   
073200     SKIP3                                                                
073300 IMS-GU-INLA-INLA01 SECTION.                                              
073400     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
073500          DELIMITED BY SIZE INTO SSA1                                     
073600     MOVE '  GE' TO GODK-STATUSKODER                                      
073700     CALL CBLTDLI USING GU INLA-PCB DLI-IO-AREA2 SSA1                     
073800     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
073900     PERFORM IMS-STATUSKONTROLL                                           
074000     .                                                                    
074100     SKIP3                                                                
074200 IMS-GNP-INLA-INLA11 SECTION.                                             
074300     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
074400          DELIMITED BY SIZE INTO SSA1                                     
074500     MOVE '  GE' TO GODK-STATUSKODER                                      
074600     CALL CBLTDLI USING GNP INLA-PCB DLI-IO-AREA SSA1                     
074700     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
074800     PERFORM IMS-STATUSKONTROLL                                           
074900     .                                                                    
075000     SKIP3                                                                
075100 IMS-GU-INLA-INLA21 SECTION.                                              
075200     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
075300          DELIMITED BY SIZE INTO SSA1                                     
075400     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
075500          DELIMITED BY SIZE INTO SSA2                                     
075600     STRING 'W6INLA21(IDRADNR  =' W-IDRADNR-X ')'                         
075700          DELIMITED BY SIZE INTO SSA3                                     
075800     MOVE '  GE' TO GODK-STATUSKODER                                      
075900     CALL CBLTDLI USING GU INLA-PCB DLI-IO-AREA2 SSA1 SSA2 SSA3           
076000     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
076100     PERFORM IMS-STATUSKONTROLL                                           
076200     .                                                                    
076300     SKIP3                                                                
076400 IMS-GU-INLD-INLD01 SECTION.                                              
076500     STRING 'W6INLD01(W6D1C1KY>=' W-W6D1C1KY-MIN-X                        
076600                    '&W6D1C1KY<=' W-W6D1C1KY-MAX-X                        
076700                    '&IDDC     =' W-IDDC ')'                              
076800          DELIMITED BY SIZE INTO SSA1                                     
076900     MOVE '  GE' TO GODK-STATUSKODER                                      
077000     CALL CBLTDLI USING GU INLD-PCB DLI-IO-AREA SSA1                      
077100     MOVE INLD-STATUS-CODE TO STATUS-WS                                   
077200     PERFORM IMS-STATUSKONTROLL                                           
077300     .                                                                    
077400     SKIP3                                                                
077500 IMS-GU-INLC-INLC01 SECTION.                                              
077600     STRING 'W6INLC01(W6D1B1KY =' W-IDLOPNRM-X ')'                        
077700          DELIMITED BY SIZE INTO SSA1                                     
077800     MOVE '  GE' TO GODK-STATUSKODER                                      
077900     CALL CBLTDLI USING GU INLC-PCB DLI-IO-AREA SSA1                      
078000     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
078100     PERFORM IMS-STATUSKONTROLL                                           
078200     .                                                                    
078300     SKIP3                                                                
078400 IMS-GU-PLAA-PLAA11 SECTION.                                              
078500     STRING 'W6PLAA01(W6GXKEY  =' W-W6GXKEY-6005-X ')'                    
078600          DELIMITED BY SIZE INTO SSA1                                     
078700     STRING 'W6PLAA11(W6GXKEY  =' W-W6GXKEY-6006-X ')'                    
078800          DELIMITED BY SIZE INTO SSA1                                     
078900     MOVE '  GE' TO GODK-STATUSKODER                                      
079000     CALL CBLTDLI USING GU PLAA-PCB DLI-IO-AREA SSA1                      
079100     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
079200     PERFORM IMS-STATUSKONTROLL                                           
079300     .                                                                    
079400     EJECT                                                                
079500 IMS-GU-ARTC-ARTC11 SECTION.                                              
079600     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
079700          DELIMITED BY SIZE INTO SSA1                                     
079800     MOVE 'WLARTC11 ' TO SSA2                                             
079900     MOVE '  GE' TO GODK-STATUSKODER                                      
080000     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1 SSA2                 
080100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
080200     PERFORM IMS-STATUSKONTROLL                                           
080300     .                                                                    
080400     SKIP3                                                                
080500 IMS-GU-WDK7-WDK711 SECTION.                                              
080600     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
080700          DELIMITED BY SIZE INTO SSA1                                     
080800     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
080900          DELIMITED BY SIZE INTO SSA2                                     
081000     MOVE '  GE' TO GODK-STATUSKODER                                      
081100     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK711 SSA1 SSA2          
081200     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
081300     PERFORM IMS-STATUSKONTROLL                                           
081400     .                                                                    
081500     SKIP3                                                                
081510 IMS-GU-WDK712 SECTION.                                                   
081520     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
081530          DELIMITED BY SIZE INTO SSA1                                     
081540     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
081550          DELIMITED BY SIZE INTO SSA2                                     
081560     MOVE 'GE  ' TO GODK-STATUSKODER                                      
081570     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK712 SSA1 SSA2          
081580     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
081590     PERFORM IMS-STATUSKONTROLL                                           
081591     .                                                                    
081592     SKIP3                                                                
081593 IMS-GU-WDK7-WDK722 SECTION.                                              
081594                                                                          
081595     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
081596          DELIMITED BY SIZE INTO SSA1                                     
081597     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
081598          DELIMITED BY SIZE INTO SSA2                                     
081599     STRING 'WDK722  (KDSEGKEY =' W-KDSEGKEY-K722-X ')'                   
081600          DELIMITED BY SIZE INTO SSA3                                     
081601     MOVE '  GE'              TO GODK-STATUSKODER                         
081602     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK722 SSA1 SSA2          
081603                                                       SSA3               
081604     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
081605     PERFORM IMS-STATUSKONTROLL                                           
081606     .                                                                    
081607     EJECT                                                                
081610 IMS-GU-ARTD-ARTD11 SECTION.                                              
081700     STRING 'WLARTD01*P(IDARTNR  =' W-IDARTNR-X ')'                       
081800          DELIMITED BY SIZE INTO SSA1                                     
081900     STRING 'WLARTD11(IDDC     =' W-IDDC   ')'                            
082000          DELIMITED BY SIZE INTO SSA2                                     
082100     MOVE '  GE' TO GODK-STATUSKODER                                      
082200     CALL CBLTDLI USING GU ARTD-PCB DLI-IO-AREA SSA1 SSA2                 
082300     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
082400     PERFORM IMS-STATUSKONTROLL                                           
082500     .                                                                    
082600     SKIP3                                                                
082700 IMS-GNP-ARTD-ARTD11 SECTION.                                             
082800     STRING 'WLARTD11(IDDC     =' W-IDDC   ')'                            
082900          DELIMITED BY SIZE INTO SSA1                                     
083000     MOVE '  GE' TO GODK-STATUSKODER                                      
083100     CALL CBLTDLI USING GNP ARTD-PCB DLI-IO-AREA SSA1                     
083200     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
083300     PERFORM IMS-STATUSKONTROLL                                           
083400     .                                                                    
083500     SKIP3                                                                
083600 IMS-GNP-ARTD-ARTD11-FIRST SECTION.                                       
083700     STRING 'WLARTD11*F(IDDC     =' W-IDDC   ')'                          
083800          DELIMITED BY SIZE INTO SSA1                                     
083900     MOVE '  GE' TO GODK-STATUSKODER                                      
084000     CALL CBLTDLI USING GNP ARTD-PCB DLI-IO-AREA SSA1                     
084100     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
084200     PERFORM IMS-STATUSKONTROLL                                           
084300     .                                                                    
084400     EJECT                                                                
084500 IMS-GU-WDF502      SECTION.                                              
084600     STRING 'WDF501  (IDARTNR  =' W-IDARTNR-X ')'                         
084700          DELIMITED BY SIZE INTO SSA1                                     
084800     STRING 'WDF502  (IDLEVNR  =' W-IDLEVNR-X ')'                         
084900          DELIMITED BY SIZE INTO SSA2                                     
085000     MOVE '  GE' TO GODK-STATUSKODER                                      
085100     CALL CBLTDLI USING GU WDF5-PCB DLI-IO-AREA SSA1 SSA2                 
085200     MOVE WDF5-STATUS-CODE TO STATUS-WS                                   
085300     PERFORM IMS-STATUSKONTROLL                                           
085400     .                                                                    
085500     EJECT                                                                
085600 IMS-GN-WDF502      SECTION.                                              
085700     STRING 'WDF501  (IDARTNR  =' W-IDARTNR-X ')'                         
085800          DELIMITED BY SIZE INTO SSA1                                     
085900     STRING 'WDF502  (IDLEVNR  =' W-IDLEVNR-X ')'                         
086000          DELIMITED BY SIZE INTO SSA2                                     
086100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
086200     CALL CBLTDLI USING GN WDF5-PCB DLI-IO-AREA SSA1 SSA2                 
086300     MOVE WDF5-STATUS-CODE TO STATUS-WS                                   
086400     PERFORM IMS-STATUSKONTROLL                                           
086500     .                                                                    
086600     EJECT                                                                
086700 IMS-GU-WDB601    SECTION.                                                
086800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
086900          DELIMITED BY SIZE INTO SSA1                                     
087000     MOVE '  GE' TO GODK-STATUSKODER                                      
087100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
087200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
087300     PERFORM IMS-STATUSKONTROLL                                           
087400     IF SEGMENT-SAKNAS                                                    
087500         MOVE SPACE TO DCS-KDDC                                           
087600     END-IF                                                               
087700     .                                                                    
087710 IMS-GET-BENA-TEXT SECTION.                                               
087720     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
087730             DELIMITED BY SIZE INTO SSA1                                  
087740     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
087750              DELIMITED BY SIZE INTO SSA2                                 
087760     MOVE '  GE' TO GODK-STATUSKODER                                      
087770     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA-BENA11 SSA1 SSA2          
087780     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
087790     PERFORM IMS-STATUSKONTROLL                                           
087791     .                                                                    
087792     EJECT                                                                
087800 IMS-STATUSKONTROLL SECTION.                                              
087900                                                                          
088000     SET STATUS-IX TO 1                                                   
088100     SEARCH GODK-STATUS                                                   
088200       AT END                                                             
088300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
088400         DELIMITED BY SIZE INTO FELTEXT                                   
088500         CALL FELLOG                                                      
088600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
088700         CONTINUE                                                         
088800     END-SEARCH                                                           
088900     .                                                                    
