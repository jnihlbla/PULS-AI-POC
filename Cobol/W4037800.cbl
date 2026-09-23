000100*                                                                         
000200******************************************************************        
000300*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0134      *        
000400******************************************************************        
000500*                                                                         
000600 ID DIVISION.                                                             
000700 PROGRAM-ID.     W4037800.                                                
000800 AUTHOR.         CAMELIA OLGRENER.                                        
000900 DATE-WRITTEN.   90/08/14.                                                
001000 DATE-COMPILED.                                                           
001100                                                                          
001200                                                                          
001300*    FUNKTION.                                                            
001400*        MPP-PROGRAM SOM INGÅR I WOPS.                                    
001500*                                                                         
001600*        PROGRAMMET STARTAS AV OCH EFTER W4037700 ÄR KLART                
001700*        (UTSKRIFT PACKUNDERLAG).                                         
001800*                                                                         
001900*        PROGRAMMET LÄGGER UPP SEGMENT PÅ :                               
002000*                                                                         
002100*        - WDE4   - WDE401 (ORDERHUVUD)                                   
002200*                   WDE411 (ORDERRADER)                                   
002300*                   WDE421 (KUNDORDERREG)                                 
002400*                   WDE401 (CSEQ)                                         
002500*                                                                         
002600*        - WDE6   - WDE601 (KOLLIREG - ORDERHUVUD LAGRETS ORDER)          
002700*                 * OM DET ÄR AUTOMAT PACKNING :                          
002800*                 - WDE611 (KOLLIREG - KOLLISEG)                          
002900*                                                                         
003000*        - WDG6   - WDG601 (RECORD TILL KONTROLLLISTAN                    
003100*                           OCH ANDRA SEKV.FILER)                         
003200*                                                                         
003300*                 * OM DET ÄR AUTOMAT PACKNING OCH AUTOMAT FAKT :         
003400*        - WDG7   - WDG717 (WDGX4726 - AUTOMATFAKTURERINGSREG)            
003500*                 - WDG718 (WDGX4727 -       ''           '' )            
003600                                                                          
003700*                                                                         
003800*        - WDR4   - WDR401 (    4487 - ORDERPLANNERING/LAGRET)            
003900*                 -        (WDGX4488 -       ''          ''  )            
004000*                 -        (WDGX4490 -       ''          ''  )            
004100*                                                                         
004200*                                                                         
004300*        PROGRAMMET LÄSER : - WDQ2 (ORDERHUVUDREG KÖ):                    
004400*                                   - WDQ201                              
004500*                                   - WDQ212                              
004600*                                                                         
004700*                           - WDQ3 (ORDERDELSKÖ):                         
004800*                                   - WDQ301                              
004900*                                                                         
005000*                           - WDR1 (PRC TABELLER):                        
005100*                                   - WDGX4447                            
005200*                                   - WDGX4448                            
005300*                                                                         
005400*                           - WDR4 (PLOCKSATSKÖ):                         
005500*                                   - WDGX4463                            
005600*                                   - WDGX4464                            
005700*                                   - WDGX4468                            
005800*                                                                         
005900*                           - WDD1 (ARTIKELINFORMATION):                  
006000*                                   - WDD102                              
006100*                                                                         
006200*                           - WDB6 (DC-INFORMATION):                      
006300*                                   - WDB601                              
006400*                                                                         
006500*    INDATA.                                                              
006600*        TRANSAKTION: W4T378X                                             
006700*        MID:         W4I37801                                            
006800*                                                                         
006900*    UTDATA.                                                              
007000*                                                                         
007100*    CHANGE LOG                                                           
007200*                                                                         
007300*    DIGAMBAR/021011                                                      
007400*    STRUCTURE OF ACTION TRANSACTION 4487 IS CHANGED TO IMPROVE           
007500*    THE RESPONSE TIME OF THE SCREEN 4312.                                
007600*                                                                         
007700*    E-TRACKER: 7450328  2008-HÖST  VOHF                                  
007800*    E-TRACKER: 10254592 2015       DECOMISSION VOHF                      
007900*                                                                         
008000     SKIP3                                                                
008100 ENVIRONMENT DIVISION.                                                    
008200     EJECT                                                                
008300 DATA DIVISION.                                                           
008400 WORKING-STORAGE SECTION.                                                 
008500                                                                          
008600*    -- CHECKED BY WY2000                                                 
008700 77  IDPGM                       PIC X(08)   VALUE 'W4037800'.            
008800 77  JA                          PIC X       VALUE 'J'.                   
008900 77  YES                         PIC X       VALUE 'Y'.                   
009000 77  NEJ                         PIC X       VALUE 'N'.                   
009100 77  FILLER                      PIC X(8)    VALUE 'ERRORTEX'.            
009200 77  ERROR-TEXT                  PIC X(48)   VALUE SPACE.                 
009300                                                                          
009400 77  FILLER                      PIC X(08)   VALUE 'CURRENT'.             
009500 77  WS-CURRENT-SECTION          PIC X(48)   VALUE SPACE.                 
009600 77  FILLER                      PIC X(08)   VALUE 'IMS-SEC'.             
009700 77  WS-CURRENT-IMS-SECTION      PIC X(48)   VALUE SPACE.                 
009800                                                                          
009900 77  WS-IDKONTO-ALFA             PIC X(11)   VALUE SPACE.                 
010000 77  WS-TIRFS-ALFA               PIC X(11)   VALUE SPACE.                 
010100                                                                          
010200 77  MAX-RADER                   PIC S9(9)   VALUE 100  COMP SYNC.        
010300 77  RAD-INDEX                   PIC S9(9)   VALUE ZERO COMP SYNC.        
010500 77  FG-INDX                     PIC S9(9)   VALUE ZERO COMP SYNC.        
010600 77  MAX-FG-INDX                 PIC S9(9)   VALUE +10  COMP SYNC.        
010700 77  KDRC-DISP                   PIC 9(4)    VALUE ZERO.                  
010800 77  WS-IDCOM                    PIC S9(9)   VALUE ZERO COMP-3.           
010900 77  W-IDSHIPM                   PIC 9(7)    VALUE ZERO.                  
011000                                                                          
011100 77  KVORDRAD-RAEKNARE           PIC S9(6)   VALUE ZERO COMP-3.           
011200 77  SUORDV-RAEKNARE             PIC S9(9)V9(2)                           
011300                                             VALUE ZERO COMP-3.           
011400 77  SUORDV-RAEKNARE-EXP         PIC S9(9)V9(2)                           
011500                                             VALUE ZERO COMP-3.           
011600 77  SUORDV-RAEKNARE-LOC         PIC S9(9)V9(2)                           
011700                                             VALUE ZERO COMP-3.           
011800 77  SUORDV-RAEKNARE-LOCPREL     PIC S9(9)V9(2)                           
011900                                             VALUE ZERO COMP-3.           
012000 77  VKORDNTO-RAEKNARE           PIC S9(9)V9(1)                           
012100                                             VALUE ZERO COMP-3.           
012200 77  VLORDNTO-RAEKNARE           PIC S9(4)V9(3)                           
012300                                             VALUE ZERO COMP-3.           
012400 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
012500                                                                          
012600 77  SPRAK-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
012700                                                                          
012800 77  FELTEXT                     PIC X(64)  VALUE SPACE.                  
012900 77  RKOD-ABEND                  PIC S9(4)  VALUE +33   COMP SYNC.        
013000 77  IX                          PIC S9(9)  VALUE ZERO  COMP SYNC.        
013100 77  IX-MAX                      PIC S9(9)  VALUE +7    COMP SYNC.        
013200     EJECT                                                                
013300 77  ALLT-SW                     PIC X.                                   
013400     88  ALLT-OK                             VALUE 'J'.                   
013500     88  ALLT-FEL                            VALUE 'N'.                   
013600                                                                          
013700 77  AUTOMAT-PACK-SW             PIC X.                                   
013800     88  AUTOMAT-PACKNING                    VALUE 'J'.                   
013900                                                                          
014000 77  SEG-4010-SW                 PIC X.                                   
014100     88  SEG-4010-FINNS                      VALUE 'J'.                   
014200     88  SEG-4010-FINNS-EJ                   VALUE 'N'.                   
014300                                                                          
014400 77  SEG-WDE601-SW               PIC X.                                   
014500     88  SEG-WDE601-FINNS                    VALUE 'J'.                   
014600     88  SEG-WDE601-FINNS-EJ                 VALUE 'N'.                   
014700                                                                          
014800 77  BEKUNDRF-SW                 PIC X       VALUE 'N'.                   
014900     88  BEKUNDRF-OK                         VALUE 'J'.                   
015000     88  BEKUNDRF-FEL                        VALUE 'N'.                   
015100                                                                          
015200 77  SW-TIKLAR-UPPDATERAD        PIC X(01).                               
015300                                                                          
015400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
015500     88  GODK-TRANS                          VALUE '4377' '4378'          
015600                                                   '437A' '437B'.         
015700*      --- VALID IDDC CODES                                               
015800*                                                                         
015900*01    -COPY WWDC99                                                       
016000       EJECT                                                              
016100*01    -COPY WWDCKONS                                                     
016200                                                                          
016300 01  WS-PRODNR-PLKLST.                                                    
016400     03  WS-IDPRODNR             PIC S9(7)   VALUE ZERO COMP-3.           
016500     03  WS-IDPLKLST             PIC S9(3)   VALUE ZERO COMP-3.           
016600                                                                          
016700 01  WS-SPAR-PRODNR-PLKLST.                                               
016800     03  WS-SPAR-IDPRODNR        PIC S9(7)   VALUE ZERO COMP-3.           
016900     03  WS-SPAR-IDPLKLST        PIC S9(3)   VALUE ZERO COMP-3.           
017000                                                                          
017100 01  WS-KLOCKAN.                                                          
017200     03  WS-TIHHMMSS             PIC  9(6).                               
017300     03  FILLER                  PIC  X(2).                               
017400                                                                          
017500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
017600 01  GENERELLA-SUBPROGRAM.                                                
017700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
017800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
017900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
018000     EJECT                                                                
018100 01  GEMENSAMMA-SUBPROGRAM.                                               
018200*                                                                         
018300     03  W411DNOT                PIC X(8)    VALUE 'W411DNOT'.            
018400*                                                                         
018500                                                                          
018600*    --- PARAMETRAR TILL GEMESAMMA SUBPROGRAM                             
018700                                                                          
018800 01  FILLER                      PIC X(8)    VALUE 'W411DNOT'.            
018900*    -COPY W411DNOT                                                       
019000     EJECT                                                                
019100     SKIP3                                                                
019200                                                                          
019300 01  FILLER                     PIC X(16)   VALUE 'WDG6-TRANS'.           
019400*    --- ARBETSAREA FÖR WDG6-TRANSAKTION                                  
019500 01  W-LOGGPOST.                                                          
019600*    03  -COPY WDGZRYE                                                    
019700     EJECT                                                                
019800 01  W-SORTPOST.                                                          
019900*    03  -COPY WDGZRYES                                                   
020000     EJECT                                                                
020100*    --- ARBETSAREA FÖR DISTR-CTEXT                                       
020200 01  FILLER                     PIC X(16)   VALUE 'DIST-CTEXT'.           
020300     SKIP3                                                                
020400 01  TEST-IDDISTR               PIC 9(5)    COMP-3.                       
020500     SKIP2                                                                
020600 01  FILLER REDEFINES TEST-IDDISTR.                                       
020700*    03   -COPY WWDIST03.                                                 
020800     EJECT                                                                
020900 01  FILLER REDEFINES TEST-IDDISTR.                                       
021000*    03   -COPY WWDIST07.                                                 
021100     EJECT                                                                
021200 01  FILLER REDEFINES TEST-IDDISTR.                                       
021300*    03   -COPY WWDIST18.                                                 
021400     SKIP3                                                                
021500 01  FILLER REDEFINES TEST-IDDISTR.                                       
021600*    03   -COPY WWDIST19.                                                 
021700     EJECT                                                                
021800*    --- MID-AREA                                                         
021900*                                                                         
022000 01  FILLER                     PIC X(16)   VALUE 'MID-AREA'.             
022100*                                                                         
022200*01  -COPY W4I37801                                                       
022300     EJECT                                                                
022400*    --- AREOR FÖR MSG HANTERING                                          
022500*                                                                         
022600 01  FILLER                     PIC X(16)   VALUE 'MSG-AREA'.             
022700*                                                                         
022800*01  -COPY WMSGAREA                                                       
022900     EJECT                                                                
023000 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-SW'.           
023100                                                                          
023200 01  P-TO-P-SW.                                                           
023300     03  PTOP-LL                 PIC S9(4)   VALUE 34 COMP SYNC.          
023400     03  PTOP-Z1                 PIC  X(1)   VALUE LOW-VALUE.             
023500     03  PTOP-Z2                 PIC  X(1)   VALUE LOW-VALUE.             
023600     03  PTOP-TRANSKOD           PIC  X(7)   VALUE 'W4T378X'.             
023700     03  FILLER                  PIC  X(1)   VALUE SPACE.                 
023800     03  FILLER                  PIC  X(4)   VALUE '4378'.                
023900     03  PTOP-KDMFSFOR           PIC  X(1).                               
024000*    03  -COPY W4I37801  -PRE PTOP-                                       
024100     EJECT                                                                
024200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
024300*                                                                         
024400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
024500     SKIP3                                                                
024600 01  NYCKLAR-TILL-DLI.                                                    
024700                                                                          
024800*-------- PLCKSATSKÖN                                                     
024900     03  W-4001-IDHTYP-X.                                                 
025000         05  W-4001-IDHTYP       PIC X(4)    VALUE '4007'.                
025100         05  W-4001-IDPRODNR     PIC 9(7).                                
025200         05  W-4001-IDPLKLST     PIC 9(3).                                
025300         05  FILLER              PIC X(16)   VALUE LOW-VALUE.             
025400                                                                          
025500*-------- ORDERHUVUDREG KÖN                                               
025600     03  W-WDQ201-IDORDER-X.                                              
025700         05  W-IDORDER-OHUV      PIC S9(7)   VALUE ZERO COMP-3.           
025800                                                                          
025900     03  W-WDQ212-IDDC-X.                                                 
026000         05  W-IDDC-ARB          PIC X(2).                                
026100                                                                          
026200     03  W-WDQ2CSEQ.                                                      
026300         05  W-IDDISTR-CSEQ      PIC S9(5)   VALUE ZERO COMP-3.           
026400         05  W-IDKUNDNR-CSEQ     PIC S9(7)   VALUE ZERO COMP-3.           
026500         05  FILLER              PIC 9(2)    VALUE ZERO.                  
026600         05  W-IDORDNR5-CSEQ     PIC 9(5).                                
026700         05  FILLER              PIC X(3)    VALUE SPACE.                 
026800                                                                          
026900*-------- ORDERDELSKÖ (WDQ3)                                              
027000     03  W-WDQ301KY-X.                                                    
027100         05  W-IDORDER-ODEL      PIC S9(7)   VALUE ZERO COMP-3.           
027200         05  W-IDDC-ODEL         PIC X(2).                                
027300         05  W-IDPRODNR-ODEL     PIC S9(7)   VALUE ZERO COMP-3.           
027400         05  W-IDPLKLST-ODEL     PIC S9(3)   VALUE ZERO COMP-3.           
027500     EJECT                                                                
027600*-------- KUNDORDERREG                                                    
027700     03  W-WDE401KY-X.                                                    
027800         05  W-IDDISTR-KORD      PIC S9(5)   VALUE ZERO COMP-3.           
027900         05  W-IDKUNDNR-KORD     PIC S9(7)   VALUE ZERO COMP-3.           
028000         05  W-IDKUNDRF-KORD     PIC X(10)   VALUE SPACE.                 
028100         05  W-IDPRODNR-KORD     PIC S9(7)   VALUE ZERO COMP-3.           
028200         05  W-IDPLKLST-KORD     PIC S9(3)   VALUE ZERO COMP-3.           
028300                                                                          
028400     03  W-WDE411-IDPURAD-X.                                              
028500         05  W-IDPURAD-ORAD      PIC S9(5)   VALUE ZERO COMP-3.           
028600                                                                          
028700                                                                          
028800*-------- KOLLIREG                                                        
028900     03  W-WDE601-IDPRODNR-X.                                             
029000         05  W-IDPRODNR-VORD     PIC S9(7)   VALUE ZERO COMP-3.           
029100                                                                          
029200     03  W-WDE611-IDKOLLI-X.                                              
029300         05  W-IDKOLLI-KOLLI     PIC S9(5)   VALUE ZERO COMP-3.           
029400     EJECT                                                                
029500*-------- PRC TABELLER                                                    
029600     03  W-WDGXKEY-4447-X.                                                
029700         05  W-IDHDTYP-4447      PIC X(4)    VALUE '4447'.                
029800         05  W-IDDC-4447         PIC X(2).                                
029900         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
030000                                                                          
030100     03  W-WDGXKEY-4448-X.                                                
030200         05  W-IDPRC-4448        PIC X(4)    VALUE SPACE.                 
030300         05  FILLER              PIC X       VALUE LOW-VALUE.             
030400                                                                          
030500*-------- ORDERPLANNERING/LAGRET                                          
030600     03  W-WDGXKEY-4487-X.                                                
030700         05  W-IDHDTYP-4487      PIC X(4)    VALUE '4487'.                
030800         05  W-IDDC-4487         PIC X(2).                                
030900         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
031000                                                                          
031100     03  W-WDGXKEY-4488-X.                                                
031200         05  W-KDPRCGRP-4488     PIC X(5)    VALUE SPACE.                 
031300                                                                          
031400*-------- AUTOMATFAKTURERING                                              
031500     03  W-WDGXKEY-4726-ROT-X.                                            
031600         05  W-IDHDTYP-4726-ROT  PIC X(4)    VALUE '4726'.                
031700         05  W-FLBATCH-4726-ROT  PIC X       VALUE SPACE.                 
031800         05  FILLER              PIC X(25)   VALUE LOW-VALUE.             
031900                                                                          
032000     03  W-WDGXKEY-4726-X.                                                
032100         05  W-IDDISTR-4726      PIC S9(5)   VALUE ZERO COMP-3.           
032200         05  W-IDKUNDNR-4726     PIC S9(7)   VALUE ZERO COMP-3.           
032300         05  W-IDDC-4726         PIC X(2).                                
032400         05  W-KDFAKTYP-4726     PIC X       VALUE SPACE.                 
032500                                                                          
032600*-------- ARTIKELINFORMATION (WDD1)                                       
032700     03  W-IDARTNR-X.                                                     
032800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
032900                                                                          
033000*-------- DC-INFORMATION (WDB6)                                           
033100     03  W-IDDC-B6-X.                                                     
033200         05 W-IDDC-B6            PIC X(2).                                
033300                                                                          
033400*-------- VOR-REGSITER NYTT (WDA6)                                        
033500   03    W-WDA601KY-MIN-X.                                                
033600     05    W-A601KY-MIN-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
033700     05    W-A601KY-MIN-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
033800     05    W-A601KY-MIN-IDORDNR      PIC 9(07) VALUE ZERO.                
033900     05    FILLER                    PIC X(03) VALUE SPACE.               
034000     05    W-A601KY-MIN-TIREGDAT     PIC S9(7) VALUE ZERO COMP-3.         
034100     05    W-A601KY-MIN-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
034200     05    W-A601KY-MIN-TIREGTID     PIC S9(9) VALUE ZERO COMP-3.         
034300     05    W-A601KY-MIN-TIREGDAT-AVV PIC S9(7) VALUE ZERO COMP-3.         
034400     05    W-A601KY-MIN-TIREGTID-AVV PIC S9(9) VALUE ZERO COMP-3.         
034500                                                                          
034600   03    W-WDA601KY-MAX-X.                                                
034700     05    W-A601KY-MAX-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
034800     05    W-A601KY-MAX-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
034900     05    W-A601KY-MAX-IDORDNR      PIC 9(07) VALUE ZERO.                
035000     05    FILLER                    PIC X(03) VALUE SPACE.               
035100     05    W-A601KY-MAX-TIREGDAT     PIC S9(7) VALUE ZERO COMP-3.         
035200     05    W-A601KY-MAX-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
035300     05    W-A601KY-MAX-TIREGTID     PIC S9(9) VALUE ZERO COMP-3.         
035400     05    W-A601KY-MAX-TIREGDAT-AVV PIC S9(7) VALUE ZERO COMP-3.         
035500     05    W-A601KY-MAX-TIREGTID-AVV PIC S9(9) VALUE ZERO COMP-3.         
035600     EJECT                                                                
035700*------- STATUS-KOD FRÅN IMS                                              
035800                                                                          
035900 01  FILLER                      PIC X(16)   VALUE 'STATUS-WS'.           
036000                                                                          
036100 01  STATUS-WS                   PIC XX.                                  
036200     88  SEGMENT-FINNS                       VALUE '  '.                  
036300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
036400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
036500     88  SLUT-AV-DATA                        VALUE 'GB'.                  
036600     SKIP2                                                                
036700 01  GODK-STATUSKODER.                                                    
036800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
036900     SKIP3                                                                
037000 01  SSA1                        PIC X(160).                              
037100 01  SSA2                        PIC X(96).                               
037200 01  SSA3                        PIC X(96).                               
037300     EJECT                                                                
037400*    --- IMS FUNKTIONSKODER                                               
037500*01  -COPY W0003                                                          
037600     EJECT                                                                
037700*    ---  DLI INPUT-OUTPUT AREA                                           
037800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-4001'.         
037900     SKIP3                                                                
038000 01  DLI-IO-AREA-4001.                                                    
038100     03  WL400701.                                                        
038200*        05  -COPY WDGX4007                                               
038300     EJECT                                                                
038400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-4008'.         
038500     SKIP3                                                                
038600 01  DLI-IO-AREA-4008.                                                    
038700     03  WL400711.                                                        
038800*        05  -COPY WDGX4008                                               
038900     EJECT                                                                
039000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-4010'.         
039100     SKIP3                                                                
039200 01  DLI-IO-AREA-4010.                                                    
039300     03  WL400721.                                                        
039400*        05  -COPY WDGX4010                                               
039500     EJECT                                                                
039600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-Q201'.         
039700     SKIP3                                                                
039800 01  DLI-IO-AREA-OHUV.                                                    
039900     03  WLORQI01.                                                        
040000*        05  -COPY WDQ201                                                 
040100     EJECT                                                                
040200 01  FILLER                      PIC X(16) VALUE 'DLI-IO-Q201-OC'.        
040300     SKIP3                                                                
040400 01  DLI-IO-AREA-WDQ201.                                                  
040500*    03  -COPY WDQ201 -PRE OC-                                            
040600     EJECT                                                                
040700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-Q212'.         
040800     SKIP3                                                                
040900 01  DLI-IO-AREA-ARB.                                                     
041000     03  WLORQI12.                                                        
041100*        05  -COPY WDQ212                                                 
041200     EJECT                                                                
041300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-Q301'.         
041400     SKIP3                                                                
041500 01  DLI-IO-AREA-ODEL.                                                    
041600     03  WLORQA01.                                                        
041700*        05  -COPY WDQ301                                                 
041800     EJECT                                                                
041900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-E401'.         
042000     SKIP3                                                                
042100 01  DLI-IO-WDE401.                                                       
042200*    03  -COPY WDE401                                                     
042300     EJECT                                                                
042400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-E411'.         
042500     SKIP3                                                                
042600 01  DLI-IO-WDE411.                                                       
042700*    03  -COPY WDE411                                                     
042800     EJECT                                                                
042900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-E421'.         
043000     SKIP3                                                                
043100 01  DLI-IO-WDE421.                                                       
043200*    03  -COPY WDE421                                                     
043300     EJECT                                                                
043400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-E601'.         
043500     SKIP3                                                                
043600 01  DLI-IO-WDE601.                                                       
043700*    03  -COPY WDE601                                                     
043800     EJECT                                                                
043900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-E611'.         
044000     SKIP3                                                                
044100 01  DLI-IO-WDE611.                                                       
044200*    03  -COPY WDE611                                                     
044300     EJECT                                                                
044400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-WDG6'.         
044500     SKIP3                                                                
044600 01  DLI-IO-AREA-WDG6.                                                    
044700     03  WLZZAC01.                                                        
044800*        05  -COPY WDGZ01  -PRE WDG6-                                     
044900     EJECT                                                                
045000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-4448'.         
045100     SKIP3                                                                
045200 01  DLI-IO-AREA-4448.                                                    
045300     03  WLXXKH11.                                                        
045400*        05  -COPY WDGX4448                                               
045500     EJECT                                                                
045600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-4487'.         
045700     SKIP3                                                                
045800 01  DLI-IO-AREA-4487.                                                    
045900     03  WL448701.                                                        
046000*        05  -COPY WDGX4487                                               
046100     EJECT                                                                
046200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-4488'.         
046300     SKIP3                                                                
046400 01  DLI-IO-AREA-4488.                                                    
046500     03  WL448711.                                                        
046600*        05  -COPY WDGX4488                                               
046700     EJECT                                                                
046800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-4490'.         
046900     SKIP3                                                                
047000 01  DLI-IO-AREA-4490.                                                    
047100     03  WDGX4490.                                                        
047200*        05  -COPY WDGX4490                                               
047300     EJECT                                                                
047400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-4726'.         
047500     SKIP3                                                                
047600 01  DLI-IO-AREA-4726.                                                    
047700     03  WLXXDV11.                                                        
047800*        05  -COPY WDGX4726                                               
047900     EJECT                                                                
048000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-4727'.         
048100     SKIP3                                                                
048200 01  DLI-IO-AREA-4727.                                                    
048300     03  WLXXDV21.                                                        
048400*        05  -COPY WDGX4727                                               
048500                                                                          
048600 01  FILLER                      PIC X(16)   VALUE 'WDB601 AREA'.         
048700 01   DLI-IO-AREA-B601.                                                   
048800*     03  -COPY WDB601                                                    
048900                                                                          
049000 01  FILLER                      PIC X(16)   VALUE 'WDA601 AREA'.         
049100 01   DLI-IO-AREA-A601.                                                   
049200*     03  -COPY WDA601                                                    
049300     EJECT                                                                
049400 LINKAGE SECTION.                                                         
049500                                                                          
049600*01  -COPY W0009      -PRE MSG-                                           
049700     EJECT                                                                
049800*01  -COPY W0009      -PRE ALT1-                                          
049900     EJECT                                                                
050000*01  -COPY W0008      -PRE 4007-                                          
050100     05  FILLER                  PIC X.                                   
050200     EJECT                                                                
050300*01  -COPY W0008      -PRE ORQI-                                          
050400     05  FILLER                  PIC X.                                   
050500     EJECT                                                                
050600*01  -COPY W0008      -PRE WDQ2-                                          
050700     05  FILLER                  PIC X.                                   
050800     EJECT                                                                
050900*01  -COPY W0008      -PRE ORQA-                                          
051000     05  FILLER                  PIC X.                                   
051100     EJECT                                                                
051200*01  -COPY W0008      -PRE WDE4-                                          
051300     05  FILLER                  PIC X.                                   
051400     EJECT                                                                
051500*01  -COPY W0008      -PRE WDE6-                                          
051600     05  FILLER                  PIC X.                                   
051700     EJECT                                                                
051800*01  -COPY W0008      -PRE ZZAC-                                          
051900     05  FILLER                  PIC X.                                   
052000     EJECT                                                                
052100*01  -COPY W0008      -PRE XXKH-                                          
052200     05  FILLER                  PIC X.                                   
052300     EJECT                                                                
052400*01  -COPY W0008      -PRE 4487-                                          
052500     05  FILLER                  PIC X.                                   
052600     EJECT                                                                
052700*01  -COPY W0008      -PRE XXDV-                                          
052800     05  FILLER                  PIC X.                                   
052900     EJECT                                                                
053000*01  -COPY W0008      -PRE WDB6-                                          
053100     05  FILLER                  PIC X.                                   
053200     EJECT                                                                
053300*01  -COPY W0008      -PRE WDA6B-                                         
053400     05  FILLER                  PIC X.                                   
053500     EJECT                                                                
053600                                                                          
053700 01  DNOT-ORQP-PCB               PIC X.                                   
053800 01  DNOT-ORQP2-PCB              PIC X.                                   
053900 01  DNOT-ORQP3-PCB              PIC X.                                   
054000 01  DNOT-4013-PCB               PIC X.                                   
054100 01  DNOT-BENA-PCB               PIC X.                                   
054200 EJECT                                                                    
054300                                                                          
054400 PROCEDURE DIVISION  USING MSG-PCB   ALT1-PCB                             
054500                           4007-PCB  ORQI-PCB  WDQ2-PCB                   
054600                           ORQA-PCB  WDE4-PCB  WDE6-PCB  ZZAC-PCB         
054700                           XXKH-PCB  4487-PCB  XXDV-PCB  WDB6-PCB         
054800                           WDA6B-PCB                                      
054900                           DNOT-ORQP-PCB                                  
055000                           DNOT-ORQP2-PCB                                 
055100                           DNOT-ORQP3-PCB                                 
055200                           DNOT-4013-PCB                                  
055300                           DNOT-BENA-PCB.                                 
055400                                                                          
055500     ENTRY 'DLITCBL' USING MSG-PCB   ALT1-PCB                             
055600                           4007-PCB  ORQI-PCB  WDQ2-PCB                   
055700                           ORQA-PCB  WDE4-PCB  WDE6-PCB  ZZAC-PCB         
055800                           XXKH-PCB  4487-PCB  XXDV-PCB  WDB6-PCB         
055900                           WDA6B-PCB                                      
056000                           DNOT-ORQP-PCB                                  
056100                           DNOT-ORQP2-PCB                                 
056200                           DNOT-ORQP3-PCB                                 
056300                           DNOT-4013-PCB                                  
056400                           DNOT-BENA-PCB.                                 
056500                                                                          
056600     PERFORM IMS-GET-MSG                                                  
056700                                                                          
056800     IF SEGMENT-FINNS                                                     
056900        PERFORM A-INIT                                                    
057000        IF ALLT-OK                                                        
057100           PERFORM B-LAES-BEHANDLA-PLOCKSATS                              
057200           IF SEG-4010-FINNS                                              
057300              MOVE MID-W4I37801 TO PTOP-MID-W4I37801                      
057400              PERFORM IMS-ISRT-MSG-ALT1                                   
057500           ELSE                                                           
057600              PERFORM IMS-GHU-4007-4001                                   
057700              PERFORM IMS-DLET-4007-4001                                  
057800           END-IF                                                         
057900        END-IF                                                            
058000     END-IF                                                               
058100                                                                          
058200     MOVE ZERO TO RETURN-CODE                                             
058300     GOBACK                                                               
058400     .                                                                    
058500     EJECT                                                                
058600 A-INIT SECTION.                                                          
058700     MOVE 'A-INIT         '   TO WS-CURRENT-SECTION                       
058800                                                                          
058900     MOVE JA  TO ALLT-SW                                                  
059000                                                                          
059100     MOVE 0  TO  KVORDRAD-RAEKNARE                                        
059200                 SUORDV-RAEKNARE                                          
059300                 SUORDV-RAEKNARE-EXP                                      
059400                 SUORDV-RAEKNARE-LOC                                      
059500                 SUORDV-RAEKNARE-LOCPREL                                  
059600                 VKORDNTO-RAEKNARE                                        
059700                 VLORDNTO-RAEKNARE                                        
059800                 WDG6-IDLOGLOP                                            
059900                                                                          
060000     ACCEPT DAGENS-DATUM FROM DATE                                        
060100     ACCEPT WS-KLOCKAN   FROM TIME                                        
060200                                                                          
060300     IF MSG-KDTRANS-1  NOT = 'W4T378X '                                   
060400       MOVE NEJ TO ALLT-SW                                                
060500     END-IF                                                               
060600                                                                          
060700     MOVE MSG-IDTRANS-1 TO W-IDTRANS                                      
060800     IF NOT GODK-TRANS                                                    
060900       MOVE NEJ TO ALLT-SW                                                
061000     END-IF                                                               
061100                                                                          
061200     IF ALLT-OK                                                           
061300        MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W4I37801                  
061400        MOVE MSG-KDMFSFOR-1              TO PTOP-KDMFSFOR                 
061500     END-IF                                                               
061600     .                                                                    
061700     EJECT                                                                
061800 B-LAES-BEHANDLA-PLOCKSATS SECTION.                                       
061900     MOVE ' B-LAES-BEHANDLA ' TO WS-CURRENT-SECTION                       
062000                                                                          
062100                                                                          
062200     MOVE MID-IDPRODNR TO W-4001-IDPRODNR                                 
062300     MOVE MID-IDPLKLST TO W-4001-IDPLKLST                                 
062400                                                                          
062500     PERFORM IMS-GU-4007-4008                                             
062600                                                                          
062700     PERFORM BA-LAES-4010-FIRST                                           
062800     PERFORM S01-KOLLA-OM-4010-FINNS                                      
062900                                                                          
063000     PERFORM UNTIL SEG-4010-FINNS-EJ    OR                                
063100                  (RAD-INDEX > MAX-RADER)                                 
063200        PERFORM BB-UPPDATERA-WDR4-HT4487                                  
063300        PERFORM BC-SPARA-PRODNR-PLKLST                                    
063400        PERFORM BD-LAES-FRAM-DATA                                         
063500        PERFORM BE-FLYTTA-DATA-TILL-DB                                    
063600                                                                          
063700        PERFORM UNTIL  SEG-4010-FINNS-EJ        OR                        
063800                      (WS-PRODNR-PLKLST NOT =                             
063900                       WS-SPAR-PRODNR-PLKLST)   OR                        
064000                      (RAD-INDEX > MAX-RADER)                             
064100           PERFORM BF-BERAEKNA-FAELT                                      
064200           PERFORM BG-FLYTTA-INSERT-WDE411                                
064300           IF NDC-US OR NDC-CA                                            
064400              PERFORM S02-DATA-TILL-DEL-NOTE                              
064500           END-IF                                                         
064600           PERFORM BH-FLYTTA-INSERT-WDG6                                  
064700           IF AUTOMAT-PACKNING                                            
064800              PERFORM BI-FLYTTA-INSERT-WDE421                             
064900           ELSE                                                           
065000              PERFORM BJ-FLYTTA-DATA-TILL-WDR4-4490                       
065100           END-IF                                                         
065200           PERFORM IMS-DLET-4007-4010                                     
065300           PERFORM IMS-GHNP-4007-4010                                     
065400           PERFORM S01-KOLLA-OM-4010-FINNS                                
065500        END-PERFORM                                                       
065600                                                                          
065700        PERFORM BK-UPPDATERA-WDE6                                         
065800        PERFORM BL-UPPDAT-WDQ3-OM-AUT-PACK                                
065900        PERFORM BO-UPPDAT-WDQ2-OM-AUT-PACK                                
066000        PERFORM BM-UPPDATERA-WDE4                                         
066100        PERFORM BN-KOLLA-OM-ISRT-4490                                     
066200     END-PERFORM                                                          
066300     .                                                                    
066400     EJECT                                                                
066500 BA-LAES-4010-FIRST SECTION.                                              
066600     MOVE 'BA-LAES-4010-FIRST'     TO WS-CURRENT-SECTION                  
066700                                                                          
066800     PERFORM IMS-GHNP-4007-4010-FIRST                                     
066900     .                                                                    
067000     EJECT                                                                
067100 BB-UPPDATERA-WDR4-HT4487 SECTION.                                        
067200     MOVE 'BB-UPPDATERA-WDR4-HT44' TO WS-CURRENT-SECTION                  
067300                                                                          
067400     IF SEG-4010-FINNS                                                    
067500                                                                          
067600       MOVE 4010-IDDC            TO W-IDDC-4447                           
067700                                    WS-IDDC                               
067800       MOVE 4010-IDPRC           TO W-IDPRC-4448                          
067900                                                                          
068000       PERFORM IMS-GU-XXKH-4448                                           
068100                                                                          
068200       MOVE 4010-IDDC            TO W-IDDC-4487                           
068300       MOVE 4448-KDPRCGRP        TO W-KDPRCGRP-4488                       
068400                                                                          
068500       PERFORM IMS-GU-4487                                                
068600                                                                          
068700       IF SEGMENT-SAKNAS                                                  
068800         MOVE '4487'             TO 4487-IDHTYP                           
068900         MOVE 4010-IDDC          TO 4487-IDDC                             
069000         MOVE LOW-VALUE          TO 4487-LOW-VALUE                        
069100         PERFORM IMS-ISRT-4487                                            
069200                                                                          
069300         MOVE 4448-KDPRCGRP      TO 4488-KDPRCGRP                         
069400         PERFORM IMS-ISRT-WDGX4488                                        
069500       ELSE                                                               
069600         PERFORM IMS-GNP-WDGX4488                                         
069700         IF SEGMENT-SAKNAS                                                
069800           MOVE 4448-KDPRCGRP    TO 4488-KDPRCGRP                         
069900           PERFORM IMS-ISRT-WDGX4488                                      
070000         END-IF                                                           
070100       END-IF                                                             
070200                                                                          
070300     END-IF                                                               
070400     .                                                                    
070500     EJECT                                                                
070600 BC-SPARA-PRODNR-PLKLST SECTION.                                          
070700     MOVE 'BC-SPARA-PRODNR-PLKLST' TO WS-CURRENT-SECTION                  
070800                                                                          
070900     MOVE 4010-IDPRODNR TO WS-SPAR-IDPRODNR                               
071000                                                                          
071100     MOVE 4010-IDPLKLST TO WS-SPAR-IDPLKLST                               
071200     .                                                                    
071300     EJECT                                                                
071400 BD-LAES-FRAM-DATA SECTION.                                               
071500     MOVE 'BD-LAES-FRAM-DATA   ' TO WS-CURRENT-SECTION                    
071600                                                                          
071700     IF 4010-IDORDER NOT = W-IDORDER-OHUV                                 
071800                                                                          
072000       MOVE 4010-IDORDER  TO W-IDORDER-OHUV                               
072100       MOVE 4010-IDDC     TO WS-IDDC                                      
072200                             W-IDDC-ARB                                   
072300                             W-IDDC-B6                                    
072400                                                                          
072500       PERFORM IMS-GU-ORQI-WDQ201                                         
072600       PERFORM IMS-GU-WDB601                                              
072900       PERFORM IMS-GHNP-ORQI-WDQ212                                       
073000                                                                          
073500     END-IF                                                               
073600     .                                                                    
073700     EJECT                                                                
073800 BE-FLYTTA-DATA-TILL-DB SECTION.                                          
073900     MOVE 'BE-FLYTTA-DATA-TILL-DB'  TO WS-CURRENT-SECTION                 
074000                                                                          
074100     PERFORM BEA-FLYTTA-INSERT-WDE401                                     
074200                                                                          
074300*    MOVE 4010-IDPRODNR TO KVORD-IDPRODNR                                 
074400*                          W-IDPRODNR-KVORD                               
074500                                                                          
074600     PERFORM BEC-KOLLA-OM-AUTOMAT-PACKNING                                
074700     PERFORM BED-KOLLA-OM-WDE601-FINNS                                    
074800                                                                          
074900     IF AUTOMAT-PACKNING                                                  
075000       PERFORM BEE-FOERBERED-NYCKLAR-WDQ3                                 
075100       PERFORM BEF-UPPDAT-WDE6-SOM-PACKAD                                 
075200       PERFORM BEG-KOLLA-OM-AUTOMAT-FAKT                                  
075300                                                                          
075400     ELSE                                                                 
075500       MOVE 4010-TIRFS        TO 4490-DARFS                               
075600       IF 4010-TIRFS NOT = ZERO                                           
075700         IF 4010-TIRFS < 5000000000                                       
075800           MOVE 20            TO 4490-DARFS (1:2)                         
075900         ELSE                                                             
076000           IF 4010-TIRFS < 9999999999                                     
076100             MOVE 19          TO 4490-DARFS (1:2)                         
076200           ELSE                                                           
076300             MOVE 999999999999 TO 4490-DARFS                              
076400           END-IF                                                         
076500         END-IF                                                           
076600       END-IF                                                             
076700     END-IF                                                               
076800     .                                                                    
076900     EJECT                                                                
077000 BEA-FLYTTA-INSERT-WDE401 SECTION.                                        
077100     MOVE 'BEA-FLYTTA-INSERT-WDE401'  TO WS-CURRENT-SECTION               
077200                                                                          
077300     MOVE OHUV-IDDISTR           TO KORD-IDDISTR                          
077400                                    W-IDDISTR-KORD                        
077500     MOVE OHUV-IDKUNDNR          TO KORD-IDKUNDNR                         
077600                                    W-IDKUNDNR-KORD                       
077700     MOVE SPACE                  TO KORD-IDKUNDRF                         
077800     MOVE 4010-IDKUNDRF (3:5)    TO KORD-IDKUNDRF                         
077900                                    W-IDKUNDRF-KORD                       
078000     MOVE 4010-IDPRODNR          TO KORD-IDPRODNR                         
078100                                    W-IDPRODNR-KORD                       
078200     MOVE 4010-IDPLKLST          TO KORD-IDPLKLST                         
078300                                    W-IDPLKLST-KORD                       
078400     MOVE OHUV-IDORDER           TO KORD-IDORDER                          
078500     MOVE 4010-IDUSER            TO KORD-IDUSER                           
078600     MOVE 4010-IDDC              TO KORD-IDDC                             
078700                                                                          
078800     IF OHUV-FLEMBORD = 'J'                                               
078900       MOVE +1                   TO KORD-KDFAKPAP                         
079000     ELSE                                                                 
079100       MOVE ZERO                 TO KORD-KDFAKPAP                         
079200     END-IF                                                               
079300                                                                          
079400     MOVE OHUV-KDFAKTYP          TO KORD-KDFAKTYP                         
079500     MOVE 4010-KDFDKRAV          TO KORD-KDFDKRAV                         
079600     MOVE ARB-KDFRAKT            TO KORD-KDFRAKT                          
079700     MOVE OHUV-KDORDKL           TO KORD-KDORDKL                          
079800     MOVE ARB-KDROPACK           TO KORD-KDROPACK                         
079900     IF 4010-PRAVCOST > 0                                                 
080000        MOVE 4010-KDVALISO       TO KORD-KDVALISO-EXP                     
080100        MOVE 'SEK'               TO KORD-KDVALISO                         
080200     ELSE                                                                 
080300        MOVE 4010-KDVALISO       TO KORD-KDVALISO                         
080400        MOVE SPACE               TO KORD-KDVALISO-EXP                     
080500     END-IF                                                               
080600     MOVE ZERO                   TO KORD-KDPERSON                         
080700                                    KORD-KVORDRAD                         
080800                                    KORD-KVORDRAD-LEVPL                   
080900                                    KORD-KVORDRAD-VO                      
081000                                    KORD-KVORDRAD-VO-LEVPL                
081100                                    KORD-SUORDV                           
081200                                    KORD-SUORDV-EXP                       
081300                                    KORD-SUORDV-LOC                       
081400                                    KORD-SUORDV-LOCPREL                   
081500                                    KORD-SUORDV-LEVPL                     
081600                                    KORD-SUORDV-LEVPL-LOC                 
081700                                    KORD-SUORDV-LEVPL-LOCPREL             
081800                                    KORD-TIBEGPAC                         
081900                                    KORD-TIUTSKR                          
082000                                    KORD-IDARTNR-SATS                     
082100                                    KORD-KVBEART-SATS                     
082200                                    KORD-VKORDNTO                         
082300                                    KORD-VLORDNTO                         
082400                                    KORD-KDPAKOLL                         
082500                                    KORD-KVORDRAD-PACK                    
082600                                                                          
082700     MOVE ARB-TIRFS              TO WS-TIRFS-ALFA                         
082800     MOVE WS-TIRFS-ALFA (2:6)    TO KORD-TIBEGPAC                         
082900     MOVE 4010-TIUTSKR           TO KORD-TIUTSKR                          
083000                                                                          
083100     MOVE OHUV-TIREGDAT          TO KORD-TIORDREG                         
083200     MOVE OHUV-FLLSBOK           TO KORD-FLLSBOK                          
083300     MOVE OHUV-FLORDSPE          TO KORD-FLORDSPE                         
083400     MOVE OHUV-FLOVRLEV          TO KORD-FLOVRLEV                         
083500     MOVE 'N'                    TO KORD-FLPAFEL                          
083600                                                                          
083700     PERFORM IMS-ISRT-WDE401                                              
083800     .                                                                    
083900     EJECT                                                                
084000 BEC-KOLLA-OM-AUTOMAT-PACKNING SECTION.                                   
084100     MOVE 'BEC-KOLLA-OM-AUTOMAT-PACKNI' TO WS-CURRENT-SECTION             
084200                                                                          
084300     MOVE NEJ TO AUTOMAT-PACK-SW                                          
084400                                                                          
084500     IF OHUV-FLORDSPE = 'J' OR                                            
084600        OHUV-FLOVRLEV = 'J'                                               
084700                                                                          
084800       IF OHUV-FLAUTPAC = 'J'                                             
084900                                                                          
085000         MOVE JA TO AUTOMAT-PACK-SW                                       
085100                                                                          
085200       END-IF                                                             
085300     END-IF                                                               
085400     .                                                                    
085500     EJECT                                                                
085600 BED-KOLLA-OM-WDE601-FINNS SECTION.                                       
085700     MOVE 'BED-KOLLA-OM-WDE601-FINNS  ' TO WS-CURRENT-SECTION             
085800                                                                          
085900     MOVE 4010-IDPRODNR          TO W-IDPRODNR-VORD                       
086000     PERFORM IMS-GHU-WDE601                                               
086100                                                                          
086200     IF SEGMENT-SAKNAS                                                    
086300       MOVE NEJ                  TO SEG-WDE601-SW                         
086400                                                                          
086500       MOVE 4010-IDPRODNR        TO VORD-IDPRODNR                         
086600                                    VORD-IDPRODNR-SAMP                    
086700       MOVE OHUV-IDDISTR         TO VORD-IDDISTR                          
086800       MOVE OHUV-IDKUNDNR        TO VORD-IDKUNDNR                         
086900       MOVE OHUV-KDORDKL         TO VORD-KDORDKL                          
087000       MOVE MID-IDLOPNR          TO VORD-IDLOTNR                          
087100       MOVE MID-IDPRC (3:2)      TO VORD-KDORDLOT                         
087200       MOVE ZERO                 TO VORD-KDPERSON                         
087300       MOVE ZERO                 TO VORD-ADLEVPL                          
087400       MOVE 'N'                  TO VORD-FLCONTL                          
087500       MOVE 'N'                  TO VORD-FLDIRLEV                         
087600       MOVE 'N'                  TO VORD-FLSPARR                          
087700       MOVE 'N'                  TO VORD-FLMANORD                         
087800       MOVE 4010-IDDC            TO VORD-IDDC                             
087900                                                                          
088000       MOVE 4010-IDORDER         TO W-IDORDER-ODEL                        
088100       MOVE 4010-IDDC            TO W-IDDC-ODEL                           
088200       MOVE 4010-IDPRODNR        TO W-IDPRODNR-ODEL                       
088300       MOVE 4010-IDPLKLST        TO W-IDPLKLST-ODEL                       
088400       PERFORM IMS-GHU-ORQA-WDQ301                                        
088500       MOVE ODEL-IDDC-EXP        TO VORD-IDDC-EXP                         
088600                                                                          
088700       MOVE OHUV-KDFAKTYP        TO VORD-KDFAKTYP                         
088800       MOVE ARB-KDFRAKT          TO VORD-KDFRAKT                          
088900       MOVE ZERO                 TO VORD-KDMETOD                          
089000       MOVE +1                   TO VORD-KDORDSTA                         
089100       IF 4010-PRAVCOST > ZERO                                            
089200          MOVE 'SEK'             TO VORD-KDVALISO                         
089300          MOVE 4010-KDVALISO     TO VORD-KDVALISO-EXP                     
089400       ELSE                                                               
089500          MOVE 4010-KDVALISO     TO VORD-KDVALISO                         
089600          MOVE SPACE             TO VORD-KDVALISO-EXP                     
089700       END-IF                                                             
089800       MOVE ZERO                 TO VORD-KVKOLLI                          
089900                                    VORD-KVKOLLI-FAKT                     
090000                                    VORD-KVKOLLI-LAST                     
090100                                    VORD-KVKOLLI-FL                       
090200                                    VORD-KVKOLPAC                         
090300                                    VORD-KVORDRAD                         
090400                                    VORD-KVORDRAD-PACK                    
090500                                    VORD-SUORDV                           
090600                                    VORD-SUORDV-EXP                       
090700                                    VORD-SUORDV-LOC                       
090800                                    VORD-SUORDV-LOCPREL                   
090900                                    VORD-SUORDV-PACK                      
091000                                    VORD-SUORDV-PACK-LOC                  
091100                                    VORD-SUORDV-PACK-LOCPREL              
091200                                    VORD-SUORDV-FL                        
091300                                    VORD-SUORDV-FL-LOC                    
091400                                    VORD-SUORDV-FL-LOCPREL                
091500                                                                          
091600* OBS - HÄR INITIERAS NYA DDGS-FÄLT RENT GENERELLT                        
091700* OBS - KONTROLLERA OM DETTA ÄR KORREKT!!!                                
091800       MOVE SPACE                TO VORD-KDVIA                            
091900                                    VORD-IDLEVNR                          
092000                                                                          
092100       MOVE OHUV-TIREGTID        TO VORD-TIREGTID                         
092200                                                                          
092300       MOVE ARB-TIRFS            TO WS-TIRFS-ALFA                         
092400       MOVE ARB-BEGMRK           TO VORD-BEGMRK                           
092500       MOVE WS-TIRFS-ALFA (2:6)  TO VORD-DABEGPAC                         
092600       IF VORD-DABEGPAC NOT = ZERO                                        
092700         IF VORD-DABEGPAC < 500000                                        
092800           MOVE 20               TO VORD-DABEGPAC (1:2)                   
092900         ELSE                                                             
093000           IF VORD-DABEGPAC < 999999                                      
093100             MOVE 19             TO VORD-DABEGPAC (1:2)                   
093200           ELSE                                                           
093300             MOVE 99999999       TO VORD-DABEGPAC                         
093400           END-IF                                                         
093500         END-IF                                                           
093600       END-IF                                                             
093700                                                                          
093800       MOVE ZERO                 TO VORD-TIFAKT-SK                        
093900                                    VORD-TILASTN-SK                       
094000                                    VORD-TIPACKN-SK                       
094100       MOVE 4010-TIUTSTID        TO VORD-TIUTSTID                         
094200       MOVE 4010-TIUTSKR         TO VORD-TIUTSKR                          
094300       MOVE 4010-TIRFS           TO VORD-DARFS                            
094400       IF 4010-TIRFS NOT = ZERO                                           
094500         IF 4010-TIRFS < 5000000000                                       
094600           MOVE 20               TO VORD-DARFS (1:2)                      
094700         ELSE                                                             
094800           IF 4010-TIRFS < 9999999999                                     
094900             MOVE 19             TO VORD-DARFS (1:2)                      
095000           ELSE                                                           
095100             MOVE 999999999999   TO VORD-DARFS                            
095200           END-IF                                                         
095300         END-IF                                                           
095400       END-IF                                                             
095500       MOVE ZERO                 TO VORD-VKORDBTO                         
095600                                    VORD-VKORDBTO-FL                      
095700                                    VORD-VKORDNTO                         
095800                                    VORD-VLORDBTO                         
095900                                    VORD-VLORDBTO-FL                      
096000                                    VORD-VLORDNTO                         
096100                                                                          
096200       MOVE OHUV-IDDISTR TO TEST-IDDISTR                                  
096300       IF OHUV-FLORDSPE = JA                                              
096400         MOVE OHUV-FLAUTFAK      TO VORD-FLAUTFAK                         
096500       ELSE                                                               
096600         IF DIST03-SVERIGE AND SDC                                        
096700           MOVE NEJ              TO VORD-FLAUTFAK                         
096800         ELSE                                                             
096900           MOVE OHUV-FLAUTFAK    TO VORD-FLAUTFAK                         
097000         END-IF                                                           
097100       END-IF                                                             
097200                                                                          
097300       MOVE 'N'                  TO VORD-FLFRAKTS                         
097400                                                                          
097500     ELSE                                                                 
097600       MOVE JA                   TO SEG-WDE601-SW                         
097700     END-IF                                                               
097800     .                                                                    
097900     EJECT                                                                
098000 BEE-FOERBERED-NYCKLAR-WDQ3 SECTION.                                      
098100     MOVE 'BEE-FOERBERED-NYCKLAR-WDQ3 ' TO WS-CURRENT-SECTION             
098200                                                                          
098300     MOVE 4010-IDORDER  TO W-IDORDER-ODEL                                 
098400     MOVE 4010-IDDC     TO W-IDDC-ODEL                                    
098500     MOVE 4010-IDPRODNR TO W-IDPRODNR-ODEL                                
098600     MOVE 4010-IDPLKLST TO W-IDPLKLST-ODEL                                
098700     .                                                                    
098800     EJECT                                                                
098900 BEF-UPPDAT-WDE6-SOM-PACKAD SECTION.                                      
099000     MOVE 'BEF-UPPDAT-WDE6-SOM-PACKAD ' TO WS-CURRENT-SECTION             
099100                                                                          
099200     MOVE +3           TO VORD-KDORDSTA                                   
099300     MOVE +1           TO VORD-KVKOLLI                                    
099400     MOVE +1           TO VORD-KVKOLPAC                                   
099500     MOVE DAGENS-DATUM TO VORD-TIPACKN-SK                                 
099600                                                                          
099700     IF SEG-WDE601-FINNS                                                  
099800       PERFORM IMS-REPL-WDE601                                            
099900     ELSE                                                                 
100000       PERFORM IMS-ISRT-WDE601                                            
100100     END-IF                                                               
100200                                                                          
100300     PERFORM BEFA-UPPDATERA-WDE611                                        
100400     .                                                                    
100500     EJECT                                                                
100600 BEFA-UPPDATERA-WDE611 SECTION.                                           
100700     MOVE 'BEFA-UPPDATERA-WDE611    ' TO WS-CURRENT-SECTION               
100800                                                                          
100900     MOVE +1              TO W-IDKOLLI-KOLLI                              
101000     PERFORM IMS-GHU-WDE611                                               
101100                                                                          
101200     MOVE +1              TO KOLLI-IDKOLLI                                
101300*    MOVE KOLLI-IDKOLLI   TO KKOLLI-IDKOLLI                               
101400     MOVE ZERO            TO KOLLI-IDKOLLI-FLER                           
101500                             KOLLI-IDTRPTNR                               
101600     MOVE 4010-IDDC       TO KOLLI-IDDC                                   
101700     MOVE SPACE           TO KOLLI-ADFLGEO                                
101800     MOVE ZERO            TO KOLLI-ADFLOMR                                
101900                             KOLLI-ADRUTNIV                               
102000                             KOLLI-ADVMODUL                               
102100                             KOLLI-DIDMODUL                               
102200                             KOLLI-DIHMODUL                               
102300     MOVE 'N'             TO KOLLI-FLUTLAST                               
102400                             KOLLI-FLBANDST                               
102500                             KOLLI-FLFRSUTS                               
102600     MOVE ZERO            TO KOLLI-DIKOLLIL                               
102700                             KOLLI-DIKOLLIB                               
102800                             KOLLI-DIKOLLIH                               
102900     MOVE SPACE           TO KOLLI-FLTULLG                                
103000                             KOLLI-IDTULFTG                               
103100     MOVE ZERO            TO KOLLI-IDFAKLOP                               
103200                             KOLLI-IDTULLNR                               
103300                             KOLLI-RETULKS                                
103400                             KOLLI-IDFAKT                                 
103500                             KOLLI-IDFAKT-EXP                             
103600                             KOLLI-IDSHIPM                                
103700     MOVE SPACE           TO KOLLI-IDLBBET                                
103800     MOVE ZERO            TO KOLLI-IDPLOCK                                
103900     MOVE SPACE           TO KOLLI-KDARTURS-KOLLI                         
104000     MOVE ZERO            TO KOLLI-KDEMBTYP                               
104100                             KOLLI-KVFALRAD                               
104200                             KOLLI-KDKOLLI                                
104300     MOVE +1              TO KOLLI-KDKOLSTA                               
104400     IF 4010-PRAVCOST > ZERO                                              
104500       MOVE 'SEK'         TO KOLLI-KDVALISO                               
104600       MOVE 4010-KDVALISO TO KOLLI-KDVALISO-EXP                           
104700     ELSE                                                                 
104800       MOVE 4010-KDVALISO TO KOLLI-KDVALISO                               
104900       MOVE SPACE         TO KOLLI-KDVALISO-EXP                           
105000     END-IF                                                               
105100     MOVE ZERO            TO KOLLI-KVFLAMP-KOLLI                          
105200                             KOLLI-KVORDRAD                               
105300                             KOLLI-TIAAVVD-PATR                           
105400                             KOLLI-SUORDV-KOLLI                           
105500                             KOLLI-SUORDV-KLI-EXP                         
105600                             KOLLI-SUORDV-LOC                             
105700                             KOLLI-SUORDV-LOCPREL                         
105800                             KOLLI-TIFAKT                                 
105900                             KOLLI-TIFAKT-EXP                             
106000                             KOLLI-TIFAKTID                               
106100                             KOLLI-TIFAKTID-EXP                           
106200                             KOLLI-TILASTN                                
106300                             KOLLI-TILASTID                               
106400     MOVE DAGENS-DATUM    TO KOLLI-TIPACKN                                
106500     MOVE +600000         TO KOLLI-TIPACTID                               
106600     MOVE ZERO            TO KOLLI-VKORDNTO-KOLLI                         
106700     MOVE VORD-IDDISTR    TO KOLLI-IDDISTR                                
106800     MOVE VORD-IDKUNDNR   TO KOLLI-IDKUNDNR                               
106900     MOVE ZERO            TO KOLLI-ADHMODUL                               
107000                             KOLLI-KDFARLIG-KOLLI                         
107100                             KOLLI-VKORDBTO-KOLLI                         
107200                             KOLLI-VLORDBTO-KOLLI                         
107300     MOVE VORD-KDORDKL    TO KOLLI-KDORDKL                                
107400     MOVE VORD-FLAUTFAK   TO KOLLI-FLAUTFAK                               
107500     MOVE ZERO            TO KOLLI-IDLASTN                                
107600                             KOLLI-IDTRP                                  
107700                                                                          
107800     MOVE +1 TO FG-INDX                                                   
107900     PERFORM UNTIL FG-INDX > MAX-FG-INDX                                  
108000                                                                          
108100       MOVE ZERO          TO KOLLI-IDPSN(FG-INDX)                         
108200                             KOLLI-VKART-FG(FG-INDX)                      
108300                             KOLLI-VLFG(FG-INDX)                          
108400       ADD +1 TO FG-INDX                                                  
108500     END-PERFORM                                                          
108600     MOVE ZERO            TO KOLLI-SUEQFG                                 
108700     MOVE VORD-DARFS      TO KOLLI-DARFS                                  
108800     MOVE ZERO            TO KOLLI-IDKOLLI-SAMP                           
108900     MOVE SPACE           TO KOLLI-KDSTASKLI                              
109100                                                                          
109200* OBS - HÄR INITIERAS NYA DDGS-FÄLT RENT GENERELLT                        
109300* OBS - KONTROLLERA OM DETTA ÄR KORREKT!!!                                
109400     MOVE SPACE           TO KOLLI-IDSUPREF                               
109500                             KOLLI-IDLEVNR                                
109600     MOVE ZERO            TO KOLLI-DASUPREF                               
109700                             KOLLI-TISUPTID                               
109800                             KOLLI-KDVIA                                  
109900                                                                          
110000     IF SEGMENT-FINNS                                                     
110100       PERFORM IMS-REPL-WDE611                                            
110200     ELSE                                                                 
110300       PERFORM IMS-ISRT-WDE611                                            
110400     END-IF                                                               
110500     .                                                                    
110600     EJECT                                                                
110700 BEG-KOLLA-OM-AUTOMAT-FAKT SECTION.                                       
110800     MOVE 'BEG-KOLLA-OM-AUTOMAT-FAKT' TO WS-CURRENT-SECTION               
110900                                                                          
111000     IF VORD-FLAUTFAK = 'J'                                               
111100                                                                          
111200          PERFORM BEGA-FOERBERED-4726-ROT                                 
111300                                                                          
111400          PERFORM BEGB-SKAPA-SEG-4726                                     
111500                                                                          
111600          PERFORM BEGC-SKAPA-SEG-4727                                     
111700                                                                          
111800                                                                          
111900     END-IF                                                               
112000     IF OHUV-IDSYSTEM = 'SOFT'                                            
112100          PERFORM BEGA-FOERBERED-4726-ROT                                 
112200                                                                          
112300          PERFORM BEGB-SKAPA-SEG-4726                                     
112400                                                                          
112500          PERFORM BEGC-SKAPA-SEG-4727                                     
112600     END-IF                                                               
112700     .                                                                    
112800     EJECT                                                                
112900 BEGA-FOERBERED-4726-ROT SECTION.                                         
113000     MOVE 'BEGA-FOERBERED-4726-ROT  ' TO WS-CURRENT-SECTION               
113100                                                                          
113200     MOVE OHUV-IDDISTR TO TEST-IDDISTR                                    
113300     IF OHUV-FLOVRLEV = 'J'                                               
113400       MOVE NEJ          TO W-FLBATCH-4726-ROT                            
113500                                                                          
113600     ELSE                                                                 
113700                                                                          
113800       IF DIST19-SATS                                                     
113900         MOVE NEJ        TO W-FLBATCH-4726-ROT                            
114000                                                                          
114100       ELSE                                                               
114200*         IF DCS-IDLANDX2 = 'SE'                                          
114300*         AND DIST03-SVERIGE                                              
114400*           MOVE JA      TO W-FLBATCH-4726-ROT                            
114500*                                                                         
114600*         ELSE                                                            
114700            MOVE NEJ     TO W-FLBATCH-4726-ROT                            
114800*                                                                         
114900*         END-IF                                                          
115000       END-IF                                                             
115100     END-IF                                                               
115200     .                                                                    
115300     EJECT                                                                
115400 BEGB-SKAPA-SEG-4726 SECTION.                                             
115500     MOVE ' EGB-SKAPA-SEG-4726     ' TO WS-CURRENT-SECTION                
115600                                                                          
115700     MOVE VORD-IDDISTR   TO W-IDDISTR-4726                                
115800                            AUTFAKT-IDDISTR                               
115900     MOVE VORD-IDKUNDNR  TO W-IDKUNDNR-4726                               
116000                            AUTFAKT-IDKUNDNR                              
116100     MOVE VORD-IDDC      TO W-IDDC-4726                                   
116200                            AUTFAKT-IDDC                                  
116300     MOVE VORD-KDFAKTYP  TO W-KDFAKTYP-4726                               
116400                            AUTFAKT-KDFAKTYP                              
116500                                                                          
116600     PERFORM IMS-ISRT-XXDV-4726                                           
116700     .                                                                    
116800     EJECT                                                                
116900 BEGC-SKAPA-SEG-4727 SECTION.                                             
117000     MOVE 'BEGC-SKAPA-SEG-4727     ' TO WS-CURRENT-SECTION                
117100                                                                          
117200     MOVE VORD-IDPRODNR  TO AUTFAKT-IDPRODNR                              
117300     MOVE ZERO           TO AUTFAKT-IDSKEPPN                              
117400                            AUTFAKT-PRFRAKT                               
117500                                                                          
117600     MOVE NEJ            TO AUTFAKT-FLLASTA                               
117700                                                                          
117800     PERFORM IMS-ISRT-XXDV-4727                                           
117900     .                                                                    
118000     EJECT                                                                
118100 BF-BERAEKNA-FAELT SECTION.                                               
118200     MOVE 'BF-BERAEKNA-FAELT     ' TO WS-CURRENT-SECTION                  
118300                                                                          
118400     COMPUTE KVORDRAD-RAEKNARE = KVORDRAD-RAEKNARE + 1                    
118500     END-COMPUTE                                                          
118600     COMPUTE SUORDV-RAEKNARE-EXP = SUORDV-RAEKNARE-EXP +                  
118700                               ( 4010-PRAVCOST * 4010-KVAVBART )          
118800     COMPUTE SUORDV-RAEKNARE = SUORDV-RAEKNARE +                          
118900                               ( 4010-PRARTNTO * 4010-KVAVBART )          
119000     COMPUTE SUORDV-RAEKNARE-LOC = SUORDV-RAEKNARE-LOC +                  
119100                        ( 4010-PRARTNTO-LOC * 4010-KVAVBART )             
119200     COMPUTE SUORDV-RAEKNARE-LOCPREL = SUORDV-RAEKNARE-LOCPREL +          
119300                        ( 4010-PRARTNTO-LOCPREL * 4010-KVAVBART )         
119400     END-COMPUTE                                                          
119500                                                                          
119600     COMPUTE VKORDNTO-RAEKNARE = VKORDNTO-RAEKNARE +                      
119700                                 4010-VKORDNTO                            
119800     END-COMPUTE                                                          
119900     COMPUTE VLORDNTO-RAEKNARE = VLORDNTO-RAEKNARE +                      
120000                                 4010-VLORDNTO                            
120100     END-COMPUTE                                                          
120200     .                                                                    
120300     EJECT                                                                
120400 BG-FLYTTA-INSERT-WDE411 SECTION.                                         
120500     MOVE 'BG-FLYTTA-INSERT-WDE41' TO WS-CURRENT-SECTION                  
120600                                                                          
120700     MOVE 4010-IDPURAD           TO ORAD-IDPURAD                          
120800                                    W-IDPURAD-ORAD                        
120900     MOVE 4010-IDARTNR           TO ORAD-IDARTNR                          
121000*    MOVE '11'                   TO ORAD-IDDC-RO                          
121100     MOVE 4010-IDDC-RO           TO ORAD-IDDC-RO                          
121200     MOVE 4010-REKSIFFR          TO ORAD-REKSIFFR                         
121300     MOVE 4010-ADLAGOMR-ORD      TO ORAD-ADLAGOMR                         
121400     MOVE ZERO                   TO ORAD-ADLEVPL                          
121500     MOVE 4010-BERADREF          TO ORAD-BERADREF                         
121600                                                                          
121700     IF 4010-IDLEVNR NOT = SPACE OR                                       
121800        OHUV-FLLSBOK = NEJ                                                
121900       MOVE 'J'                  TO ORAD-FLDIRLEV                         
122000     ELSE                                                                 
122100       MOVE 'N'                  TO ORAD-FLDIRLEV                         
122200     END-IF                                                               
122300                                                                          
122400     MOVE 4010-KDORDING          TO ORAD-KDORDING                         
122500     MOVE 4010-FLRESTN           TO ORAD-FLRESTN                          
122600                                                                          
122700     MOVE 4010-IDBIL             TO ORAD-IDBIL                            
122800     MOVE 4010-IDKLIENT          TO ORAD-IDKLIENT                         
122900     MOVE 4010-IDARBREF          TO ORAD-IDARBREF                         
123000     MOVE 4010-IDVIN             TO ORAD-IDVIN                            
123100     MOVE 4010-IDLEVNR           TO ORAD-IDLEVNR                          
123200     MOVE 4010-IDPRODNR          TO ORAD-IDPRODNR                         
123300     MOVE 4010-KDVRINFO          TO ORAD-KDVRINFO                         
123400     MOVE SPACE                  TO ORAD-IDKUNDRF-RO                      
123500     MOVE 4010-IDKUNDRF-RO (3:5) TO ORAD-IDKUNDRF-RO                      
123600     MOVE 4010-KDARTURS          TO ORAD-KDARTURS                         
123700     MOVE 4010-KDDSP             TO ORAD-KDDSP                            
123800     MOVE 4010-KDFARLIG          TO ORAD-KDFARLIG                         
123900                                                                          
124000     MOVE 4010-IDKONTO           TO WS-IDKONTO-ALFA                       
124100     MOVE WS-IDKONTO-ALFA (2:1)  TO ORAD-KDFTG                            
124200                                                                          
124300     MOVE ZERO                   TO ORAD-KDTVA                            
124400                                                                          
124500     MOVE ARB-KDFRAKT            TO ORAD-KDFRAKT                          
125200                                                                          
125300     MOVE 4010-KDKVBRYT          TO ORAD-KDKVBRYT                         
125400     MOVE ZERO                   TO ORAD-KDOFFERT                         
125500     MOVE 4010-KDOI              TO ORAD-KDOI                             
125600     MOVE 4010-CLEARGROUP        TO ORAD-CLEARGROUP                       
125700     MOVE 4010-KDORDKL           TO ORAD-KDORDKL                          
125800                                                                          
125900     MOVE OHUV-IDDISTR TO TEST-IDDISTR                                    
126000     IF DIST18-SKROT                                                      
126100       MOVE +3                   TO ORAD-KDORDTYP                         
126200     ELSE                                                                 
126300       MOVE ZERO                 TO ORAD-KDORDTYP                         
126400     END-IF                                                               
126500                                                                          
126600     MOVE ZERO                    TO ORAD-KDQPACK                         
126700     MOVE 4010-KDPRODSL           TO ORAD-KDPRODSL                        
126800     MOVE +3                      TO ORAD-KDRADSTA                        
126900     MOVE 4010-IDKONTO            TO ORAD-IDKONTO                         
127000     MOVE 4010-IDANALYS           TO ORAD-IDANALYS                        
127100     MOVE 4010-IDKST              TO ORAD-IDKST                           
127200     MOVE ZERO                    TO ORAD-KVANNANT                        
127300     MOVE 4010-KVAVBART           TO ORAD-KVAVBART                        
127400     MOVE 4010-KVBEART-Q          TO ORAD-KVBEART                         
127500     MOVE ZERO                    TO ORAD-KVFLAMP                         
127600     MOVE ZERO                    TO ORAD-KVLEVART                        
127700     MOVE 4010-KVSLATT            TO ORAD-KVSLATT                         
127800     MOVE 4010-PRARTNTO           TO ORAD-PRARTNTO                        
127900     MOVE 4010-DEAL-PR-LINE       TO ORAD-DEAL-PR-LINE                    
128000     MOVE ZERO                    TO ORAD-PRARTULL                        
128100     MOVE 4010-TIPRIS             TO ORAD-TIPRIS                          
128200     MOVE 4010-TIRODAT            TO ORAD-TIRODAT                         
128300     MOVE 4010-TIUTSKR            TO ORAD-TIUTSKR                         
128400                                                                          
128500     COMPUTE ORAD-VKARTNTO     = 4010-VKART / 1000                        
128600     COMPUTE ORAD-VKART-NTO-KG = 4010-VKART-NTO / 1000                    
128700                                                                          
128800     MOVE 4010-VLARTNTO           TO ORAD-VLARTNTO                        
128900     MOVE 'N'                     TO ORAD-FLFYSAVV                        
129000     MOVE 4010-BEART              TO ORAD-BEART                           
129100     MOVE 4010-BEVOLREF           TO ORAD-BEVOLREF                        
129200     MOVE 4010-FLINVEST           TO ORAD-FLINVEST                        
129300     MOVE 4010-FLPRTILL           TO ORAD-FLPRTILL                        
129400     MOVE 4010-FLTILLK            TO ORAD-FLTILLK                         
129500     MOVE 4010-FLSDCLEV           TO ORAD-FLSDCLEV                        
129600     MOVE 4010-IDKAMPRF           TO ORAD-IDKAMPRF                        
129700     MOVE 4010-IDLOPNR-RO         TO ORAD-IDLOPNR-RO                      
129800     MOVE 4010-IDSYSTEM           TO ORAD-IDSYSTEM                        
129900     MOVE 4010-KDPRTYP            TO ORAD-KDPRTYP                         
130000     MOVE 'N'                     TO ORAD-FLNOLLJ                         
130100     MOVE 4010-IDPSN              TO ORAD-IDPSN                           
130200     MOVE 4010-VKART-FG           TO ORAD-VKART-FG                        
130300     MOVE 4010-VLFG               TO ORAD-VLFG                            
130400     MOVE 4010-SUEQFG             TO ORAD-SUEQFG                          
130500                                                                          
130600     IF AUTOMAT-PACKNING                                                  
130700       MOVE ORAD-KVAVBART         TO ORAD-KVLEVART                        
130800       MOVE +4                    TO ORAD-KDRADSTA                        
130900       IF KORD-KDORDKL = +0                                               
131000          PERFORM BGA-UPPDATERA-VOR-TIKLAR                                
131100       END-IF                                                             
131200     END-IF                                                               
131300                                                                          
131400* OBS - HÄR INITIERAS NYA DDGS-FÄLT RENT GENERELLT                        
131500* OBS - KONTROLLERA OM DETTA ÄR KORREKT!!!                                
131600     MOVE ZERO                    TO ORAD-KDANNULL                        
131700                                     ORAD-TISLULEV                        
131800                                                                          
131900     MOVE 4010-IDKUNDRF-WIP       TO ORAD-IDKUNDRF-WIP                    
132000     MOVE 4010-PRAVCOST           TO ORAD-PRAVCOST                        
132100     IF 4010-PRAVCOST > 0                                                 
132200       MOVE 4010-KDVALISO         TO ORAD-KDVALISO-EXP                    
132300       MOVE 'SEK'                 TO ORAD-KDVALISO                        
132400     ELSE                                                                 
132500       MOVE 4010-KDVALISO         TO ORAD-KDVALISO                        
132600       MOVE SPACE                 TO ORAD-KDVALISO-EXP                    
132700     END-IF                                                               
132800                                                                          
132900     PERFORM IMS-ISRT-WDE411                                              
133000     .                                                                    
133100     EJECT                                                                
133200 BGA-UPPDATERA-VOR-TIKLAR SECTION.                                        
133300     MOVE 'BGA-UPD-VOR-TIKLAR    ' TO WS-CURRENT-SECTION                  
133400                                                                          
133500     MOVE LOW-VALUE              TO W-WDA601KY-MIN-X.                     
133600     MOVE HIGH-VALUE             TO W-WDA601KY-MAX-X.                     
133700     MOVE KORD-IDDISTR           TO W-A601KY-MIN-IDDISTR                  
133800                                    W-A601KY-MAX-IDDISTR                  
133900     MOVE KORD-IDKUNDNR          TO W-A601KY-MIN-IDKUNDNR                 
134000                                    W-A601KY-MAX-IDKUNDNR                 
134100     MOVE KORD-IDORDNR5          TO W-A601KY-MIN-IDORDNR                  
134200                                    W-A601KY-MAX-IDORDNR                  
134300                                                                          
134400     MOVE NEJ                    TO SW-TIKLAR-UPPDATERAD                  
134500                                                                          
134600     PERFORM IMS-GHN-WDA6B                                                
134700     PERFORM UNTIL SEGMENT-SAKNAS                                         
134800                OR SLUT-AV-DATA                                           
134900                OR SW-TIKLAR-UPPDATERAD = JA                              
135000                                                                          
135100         IF  VOR-IDARTNR = ORAD-IDARTNR                                   
135200         AND VOR-TIKLAR = +0                                              
135300                                                                          
135400             MOVE DAGENS-DATUM  TO VOR-TIKLAR                             
135500             MOVE WS-TIHHMMSS      TO VOR-TIKLATID                        
135600             PERFORM IMS-REPL-WDA6B                                       
135700             MOVE JA               TO SW-TIKLAR-UPPDATERAD                
135800         END-IF                                                           
135900                                                                          
136000         PERFORM IMS-GHN-WDA6B                                            
136100     END-PERFORM                                                          
136200     .                                                                    
136300 BH-FLYTTA-INSERT-WDG6 SECTION.                                           
136400     MOVE 'BH-FLYTTA-INSERT-WDG6 ' TO WS-CURRENT-SECTION                  
136500                                                                          
136600     IF WDG6-IDLOGLOP = 9                                                 
136700        MOVE 0 TO WDG6-IDLOGLOP                                           
136800     END-IF                                                               
136900                                                                          
137000     PERFORM BHA-FLYTTA-TILL-LOGGPOST                                     
137100     PERFORM BHB-FLYTTA-TILL-SORTPOST                                     
137200     ACCEPT WDG6-TIAAMMDD FROM DATE                                       
137300     ACCEPT WDG6-TIKLOCK  FROM TIME                                       
137400     ADD  1 TO WDG6-IDLOGLOP                                              
137500     PERFORM IMS-ISRT-ZZAC-WDG601                                         
137600                                                                          
137700     PERFORM UNTIL SEGMENT-FINNS                                          
137800       IF WDG6-IDLOGLOP = 9                                               
137900          MOVE 0 TO WDG6-IDLOGLOP                                         
138000          ACCEPT WDG6-TIKLOCK  FROM TIME                                  
138100       END-IF                                                             
138200       ADD 1 TO WDG6-IDLOGLOP                                             
138300       PERFORM IMS-ISRT-ZZAC-WDG601                                       
138400     END-PERFORM                                                          
138500     .                                                                    
138600     EJECT                                                                
138700 BHA-FLYTTA-TILL-LOGGPOST SECTION.                                        
138800     MOVE 'BHA-FLYTTA-TILL-LOGGPO' TO WS-CURRENT-SECTION                  
138900                                                                          
139000     MOVE 'RYE'              TO RYE-IDPTYP                                
139100     MOVE SPACE              TO RYE-BERADREF                              
139200                                RYE-BEVOLREF                              
139300     IF 4010-IDLEVNR NOT = SPACE                                          
139400       MOVE 'J'              TO RYE-FLDIRLEV                              
139500     ELSE                                                                 
139600       MOVE 'N'              TO RYE-FLDIRLEV                              
139700     END-IF                                                               
139800                                                                          
139900     MOVE OHUV-FLLSBOK       TO RYE-FLLSBOK                               
140000     MOVE OHUV-FLORDSPE      TO RYE-FLORDSPE                              
140100     MOVE NEJ                TO RYE-FLTILLK                               
140200     MOVE 4010-IDARTNR       TO RYE-IDARTNR                               
140300     MOVE 4010-IDKUNDRF      TO RYE-IDKUNDRF                              
140400     MOVE 4010-IDKUNDRF-RO   TO RYE-IDKUNDRF-RO                           
140500     MOVE 4010-IDDC          TO RYE-IDDC                                  
140600     MOVE ZERO               TO RYE-KDDSP                                 
140700     MOVE SPACE              TO RYE-KDFAKTYP                              
140800     MOVE ZERO               TO RYE-KDKVBRYT                              
140900                                RYE-KDORDBEK                              
141000     MOVE 4010-KDORDING      TO RYE-KDORDING                              
141100     MOVE 4010-KDPRODSL      TO RYE-KDPRODSL                              
141200     MOVE ZERO               TO RYE-KDVRINFO                              
141300                                RYE-KDVRTPO                               
141400     MOVE 4010-KVAVBART      TO RYE-KVAVBART                              
141500     MOVE 4010-KVBEART-Q     TO RYE-KVBEART-Q                             
141600     MOVE ZERO               TO RYE-KVRO                                  
141700                                RYE-REKSIFFR                              
141800                                RYE-TIDISPIN                              
141900     MOVE 4010-TIREGDAT      TO RYE-TIORDREG                              
142000     MOVE 4010-TIRODAT       TO RYE-TIRODAT                               
142100                                                                          
142200     MOVE W-LOGGPOST TO WDG6-LOGGPOST                                     
142300     .                                                                    
142400     EJECT                                                                
142500 BHB-FLYTTA-TILL-SORTPOST SECTION.                                        
142600     MOVE 'BHB-FLYTTA-TILL-SORTPO' TO WS-CURRENT-SECTION                  
142700                                                                          
142800     MOVE OHUV-IDDISTR       TO RYES-IDDISTR                              
142900     MOVE OHUV-IDKUNDNR      TO RYES-IDKUNDNR                             
143000     IF  OHUV-FLVORKO = JA                                                
143100     OR  OHUV-FLVORKO = YES                                               
143200         MOVE JA             TO RYES-FLVORKO                              
143300     ELSE                                                                 
143400         MOVE OHUV-FLVORKO   TO RYES-FLVORKO                              
143500     END-IF                                                               
143600     MOVE OHUV-FLFORBI       TO RYES-FLFORBI                              
143700     MOVE OHUV-FLOVRLEV      TO RYES-FLOVRLEV                             
143800     MOVE ZERO               TO RYES-KDERS                                
143900     MOVE ZERO               TO RYES-KDFRAKT                              
144000     MOVE 4010-KDORDKL       TO RYES-KDORDKL                              
144100     MOVE ZERO               TO RYES-KDTPOTYP                             
144200     MOVE ZERO               TO RYES-KVBEART                              
144300     MOVE 4010-KVSLATT       TO RYES-KVSLATT                              
144400     MOVE ZERO               TO RYES-KVQPACK-1                            
144500     MOVE SPACE              TO RYES-FILLERX7                             
144600                                RYES-FILLERX2                             
144700                                                                          
144800     MOVE W-SORTPOST TO WDG6-SORTPOST                                     
144900     .                                                                    
145000     EJECT                                                                
145100 BI-FLYTTA-INSERT-WDE421 SECTION.                                         
145200     MOVE 'BI-FLYTTA-INSERT-WDE42' TO WS-CURRENT-SECTION                  
145300                                                                          
145400     MOVE VORD-IDPRODNR  TO KKOLLI-IDPRODNR                               
145500     MOVE KOLLI-IDKOLLI  TO KKOLLI-IDKOLLI                                
145600     MOVE ORAD-KVLEVART  TO KKOLLI-KVLEVART                               
145700                                                                          
145800     PERFORM IMS-ISRT-WDE421                                              
145900                                                                          
146000     .                                                                    
146100     EJECT                                                                
146200 BJ-FLYTTA-DATA-TILL-WDR4-4490 SECTION.                                   
146300     MOVE 'BJ-FLYTTA-DATA-TILL-WDR4' TO WS-CURRENT-SECTION                
146400                                                                          
146500     MOVE 4010-IDPRODNR TO 4490-IDPRODNR                                  
146600                                                                          
146700     MOVE 4010-IDPLKLST TO 4490-IDPLKLST                                  
146800                                                                          
146900     MOVE 4010-IDUSER   TO 4490-IDUSER                                    
147000                                                                          
147100     MOVE 4010-KDORDKL  TO 4490-KDORDKL                                   
147200                                                                          
147300     MOVE ZERO          TO 4490-TIKLAR                                    
147400     .                                                                    
147500     EJECT                                                                
147600 BK-UPPDATERA-WDE6 SECTION.                                               
147700     MOVE 'BK-UPPDATERA-WDE6       ' TO WS-CURRENT-SECTION                
147800                                                                          
147900     IF AUTOMAT-PACKNING                                                  
148000       PERFORM IMS-GHU-WDE601                                             
148100       IF SEGMENT-FINNS                                                   
148200         MOVE JA  TO SEG-WDE601-SW                                        
148300       ELSE                                                               
148400         MOVE NEJ TO SEG-WDE601-SW                                        
148500       END-IF                                                             
148600     END-IF                                                               
148700                                                                          
148800     IF SEG-WDE601-FINNS                                                  
148900       COMPUTE VORD-KVORDRAD = VORD-KVORDRAD + KVORDRAD-RAEKNARE          
149000       END-COMPUTE                                                        
149100                                                                          
149200       COMPUTE VORD-SUORDV-EXP       =                                    
149300                  VORD-SUORDV-EXP + SUORDV-RAEKNARE-EXP                   
149400       END-COMPUTE                                                        
149500                                                                          
149600       COMPUTE VORD-SUORDV           =                                    
149700                  VORD-SUORDV     + SUORDV-RAEKNARE                       
149800       END-COMPUTE                                                        
149900                                                                          
150000       COMPUTE VORD-SUORDV-LOC       =                                    
150100                  VORD-SUORDV-LOC + SUORDV-RAEKNARE-LOC                   
150200       END-COMPUTE                                                        
150300                                                                          
150400       COMPUTE VORD-SUORDV-LOCPREL  =                                     
150500                  VORD-SUORDV-LOCPREL + SUORDV-RAEKNARE-LOCPREL           
150600       END-COMPUTE                                                        
150700                                                                          
150800       COMPUTE VORD-VKORDNTO = VORD-VKORDNTO + VKORDNTO-RAEKNARE          
150900       END-COMPUTE                                                        
151000                                                                          
151100       COMPUTE VORD-VLORDNTO = VORD-VLORDNTO + VLORDNTO-RAEKNARE          
151200       END-COMPUTE                                                        
151300       IF OHUV-FLORDSPE = 'J'                                             
151400         MOVE VORD-VKORDNTO          TO VORD-VKORDBTO                     
151500         MOVE VORD-VLORDNTO          TO VORD-VLORDBTO                     
151600       END-IF                                                             
151700                                                                          
151800       IF AUTOMAT-PACKNING                                                
151900         IF VORD-FLAUTFAK = JA                                            
152000*            MOVE +4          TO VORD-KDORDSTA                            
152100*            STATUS BLIR 4 I ADDIT SK 021206                              
152200           MOVE +3          TO VORD-KDORDSTA                              
152300         END-IF                                                           
152400                                                                          
152500         MOVE VORD-KVORDRAD TO VORD-KVORDRAD-PACK                         
152600         IF VORD-IDDC-EXP = SPACE OR VORD-IDDC-EXP = WC-CDC-SE            
152700*           *GLOBAL EXPORT ORDER OR NORMAL ORDER                          
152800            IF VORD-SUORDV-EXP > 0                                        
152900               MOVE VORD-SUORDV-EXP TO VORD-SUORDV-PACK                   
153000            ELSE                                                          
153100               MOVE VORD-SUORDV    TO VORD-SUORDV-PACK                    
153200            END-IF                                                        
153300         END-IF                                                           
153400         IF VORD-IDDC-EXP NOT = SPACE AND                                 
153500            VORD-IDDC-EXP NOT = WC-CDC-SE                                 
153600*           *VOR-ORDER FROM DC 11 TO CHINA INDIA                          
153700            MOVE VORD-SUORDV       TO VORD-SUORDV-PACK                    
153800         END-IF                                                           
153900         MOVE VORD-SUORDV-LOC      TO VORD-SUORDV-PACK-LOC                
154000         MOVE VORD-SUORDV-LOCPREL  TO VORD-SUORDV-PACK-LOCPREL            
154100       END-IF                                                             
154200                                                                          
154300       PERFORM IMS-REPL-WDE601                                            
154400                                                                          
154500       PERFORM BKA-UPPDAT-WDE611-OM-AUT-PACK                              
154600                                                                          
154700     ELSE                                                                 
154800       MOVE KVORDRAD-RAEKNARE  TO VORD-KVORDRAD                           
154900       MOVE SUORDV-RAEKNARE            TO VORD-SUORDV                     
155000       MOVE SUORDV-RAEKNARE-EXP        TO VORD-SUORDV-EXP                 
155100       MOVE SUORDV-RAEKNARE-LOC        TO VORD-SUORDV-LOC                 
155200       MOVE SUORDV-RAEKNARE-LOCPREL    TO VORD-SUORDV-LOCPREL             
155300       MOVE VKORDNTO-RAEKNARE  TO VORD-VKORDNTO                           
155400       MOVE VLORDNTO-RAEKNARE  TO VORD-VLORDNTO                           
155500                                                                          
155600       IF OHUV-FLORDSPE = 'J'                                             
155700         MOVE VKORDNTO-RAEKNARE  TO VORD-VKORDBTO                         
155800         MOVE VLORDNTO-RAEKNARE  TO VORD-VLORDBTO                         
155900       END-IF                                                             
156000                                                                          
156100       PERFORM IMS-ISRT-WDE601                                            
156200     END-IF                                                               
156300     .                                                                    
156400     EJECT                                                                
156500 BKA-UPPDAT-WDE611-OM-AUT-PACK SECTION.                                   
156600     MOVE 'BKA-UPPDAT-WDE611-OM-AUT' TO WS-CURRENT-SECTION                
156700                                                                          
156800     IF AUTOMAT-PACKNING                                                  
156900                                                                          
157000       PERFORM IMS-GHU-WDE611                                             
157100                                                                          
157200       MOVE VORD-KVORDRAD   TO KOLLI-KVORDRAD                             
157300       MOVE VORD-SUORDV     TO KOLLI-SUORDV-KOLLI                         
157400       MOVE VORD-SUORDV-EXP TO KOLLI-SUORDV-KLI-EXP                       
157500       MOVE VORD-SUORDV-LOC       TO KOLLI-SUORDV-LOC                     
157600       MOVE VORD-SUORDV-LOCPREL   TO KOLLI-SUORDV-LOCPREL                 
157700                                                                          
157800       IF OHUV-FLORDSPE = 'J'                                             
157900         MOVE VORD-VKORDNTO          TO KOLLI-VKORDBTO-KOLLI              
158000         MOVE VORD-VLORDNTO          TO KOLLI-VLORDBTO-KOLLI              
158100       END-IF                                                             
158200                                                                          
158300       PERFORM IMS-REPL-WDE611                                            
158400                                                                          
158500     END-IF                                                               
158600     .                                                                    
158700     EJECT                                                                
158800 BL-UPPDAT-WDQ3-OM-AUT-PACK SECTION.                                      
158900     MOVE 'BL-UPPDAT-WDQ3-OM-AUT-PA' TO WS-CURRENT-SECTION                
159000                                                                          
159100     IF AUTOMAT-PACKNING                                                  
159200                                                                          
159300       PERFORM IMS-GHU-ORQA-WDQ301                                        
159400                                                                          
159500       MOVE 'P'                TO ODEL-KDODELSTA                          
159600       MOVE VORD-KVORDRAD-PACK TO ODEL-KVPACKRAD-OD                       
159700       MOVE DAGENS-DATUM       TO ODEL-TIPACKN                            
159800       MOVE WS-TIHHMMSS        TO ODEL-TIPACTID                           
159900                                                                          
160000       PERFORM IMS-REPL-ORQA-WDQ301                                       
160100                                                                          
160200     END-IF                                                               
160300     .                                                                    
160400     EJECT                                                                
160500 BM-UPPDATERA-WDE4 SECTION.                                               
160600     MOVE 'BM-UPPDATERA-WDE4       ' TO WS-CURRENT-SECTION                
160700                                                                          
160800     PERFORM IMS-GHU-WDE401                                               
160900                                                                          
161000     ADD KVORDRAD-RAEKNARE            TO KORD-KVORDRAD                    
161100                                        KORD-KVORDRAD-VO                  
161200     ADD SUORDV-RAEKNARE-EXP          TO KORD-SUORDV-EXP                  
161300     ADD SUORDV-RAEKNARE              TO KORD-SUORDV                      
161400     ADD SUORDV-RAEKNARE-LOC          TO KORD-SUORDV-LOC                  
161500     ADD SUORDV-RAEKNARE-LOCPREL      TO KORD-SUORDV-LOCPREL              
161600                                                                          
161700     IF AUTOMAT-PACKNING                                                  
161800       MOVE KVORDRAD-RAEKNARE TO KORD-KVORDRAD-PACK                       
161900     END-IF                                                               
162000                                                                          
162100     ADD VKORDNTO-RAEKNARE    TO KORD-VKORDNTO                            
162200     ADD VLORDNTO-RAEKNARE    TO KORD-VLORDNTO                            
162300                                                                          
162400     PERFORM IMS-REPL-WDE401                                              
162500                                                                          
162600     MOVE ZERO TO KVORDRAD-RAEKNARE                                       
162700                  SUORDV-RAEKNARE                                         
162800                  SUORDV-RAEKNARE-EXP                                     
162900                  SUORDV-RAEKNARE-LOC                                     
163000                  SUORDV-RAEKNARE-LOCPREL                                 
163100                  VKORDNTO-RAEKNARE                                       
163200                  VLORDNTO-RAEKNARE                                       
163300     .                                                                    
163400     EJECT                                                                
163500 BN-KOLLA-OM-ISRT-4490 SECTION.                                           
163600     MOVE 'BN-KOLLA-OM-ISRT-4490   ' TO WS-CURRENT-SECTION                
163700                                                                          
163800     IF NOT AUTOMAT-PACKNING                                              
163900                                                                          
164000       PERFORM IMS-ISRT-WDGX4490                                          
164100                                                                          
164200     END-IF                                                               
164300     .                                                                    
164400     EJECT                                                                
164500 BO-UPPDAT-WDQ2-OM-AUT-PACK SECTION.                                      
164600     MOVE 'BO-UPPDAT-WDQ2-OM-AUT   ' TO WS-CURRENT-SECTION                
164700                                                                          
164800     IF AUTOMAT-PACKNING                                                  
164900                                                                          
165000        MOVE 'P '                TO ARB-KDORDSTA                          
165100        PERFORM IMS-REPL-ORQI-WDQ212                                      
165200     END-IF                                                               
165300     .                                                                    
165400     EJECT                                                                
165500 S01-KOLLA-OM-4010-FINNS SECTION.                                         
165600     MOVE 'S01-KOLLA-OM-4010-FINNS ' TO WS-CURRENT-SECTION                
165700                                                                          
165800     MOVE ZERO TO SEG-4010-SW                                             
165900                                                                          
166000     PERFORM UNTIL SEG-4010-FINNS OR SEG-4010-FINNS-EJ                    
166100        IF SEGMENT-FINNS                                                  
166200           ADD 1 TO RAD-INDEX                                             
166300           IF 4010-IDARTNR > 0                                            
166400              MOVE JA            TO SEG-4010-SW                           
166500              MOVE 4010-IDPRODNR TO WS-IDPRODNR                           
166600              MOVE 4010-IDPLKLST TO WS-IDPLKLST                           
166700           ELSE                                                           
166800              PERFORM IMS-DLET-4007-4010                                  
166900              PERFORM IMS-GHNP-4007-4010                                  
167000           END-IF                                                         
167100        ELSE                                                              
167200           MOVE NEJ TO SEG-4010-SW                                        
167300        END-IF                                                            
167400     END-PERFORM                                                          
167500     .                                                                    
167600     EJECT                                                                
167700                                                                          
167800                                                                          
167900 S02-DATA-TILL-DEL-NOTE SECTION.                                          
168000     MOVE 'S02-DATA-TILL-DEL-NOTE  ' TO WS-CURRENT-SECTION                
168100                                                                          
168200     MOVE OHUV-IDDISTR             TO TEST-IDDISTR                        
168300     IF DIST07-USA-RETAILER-DNOTE                                         
168400     OR DIST07-CAN-RETAILER                                               
168500                                                                          
168600        IF (ORAD-IDKUNDRF-RO(1:5) NOT = ZERO)                             
168700           PERFORM S02A-KOLLA-BEKUNDRF                                    
168800           IF BEKUNDRF-OK                                                 
168900              INITIALIZE DNOT-ORDER-INFO                                  
169000                                                                          
169100              MOVE IDPGM                 TO DNOT-IDPGM                    
169200              MOVE KORD-IDORDER          TO DNOT-IDORDER                  
169300              MOVE ORAD-IDARTNR          TO DNOT-IDARTNR                  
169400              MOVE KORD-IDDC             TO DNOT-IDDC                     
169500              MOVE OHUV-ADGMT            TO DNOT-ADGMT                    
169600              MOVE OHUV-BEGMT            TO DNOT-BEGMT                    
169700              MOVE OHUV-BEKUNDRF         TO DNOT-BEKUNDRF                 
169800              MOVE ORAD-BERADREF         TO DNOT-BERADREF                 
169900              MOVE KORD-IDDISTR          TO DNOT-IDDISTR                  
170000              MOVE KORD-IDKUNDNR         TO DNOT-IDKUNDNR                 
170100              MOVE KORD-IDORDNR5         TO DNOT-IDORDNR7                 
170200              MOVE OHUV-IDDC-PRIM        TO DNOT-IDDC-PRIM                
170300              MOVE SPACE                 TO DNOT-IDKUNDRF-RO              
170400              MOVE '00'                  TO DNOT-IDKUNDRF-RO(1:2)         
170500              MOVE ORAD-IDKUNDRF-RO      TO DNOT-IDKUNDRF-RO(3:5)         
170600              MOVE ORAD-FLDIRLEV         TO DNOT-FLDIRLEV                 
170700              MOVE ORAD-FLTILLK          TO DNOT-FLTILLK                  
170800              MOVE KORD-KDORDKL          TO DNOT-KDORDKL                  
170900              MOVE KORD-KDFRAKT          TO DNOT-KDFRAKT                  
171000              MOVE ORAD-KVBEART          TO DNOT-KVBEART                  
171100                                               DNOT-KVBEART-Q             
171200              MOVE ORAD-REKSIFFR         TO DNOT-REKSIFFR                 
171300              MOVE OHUV-TIREGDAT         TO DNOT-TIREGDAT                 
171400              MOVE OHUV-TIREGTID         TO DNOT-TIREGTID                 
171500                                                                          
171600              CALL W411DNOT USING DNOT-W411DNOT                           
171700                                  DNOT-ORQP-PCB                           
171800                                  DNOT-ORQP2-PCB                          
171900                                  DNOT-ORQP3-PCB                          
172000                                  DNOT-4013-PCB                           
172100                                  DNOT-BENA-PCB                           
172200           END-IF                                                         
172300        END-IF                                                            
172400     END-IF                                                               
172500     .                                                                    
172600     EJECT                                                                
172700                                                                          
172800     SKIP3                                                                
172900 S02A-KOLLA-BEKUNDRF SECTION.                                             
173000     MOVE 'S02A-KOLLA-BEKUNDRF     ' TO WS-CURRENT-SECTION                
173100                                                                          
173200     MOVE JA                    TO BEKUNDRF-SW                            
173300     MOVE OHUV-IDDISTR          TO W-IDDISTR-CSEQ                         
173400     MOVE OHUV-IDKUNDNR         TO W-IDKUNDNR-CSEQ                        
173500     MOVE ORAD-IDKUNDRF-RO(1:5) TO W-IDORDNR5-CSEQ                        
173600                                                                          
173700     PERFORM IMS-GU-WDQ201                                                
173800     IF OC-OHUV-BEKUNDRF(1:2) = 'OC'                                      
173900*    ORDERHUVUDET ÄR ETT TOMHUVUD FÖR ORDERKONSOLIDERING                  
174000*    NÅGON EGENTLIG BIPACKNING ÄR DET INTE TAL OM I DET HÄR FALLET        
174100*    DÄRFÖR HELLER INGET ANROP PÅ W411DNOT                                
174200                                                                          
174300        MOVE NEJ TO BEKUNDRF-SW                                           
174400     END-IF                                                               
174500     .                                                                    
174600     EJECT                                                                
174700                                                                          
174800* --- IMS SEKTIONER ---                                                   
174900     SKIP3                                                                
175000 IMS-GET-MSG SECTION.                                                     
175100     MOVE 'IMS-GET-MSG         ' TO WS-CURRENT-IMS-SECTION                
175200                                                                          
175300     MOVE '  QC' TO GODK-STATUSKODER                                      
175400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
175500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
175600     PERFORM IMS-STATUSKONTROLL                                           
175700     .                                                                    
175800                                                                          
175900 IMS-ISRT-MSG-ALT1 SECTION.                                               
176000     MOVE 'IMS-ISRT-MSG-ALT1   ' TO WS-CURRENT-IMS-SECTION                
176100                                                                          
176200     MOVE SPACE TO GODK-STATUSKODER                                       
176300     CALL CBLTDLI USING ISRT ALT1-PCB P-TO-P-SW                           
176400     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
176500     PERFORM IMS-STATUSKONTROLL                                           
176600     .                                                                    
176700     EJECT                                                                
176800 IMS-GHU-4007-4001 SECTION.                                               
176900     MOVE 'IMS-GHU-4007-4001   ' TO WS-CURRENT-IMS-SECTION                
177000                                                                          
177100     STRING 'WL400701(WDGXKEY  =' W-4001-IDHTYP-X ')'                     
177200          DELIMITED BY SIZE INTO SSA1                                     
177300     MOVE '    ' TO GODK-STATUSKODER                                      
177400     CALL CBLTDLI USING GHU 4007-PCB DLI-IO-AREA-4001 SSA1                
177500     MOVE 4007-STATUS-CODE TO STATUS-WS                                   
177600     PERFORM IMS-STATUSKONTROLL                                           
177700     .                                                                    
177800     EJECT                                                                
177900 IMS-DLET-4007-4001 SECTION.                                              
178000     MOVE 'IMS-DLET-4007-4001  ' TO WS-CURRENT-IMS-SECTION                
178100                                                                          
178200     MOVE '    ' TO GODK-STATUSKODER                                      
178300     CALL CBLTDLI USING DLET 4007-PCB DLI-IO-AREA-4001                    
178400     MOVE 4007-STATUS-CODE TO STATUS-WS                                   
178500     PERFORM IMS-STATUSKONTROLL                                           
178600     .                                                                    
178700     EJECT                                                                
178800 IMS-GU-4007-4008 SECTION.                                                
178900     MOVE 'IMS-GU-4007-4008    ' TO WS-CURRENT-IMS-SECTION                
179000                                                                          
179100     STRING 'WL400701(WDGXKEY  =' W-4001-IDHTYP-X ')'                     
179200          DELIMITED BY SIZE INTO SSA1                                     
179300     MOVE 'WL400711 ' TO SSA2                                             
179400     MOVE '  ' TO GODK-STATUSKODER                                        
179500     CALL CBLTDLI USING GU 4007-PCB DLI-IO-AREA-4008 SSA1 SSA2            
179600     MOVE 4007-STATUS-CODE TO STATUS-WS                                   
179700     PERFORM IMS-STATUSKONTROLL                                           
179800     .                                                                    
179900     EJECT                                                                
180000 IMS-GHNP-4007-4010-FIRST SECTION.                                        
180100     MOVE 'IMS-GHNP-4007-4010-FIRST' TO WS-CURRENT-IMS-SECTION            
180200                                                                          
180300     MOVE 'WL400721*F ' TO SSA1                                           
180400     MOVE '  GE' TO GODK-STATUSKODER                                      
180500     CALL CBLTDLI USING GHNP 4007-PCB DLI-IO-AREA-4010 SSA1               
180600     MOVE 4007-STATUS-CODE TO STATUS-WS                                   
180700     PERFORM IMS-STATUSKONTROLL                                           
180800     .                                                                    
180900     EJECT                                                                
181000 IMS-GHNP-4007-4010 SECTION.                                              
181100     MOVE 'IMS-GHNP-4007-4010     ' TO WS-CURRENT-IMS-SECTION             
181200                                                                          
181300     MOVE 'WL400721 ' TO SSA1                                             
181400     MOVE '  GE' TO GODK-STATUSKODER                                      
181500     CALL CBLTDLI USING GHNP 4007-PCB DLI-IO-AREA-4010 SSA1               
181600     MOVE 4007-STATUS-CODE TO STATUS-WS                                   
181700     PERFORM IMS-STATUSKONTROLL                                           
181800     .                                                                    
181900     EJECT                                                                
182000 IMS-DLET-4007-4010 SECTION.                                              
182100     MOVE 'IMS-DLET-4007-4010     ' TO WS-CURRENT-IMS-SECTION             
182200                                                                          
182300     MOVE '    ' TO GODK-STATUSKODER                                      
182400     CALL CBLTDLI USING DLET 4007-PCB DLI-IO-AREA-4010                    
182500     MOVE 4007-STATUS-CODE TO STATUS-WS                                   
182600     PERFORM IMS-STATUSKONTROLL                                           
182700     .                                                                    
182800     EJECT                                                                
182900 IMS-GU-ORQI-WDQ201 SECTION.                                              
183000     MOVE 'IMS-GU-ORQI-WDQ201     ' TO WS-CURRENT-IMS-SECTION             
183100                                                                          
183200     STRING 'WLORQI01(IDORDER  =' W-WDQ201-IDORDER-X ')'                  
183300          DELIMITED BY SIZE INTO SSA1                                     
183400     MOVE '  ' TO GODK-STATUSKODER                                        
183500     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-AREA-OHUV SSA1                 
183600     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
183700     PERFORM IMS-STATUSKONTROLL                                           
183800     .                                                                    
183900     EJECT                                                                
184000 IMS-GU-WDQ201 SECTION.                                                   
184100     MOVE 'IMS-GU-WDQ201          ' TO WS-CURRENT-IMS-SECTION             
184200                                                                          
184300     STRING 'WDQ201  (WDQ2CSEQ =' W-WDQ2CSEQ ')'                          
184400          DELIMITED BY SIZE INTO SSA1                                     
184500     MOVE '  ' TO GODK-STATUSKODER                                        
184600     CALL CBLTDLI USING GU WDQ2-PCB DLI-IO-AREA-WDQ201 SSA1               
184700     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
184800     PERFORM IMS-STATUSKONTROLL                                           
184900     .                                                                    
185000     EJECT                                                                
185100 IMS-GHNP-ORQI-WDQ212 SECTION.                                            
185200     MOVE 'IMS-GHNP-ORQI-WDQ212    ' TO WS-CURRENT-IMS-SECTION            
185300                                                                          
185400     STRING 'WLORQI12(IDDC     =' W-WDQ212-IDDC-X ')'                     
185500          DELIMITED BY SIZE INTO SSA1                                     
185600     MOVE '    ' TO GODK-STATUSKODER                                      
185700     CALL CBLTDLI USING GHNP ORQI-PCB DLI-IO-AREA-ARB SSA1                
185800     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
185900     PERFORM IMS-STATUSKONTROLL                                           
186000     .                                                                    
186100     EJECT                                                                
186200 IMS-REPL-ORQI-WDQ212 SECTION.                                            
186300     MOVE 'IMS-REPL-ORQI-WDQ212   ' TO WS-CURRENT-IMS-SECTION             
186400                                                                          
186500     MOVE '  ' TO GODK-STATUSKODER                                        
186600     CALL CBLTDLI USING REPL ORQI-PCB DLI-IO-AREA-ARB                     
186700     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
186800     PERFORM IMS-STATUSKONTROLL                                           
186900     .                                                                    
187000     EJECT                                                                
187100 IMS-GHU-ORQA-WDQ301 SECTION.                                             
187200     MOVE 'IMS-GHU-ORQA-WDQ301    ' TO WS-CURRENT-IMS-SECTION             
187300                                                                          
187400     STRING 'WLORQA01(WDQ301KY =' W-WDQ301KY-X ')'                        
187500          DELIMITED BY SIZE INTO SSA1                                     
187600     MOVE '  ' TO GODK-STATUSKODER                                        
187700     CALL CBLTDLI USING GHU ORQA-PCB DLI-IO-AREA-ODEL SSA1                
187800     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
187900     PERFORM IMS-STATUSKONTROLL                                           
188000     .                                                                    
188100     EJECT                                                                
188200 IMS-GHU-WDE401 SECTION.                                                  
188300     MOVE 'IMS-GHU-WDE401         ' TO WS-CURRENT-IMS-SECTION             
188400                                                                          
188500     STRING 'WDE401  (WDE401KY =' W-WDE401KY-X ')'                        
188600          DELIMITED BY SIZE INTO SSA1                                     
188700     MOVE '  ' TO GODK-STATUSKODER                                        
188800     CALL CBLTDLI USING GHU WDE4-PCB DLI-IO-WDE401 SSA1                   
188900     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
189000     PERFORM IMS-STATUSKONTROLL                                           
189100     .                                                                    
189200     EJECT                                                                
189300 IMS-GHU-WDE601 SECTION.                                                  
189400     MOVE 'IMS-GHU-WDE601         ' TO WS-CURRENT-IMS-SECTION             
189500                                                                          
189600     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
189700          DELIMITED BY SIZE INTO SSA1                                     
189800     MOVE '  GE' TO GODK-STATUSKODER                                      
189900     CALL CBLTDLI USING GHU WDE6-PCB DLI-IO-WDE601 SSA1                   
190000     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
190100     PERFORM IMS-STATUSKONTROLL                                           
190200     .                                                                    
190300     EJECT                                                                
190400 IMS-GHU-WDE611 SECTION.                                                  
190500     MOVE 'IMS-GHU-WDE611         ' TO WS-CURRENT-IMS-SECTION             
190600                                                                          
190700     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
190800          DELIMITED BY SIZE INTO SSA1                                     
190900     STRING 'WDE611  (IDKOLLI  =' W-WDE611-IDKOLLI-X ')'                  
191000          DELIMITED BY SIZE INTO SSA2                                     
191100     MOVE '  GE' TO GODK-STATUSKODER                                      
191200     CALL CBLTDLI USING GHU WDE6-PCB DLI-IO-WDE611 SSA1 SSA2              
191300     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
191400     PERFORM IMS-STATUSKONTROLL                                           
191500     .                                                                    
191600     EJECT                                                                
191700 IMS-GU-XXKH-4448 SECTION.                                                
191800     MOVE 'IMS-GU-XXKH-4448       ' TO WS-CURRENT-IMS-SECTION             
191900                                                                          
192000     STRING 'WLXXKH01(WDGXKEY  =' W-WDGXKEY-4447-X ')'                    
192100          DELIMITED BY SIZE INTO SSA1                                     
192200     STRING 'WLXXKH11(WDGXKEY  =' W-WDGXKEY-4448-X ')'                    
192300          DELIMITED BY SIZE INTO SSA2                                     
192400     MOVE '  ' TO GODK-STATUSKODER                                        
192500     CALL CBLTDLI USING GU XXKH-PCB DLI-IO-AREA-4448 SSA1 SSA2            
192600     MOVE XXKH-STATUS-CODE TO STATUS-WS                                   
192700     PERFORM IMS-STATUSKONTROLL                                           
192800     .                                                                    
192900     EJECT                                                                
193000 IMS-ISRT-WDE401 SECTION.                                                 
193100     MOVE 'IMS-ISRT-WDE401        ' TO WS-CURRENT-IMS-SECTION             
193200                                                                          
193300     MOVE 'WDE401   ' TO SSA1                                             
193400     MOVE '  II' TO GODK-STATUSKODER                                      
193500     CALL CBLTDLI USING ISRT WDE4-PCB DLI-IO-WDE401 SSA1                  
193600     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
193700     PERFORM IMS-STATUSKONTROLL                                           
193800     .                                                                    
193900     EJECT                                                                
194000 IMS-ISRT-WDE411 SECTION.                                                 
194100     MOVE 'IMS-ISRT-WDE411       ' TO WS-CURRENT-IMS-SECTION              
194200                                                                          
194300     STRING 'WDE401  (WDE401KY =' W-WDE401KY-X ')'                        
194400          DELIMITED BY SIZE INTO SSA1                                     
194500     MOVE 'WDE411   ' TO SSA2                                             
194600     MOVE '  ' TO GODK-STATUSKODER                                        
194700     CALL CBLTDLI USING ISRT WDE4-PCB DLI-IO-WDE411 SSA1 SSA2             
194800     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
194900     PERFORM IMS-STATUSKONTROLL                                           
195000     .                                                                    
195100     EJECT                                                                
195200 IMS-ISRT-WDE421 SECTION.                                                 
195300     MOVE 'IMS-ISRT-WDE421       ' TO WS-CURRENT-IMS-SECTION              
195400                                                                          
195500     STRING 'WDE401  (WDE401KY =' W-WDE401KY-X ')'                        
195600          DELIMITED BY SIZE INTO SSA1                                     
195700     STRING 'WDE411  (IDPURAD  =' W-WDE411-IDPURAD-X ')'                  
195800          DELIMITED BY SIZE INTO SSA2                                     
195900     MOVE 'WDE421   ' TO SSA3                                             
196000     MOVE '  ' TO GODK-STATUSKODER                                        
196100     CALL CBLTDLI USING ISRT WDE4-PCB DLI-IO-WDE421 SSA1 SSA2             
196200                                                    SSA3                  
196300     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
196400     PERFORM IMS-STATUSKONTROLL                                           
196500     .                                                                    
196600     EJECT                                                                
196700 IMS-ISRT-WDE601 SECTION.                                                 
196800     MOVE 'IMS-ISRT-WDE601       ' TO WS-CURRENT-IMS-SECTION              
196900                                                                          
197000     MOVE 'WDE601   ' TO SSA1                                             
197100     MOVE '  ' TO GODK-STATUSKODER                                        
197200     CALL CBLTDLI USING ISRT WDE6-PCB DLI-IO-WDE601 SSA1                  
197300     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
197400     PERFORM IMS-STATUSKONTROLL                                           
197500     .                                                                    
197600     EJECT                                                                
197700 IMS-ISRT-WDE611 SECTION.                                                 
197800     MOVE 'IMS-ISRT-WDE611       ' TO WS-CURRENT-IMS-SECTION              
197900                                                                          
198000     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
198100          DELIMITED BY SIZE INTO SSA1                                     
198200     MOVE 'WDE611   ' TO SSA2                                             
198300     MOVE '  ' TO GODK-STATUSKODER                                        
198400     CALL CBLTDLI USING ISRT WDE6-PCB DLI-IO-WDE611 SSA1 SSA2             
198500     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
198600     PERFORM IMS-STATUSKONTROLL                                           
198700     .                                                                    
198800     EJECT                                                                
198900 IMS-ISRT-ZZAC-WDG601 SECTION.                                            
199000     MOVE 'IMS-ISRT-ZZAC-WDG601  ' TO WS-CURRENT-IMS-SECTION              
199100                                                                          
199200     MOVE 'WLZZAC01 ' TO SSA1                                             
199300     MOVE '  II' TO GODK-STATUSKODER                                      
199400     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-AREA-WDG6 SSA1               
199500     MOVE ZZAC-STATUS-CODE TO STATUS-WS                                   
199600     PERFORM IMS-STATUSKONTROLL                                           
199700     .                                                                    
199800     EJECT                                                                
199900 IMS-GU-4487 SECTION.                                                     
200000     MOVE 'IMS-GU-4487         ' TO WS-CURRENT-IMS-SECTION                
200100                                                                          
200200     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-4487-X ')'                    
200300          DELIMITED BY SIZE INTO SSA1                                     
200400     MOVE '  GE' TO GODK-STATUSKODER                                      
200500     CALL CBLTDLI USING GU   4487-PCB DLI-IO-AREA-4487 SSA1               
200600     MOVE 4487-STATUS-CODE TO STATUS-WS                                   
200700     PERFORM IMS-STATUSKONTROLL                                           
200800     .                                                                    
200900     EJECT                                                                
201000 IMS-ISRT-4487 SECTION.                                                   
201100     MOVE 'IMS-ISRT-4487   ' TO WS-CURRENT-IMS-SECTION                    
201200                                                                          
201300     MOVE 'WDR401   ' TO SSA1                                             
201400     MOVE '  '   TO GODK-STATUSKODER                                      
201500     CALL CBLTDLI USING ISRT 4487-PCB DLI-IO-AREA-4487 SSA1               
201600     MOVE 4487-STATUS-CODE TO STATUS-WS                                   
201700     PERFORM IMS-STATUSKONTROLL                                           
201800     .                                                                    
201900     EJECT                                                                
202000 IMS-GNP-WDGX4488  SECTION.                                               
202100     MOVE 'IMS-GNP-WDGX4488 ' TO WS-CURRENT-IMS-SECTION                   
202200                                                                          
202300     STRING 'WDGX4488(KDPRCGRP =' W-WDGXKEY-4488-X ')'                    
202400          DELIMITED BY SIZE INTO SSA1                                     
202500     MOVE '  GE' TO GODK-STATUSKODER                                      
202600     CALL CBLTDLI USING GNP  4487-PCB DLI-IO-AREA-4488 SSA1               
202700     MOVE 4487-STATUS-CODE TO STATUS-WS                                   
202800     PERFORM IMS-STATUSKONTROLL                                           
202900     .                                                                    
203000     EJECT                                                                
203100 IMS-ISRT-WDGX4488  SECTION.                                              
203200     MOVE 'IMS-ISRT-WDGX4488' TO WS-CURRENT-IMS-SECTION                   
203300                                                                          
203400     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-4487-X ')'                    
203500          DELIMITED BY SIZE INTO SSA1                                     
203600     MOVE 'WDGX4488 ' TO SSA2                                             
203700     MOVE '  '   TO GODK-STATUSKODER                                      
203800     CALL CBLTDLI USING ISRT 4487-PCB DLI-IO-AREA-4488 SSA1 SSA2          
203900     MOVE 4487-STATUS-CODE TO STATUS-WS                                   
204000     PERFORM IMS-STATUSKONTROLL                                           
204100     .                                                                    
204200     EJECT                                                                
204300 IMS-ISRT-WDGX4490 SECTION.                                               
204400     MOVE 'IMS-ISRT-WDGX4490' TO WS-CURRENT-IMS-SECTION                   
204500                                                                          
204600     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-4487-X ')'                    
204700          DELIMITED BY SIZE INTO SSA1                                     
204800     STRING 'WDGX4488(KDPRCGRP =' W-WDGXKEY-4488-X ')'                    
204900          DELIMITED BY SIZE INTO SSA2                                     
205000     MOVE 'WDGX4490 ' TO SSA3                                             
205100     MOVE '  II' TO GODK-STATUSKODER                                      
205200     CALL CBLTDLI USING ISRT 4487-PCB DLI-IO-AREA-4490 SSA1 SSA2          
205300                                                       SSA3               
205400     MOVE 4487-STATUS-CODE TO STATUS-WS                                   
205500     PERFORM IMS-STATUSKONTROLL                                           
205600     .                                                                    
205700     EJECT                                                                
205800 IMS-ISRT-XXDV-4726 SECTION.                                              
205900     MOVE 'IMS-ISRT-XXDV-4726'  TO WS-CURRENT-IMS-SECTION                 
206000                                                                          
206100     STRING 'WLXXDV01(WDGXKEY  =' W-WDGXKEY-4726-ROT-X ')'                
206200          DELIMITED BY SIZE INTO SSA1                                     
206300     MOVE 'WLXXDV11 ' TO SSA2                                             
206400     MOVE '  II' TO GODK-STATUSKODER                                      
206500     CALL CBLTDLI USING ISRT XXDV-PCB DLI-IO-AREA-4726 SSA1 SSA2          
206600     MOVE XXDV-STATUS-CODE TO STATUS-WS                                   
206700     PERFORM IMS-STATUSKONTROLL                                           
206800     .                                                                    
206900     EJECT                                                                
207000 IMS-ISRT-XXDV-4727 SECTION.                                              
207100     MOVE 'IMS-ISRT-XXDV-4727'  TO WS-CURRENT-IMS-SECTION                 
207200                                                                          
207300     STRING 'WLXXDV01(WDGXKEY  =' W-WDGXKEY-4726-ROT-X ')'                
207400          DELIMITED BY SIZE INTO SSA1                                     
207500     STRING 'WLXXDV11(WDGXKEY  =' W-WDGXKEY-4726-X ')'                    
207600          DELIMITED BY SIZE INTO SSA2                                     
207700     MOVE 'WLXXDV21 ' TO SSA3                                             
207800     MOVE '  II' TO GODK-STATUSKODER                                      
207900     CALL CBLTDLI USING ISRT XXDV-PCB DLI-IO-AREA-4727 SSA1 SSA2          
208000                                                       SSA3               
208100     MOVE XXDV-STATUS-CODE TO STATUS-WS                                   
208200     PERFORM IMS-STATUSKONTROLL                                           
208300     .                                                                    
208400     EJECT                                                                
208500 IMS-REPL-WDE401 SECTION.                                                 
208600     MOVE 'IMS-REPL-WDE401   '  TO WS-CURRENT-IMS-SECTION                 
208700                                                                          
208800     MOVE '  ' TO GODK-STATUSKODER                                        
208900     CALL CBLTDLI USING REPL WDE4-PCB DLI-IO-WDE401                       
209000     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
209100     PERFORM IMS-STATUSKONTROLL                                           
209200     .                                                                    
209300     EJECT                                                                
209400 IMS-REPL-WDE601 SECTION.                                                 
209500     MOVE 'IMS-REPL-WDE601   '  TO WS-CURRENT-IMS-SECTION                 
209600                                                                          
209700     MOVE '  ' TO GODK-STATUSKODER                                        
209800     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-WDE601                       
209900     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
210000     PERFORM IMS-STATUSKONTROLL                                           
210100     .                                                                    
210200     EJECT                                                                
210300 IMS-REPL-WDE611 SECTION.                                                 
210400     MOVE 'IMS-REPL-WDE611   '  TO WS-CURRENT-IMS-SECTION                 
210500                                                                          
210600     MOVE '  ' TO GODK-STATUSKODER                                        
210700     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-WDE611                       
210800     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
210900     PERFORM IMS-STATUSKONTROLL                                           
211000     .                                                                    
211100     EJECT                                                                
211200 IMS-REPL-ORQA-WDQ301 SECTION.                                            
211300     MOVE 'IMS-REPL-ORQA-WDQ301'  TO WS-CURRENT-IMS-SECTION               
211400                                                                          
211500     MOVE '  ' TO GODK-STATUSKODER                                        
211600     CALL CBLTDLI USING REPL ORQA-PCB DLI-IO-AREA-ODEL                    
211700     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
211800     PERFORM IMS-STATUSKONTROLL                                           
211900     .                                                                    
212000                                                                          
212100 IMS-GU-WDB601    SECTION.                                                
212200     MOVE 'IMS-GU-WDB601     '  TO WS-CURRENT-IMS-SECTION                 
212300                                                                          
212400     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
212500          DELIMITED BY SIZE INTO SSA1                                     
212600     MOVE '  ' TO GODK-STATUSKODER                                        
212700     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
212800     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
212900     PERFORM IMS-STATUSKONTROLL                                           
213000     .                                                                    
213100     EJECT                                                                
213200 IMS-GHN-WDA6B SECTION.                                                   
213300     MOVE 'IMS-GHN-WDA6B     '  TO WS-CURRENT-IMS-SECTION                 
213400                                                                          
213500     STRING 'WDA601  (WDA6BSEQ>=' W-WDA601KY-MIN-X                        
213600                    '&WDA6BSEQ<=' W-WDA601KY-MAX-X ')'                    
213700            DELIMITED BY SIZE INTO SSA1                                   
213800     MOVE '  GEGB'               TO GODK-STATUSKODER                      
213900     CALL  CBLTDLI  USING GHN   WDA6B-PCB DLI-IO-AREA-A601 SSA1           
214000     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
214100     PERFORM IMS-STATUSKONTROLL                                           
214200     .                                                                    
214300     SKIP2                                                                
214400 IMS-REPL-WDA6B SECTION.                                                  
214500     MOVE 'IMS-REPL-WDA6B    '  TO WS-CURRENT-IMS-SECTION                 
214600                                                                          
214700     MOVE 'WDA601  '           TO SSA1                                    
214800     MOVE '    '               TO GODK-STATUSKODER                        
214900     CALL  CBLTDLI  USING REPL WDA6B-PCB DLI-IO-AREA-A601 SSA1            
215000     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
215100     PERFORM IMS-STATUSKONTROLL                                           
215200     .                                                                    
215300 IMS-STATUSKONTROLL SECTION.                                              
215400                                                                          
215500     SET STATUS-IX TO 1                                                   
215600     SEARCH GODK-STATUS                                                   
215700       AT END CALL FELLOG                                                 
215800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
215900     END-SEARCH                                                           
216000     .                                                                    
216100     EJECT                                                                
