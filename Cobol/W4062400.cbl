000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4062400.                                                
000300 AUTHOR.         KARANDE DIGAMBAR.                                        
000400 DATE-WRITTEN.   02/07/01.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNCTION:                                                            
000900*        ---------------------------------------------------------        
001000*        ADDITIONAL IN RELEASE 04:3                                       
001100*        SHOWS SUBTOTALS PER SENDING WITH FOLLOWING INPUT KEYS:           
001200*        IDSHIPM AND IDDC                                                 
001300*        ---------------------------------------------------------        
001400*        PROGRAM SHOWS BOTH PACKAGE INFORMATION BEFORE SHIPPING           
001500*        AND AFTER SHIPPING DEPENDING ON THE INPUT KEYS AS BELOW          
001600*        1. IDSHIPM  GIVEN - INFORMATION AFTER SHIPPING                   
001700*        2. IDTRPTNR GIVEN - INFORMATION BEFORE SHIPPING                  
001800*        BOTH IDSHIPM AND IDTRPTNR TOGETHER ARE NOT ALLOWED               
001900*        INFORMATION SHOWN ON SCREEN -                                    
002000*        SUMMERY FIELDS: TOT NO CASE,TOT WEIGHT,TOT VOLUME,TOT            
002100*        VALUE,TOT NETWEIGHT                                              
002200*        LINE FIELDS:IDKUNDNR,IDORDER,CASE,CUSTOMS-ID,LENGTH,             
002300*        HIGHT,WEIGHT,VOLUME,VALUE,NET WEIGHT AND DANGEROUS GOODS         
002400*        CODE                                                             
002500*                                                                         
002600*---- OBS!                                                                
002700*        PROGRAMET ÄR INTE ANPASSAT MED SUORDV-EXP FÖR ATT VISA           
002800*        'STUDS'DISTRIKTENS RÄTTA SUMMOR EFTERSOM DENNA BILD              
002900*        ANVÄNDS BARA PÅ CDC OCH INGA 'STUDS'DISTRIKT SKEPPAS             
003000*        HÄRIFRÅN JUST NU.                                                
003100*        DETTA MÅSTE GÖRAS IFALL FÖRUTSÄTT. ÄNDRAS!                       
003200*---- OBS!                                                                
003300*                                                                         
003400*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!             
003500*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0182               
003600*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!             
003700*                                                                         
003800*        THE PROGRAM READS     WDE1                                       
003900*        THE PROGRAM READS     WDE6                                       
004000*        THE PROGRAM READS     WDB2                                       
004100*        THE PROGRAM READS     WDB1                                       
004200*        THE PROGRAM READS     WDG2                                       
004300*        THE PROGRAM READS     WDE4                                       
004400*        THE PROGRAM READS     WDB6                                       
004500*                                                                         
004600*    INDATA.                                                              
004700*        TRANSACTION: W4T624                                              
004800*        MID:         W4I62401                                            
004900*                                                                         
005000*    OUTDATA.                                                             
005100*        MOD:         W4O62401                                            
005200                                                                          
005300     SKIP3                                                                
005400 ENVIRONMENT DIVISION.                                                    
005500                                                                          
005600 DATA DIVISION.                                                           
005700     EJECT                                                                
005800 WORKING-STORAGE SECTION.                                                 
005900                                                                          
006000 77  IDPGM                       PIC X(08)   VALUE 'W4062400'.            
006100 77  FELTEXT                     PIC X(16)   VALUE SPACE.                 
006200                                                                          
006300*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
006400 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
006500                                                                          
006600 77  YES                         PIC X       VALUE 'J'.                   
006700 77  NOO                         PIC X       VALUE 'N'.                   
006800 77  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
006900 77  WS-KDVALISO-HUV             PIC X(3)    VALUE 'SEK'.                 
007000                                                                          
007100*    --- INDEX FOR SCROLL LINES                                           
007200 77  INDX                        PIC S9(4)  VALUE +0.                     
007300 77  MAX-INDX                    PIC S9(4)  VALUE +06   COMP SYNC.        
007400 77  SPRAK-IX                    PIC S9(9)  VALUE +1    COMP SYNC.        
007500                                                                          
007600 77  W-DCS-IDPARTNR-EXP          PIC  X(9)   VALUE SPACE.                 
007700*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
007800                                                                          
007900                                                                          
008000 77  KEYS-SW                     PIC X       VALUE 'J'.                   
008100     88  KEYS-OK                             VALUE 'J'.                   
008200     88  KEYS-WRONG                          VALUE 'N'.                   
008300                                                                          
008400 77  W-IDSHIPM-GIVEN             PIC X       VALUE 'N'.                   
008500     88  IDSHIPM-GIVEN                       VALUE 'J'.                   
008600     88  IDSHIPM-NOT-GIVEN                   VALUE 'N'.                   
008700                                                                          
008800 77  W-IDTRPTNR-GIVEN            PIC X        VALUE 'N'.                  
008900     88  IDTRPTNR-GIVEN                       VALUE 'J'.                  
009000     88  IDTRPTNR-NOT-GIVEN                   VALUE 'N'.                  
009100                                                                          
009200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
009300     88  OWN-MID                             VALUE '4624'.                
009400     88  GOOD-MID                            VALUE '4621' '4622'          
009500                                                   '4623' '4624'          
009600                                                   '4625' '4626'          
009700                                                   '4627' '4628'          
009800                                                   '4629'.                
009900     88  HELP-MID                            VALUE '0551'.                
010000                                                                          
010100 77  WX-IDTRANS                  PIC X(4)    VALUE SPACE.                 
010200                                                                          
010300 77  W-IDSHIPM                   PIC 9(07).                               
010400 77  WX-IDSHIPM                  PIC X(07)   VALUE '    +++'.             
010500     EJECT                                                                
010600*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
010700 01  GENERAL-SUBPROGRAMS.                                                 
010800     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
010900     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
011000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011200     03  W411EXCH                PIC X(8)    VALUE 'W411EXCH'.            
011300     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
011400     EJECT                                                                
011500*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
011600*01 -COPY WMEDAREA                                                        
011700     SKIP3                                                                
011800*    --- PARAMETERS FOR SUBPROGRAM W510CURR                               
011900*01 -COPY W510CURR                                                        
012000     SKIP3                                                                
012100 01  MESSAGE-CODES.                                                       
012200     03  KEYS-ARE-MISSING        PIC X(3)    VALUE '005'.                 
012300     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
012400     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
012500     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
012600     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
012700     03  SHIPMENT-MISSING        PIC X(3)    VALUE '341'.                 
012800                                                                          
012900 01  W-NOT-ALLOWED-TEXT          PIC X(55)   VALUE                        
013000        'SHIPM NO AND TRANSPORT ARE NOT ALLOWED AT THE SAME TIME'.        
013100                                                                          
013200 01  FILLER-1.                                                            
013300     03  FILLER                  PIC X(40)                                
013400         VALUE '351 ONLY BILL-IT MARKET ALLOWED        '.                 
013500 01  FILLER REDEFINES FILLER-1.                                           
013600     03  ERR-NOT-BILL-IT-MARKET      OCCURS 1  PIC X(40).                 
013700     EJECT                                                                
013800     EJECT                                                                
013900*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
014000*                                                                         
014100 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
014200     SKIP3                                                                
014300*01 -COPY WMSGINIT                                                        
014400     EJECT                                                                
014500 01  FILLER                      PIC  X(16)  VALUE                        
014600                                                'W411EXCH AREA '.         
014700*01 -COPY W411EXCH                                                        
014800     EJECT                                                                
014900*01 -COPY WWDCKONS                                                        
015000                                                                          
015100 01  TEST-IDDISTR            PIC  9(5) COMP-3 VALUE ZERO.                 
015200                                                                          
015300*01  FILLER  -COPY WWDIST35 -RED TEST-IDDISTR.                            
015400                                                                          
015500*      ----DISTR-DEALER-PRICE------                                       
015600*01  FILLER  -COPY WWDIST79 -RED TEST-IDDISTR.                            
015700                                                                          
015800*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
015900*                                                                         
016000 01  FILLER                      PIC X(16)   VALUE 'SAVE AREA'.           
016100     SKIP3                                                                
016200 01  SAVE-AREA.                                                           
016300     03  SAVE-IDTRANS            PIC X(4)    VALUE '4624'.                
016400     03  SAVE-IDDISTR            PIC S9(5).                               
016500     03  SAVE-IDKUNDNR           PIC S9(7).                               
016600     03  SAVE-IDPRODNR           PIC S9(7).                               
016700     03  SAVE-IDSHIPM            PIC 9(7).                                
016800     03  SAVE-IDTRPTNR           PIC S9(3).                               
016900     03  SAVE-IDDC               PIC X(2).                                
017000** FOR 1 WHEN IDSHIPM GIVEN  - INFORMATION AFTER SHIPPING                 
017100     03  SAVE-SCROLL-1.                                                   
017200         05  SAVE-IDPRODNR-ENTER PIC S9(7)  VALUE ZERO COMP-3.            
017300         05  SAVE-IDPRODNR-NEXT  PIC S9(7)  VALUE ZERO COMP-3.            
017400         05  SAVE-IDKUNDNR-ENTER PIC S9(7)  VALUE ZERO COMP-3.            
017500         05  SAVE-IDKUNDNR-NEXT  PIC S9(7)  VALUE ZERO COMP-3.            
017600         05  SAVE-IDKOLLI-ENTER  PIC S9(5)  VALUE ZERO COMP-3.            
017700         05  SAVE-IDKOLLI-NEXT   PIC S9(5)  VALUE ZERO COMP-3.            
017800         05  SAVE-IDDISTR-ENTER  PIC S9(5)  VALUE ZERO COMP-3.            
017900         05  SAVE-IDDISTR-NEXT   PIC S9(5)  VALUE ZERO COMP-3.            
018000** FOR 2 WHEN IDTRPTNR GIVEN - INFORMATION BEFORE SHIPPING                
018100     03  SAVE-WDE6CSEQ-ENTER.                                             
018200         05  SAVE-IDTRPTNR-ENTER       PIC S9(3)     COMP-3.              
018300         05  SAVE-DARFS-ENTER          PIC 9(12).                         
018400         05  SAVE-ADKOLLI-ENTER.                                          
018500             07 SAVE-ADCLGEO-ENTER.                                       
018600                09 SAVE-IDDC-ENTER     PIC X(2).                          
018700                09 SAVE-ADFLGEO-ENTER  PIC X(3).                          
018800             07 SAVE-ADFLOMR-ENTER     PIC S9(3)     COMP-3.              
018900             07 SAVE-ADRUTNIV-ENTER    PIC S9(3)     COMP-3.              
019000             07 SAVE-ADVMODUL-ENTER    PIC S9(3)     COMP-3.              
019100         05  SAVE-TRP-IDPRODNR-ENTER   PIC S9(7)     COMP-3.              
019200         05  SAVE-TRP-IDKOLLI-ENTER    PIC S9(5)     COMP-3.              
019300     03  SAVE-WDE6CSEQ-NEXT.                                              
019400         05  SAVE-IDTRPTNR-NEXT        PIC S9(3)     COMP-3.              
019500         05  SAVE-DARFS-NEXT           PIC 9(12).                         
019600         05  SAVE-ADKOLLI-NEXT.                                           
019700             07 SAVE-ADCLGEO-NEXT.                                        
019800                09 SAVE-IDDC-NEXT      PIC X(2).                          
019900                09 SAVE-ADFLGEO-NEXT   PIC X(3).                          
020000             07 SAVE-ADFLOMR-NEXT      PIC S9(3)     COMP-3.              
020100             07 SAVE-ADRUTNIV-NEXT     PIC S9(3)     COMP-3.              
020200             07 SAVE-ADVMODUL-NEXT     PIC S9(3)     COMP-3.              
020300         05  SAVE-TRP-IDPRODNR-NEXT    PIC S9(7)     COMP-3.              
020400         05  SAVE-TRP-IDKOLLI-NEXT     PIC S9(5)     COMP-3.              
020500     03  SAVE-TOTAL.                                                      
020600         05  SAVE-KVKOLLI-SKP    PIC 9(5).                                
020700         05  SAVE-VKORDBTO-SKP   PIC S9(6)V9(1).                          
020800         05  SAVE-VLORDBTO-SKP   PIC S9(4)V9(3).                          
020900         05  SAVE-SUORDV-SKEPPN  PIC S9(9)V9(2).                          
021000         05  SAVE-VKORDNTO-SKP   PIC S9(6)V9(1).                          
021100         05  SAVE-SUORDV-LOCPREL-SKP                                      
021200                                 PIC S9(9)V9(2).                          
021300     03  SAVE-PRKURS             PIC S9(6)V9(5)    COMP-3.                
021400     EJECT                                                                
021500 01  FILLER                      PIC X(16)   VALUE 'TOTAL AREA'.          
021600     SKIP3                                                                
021700 01  W-TOTAL.                                                             
021800     03  W-SUORDV-TOT-SEK        PIC S9(9)V9(2)    VALUE ZERO.            
021900     03  W-SUORDV-LOC            PIC S9(9)V9(2)    VALUE ZERO.            
022000     03  W-SUORDV-LOCPREL        PIC S9(9)V9(2)    VALUE ZERO.            
022100     03  W-SUORDV-LOC-SEK        PIC S9(9)V9(2)    VALUE ZERO.            
022200     03  W-SUORDV-LOCPREL-SEK    PIC S9(9)V9(2)    VALUE ZERO.            
022300 01  FILLER                      PIC X(16)   VALUE 'W-SKP AREA'.          
022400     SKIP3                                                                
022500 01  W-SKP.                                                               
022600     03  W-KVKOLLI-SKP           PIC 9(5)          VALUE ZERO.            
022700     03  W-VKORDBTO-SKP          PIC S9(6)V9(1)    VALUE ZERO.            
022800     03  W-VLORDBTO-SKP          PIC S9(4)V9(3)    VALUE ZERO.            
022900     03  W-SUORDV-SKEPPN         PIC S9(9)V9(2)    VALUE ZERO.            
023000     03  W-VKORDNTO-SKP          PIC S9(6)V9(1)    VALUE ZERO.            
023100     03  W-SUORDV-LOCPREL-SKP    PIC S9(9)V9(2)    VALUE ZERO.            
023200*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
023300*                                                                         
023400 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
023500     SKIP3                                                                
023600*01  MID -COPY W4I62401                                                   
023700     EJECT                                                                
023800 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
023900     SKIP3                                                                
024000*01  -COPY WMSGAREA                                                       
024100     EJECT                                                                
024200     03  MOD REDEFINES MSG-AREA.                                          
024300*      05  -COPY W4O62401                                                 
024400     EJECT                                                                
024500 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
024600     SKIP3                                                                
024700*01  -COPY WMFSAREA                                                       
024800     EJECT                                                                
024900*    --- WORK-AREAS FOR IMS-SECTIONS                                      
025000*                                                                         
025100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
025200     SKIP3                                                                
025300 01  KEYS-TO-DLI.                                                         
025400*    --- VALUE OF SCROLLING KEY FOR FIRST LINE ON THE SCREEN              
025500     03  W-IDSHIPM-X.                                                     
025600         05  W-WDE101-GHU-IDSHIPM   PIC 9(7)    VALUE ZERO.               
025700*                                                                         
025800     03  W-WDE101-IDSHIPM-X.                                              
025900         05  W-WDE101-IDSHIPM       PIC 9(7)    VALUE ZERO.               
026000*                                                                         
026100     03  W-WDE111KY-X.                                                    
026200         05  W-WDE111-IDDISTR       PIC S9(5)   VALUE ZERO COMP-3.        
026300         05  W-WDE111-IDKUNDNR      PIC S9(7)   VALUE ZERO COMP-3.        
026400*                                                                         
026500     03  W-IDDC-X.                                                        
026600         05  W-IDDC                 PIC X(2)    VALUE SPACE.              
026700*                                                                         
026800     03  W-IDDISTR-X.                                                     
026900         05  W-IDDISTR              PIC S9(5)   VALUE ZERO COMP-3.        
027000*                                                                         
027100     03  W-IDKUNDNR-X.                                                    
027200         05  W-IDKUNDNR             PIC S9(7)   VALUE ZERO COMP-3.        
027300*                                                                         
027400     03  W-IDPRODNR-X.                                                    
027500         05  W-IDPRODNR             PIC S9(7)   VALUE ZERO COMP-3.        
027600*                                                                         
027700     03  W-IDKOLLI-X.                                                     
027800         05  W-IDKOLLI              PIC S9(5)   VALUE ZERO COMP-3.        
027900*                                                                         
028000     03  W-WDE111KY-MIN.                                                  
028100         05  W-WDE111-IDDISTR-MIN   PIC S9(5)   VALUE ZERO COMP-3.        
028200         05  W-WDE111-IDKUNDNR-MIN  PIC S9(7)   VALUE ZERO COMP-3.        
028300*                                                                         
028400     03  W-WDE111KY-MAX.                                                  
028500         05  W-WDE111-IDDISTR-MAX   PIC S9(5) VALUE 99999 COMP-3.         
028600         05  W-WDE111-IDKUNDNR-MAX PIC S9(7) VALUE 9999999 COMP-3.        
028700*                                                                         
028800     03  W-WDE121KY-MIN.                                                  
028900         05  W-WDE121-IDPRODNR-MIN  PIC S9(7)           COMP-3.           
029000         05  W-WDE121-IDKOLLI-MIN   PIC S9(5)           COMP-3.           
029100*                                                                         
029200     03  W-WDE121KY-MAX.                                                  
029300         05  W-WDE121-IDPRODNR-MAX  PIC S9(7)           COMP-3.           
029400         05  W-WDE121-IDKOLLI-MAX   PIC S9(5)           COMP-3.           
029500*                                                                         
029600     03  W-WDE601-IDPRODNR-X.                                             
029700         05  W-WDE601-IDPRODNR      PIC S9(7)   VALUE ZERO COMP-3.        
029800*                                                                         
029900     03  W-WDE611-IDKOLLI-X.                                              
030000         05  W-WDE611-IDKOLLI       PIC S9(5)   VALUE ZERO COMP-3.        
030100*                                                                         
030200     03  W-WDE6CSEQ-MIN.                                                  
030300         05  W-WDE611-IDTRPTNR-MIN         PIC S9(3)    COMP-3.           
030400         05  W-WDE611-DARFS-MIN            PIC 9(12).                     
030500         05  W-WDE611-ADKOLLI-MIN.                                        
030600             07  W-WDE611-ADCLGEO-MIN.                                    
030700                 09  W-WDE611-IDDC-MIN     PIC X(2).                      
030800                 09  W-WDE611-ADFLGEO-MIN  PIC X(3).                      
030900             07  W-WDE611-ADFLOMR-MIN      PIC S9(3)    COMP-3.           
031000             07  W-WDE611-ADRUTNIV-MIN     PIC S9(3)    COMP-3.           
031100             07  W-WDE611-ADVMODUL-MIN     PIC S9(3)    COMP-3.           
031200*                                                                         
031300     03  W-WDE6CSEQ-MAX.                                                  
031400         05  W-WDE611-IDTRPTNR-MAX         PIC S9(3)    COMP-3.           
031500         05  W-WDE611-DARFS-MAX            PIC 9(12).                     
031600         05  W-WDE611-ADKOLLI-MAX.                                        
031700             07  W-WDE611-ADCLGEO-MAX.                                    
031800                 09  W-WDE611-IDDC-MAX     PIC X(2).                      
031900                 09  W-WDE611-ADFLGEO-MAX  PIC X(3).                      
032000             07  W-WDE611-ADFLOMR-MAX      PIC S9(3)    COMP-3.           
032100             07  W-WDE611-ADRUTNIV-MAX     PIC S9(3)    COMP-3.           
032200             07  W-WDE611-ADVMODUL-MAX     PIC S9(3)    COMP-3.           
032300*                                                                         
032400     03  W-WDE4F1KY-MIN-X.                                                
032500         05  W-E4-IDPRODNR-MIN   PIC S9(07)   VALUE ZERO COMP-3.          
032600         05  W-E4-IDKOLLI-MIN    PIC S9(05)   VALUE ZERO COMP-3.          
032700         05  FILLER              PIC X(22)    VALUE LOW-VALUE.            
032800*                                                                         
032900     03  W-WDE4F1KY-MAX-X.                                                
033000         05  W-E4-IDPRODNR-MAX   PIC S9(07)   VALUE ZERO COMP-3.          
033100         05  W-E4-IDKOLLI-MAX    PIC S9(05)   VALUE ZERO COMP-3.          
033200         05  FILLER              PIC X(22)    VALUE HIGH-VALUE.           
033300*                                                                         
033400     03  W-WDB201-IDGMT-X.                                                
033500         05 W-WDB201-IDDISTR     PIC S9(5) VALUE ZERO COMP-3.             
033600         05 W-WDB201-IDKUNDNR    PIC S9(7) VALUE ZERO COMP-3.             
033700*                                                                         
033800     03  W-WDB201-IDGMT-MIN-X.                                            
033900         05 W-WDB201-IDDISTR-MIN   PIC S9(5) VALUE ZERO COMP-3.           
034000         05 W-WDB201-IDKUNDNR-MIN  PIC S9(7) VALUE ZERO COMP-3.           
034100*                                                                         
034200     03  W-WDB201-IDGMT-MAX-X.                                            
034300         05 W-WDB201-IDDISTR-MAX   PIC S9(5) VALUE ZERO COMP-3.           
034400         05 W-WDB201-IDKUNDNR-MAX  PIC S9(7) VALUE ZERO COMP-3.           
034500*                                                                         
034600     03  W-WDB101KY-X.                                                    
034700         05 W-WDB101-IDPARTNR      PIC X(9)  VALUE SPACE.                 
034800         05 W-WDB101-IDFTG         PIC 9(2)  VALUE ZERO.                  
034900*                                                                         
035400     03  W-IDDC-B6-X.                                                     
035500         05 W-IDDC-B6             PIC X(2).                               
035600*                                                                         
035700     SKIP2                                                                
035800*    --- STATUS-KOD FRÅN IMS                                              
035900 01  STATUS-WS                   PIC XX.                                  
036000     88  SEGMENT-FOUND                       VALUE '  '.                  
036100     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
036200     88  SEGMENT-MISSING                     VALUE 'GE'.                  
036300     SKIP2                                                                
036400 01  GOOD-STATUSCODES.                                                    
036500     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
036600     SKIP3                                                                
036700 01  SSA1                        PIC X(144).                              
036800 01  SSA2                        PIC X(128).                              
036900     EJECT                                                                
037000*    --- IMS FUNCTION CODES                                               
037100*01  -COPY W0003                                                          
037200     EJECT                                                                
037300*    ---  DLI INPUT-OUTPUT AREA                                           
037400                                                                          
037500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE101'.                      
037600 01  DLI-IO-WDE101.                                                       
037700*    03  -COPY WDE101                                                     
037800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE111'.                      
037900 01  DLI-IO-WDE111.                                                       
038000*    03  -COPY WDE111                                                     
038100     EJECT                                                                
038200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE121'.                      
038300 01  DLI-IO-WDE121.                                                       
038400*    03  -COPY WDE121                                                     
038500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE611-01'.                   
038600 01  DLI-IO-WDE611-01.                                                    
038700   03  DLI-IO-WDE611.                                                     
038800*    05  -COPY WDE611                                                     
038900   03  DLI-IO-WDE601.                                                     
039000*    05  -COPY WDE601                                                     
039100     EJECT                                                                
039200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE4F1'.                      
039300 01  DLI-IO-WDE4F1.                                                       
039400*    03  -COPY WDE4F1                                                     
039500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
039600 01  DLI-IO-WDB201.                                                       
039700*    03  -COPY WDB201                                                     
039800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB101'.                      
039900 01  DLI-IO-WDB101.                                                       
040000*    03  -COPY WDB101                                                     
040400 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
040500 01   DLI-IO-AREA-B601.                                                   
040600*     03  -COPY WDB601                                                    
040700                                                                          
040800     EJECT                                                                
040900 LINKAGE SECTION.                                                         
041000*01  -COPY W0009   -PRE MSG-                                              
041100*01  -COPY W0008   -PRE WDP7-                                             
041200     05  FILLER                  PIC X.                                   
041300                                                                          
041400*01  -COPY W0008  -PRE WDE1-                                              
041500     05  FILLER                  PIC X.                                   
041600                                                                          
041700*01  -COPY W0008  -PRE WDE6-                                              
041800     05  FILLER                  PIC X.                                   
041900                                                                          
042000*01  -COPY W0008  -PRE WDE6CSQ-                                           
042100     05  FILLER                  PIC X.                                   
042200                                                                          
042300*01  -COPY W0008  -PRE WDE4F-                                             
042400     05  FILLER                  PIC X.                                   
042500                                                                          
042600*01  -COPY W0008  -PRE WDB2-                                              
042700     05  FILLER                  PIC X.                                   
042800                                                                          
042900*01  -COPY W0008  -PRE WDB1-                                              
043000     05  FILLER                  PIC X.                                   
043100                                                                          
043200*01  -COPY W0008  -PRE WDG2-                                              
043300     05  FILLER                  PIC X.                                   
043400                                                                          
043500*01  -COPY W0008  -PRE WDB6-                                              
043600     05  FILLER                  PIC X.                                   
043700                                                                          
043800     EJECT                                                                
043900 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDE1-PCB WDE6-PCB             
044000     WDE6CSQ-PCB WDE4F-PCB WDB2-PCB WDB1-PCB WDG2-PCB WDB6-PCB.           
044100 MAIN SECTION.                                                            
044200     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDE1-PCB WDE6-PCB             
044300     WDE6CSQ-PCB WDE4F-PCB WDB2-PCB WDB1-PCB WDG2-PCB WDB6-PCB.           
044400                                                                          
044500     PERFORM IMS-GET-MSG                                                  
044600     IF SEGMENT-FOUND                                                     
044700       PERFORM A-INIT                                                     
044800       PERFORM B-CHECK-KEYS                                               
044900       IF KEYS-OK                                                         
045000           IF MFS-FIRST                                                   
045100             PERFORM C-FIRST-PAGE                                         
045200           ELSE                                                           
045300             IF MFS-NEXT                                                  
045400               PERFORM D-NEXT-PAGE                                        
045500             ELSE                                                         
045600               PERFORM E-SAME-PAGE                                        
045700             END-IF                                                       
045800           END-IF                                                         
045900           PERFORM F-READ-SHOW-INFO                                       
046000       END-IF                                                             
046100*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
046200*    --- IF PROGRAM-TO-PROGRAM-SWITCH:       = MOD-LENGTH + 17            
046300       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O62401 + 4                      
046400       PERFORM IMS-INSERT-MSG                                             
046500     END-IF                                                               
046600                                                                          
046700     MOVE ZERO TO RETURN-CODE                                             
046800     GOBACK                                                               
046900     .                                                                    
047000     EJECT                                                                
047100 A-INIT SECTION.                                                          
047200                                                                          
047300     IF MSG-DOUBLE-TRANSACTIONS                                           
047400       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W4I62401                 
047500       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
047600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
047700     ELSE                                                                 
047800       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W4I62401                  
047900       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
048000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
048100     END-IF                                                               
048200                                                                          
048300     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
048400     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
048500     MOVE MFS-IDTRANS TO W-IDTRANS                                        
048600                         WX-IDTRANS                                       
048700                                                                          
048800     MOVE LOW-VALUE TO MSG-AREA                                           
048900     MOVE 'W4O624N1' TO MFS-IDMOD                                         
049000     MOVE '4624' TO MOD-IDTRANS                                           
049100     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
049200                                                                          
049300     IF OWN-MID OR HELP-MID                                               
049400       CONTINUE                                                           
049500     ELSE                                                                 
049600       MOVE SPACE TO MFS-KDTRTYP                                          
049700       MOVE '7' TO MFS-IDPFK                                              
049800     END-IF                                                               
049900                                                                          
050000     MOVE  ZERO             TO  W-WDE121-IDPRODNR-MIN                     
050100                                W-WDE121-IDKOLLI-MIN                      
050200                                W-WDE611-IDTRPTNR-MIN                     
050300                                W-WDE611-DARFS-MIN                        
050400                                W-WDE611-ADFLOMR-MIN                      
050500                                W-WDE611-ADRUTNIV-MIN                     
050600                                W-WDE611-ADVMODUL-MIN                     
050700                                W-WDB201-IDDISTR-MIN                      
050800                                W-WDB201-IDKUNDNR-MIN                     
050900                                                                          
051000     MOVE LOW-VALUE         TO  W-WDE611-IDDC-MIN                         
051100                                W-WDE611-ADFLGEO-MIN                      
051200                                                                          
051300     MOVE  ALL '9'          TO  W-WDE121-IDPRODNR-MAX                     
051400                                W-WDE121-IDKOLLI-MAX                      
051500                                W-WDE611-IDTRPTNR-MAX                     
051600                                W-WDE611-DARFS-MAX                        
051700                                W-WDE611-ADFLOMR-MAX                      
051800                                W-WDE611-ADRUTNIV-MAX                     
051900                                W-WDE611-ADVMODUL-MAX                     
052000                                W-WDB201-IDDISTR-MAX                      
052100                                W-WDB201-IDKUNDNR-MAX                     
052200                                                                          
052300     MOVE HIGH-VALUE        TO  W-WDE611-IDDC-MAX                         
052400                                W-WDE611-ADFLGEO-MAX                      
052500     MOVE FUNCTION CURRENT-DATE(3:2) TO W-DATE-AAMM(1:2)                  
052600     MOVE FUNCTION CURRENT-DATE(5:2) TO W-DATE-AAMM(3:2)                  
052700                                                                          
052800                                                                          
052900     .                                                                    
053000     EJECT                                                                
053100 B-CHECK-KEYS SECTION.                                                    
053200                                                                          
053300     MOVE ALL '+'           TO MSGI-WMSGINIT                              
053400     MOVE '001'             TO MSGI-KDCALL                                
053500     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
053600     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
053700     MOVE '4624'            TO MSGI-IDTRANS                               
053800     IF MID-IDSHIPM-IN = WX-IDSHIPM                                       
053900        MOVE ALL '+'             TO MID-IDSHIPM-IN                        
054000     END-IF                                                               
054100     IF GOOD-MID                                                          
054200         MOVE MID-IDDISTR-IN     TO MSGI-IDDISTR                          
054300         MOVE MID-IDKUNDNR-IN    TO MSGI-IDKUNDNR                         
054400         MOVE MID-IDSHIPM-IN     TO MSGI-IDSHIPM                          
054500         MOVE MID-IDTRPTNR-IN    TO MSGI-IDTRPTNR                         
054600         MOVE MID-IDDC-IN        TO MSGI-IDDC-KEY                         
054700     END-IF                                                               
054800     IF MID-IDSHIPM-IN  NOT = ALL '+' AND                                 
054900        MID-IDDISTR-IN      = ALL '+' AND                                 
055000        MID-IDKUNDNR-IN     = ALL '+'                                     
055100         MOVE ZERO               TO MSGI-IDDISTR                          
055200                                    MSGI-IDKUNDNR                         
055300                                    MSGI-IDTRPTNR                         
055400     END-IF                                                               
055500     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
055600     MOVE MSGI-SPAR-AREA TO SAVE-AREA                                     
055700     IF WX-IDTRANS NOT = '4624'                                           
055800       PERFORM S03-NOLLA-SAVE                                             
055900     END-IF                                                               
056000     IF GOOD-MID                                                          
056100        IF MID-IDSHIPM-IN  NOT = ALL '+'                                  
056200           IF MID-IDSHIPM-IN NUMERIC                                      
056300              IF MID-IDSHIPM-IN > ZERO                                    
056400                MOVE MID-IDSHIPM-IN  TO W-IDSHIPM                         
056500                                        W-IDSHIPM-X                       
056600                PERFORM S03-NOLLA-SAVE-SHIPM                              
056700              ELSE                                                        
056800                MOVE ZERO            TO W-IDSHIPM                         
056900              END-IF                                                      
057000           ELSE                                                           
057100             MOVE ZERO               TO W-IDSHIPM                         
057200           END-IF                                                         
057300        ELSE                                                              
057400         IF MID-IDTRPTNR-IN NOT = ALL '+'                                 
057500             MOVE ZERO   TO SAVE-IDSHIPM                                  
057600         ELSE                                                             
057700           IF SAVE-IDSHIPM NUMERIC                                        
057800              IF SAVE-IDSHIPM > ZERO                                      
057900                 MOVE SAVE-IDSHIPM  TO W-IDSHIPM                          
058000                                       W-IDSHIPM-X                        
058100              ELSE                                                        
058200                 MOVE ZERO   TO W-IDSHIPM                                 
058300              END-IF                                                      
058400           ELSE                                                           
058500              MOVE ZERO      TO W-IDSHIPM                                 
058600           END-IF                                                         
058700         END-IF                                                           
058800        END-IF                                                            
058900      ELSE                                                                
059000         MOVE NOO       TO KEYS-SW                                        
059100     END-IF                                                               
059200                                                                          
059300     IF SAVE-IDSHIPM NUMERIC                                              
059400       IF W-IDSHIPM    NOT  =  SAVE-IDSHIPM                               
059500         MOVE W-IDSHIPM  TO SAVE-IDSHIPM                                  
059600*        PERFORM S03-NOLLA-SAVE-TRP                                       
059700*        MOVE ZERO       TO SAVE-IDPRODNR-ENTER                           
059800*                           SAVE-IDKOLLI-ENTER                            
059900         MOVE '002'      TO MSGI-KDCALL                                   
060000         MOVE '4624'     TO SAVE-IDTRANS                                  
060100         MOVE SAVE-AREA  TO MSGI-SPAR-AREA                                
060200         CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                       
060300       END-IF                                                             
060400     ELSE                                                                 
060500       MOVE ZERO TO SAVE-IDSHIPM                                          
060600         PERFORM S03-NOLLA-SAVE-TRP                                       
060700         MOVE ZERO       TO SAVE-IDPRODNR-ENTER                           
060800                            SAVE-IDKOLLI-ENTER                            
060900         MOVE '002'      TO MSGI-KDCALL                                   
061000         MOVE '4624'     TO SAVE-IDTRANS                                  
061100         MOVE SAVE-AREA  TO MSGI-SPAR-AREA                                
061200         CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                       
061300     END-IF                                                               
061400                                                                          
061500                                                                          
061600*    - LANGUAGE TO BE USED BY MEDKONV                                     
061700     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
061800                                                                          
061900     MOVE YES TO KEYS-SW                                                  
062000                                                                          
062100     MOVE MFS-ERASE-FIELD TO MOD-IDDISTR-IN                               
062200                             MOD-IDKUNDNR-IN                              
062300                             MOD-IDTRPTNR-IN                              
062400                             MOD-IDSHIPM-IN                               
062500                             MOD-IDDC-IN                                  
062600                                                                          
062700*    -- CHECK OF KEYS                                                     
062800     IF MID-IDDISTR-IN NOT = ALL '+'                                      
062900       MOVE '7'         TO MFS-IDPFK                                      
063000       MOVE SPACE       TO MFS-KDTRTYP                                    
063100       IF MID-IDDISTR-IN NUMERIC AND MID-IDDISTR-IN >= ZERO               
063200          MOVE MSGI-IDDISTR  TO W-WDE111-IDDISTR                          
063300                                W-WDE111-IDDISTR-MIN                      
063400                                W-IDDISTR                                 
063500       ELSE                                                               
063600         MOVE ZERO      TO MSGI-IDDISTR                                   
063700         MOVE NOO       TO KEYS-SW                                        
063800       END-IF                                                             
063900     ELSE                                                                 
064000       IF MSGI-IDDISTR NUMERIC                                            
064100          IF MSGI-IDDISTR >= ZERO                                         
064200             MOVE MSGI-IDDISTR TO W-WDE111-IDDISTR                        
064300                                  W-WDE111-IDDISTR-MIN                    
064400                                  W-IDDISTR                               
064500          ELSE                                                            
064600             MOVE ZERO        TO MSGI-IDDISTR                             
064700             MOVE NOO         TO KEYS-SW                                  
064800          END-IF                                                          
064900       ELSE                                                               
065000          MOVE ZERO           TO MSGI-IDDISTR                             
065100          MOVE NOO            TO KEYS-SW                                  
065200       END-IF                                                             
065300                                                                          
065400     END-IF                                                               
065500                                                                          
065600     MOVE MSGI-IDDISTR TO TEST-IDDISTR                                    
065700     IF MSGI-IDDISTR = ZERO                                               
065800        MOVE YES              TO KEYS-SW                                  
065900     END-IF                                                               
066000                                                                          
066100     IF MID-IDKUNDNR-IN NOT = ALL '+'                                     
066200       MOVE '7'         TO MFS-IDPFK                                      
066300       MOVE SPACE       TO MFS-KDTRTYP                                    
066400       IF MID-IDKUNDNR-IN NUMERIC AND MID-IDKUNDNR-IN >= ZERO             
066500         MOVE MSGI-IDKUNDNR    TO W-WDE111-IDKUNDNR                       
066600                                  W-WDE111-IDKUNDNR-MIN                   
066700                                  W-IDKUNDNR                              
066800       ELSE                                                               
066900         MOVE NOO       TO KEYS-SW                                        
067000       END-IF                                                             
067100     ELSE                                                                 
067200       IF MSGI-IDKUNDNR NUMERIC                                           
067300         IF MSGI-IDKUNDNR >= ZERO                                         
067400          MOVE MSGI-IDKUNDNR    TO W-WDE111-IDKUNDNR                      
067500                                   W-WDE111-IDKUNDNR-MIN                  
067600                                   W-IDKUNDNR                             
067700         ELSE                                                             
067800            MOVE NOO              TO KEYS-SW                              
067900         END-IF                                                           
068000       ELSE                                                               
068100          MOVE NOO              TO KEYS-SW                                
068200       END-IF                                                             
068300     END-IF                                                               
068400                                                                          
068500     IF MID-IDTRPTNR-IN NOT = ALL '+'                                     
068600       MOVE '7'         TO MFS-IDPFK                                      
068700       MOVE SPACE       TO MFS-KDTRTYP                                    
068800       IF MID-IDTRPTNR-IN NOT NUMERIC                                     
068900         MOVE NOO       TO KEYS-SW                                        
069000       END-IF                                                             
069100     END-IF                                                               
069200                                                                          
069300     IF MID-IDSHIPM-IN NOT = ALL '+'                                      
069400       MOVE '7'         TO MFS-IDPFK                                      
069500       MOVE SPACE       TO MFS-KDTRTYP                                    
069600     END-IF                                                               
069700                                                                          
069800     IF MSGI-IDTRPTNR NUMERIC AND MSGI-IDTRPTNR > ZERO                    
069900        MOVE MSGI-IDTRPTNR    TO W-WDE611-IDTRPTNR-MIN                    
070000                                 W-WDE611-IDTRPTNR-MAX                    
070100        MOVE YES              TO W-IDTRPTNR-GIVEN                         
070200     END-IF                                                               
070300                                                                          
070400     IF SAVE-IDSHIPM NUMERIC AND SAVE-IDSHIPM > ZERO                      
070500        MOVE SAVE-IDSHIPM     TO W-WDE101-IDSHIPM                         
070600                                 W-WDE101-GHU-IDSHIPM                     
070700        MOVE YES              TO W-IDSHIPM-GIVEN                          
070800     END-IF                                                               
070900                                                                          
071000     IF MSGI-IDSHIPM NUMERIC AND MSGI-IDSHIPM > ZERO                      
071100        MOVE MSGI-IDSHIPM     TO W-WDE101-IDSHIPM                         
071200                                 W-WDE101-GHU-IDSHIPM                     
071300        MOVE YES              TO W-IDSHIPM-GIVEN                          
071400     ELSE                                                                 
071500        MOVE ZERO             TO MSGI-IDSHIPM                             
071600        PERFORM MFS-ERASE-FIELD-OUT                                       
071700     END-IF                                                               
071800                                                                          
071900     IF IDSHIPM-GIVEN AND IDTRPTNR-GIVEN                                  
072000        MOVE NOO              TO KEYS-SW                                  
072100        MOVE W-NOT-ALLOWED-TEXT                                           
072200                              TO MOD-TEMFSINF                             
072300     ELSE                                                                 
072400        IF IDSHIPM-NOT-GIVEN AND IDTRPTNR-NOT-GIVEN                       
072500           MOVE NOO              TO KEYS-SW                               
072600        END-IF                                                            
072700     END-IF                                                               
072800                                                                          
072900     IF DIST35-NONVCC-NONVCC-REFILL  OR                                   
072910        DIST35-NONVCC-NONVCC-TRANSFER                                     
073000       MOVE WC-CDC-SE          TO W-IDDC-B6                               
073100       PERFORM IMS-GU-WDB601                                              
073200       MOVE DCS-IDPARTNR       TO W-DCS-IDPARTNR-EXP                      
073300     END-IF                                                               
073400                                                                          
073500     IF  MSG-SIGNON-USERID(1:5) = 'PC361'                                 
073600        MOVE MSGI-IDDC-KEY       TO W-IDDC-B6                             
073700        PERFORM IMS-GU-WDB601                                             
073800        IF DCS-KDDC = SPACE                                               
073900           MOVE MSGI-IDDC        TO W-WDE611-IDDC-MIN                     
074000                                    W-WDE611-IDDC-MAX                     
074100                                    W-IDDC                                
074200        ELSE                                                              
074300           MOVE MSGI-IDDC-KEY    TO W-WDE611-IDDC-MIN                     
074400                                    W-WDE611-IDDC-MAX                     
074500                                    W-IDDC                                
074600        END-IF                                                            
074700     ELSE                                                                 
074800       MOVE MSGI-IDDC           TO W-IDDC-B6                              
074900       PERFORM IMS-GU-WDB601                                              
075000       IF DCS-CDC                                                         
075100          MOVE MSGI-IDDC-KEY TO W-IDDC-B6                                 
075200          PERFORM IMS-GU-WDB601                                           
075300          IF DCS-CDC OR (DCS-DDC AND DCS-IDLANDX2 = 'SE')                 
075400             MOVE MSGI-IDDC-KEY    TO W-WDE611-IDDC-MIN                   
075500                                      W-WDE611-IDDC-MAX                   
075600                                      W-IDDC                              
075700          ELSE                                                            
075800             MOVE MSGI-IDDC        TO W-WDE611-IDDC-MIN                   
075900                                      W-WDE611-IDDC-MAX                   
076000                                      MSGI-IDDC-KEY                       
076100                                      W-IDDC                              
076200          END-IF                                                          
076300       ELSE                                                               
076400          MOVE MSGI-IDDC           TO W-WDE611-IDDC-MIN                   
076500                                      W-WDE611-IDDC-MAX                   
076600                                      MSGI-IDDC-KEY                       
076700                                      W-IDDC                              
076800       END-IF                                                             
076900     END-IF                                                               
077000                                                                          
077100*    MOVE MSGI-IDDC-KEY       TO WS-IDDC                                  
077200*    IF GOOD-DC                                                           
077300*       MOVE MSGI-IDDC-KEY    TO W-IDDC                                   
077400*                                W-WDE611-IDDC-MIN                        
077500*                                W-WDE611-IDDC-MAX                        
077600*    ELSE                                                                 
077700*       MOVE MSGI-IDDC        TO WS-IDDC                                  
077800*       IF GOOD-DC                                                        
077900*          MOVE MSGI-IDDC     TO W-IDDC                                   
078000*                                W-WDE611-IDDC-MIN                        
078100*                                W-WDE611-IDDC-MAX                        
078200*                                MSGI-IDDC-KEY                            
078300*       ELSE                                                              
078400*          MOVE NOO           TO KEYS-SW                                  
078500*       END-IF                                                            
078600*    END-IF                                                               
078700     IF SAVE-TRP-IDPRODNR-ENTER NOT NUMERIC                               
078800       MOVE ZERO   TO SAVE-TRP-IDPRODNR-ENTER                             
078900     END-IF                                                               
079000     IF SAVE-TRP-IDKOLLI-ENTER NOT NUMERIC                                
079100       MOVE ZERO   TO SAVE-TRP-IDKOLLI-ENTER                              
079200     END-IF                                                               
079300                                                                          
079400     IF GOOD-MID OR KEYS-OK                                               
079500        MOVE MSGI-IDDISTR      TO MOD-IDDISTR-UT                          
079600        INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE            
079700        MOVE MSGI-IDKUNDNR     TO MOD-IDKUNDNR-UT                         
079800        INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE           
079900        INSPECT MOD-IDKUNDNR-UT REPLACING LEADING '+' BY SPACE            
080000        MOVE MSGI-IDTRPTNR     TO MOD-IDTRPTNR-UT                         
080100        INSPECT MOD-IDTRPTNR-UT REPLACING LEADING ZERO BY SPACE           
080200*       MOVE SAVE-IDSHIPM      TO MOD-IDSHIPM-UT                          
080300        MOVE MSGI-IDSHIPM      TO MOD-IDSHIPM-UT                          
080400        MOVE MSGI-IDDC-KEY     TO MOD-IDDC-UT                             
080500        INSPECT MOD-IDDC-UT    REPLACING LEADING '+' BY SPACE             
080600     ELSE                                                                 
080700        MOVE MFS-ERASE-FIELD   TO MOD-IDDISTR-UT                          
080800                                  MOD-IDKUNDNR-UT                         
080900                                  MOD-IDTRPTNR-UT                         
081000                                  MOD-IDSHIPM-UT                          
081100                                  MOD-IDDC-UT                             
081200     END-IF                                                               
081300                                                                          
081400     IF KEYS-WRONG                                                        
081500        MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                
081600        CALL WMEDKONV   USING MED-WMEDAREA                                
081700        MOVE MED-MFSFEL    TO MOD-TEMFSFEL                                
081800        PERFORM MFS-ERASE-FIELD-IN                                        
081900        PERFORM MFS-ERASE-LINE-FIELD-OUT                                  
082000     END-IF                                                               
082100     .                                                                    
082200     EJECT                                                                
082300 C-FIRST-PAGE SECTION.                                                    
082400                                                                          
082500     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
082600     CALL WMEDKONV USING MED-WMEDAREA                                     
082700     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
082800                                                                          
082900     PERFORM MFS-ERASE-FIELD-IN                                           
083000     .                                                                    
083100     EJECT                                                                
083200 D-NEXT-PAGE SECTION.                                                     
083300                                                                          
083400     IF SAVE-IDTRANS = '4624'                                             
083500       IF IDSHIPM-GIVEN                                                   
083600          MOVE SAVE-IDSHIPM        TO W-WDE101-GHU-IDSHIPM                
083700          MOVE SAVE-IDDISTR-NEXT   TO W-WDE111-IDDISTR-MIN                
083800          MOVE SAVE-IDKUNDNR-NEXT  TO W-WDE111-IDKUNDNR-MIN               
083900          MOVE SAVE-IDPRODNR-NEXT  TO W-WDE121-IDPRODNR-MIN               
084000          MOVE SAVE-IDKOLLI-NEXT   TO W-WDE121-IDKOLLI-MIN                
084100       END-IF                                                             
084200       IF IDTRPTNR-GIVEN                                                  
084300          MOVE SAVE-IDTRPTNR-NEXT  TO W-WDE611-IDTRPTNR-MIN               
084400          MOVE SAVE-DARFS-NEXT     TO W-WDE611-DARFS-MIN                  
084500          MOVE SAVE-ADCLGEO-NEXT   TO W-WDE611-ADCLGEO-MIN                
084600          MOVE SAVE-ADFLOMR-NEXT   TO W-WDE611-ADFLOMR-MIN                
084700          MOVE SAVE-ADRUTNIV-NEXT  TO W-WDE611-ADRUTNIV-MIN               
084800          MOVE SAVE-ADVMODUL-NEXT  TO W-WDE611-ADVMODUL-MIN               
084900          MOVE SAVE-TRP-IDPRODNR-NEXT  TO W-IDPRODNR                      
085000          MOVE SAVE-TRP-IDKOLLI-NEXT   TO W-IDKOLLI                       
085100       END-IF                                                             
085200     ELSE                                                                 
085300       PERFORM MFS-ERASE-FIELD-IN                                         
085400     END-IF                                                               
085500     .                                                                    
085600     EJECT                                                                
085700 E-SAME-PAGE SECTION.                                                     
085800                                                                          
085900     IF SAVE-IDTRANS = '4624' OR '0551'                                   
086000       IF IDSHIPM-GIVEN                                                   
086100          MOVE SAVE-IDDISTR-ENTER  TO W-WDE111-IDDISTR-MIN                
086200          MOVE SAVE-IDKUNDNR-ENTER TO W-WDE111-IDKUNDNR-MIN               
086300          MOVE SAVE-IDPRODNR-ENTER TO W-WDE121-IDPRODNR-MIN               
086400          MOVE SAVE-IDKOLLI-ENTER  TO W-WDE121-IDKOLLI-MIN                
086500       END-IF                                                             
086600       IF IDTRPTNR-GIVEN                                                  
086700          MOVE SAVE-IDTRPTNR-ENTER TO W-WDE611-IDTRPTNR-MIN               
086800          MOVE SAVE-DARFS-ENTER    TO W-WDE611-DARFS-MIN                  
086900          MOVE SAVE-ADCLGEO-ENTER  TO W-WDE611-ADCLGEO-MIN                
087000          MOVE SAVE-ADFLOMR-ENTER  TO W-WDE611-ADFLOMR-MIN                
087100          MOVE SAVE-ADRUTNIV-ENTER TO W-WDE611-ADRUTNIV-MIN               
087200          MOVE SAVE-ADVMODUL-ENTER TO W-WDE611-ADVMODUL-MIN               
087300          MOVE SAVE-TRP-IDPRODNR-ENTER TO W-IDPRODNR                      
087400          MOVE SAVE-TRP-IDKOLLI-ENTER  TO W-IDKOLLI                       
087500       END-IF                                                             
087600     ELSE                                                                 
087700       PERFORM MFS-ERASE-FIELD-IN                                         
087800     END-IF                                                               
087900     .                                                                    
088000     EJECT                                                                
088100 F-READ-SHOW-INFO SECTION.                                                
088200                                                                          
088300     IF IDSHIPM-GIVEN                                                     
088400       IF MSGI-IDDISTR = ZERO AND MSGI-IDKUNDNR = ZERO                    
088500         PERFORM FC-READ-SUM-PER-SHIPMENT                                 
088600       ELSE                                                               
088700         PERFORM FA-READ-AFTER-SHIPMENT                                   
088800       END-IF                                                             
088900     END-IF                                                               
089000     IF IDTRPTNR-GIVEN                                                    
089100        PERFORM FB-READ-BEFORE-SHIPMENT                                   
089200     END-IF                                                               
089300                                                                          
089400     .                                                                    
089500     EJECT                                                                
089600                                                                          
089700 FA-READ-AFTER-SHIPMENT SECTION.                                          
089800                                                                          
089900     PERFORM IMS-GU-WDE111                                                
090000     IF SEGMENT-FOUND                                                     
090100        PERFORM IMS-GNP-WDE121                                            
090200     END-IF                                                               
090300                                                                          
090400     PERFORM S03-NOLLA-SAVE-TRP                                           
090500     IF SEGMENT-MISSING                                                   
090600        MOVE KEYS-ARE-MISSING       TO MED-IDMFSFEL                       
090700        CALL WMEDKONV            USING MED-WMEDAREA                       
090800        MOVE MED-MFSFEL             TO MOD-TEMFSFEL                       
090900        PERFORM MFS-ERASE-LINE-FIELD-OUT                                  
091000        MOVE W-WDE121-IDPRODNR-MIN  TO SAVE-IDPRODNR-ENTER                
091100        MOVE W-WDE121-IDKOLLI-MIN   TO SAVE-IDKOLLI-ENTER                 
091200        MOVE '002'       TO MSGI-KDCALL                                   
091300        MOVE '4624'      TO SAVE-IDTRANS                                  
091400        MOVE SAVE-AREA   TO MSGI-SPAR-AREA                                
091500        CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                        
091600     ELSE                                                                 
091700*      -- POSITION FOR READING DATA TO UPPERMOST LINE                     
091800*      -- (NOT NECESSARY IF -MIN KEYS ARE DIRECTLY USED IN SSA)           
091900                                                                          
092000       MOVE +1                      TO INDX                               
092100       IF SEGMENT-FOUND                                                   
092200         MOVE SKOLLI-IDPRODNR       TO SAVE-IDPRODNR-ENTER                
092300         MOVE SKOLLI-IDKOLLI        TO SAVE-IDKOLLI-ENTER                 
092400       ELSE                                                               
092500         MOVE W-WDE121-IDPRODNR-MIN TO SAVE-IDPRODNR-ENTER                
092600         MOVE W-WDE121-IDKOLLI-MIN  TO SAVE-IDKOLLI-ENTER                 
092700       END-IF                                                             
092800                                                                          
092900       PERFORM UNTIL INDX > MAX-INDX                                      
093000         IF SEGMENT-FOUND                                                 
093100           MOVE SGMT-IDDISTR        TO TEST-IDDISTR                       
093200           IF  DIST79-DEALER-PRICE OR                                     
093220               DIST79-ECOM-PRICE                                          
093300              IF MFS-FIRST                                                
093400                 IF  INDX = 1                                             
093500                    PERFORM S01-FIND-PRKURS                               
093600                 END-IF                                                   
093700              END-IF                                                      
093800              MOVE SKOLLI-SUORDV-LOC     TO W-SUORDV-LOC                  
093900              MOVE SKOLLI-SUORDV-LOCPREL TO W-SUORDV-LOCPREL              
094000              PERFORM S02-CONVERT-TO-SEK                                  
094100           ELSE                                                           
094200              MOVE SKOLLI-SUORDV-LOC   TO W-SUORDV-LOC-SEK                
094300              MOVE SKOLLI-SUORDV-LOCPREL                                  
094400                                       TO W-SUORDV-LOCPREL-SEK            
094500           END-IF                                                         
094600           MOVE SGMT-IDKUNDNR     TO MOD-IDKUNDNR (INDX)                  
094700           MOVE SKOLLI-IDORDNR7   TO MOD-IDORDNR5 (INDX)                  
094800           MOVE SKOLLI-IDKOLLI    TO MOD-IDKOLLI (INDX)                   
094900                                                                          
095000           MOVE SKOLLI-IDPRODNR   TO W-WDE601-IDPRODNR                    
095100           MOVE SKOLLI-IDKOLLI    TO W-WDE611-IDKOLLI                     
095200           PERFORM IMS-GU-WDE611                                          
095300           IF SEGMENT-FOUND                                               
095400              MOVE KOLLI-IDTULL   TO MOD-IDTULL (INDX)                    
095500              IF  KOLLI-IDPSN (1)  > ZERO                                 
095600                  MOVE   'Y'      TO MOD-FLFARLIG (INDX)                  
095700            END-IF                                                        
095800           ELSE                                                           
095900              MOVE SPACE          TO MOD-IDTULL (INDX)                    
096000                                     MOD-FLFARLIG (INDX)                  
096100           END-IF                                                         
096200                                                                          
096300           MOVE SKOLLI-DIKOLLIL   TO MOD-DIKOLLIL (INDX)                  
096400           MOVE SKOLLI-DIKOLLIB   TO MOD-DIKOLLIB (INDX)                  
096500           MOVE SKOLLI-DIKOLLIH   TO MOD-DIKOLLIH (INDX)                  
096600           MOVE SKOLLI-VKORDBTO-KOLLI                                     
096700                                  TO MOD-VKORDBTO-KOLLI (INDX)            
096800           MOVE SKOLLI-VLORDBTO-KOLLI                                     
096900                                  TO MOD-VLORDBTO-KOLLI (INDX)            
097000           MOVE SKOLLI-VKORDNTO-KOLLI                                     
097100                                  TO MOD-VKORDNTO-KOLLI (INDX)            
097200           COMPUTE W-SUORDV-TOT-SEK = SKOLLI-SUORDV    +                  
097300                                      W-SUORDV-LOC-SEK +                  
097400                                      W-SUORDV-LOCPREL-SEK                
097500           MOVE W-SUORDV-TOT-SEK  TO MOD-SUORDV-KOLLI (INDX)              
097600                                                                          
097700           IF W-SUORDV-LOCPREL-SEK   > ZERO                               
097800             MOVE '*'             TO MOD-TEASTRIX-KLI (INDX)              
097900           ELSE                                                           
098000             MOVE ' '             TO MOD-TEASTRIX-KLI (INDX)              
098100           END-IF                                                         
098200                                                                          
098300           IF  MFS-FIRST                                                  
098400               COMPUTE W-KVKOLLI-SKP   = W-KVKOLLI-SKP  +  1              
098500               COMPUTE W-VKORDBTO-SKP  = W-VKORDBTO-SKP +                 
098600                                         SKOLLI-VKORDBTO-KOLLI            
098700               COMPUTE W-VLORDBTO-SKP  = W-VLORDBTO-SKP +                 
098800                                         SKOLLI-VLORDBTO-KOLLI            
098900               COMPUTE W-VKORDNTO-SKP  = W-VKORDNTO-SKP +                 
099000                                         SKOLLI-VKORDNTO-KOLLI            
099100               COMPUTE W-SUORDV-SKEPPN = W-SUORDV-SKEPPN +                
099200                                         W-SUORDV-TOT-SEK                 
099300               COMPUTE W-SUORDV-LOCPREL-SKP                               
099400                                       = W-SUORDV-LOCPREL-SKP +           
099500                                         W-SUORDV-LOCPREL-SEK             
099600           END-IF                                                         
099700           PERFORM IMS-GNP-WDE121                                         
099800         ELSE                                                             
099900           MOVE MFS-ERASE-FIELD   TO  MOD-IDKUNDNR (INDX)                 
100000                                      MOD-IDORDNR5 (INDX)                 
100100                                      MOD-IDKOLLI  (INDX)                 
100200                                      MOD-IDTULL   (INDX)                 
100300                                      MOD-DIKOLLIL (INDX)                 
100400                                      MOD-DIKOLLIB (INDX)                 
100500                                      MOD-DIKOLLIH (INDX)                 
100600                                      MOD-VKORDBTO-KOLLI (INDX)           
100700                                      MOD-VLORDBTO-KOLLI (INDX)           
100800                                      MOD-SUORDV-KOLLI (INDX)             
100900                                      MOD-TEASTRIX-KLI (INDX)             
101000                                      MOD-VKORDNTO-KOLLI (INDX)           
101100                                      MOD-FLFARLIG (INDX)                 
101200         END-IF                                                           
101300         ADD 1 TO INDX                                                    
101400       END-PERFORM                                                        
101500                                                                          
101600       IF SEGMENT-FOUND                                                   
101700         MOVE SKOLLI-IDPRODNR       TO SAVE-IDPRODNR-NEXT                 
101800         MOVE SKOLLI-IDKOLLI        TO SAVE-IDKOLLI-NEXT                  
101900                                                                          
102000         MOVE INF-MORE-INFO-EXISTS  TO MED-IDMFSINF                       
102100         CALL WMEDKONV USING MED-WMEDAREA                                 
102200         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
102300       ELSE                                                               
102400         MOVE SAVE-IDPRODNR-ENTER   TO SAVE-IDPRODNR-NEXT                 
102500         MOVE SAVE-IDKOLLI-ENTER    TO SAVE-IDKOLLI-NEXT                  
102600         MOVE INF-LAST-PAGE         TO MED-IDMFSINF                       
102700         CALL WMEDKONV USING MED-WMEDAREA                                 
102800         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
102900       END-IF                                                             
103000       IF MFS-FIRST                                                       
103100         PERFORM UNTIL SEGMENT-MISSING                                    
103200                                                                          
103300           IF  DIST79-DEALER-PRICE OR                                     
103320               DIST79-ECOM-PRICE                                          
103400               MOVE SKOLLI-SUORDV-LOC     TO W-SUORDV-LOC                 
103500               MOVE SKOLLI-SUORDV-LOCPREL TO W-SUORDV-LOCPREL             
103600               PERFORM S02-CONVERT-TO-SEK                                 
103700           ELSE                                                           
103800              MOVE SKOLLI-SUORDV-LOC   TO W-SUORDV-LOC-SEK                
103900              MOVE SKOLLI-SUORDV-LOCPREL                                  
104000                                       TO W-SUORDV-LOCPREL-SEK            
104100           END-IF                                                         
104200           COMPUTE W-SUORDV-TOT-SEK  = W-SUORDV-LOC-SEK     +             
104300                                       W-SUORDV-LOCPREL-SEK +             
104400                                       SKOLLI-SUORDV                      
104500                                                                          
104600           COMPUTE W-KVKOLLI-SKP   = W-KVKOLLI-SKP  +  1                  
104700           COMPUTE W-VKORDBTO-SKP  = W-VKORDBTO-SKP +                     
104800                                     SKOLLI-VKORDBTO-KOLLI                
104900           COMPUTE W-VLORDBTO-SKP  = W-VLORDBTO-SKP +                     
105000                                     SKOLLI-VLORDBTO-KOLLI                
105100           COMPUTE W-VKORDNTO-SKP  = W-VKORDNTO-SKP +                     
105200                                     SKOLLI-VKORDNTO-KOLLI                
105300           COMPUTE W-SUORDV-SKEPPN = W-SUORDV-SKEPPN +                    
105400                                     W-SUORDV-TOT-SEK                     
105500           COMPUTE W-SUORDV-LOCPREL-SKP                                   
105600                                   = W-SUORDV-LOCPREL-SKP +               
105700                                     W-SUORDV-LOCPREL-SEK                 
105800           PERFORM IMS-GNP-WDE121                                         
105900         END-PERFORM                                                      
106000       ELSE                                                               
106100         MOVE SAVE-KVKOLLI-SKP   TO W-KVKOLLI-SKP                         
106200         MOVE SAVE-VKORDBTO-SKP  TO W-VKORDBTO-SKP                        
106300         MOVE SAVE-VLORDBTO-SKP  TO W-VLORDBTO-SKP                        
106400         MOVE SAVE-VKORDNTO-SKP  TO W-VKORDNTO-SKP                        
106500         MOVE SAVE-SUORDV-SKEPPN TO W-SUORDV-SKEPPN                       
106600         MOVE SAVE-SUORDV-LOCPREL-SKP                                     
106700                                 TO W-SUORDV-LOCPREL-SKP                  
106800       END-IF                                                             
106900                                                                          
107000       IF W-KVKOLLI-SKP           > ZERO                                  
107100          MOVE W-KVKOLLI-SKP     TO MOD-KVKOLLI-SKP                       
107200                                    SAVE-KVKOLLI-SKP                      
107300       ELSE                                                               
107400          MOVE MFS-ERASE-FIELD   TO MOD-KVKOLLI-SKP                       
107500          MOVE ZERO              TO SAVE-KVKOLLI-SKP                      
107600       END-IF                                                             
107700       IF W-VKORDBTO-SKP          > ZERO                                  
107800          MOVE W-VKORDBTO-SKP    TO MOD-VKORDBTO-SKP                      
107900                                    SAVE-VKORDBTO-SKP                     
108000       ELSE                                                               
108100          MOVE MFS-ERASE-FIELD   TO MOD-VKORDBTO-SKP                      
108200          MOVE ZERO              TO SAVE-VKORDBTO-SKP                     
108300       END-IF                                                             
108400       IF W-VLORDBTO-SKP         > ZERO                                   
108500          MOVE W-VLORDBTO-SKP   TO MOD-VLORDBTO-SKP                       
108600                                   SAVE-VLORDBTO-SKP                      
108700       ELSE                                                               
108800          MOVE MFS-ERASE-FIELD  TO MOD-VLORDBTO-SKP                       
108900          MOVE ZERO             TO SAVE-VLORDBTO-SKP                      
109000       END-IF                                                             
109100                                                                          
109200       IF W-VKORDNTO-SKP         > ZERO                                   
109300          MOVE W-VKORDNTO-SKP   TO MOD-VKORDNTO-SKP                       
109400                                   SAVE-VKORDNTO-SKP                      
109500       ELSE                                                               
109600          MOVE MFS-ERASE-FIELD  TO MOD-VKORDNTO-SKP                       
109700          MOVE ZERO             TO SAVE-VKORDNTO-SKP                      
109800       END-IF                                                             
109900                                                                          
110000       IF W-SUORDV-SKEPPN     NUMERIC                                     
110100          MOVE W-SUORDV-SKEPPN  TO MOD-SUORDV-SKEPPN                      
110200                                   SAVE-SUORDV-SKEPPN                     
110300       ELSE                                                               
110400          MOVE MFS-ERASE-FIELD  TO MOD-SUORDV-SKEPPN                      
110500          MOVE ZERO             TO SAVE-SUORDV-SKEPPN                     
110600       END-IF                                                             
110700                                                                          
110800       IF W-SUORDV-LOCPREL-SKP  > ZERO                                    
110900         MOVE '*'               TO MOD-TEASTRIX-TRP                       
111000         MOVE W-SUORDV-LOCPREL-SKP                                        
111100                                TO SAVE-SUORDV-LOCPREL-SKP                
111200       ELSE                                                               
111300         MOVE SPACE             TO MOD-TEASTRIX-TRP                       
111400         MOVE ZERO              TO SAVE-SUORDV-LOCPREL-SKP                
111500       END-IF                                                             
111600                                                                          
111700       MOVE '002'       TO MSGI-KDCALL                                    
111800       MOVE '4624'      TO SAVE-IDTRANS                                   
111900       MOVE SAVE-AREA   TO MSGI-SPAR-AREA                                 
112000       CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                         
112100     END-IF                                                               
112200     .                                                                    
112300     EJECT                                                                
112400                                                                          
112500 FB-READ-BEFORE-SHIPMENT SECTION.                                         
112600                                                                          
112700     IF MFS-FIRST                                                         
112800       PERFORM IMS-GU-WDE611-CSEQ                                         
112900     ELSE                                                                 
113000       PERFORM IMS-GU-WDE611-CSEQ-UNIK                                    
113100     END-IF                                                               
113200                                                                          
113300     IF SEGMENT-MISSING                                                   
113400        MOVE KEYS-ARE-MISSING       TO MED-IDMFSFEL                       
113500        CALL WMEDKONV            USING MED-WMEDAREA                       
113600        MOVE MED-MFSFEL             TO MOD-TEMFSFEL                       
113700        PERFORM MFS-ERASE-LINE-FIELD-OUT                                  
113800        MOVE W-WDE611-IDTRPTNR-MIN  TO SAVE-IDTRPTNR-ENTER                
113900        MOVE W-WDE611-DARFS-MIN     TO SAVE-DARFS-ENTER                   
114000        MOVE W-WDE611-ADCLGEO-MIN   TO SAVE-ADCLGEO-ENTER                 
114100        MOVE W-WDE611-ADFLOMR-MIN   TO SAVE-ADFLOMR-ENTER                 
114200        MOVE W-WDE611-ADRUTNIV-MIN  TO SAVE-ADRUTNIV-ENTER                
114300        MOVE W-WDE611-ADVMODUL-MIN  TO SAVE-ADVMODUL-ENTER                
114400        MOVE ZERO                   TO SAVE-IDPRODNR-ENTER                
114500                                       SAVE-IDKOLLI-ENTER                 
114600                                       SAVE-KVKOLLI-SKP                   
114700                                       SAVE-VKORDBTO-SKP                  
114800                                       SAVE-VLORDBTO-SKP                  
114900                                       SAVE-VKORDNTO-SKP                  
115000                                       SAVE-SUORDV-SKEPPN                 
115100                                       SAVE-SUORDV-LOCPREL-SKP            
115200        MOVE '002'                  TO MSGI-KDCALL                        
115300        MOVE '4624'                 TO SAVE-IDTRANS                       
115400        MOVE SAVE-AREA              TO MSGI-SPAR-AREA                     
115500        CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                        
115600     ELSE                                                                 
115700*      -- POSITION FOR READING DATA TO UPPERMOST LINE                     
115800*      -- (NOT NECESSARY IF -MIN KEYS ARE DIRECTLY USED IN SSA)           
115900                                                                          
116000       MOVE +1                      TO INDX                               
116100       IF SEGMENT-FOUND                                                   
116200         MOVE KOLLI-IDTRPTNR        TO SAVE-IDTRPTNR-ENTER                
116300         MOVE KOLLI-DARFS           TO SAVE-DARFS-ENTER                   
116400         MOVE KOLLI-ADCLGEO         TO SAVE-ADCLGEO-ENTER                 
116500         MOVE KOLLI-ADFLOMR         TO SAVE-ADFLOMR-ENTER                 
116600         MOVE KOLLI-ADRUTNIV        TO SAVE-ADRUTNIV-ENTER                
116700         MOVE KOLLI-ADVMODUL        TO SAVE-ADVMODUL-ENTER                
116800         MOVE KOLLI-IDKOLLI         TO SAVE-TRP-IDKOLLI-ENTER             
116900         MOVE ZERO                  TO SAVE-IDPRODNR-ENTER                
117000                                       SAVE-IDKOLLI-ENTER                 
117100       ELSE                                                               
117200         MOVE W-WDE611-IDTRPTNR-MIN TO SAVE-IDTRPTNR-ENTER                
117300         MOVE W-WDE611-DARFS-MIN    TO SAVE-DARFS-ENTER                   
117400         MOVE W-WDE611-ADCLGEO-MIN  TO SAVE-ADCLGEO-ENTER                 
117500         MOVE W-WDE611-ADFLOMR-MIN  TO SAVE-ADFLOMR-ENTER                 
117600         MOVE W-WDE611-ADRUTNIV-MIN TO SAVE-ADRUTNIV-ENTER                
117700         MOVE W-WDE611-ADVMODUL-MIN TO SAVE-ADVMODUL-ENTER                
117800         MOVE ZERO                  TO SAVE-IDPRODNR-ENTER                
117900                                       SAVE-IDKOLLI-ENTER                 
118000       END-IF                                                             
118100                                                                          
118200       PERFORM UNTIL INDX > MAX-INDX                                      
118300         IF SEGMENT-FOUND                                                 
118400           MOVE KOLLI-IDDISTR        TO TEST-IDDISTR                      
118500           IF  DIST79-DEALER-PRICE OR                                     
118520               DIST79-ECOM-PRICE                                          
118600              IF MFS-FIRST                                                
118700                 IF INDX = 1                                              
118800                    MOVE KOLLI-KDVALISO                                   
118900                                        TO CURR-KDVALISO-ROW              
119000                    MOVE KOLLI-IDDISTR  TO W-WDB201-IDDISTR               
119100                                           W-WDB201-IDDISTR-MIN           
119200                                           W-WDB201-IDDISTR-MAX           
119300                    MOVE KOLLI-IDKUNDNR TO W-WDB201-IDKUNDNR              
119400                    PERFORM S06-FIND-PRKURS-FOR-FB                        
119500                 END-IF                                                   
119600              END-IF                                                      
119700              MOVE KOLLI-SUORDV-LOC     TO W-SUORDV-LOC                   
119800              MOVE KOLLI-SUORDV-LOCPREL TO W-SUORDV-LOCPREL               
119900              PERFORM S02-CONVERT-TO-SEK                                  
120000           ELSE                                                           
120100              MOVE KOLLI-SUORDV-LOC   TO W-SUORDV-LOC-SEK                 
120200              MOVE KOLLI-SUORDV-LOCPREL                                   
120300                                       TO W-SUORDV-LOCPREL-SEK            
120400           END-IF                                                         
120500           MOVE KOLLI-IDKUNDNR    TO MOD-IDKUNDNR (INDX)                  
120600           PERFORM IMS-GNP-WDE601-CSEQ                                    
120700           MOVE VORD-IDPRODNR     TO W-E4-IDPRODNR-MIN                    
120800                                     W-E4-IDPRODNR-MAX                    
120900           IF INDX = +1                                                   
121000             MOVE VORD-IDPRODNR   TO SAVE-TRP-IDPRODNR-ENTER              
121100           END-IF                                                         
121200           MOVE KOLLI-IDKOLLI     TO W-E4-IDKOLLI-MIN                     
121300                                     W-E4-IDKOLLI-MAX                     
121400           PERFORM IMS-GU-WDE4F1                                          
121500           IF SEGMENT-FOUND                                               
121600             MOVE SEQF-IDORDNR5   TO MOD-IDORDNR5 (INDX)                  
121700           ELSE                                                           
121800             MOVE MFS-ERASE-FIELD TO MOD-IDORDNR5 (INDX)                  
121900           END-IF                                                         
122000           MOVE KOLLI-IDKOLLI     TO MOD-IDKOLLI (INDX)                   
122100           MOVE KOLLI-IDTULL      TO MOD-IDTULL (INDX)                    
122200           IF  KOLLI-IDPSN (1)     > ZERO                                 
122300               MOVE   'Y'         TO MOD-FLFARLIG (INDX)                  
122400           END-IF                                                         
122500           MOVE KOLLI-DIKOLLIL    TO MOD-DIKOLLIL (INDX)                  
122600           MOVE KOLLI-DIKOLLIB    TO MOD-DIKOLLIB (INDX)                  
122700           MOVE KOLLI-DIKOLLIH    TO MOD-DIKOLLIH (INDX)                  
122800           MOVE KOLLI-VKORDBTO-KOLLI                                      
122900                                  TO MOD-VKORDBTO-KOLLI (INDX)            
123000           MOVE KOLLI-VLORDBTO-KOLLI                                      
123100                                  TO MOD-VLORDBTO-KOLLI (INDX)            
123200           MOVE KOLLI-VKORDNTO-KOLLI                                      
123300                                  TO MOD-VKORDNTO-KOLLI (INDX)            
123400           MOVE  ZERO   TO   W-SUORDV-TOT-SEK                             
123500           COMPUTE W-SUORDV-TOT-SEK = KOLLI-SUORDV-KOLLI +                
123600                                      W-SUORDV-LOC-SEK   +                
123700                                      W-SUORDV-LOCPREL-SEK                
123800           MOVE W-SUORDV-TOT-SEK  TO MOD-SUORDV-KOLLI (INDX)              
123900                                                                          
124000           IF W-SUORDV-LOCPREL-SEK   > ZERO                               
124100             MOVE '*'             TO MOD-TEASTRIX-KLI (INDX)              
124200           ELSE                                                           
124300             MOVE ' '             TO MOD-TEASTRIX-KLI (INDX)              
124400           END-IF                                                         
124500                                                                          
124600           IF  MFS-FIRST                                                  
124700               COMPUTE W-KVKOLLI-SKP   = W-KVKOLLI-SKP  +  1              
124800               COMPUTE W-VKORDBTO-SKP  = W-VKORDBTO-SKP +                 
124900                                         KOLLI-VKORDBTO-KOLLI             
125000               COMPUTE W-VLORDBTO-SKP  = W-VLORDBTO-SKP +                 
125100                                         KOLLI-VLORDBTO-KOLLI             
125200               COMPUTE W-VKORDNTO-SKP  = W-VKORDNTO-SKP +                 
125300                                         KOLLI-VKORDNTO-KOLLI             
125400               COMPUTE W-SUORDV-SKEPPN = W-SUORDV-SKEPPN +                
125500                                         W-SUORDV-TOT-SEK                 
125600               COMPUTE W-SUORDV-LOCPREL-SKP                               
125700                                       = W-SUORDV-LOCPREL-SKP +           
125800                                         W-SUORDV-LOCPREL-SEK             
125900           END-IF                                                         
126000           PERFORM IMS-GN-WDE611-CSEQ                                     
126100         ELSE                                                             
126200           MOVE MFS-ERASE-FIELD   TO  MOD-IDKUNDNR (INDX)                 
126300                                      MOD-IDORDNR5 (INDX)                 
126400                                      MOD-IDKOLLI  (INDX)                 
126500                                      MOD-IDTULL   (INDX)                 
126600                                      MOD-DIKOLLIL (INDX)                 
126700                                      MOD-DIKOLLIB (INDX)                 
126800                                      MOD-DIKOLLIH (INDX)                 
126900                                      MOD-VKORDBTO-KOLLI (INDX)           
127000                                      MOD-VLORDBTO-KOLLI (INDX)           
127100                                      MOD-SUORDV-KOLLI (INDX)             
127200                                      MOD-TEASTRIX-KLI (INDX)             
127300                                      MOD-VKORDNTO-KOLLI (INDX)           
127400                                      MOD-FLFARLIG (INDX)                 
127500         END-IF                                                           
127600         ADD 1 TO INDX                                                    
127700       END-PERFORM                                                        
127800                                                                          
127900       IF SEGMENT-FOUND                                                   
128000                                                                          
128100         IF INDX > MAX-INDX                                               
128200           PERFORM IMS-GNP-WDE601-CSEQ                                    
128300         END-IF                                                           
128400                                                                          
128500         MOVE KOLLI-IDTRPTNR TO SAVE-IDTRPTNR-NEXT                        
128600         MOVE KOLLI-DARFS    TO SAVE-DARFS-NEXT                           
128700         MOVE KOLLI-ADCLGEO  TO SAVE-ADCLGEO-NEXT                         
128800         MOVE KOLLI-ADFLOMR  TO SAVE-ADFLOMR-NEXT                         
128900         MOVE KOLLI-ADRUTNIV TO SAVE-ADRUTNIV-NEXT                        
129000         MOVE KOLLI-ADVMODUL TO SAVE-ADVMODUL-NEXT                        
129100         MOVE KOLLI-IDKOLLI  TO SAVE-TRP-IDKOLLI-NEXT                     
129200         MOVE VORD-IDPRODNR  TO SAVE-TRP-IDPRODNR-NEXT                    
129300                                                                          
129400         MOVE INF-MORE-INFO-EXISTS  TO MED-IDMFSINF                       
129500         CALL WMEDKONV USING MED-WMEDAREA                                 
129600         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
129700       ELSE                                                               
129800         MOVE SAVE-IDTRPTNR-ENTER TO  SAVE-IDTRPTNR-NEXT                  
129900         MOVE SAVE-DARFS-ENTER    TO  SAVE-DARFS-NEXT                     
130000         MOVE SAVE-ADCLGEO-ENTER  TO  SAVE-ADCLGEO-NEXT                   
130100         MOVE SAVE-ADFLOMR-ENTER  TO  SAVE-ADFLOMR-NEXT                   
130200         MOVE SAVE-ADRUTNIV-ENTER TO  SAVE-ADRUTNIV-NEXT                  
130300         MOVE SAVE-ADVMODUL-ENTER TO  SAVE-ADVMODUL-NEXT                  
130400         MOVE SAVE-TRP-IDPRODNR-ENTER TO SAVE-TRP-IDPRODNR-NEXT           
130500         MOVE SAVE-TRP-IDKOLLI-ENTER TO SAVE-TRP-IDKOLLI-NEXT             
130600                                                                          
130700         MOVE INF-LAST-PAGE       TO  MED-IDMFSINF                        
130800         CALL WMEDKONV USING MED-WMEDAREA                                 
130900         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
131000       END-IF                                                             
131100       IF MFS-FIRST                                                       
131200         PERFORM UNTIL SEGMENT-MISSING                                    
131300                                                                          
131400           IF DIST79-DEALER-PRICE OR                                      
131420              DIST79-ECOM-PRICE                                           
131500              MOVE KOLLI-SUORDV-LOC      TO W-SUORDV-LOC                  
131600              MOVE KOLLI-SUORDV-LOCPREL  TO W-SUORDV-LOCPREL              
131700              PERFORM S02-CONVERT-TO-SEK                                  
131800           ELSE                                                           
131900              MOVE KOLLI-SUORDV-LOC      TO W-SUORDV-LOC-SEK              
132000              MOVE KOLLI-SUORDV-LOCPREL  TO W-SUORDV-LOCPREL-SEK          
132100           END-IF                                                         
132200           COMPUTE W-SUORDV-TOT-SEK  = W-SUORDV-LOC-SEK     +             
132300                                       W-SUORDV-LOCPREL-SEK +             
132400                                       KOLLI-SUORDV-KOLLI                 
132500                                                                          
132600           COMPUTE W-KVKOLLI-SKP   = W-KVKOLLI-SKP  +  1                  
132700           COMPUTE W-VKORDBTO-SKP  = W-VKORDBTO-SKP +                     
132800                                     KOLLI-VKORDBTO-KOLLI                 
132900           COMPUTE W-VLORDBTO-SKP  = W-VLORDBTO-SKP +                     
133000                                     KOLLI-VLORDBTO-KOLLI                 
133100           COMPUTE W-VKORDNTO-SKP  = W-VKORDNTO-SKP +                     
133200                                     KOLLI-VKORDNTO-KOLLI                 
133300           COMPUTE W-SUORDV-SKEPPN = W-SUORDV-SKEPPN +                    
133400                                     W-SUORDV-TOT-SEK                     
133500           COMPUTE W-SUORDV-LOCPREL-SKP                                   
133600                                   = W-SUORDV-LOCPREL-SKP +               
133700                                     W-SUORDV-LOCPREL-SEK                 
133800           PERFORM IMS-GN-WDE611-CSEQ                                     
133900         END-PERFORM                                                      
134000       ELSE                                                               
134100         MOVE SAVE-KVKOLLI-SKP   TO W-KVKOLLI-SKP                         
134200         MOVE SAVE-VKORDBTO-SKP  TO W-VKORDBTO-SKP                        
134300         MOVE SAVE-VLORDBTO-SKP  TO W-VLORDBTO-SKP                        
134400         MOVE SAVE-VKORDNTO-SKP  TO W-VKORDNTO-SKP                        
134500         MOVE SAVE-SUORDV-SKEPPN TO W-SUORDV-SKEPPN                       
134600         MOVE SAVE-SUORDV-LOCPREL-SKP                                     
134700                                 TO W-SUORDV-LOCPREL-SKP                  
134800       END-IF                                                             
134900                                                                          
135000       IF W-KVKOLLI-SKP           > ZERO                                  
135100          MOVE W-KVKOLLI-SKP     TO MOD-KVKOLLI-SKP                       
135200                                    SAVE-KVKOLLI-SKP                      
135300       ELSE                                                               
135400          MOVE MFS-ERASE-FIELD   TO MOD-KVKOLLI-SKP                       
135500          MOVE ZERO              TO SAVE-KVKOLLI-SKP                      
135600       END-IF                                                             
135700                                                                          
135800       IF W-VKORDBTO-SKP          > ZERO                                  
135900          MOVE W-VKORDBTO-SKP    TO MOD-VKORDBTO-SKP                      
136000                                    SAVE-VKORDBTO-SKP                     
136100       ELSE                                                               
136200          MOVE MFS-ERASE-FIELD   TO MOD-VKORDBTO-SKP                      
136300          MOVE ZERO              TO SAVE-VKORDBTO-SKP                     
136400       END-IF                                                             
136500       IF W-VLORDBTO-SKP         > ZERO                                   
136600          MOVE W-VLORDBTO-SKP   TO MOD-VLORDBTO-SKP                       
136700                                   SAVE-VLORDBTO-SKP                      
136800       ELSE                                                               
136900          MOVE MFS-ERASE-FIELD  TO MOD-VLORDBTO-SKP                       
137000          MOVE ZERO             TO SAVE-VLORDBTO-SKP                      
137100       END-IF                                                             
137200       IF W-VKORDNTO-SKP         > ZERO                                   
137300          MOVE W-VKORDNTO-SKP   TO MOD-VKORDNTO-SKP                       
137400                                   SAVE-VKORDNTO-SKP                      
137500       ELSE                                                               
137600          MOVE MFS-ERASE-FIELD  TO MOD-VKORDNTO-SKP                       
137700          MOVE ZERO             TO SAVE-VKORDNTO-SKP                      
137800       END-IF                                                             
137900       IF W-SUORDV-SKEPPN     NUMERIC                                     
138000          MOVE W-SUORDV-SKEPPN  TO MOD-SUORDV-SKEPPN                      
138100                                   SAVE-SUORDV-SKEPPN                     
138200       ELSE                                                               
138300          MOVE MFS-ERASE-FIELD  TO MOD-SUORDV-SKEPPN                      
138400          MOVE ZERO             TO SAVE-SUORDV-SKEPPN                     
138500       END-IF                                                             
138600       IF W-SUORDV-LOCPREL-SKP  > ZERO                                    
138700         MOVE '*'               TO MOD-TEASTRIX-TRP                       
138800         MOVE W-SUORDV-LOCPREL-SKP                                        
138900                                TO SAVE-SUORDV-LOCPREL-SKP                
139000       ELSE                                                               
139100         MOVE SPACE             TO MOD-TEASTRIX-TRP                       
139200         MOVE ZERO              TO SAVE-SUORDV-LOCPREL-SKP                
139300       END-IF                                                             
139400                                                                          
139500       MOVE '002'       TO MSGI-KDCALL                                    
139600       MOVE '4624'      TO SAVE-IDTRANS                                   
139700       MOVE SAVE-AREA   TO MSGI-SPAR-AREA                                 
139800       CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                         
139900     END-IF                                                               
140000     .                                                                    
140100     EJECT                                                                
140200                                                                          
140300 FC-READ-SUM-PER-SHIPMENT SECTION.                                        
140400                                                                          
140500     PERFORM IMS-GHU-WDE101                                               
140600     IF SEGMENT-MISSING                                                   
140700       MOVE SHIPMENT-MISSING TO MED-IDMFSFEL                              
140800       CALL WMEDKONV USING MED-WMEDAREA                                   
140900       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
141000       PERFORM MFS-ERASE-FIELD-OUT                                        
141100     ELSE                                                                 
141200       PERFORM IMS-GNP-WDE111                                             
141300       IF SEGMENT-MISSING                                                 
141400         MOVE KEYS-ARE-MISSING TO MED-IDMFSFEL                            
141500         CALL WMEDKONV USING MED-WMEDAREA                                 
141600         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
141700         PERFORM MFS-ERASE-FIELD-OUT                                      
141800       ELSE                                                               
141900         MOVE SGMT-IDDISTR     TO W-WDE111-IDDISTR-MIN                    
142000         MOVE SGMT-IDKUNDNR    TO W-WDE111-IDKUNDNR-MIN                   
142100         PERFORM IMS-GNP-WDE121-INCL-WDE111                               
142200         PERFORM S01-FIND-PRKURS                                          
142300         MOVE +1 TO INDX                                                  
142400         PERFORM UNTIL INDX > MAX-INDX                                    
142500           IF SEGMENT-FOUND                                               
142600             IF INDX = +1                                                 
142700               MOVE SKOLLI-IDDISTR   TO SAVE-IDDISTR-ENTER                
142800                                        SAVE-IDDISTR-NEXT                 
142900               MOVE SKOLLI-IDKUNDNR  TO SAVE-IDKUNDNR-ENTER               
143000                                        SAVE-IDKUNDNR-NEXT                
143100               MOVE SKOLLI-IDPRODNR  TO SAVE-IDPRODNR-ENTER               
143200                                        SAVE-IDPRODNR-NEXT                
143300               MOVE SKOLLI-IDKOLLI   TO SAVE-IDKOLLI-ENTER                
143400                                        SAVE-IDKOLLI-NEXT                 
143500             END-IF                                                       
143600                                                                          
143700             MOVE SKOLLI-IDKUNDNR TO MOD-IDKUNDNR (INDX)                  
143800             MOVE SKOLLI-IDORDNR7 TO MOD-IDORDNR5 (INDX)                  
143900             MOVE SKOLLI-IDKOLLI  TO MOD-IDKOLLI (INDX)                   
144000                                                                          
144100             MOVE SKOLLI-DIKOLLIL TO MOD-DIKOLLIL (INDX)                  
144200             MOVE SKOLLI-DIKOLLIB TO MOD-DIKOLLIB (INDX)                  
144300             MOVE SKOLLI-DIKOLLIH TO MOD-DIKOLLIH (INDX)                  
144400             MOVE SKOLLI-VKORDBTO-KOLLI                                   
144500                                    TO MOD-VKORDBTO-KOLLI (INDX)          
144600             MOVE SKOLLI-VLORDBTO-KOLLI                                   
144700                                    TO MOD-VLORDBTO-KOLLI (INDX)          
144800             MOVE SKOLLI-VKORDNTO-KOLLI                                   
144900                                    TO MOD-VKORDNTO-KOLLI (INDX)          
145000             MOVE SKOLLI-SUORDV-LOC   TO W-SUORDV-LOC-SEK                 
145100             MOVE SKOLLI-SUORDV-LOCPREL                                   
145200                                       TO W-SUORDV-LOCPREL-SEK            
145300             COMPUTE W-SUORDV-TOT-SEK = SKOLLI-SUORDV  +                  
145400                                        W-SUORDV-LOC-SEK +                
145500                                        W-SUORDV-LOCPREL-SEK              
145600             MOVE W-SUORDV-TOT-SEK TO MOD-SUORDV-KOLLI (INDX)             
145700                                                                          
145800             IF W-SUORDV-LOCPREL-SEK > ZERO                               
145900               MOVE '*'           TO MOD-TEASTRIX-KLI (INDX)              
146000             ELSE                                                         
146100               MOVE ' '           TO MOD-TEASTRIX-KLI (INDX)              
146200             END-IF                                                       
146300                                                                          
146400             MOVE SKOLLI-IDPRODNR TO W-WDE601-IDPRODNR                    
146500             MOVE SKOLLI-IDKOLLI  TO W-WDE611-IDKOLLI                     
146600             PERFORM IMS-GU-WDE611                                        
146700             IF SEGMENT-FOUND                                             
146800                MOVE KOLLI-IDTULL TO MOD-IDTULL (INDX)                    
146900                IF KOLLI-IDPSN (1) > ZERO                                 
147000                    MOVE 'Y'      TO MOD-FLFARLIG (INDX)                  
147100                END-IF                                                    
147200             ELSE                                                         
147300                MOVE SPACE        TO MOD-IDTULL (INDX)                    
147400                                     MOD-FLFARLIG (INDX)                  
147500             END-IF                                                       
147600                                                                          
147700             PERFORM S04-WDE121-SUM                                       
147800             PERFORM IMS-GNP-WDE121-INCL-WDE111                           
147900             ADD 1 TO INDX                                                
148000           ELSE                                                           
148100             PERFORM IMS-GNP-WDE111                                       
148200             IF SEGMENT-MISSING                                           
148300               PERFORM UNTIL INDX > MAX-INDX                              
148400                 MOVE MFS-ERASE-FIELD TO MOD-IDKUNDNR (INDX)              
148500                                         MOD-IDORDNR5 (INDX)              
148600                                         MOD-IDKOLLI  (INDX)              
148700                                         MOD-IDTULL   (INDX)              
148800                                         MOD-DIKOLLIL (INDX)              
148900                                         MOD-DIKOLLIB (INDX)              
149000                                         MOD-DIKOLLIH (INDX)              
149100                                         MOD-VKORDBTO-KOLLI (INDX)        
149200                                         MOD-VLORDBTO-KOLLI (INDX)        
149300                                         MOD-SUORDV-KOLLI (INDX)          
149400                                         MOD-TEASTRIX-KLI (INDX)          
149500                                         MOD-VKORDNTO-KOLLI (INDX)        
149600                                         MOD-FLFARLIG (INDX)              
149700                ADD 1 TO INDX                                             
149800               END-PERFORM                                                
149900             ELSE                                                         
150000               MOVE SGMT-IDDISTR     TO W-WDE111-IDDISTR-MIN              
150100               MOVE SGMT-IDKUNDNR    TO W-WDE111-IDKUNDNR-MIN             
150200               PERFORM IMS-GNP-WDE121-INCL-WDE111                         
150300               PERFORM S01-FIND-PRKURS                                    
150400             END-IF                                                       
150500           END-IF                                                         
150600         END-PERFORM                                                      
150700                                                                          
150800         IF SEGMENT-FOUND                                                 
150900           MOVE SKOLLI-IDDISTR   TO SAVE-IDDISTR-NEXT                     
151000           MOVE SKOLLI-IDKUNDNR  TO SAVE-IDKUNDNR-NEXT                    
151100           MOVE SKOLLI-IDPRODNR  TO SAVE-IDPRODNR-NEXT                    
151200           MOVE SKOLLI-IDKOLLI   TO SAVE-IDKOLLI-NEXT                     
151300           IF MFS-FIRST                                                   
151400             PERFORM FCA-WDE1-SUM                                         
151500                                                                          
151600             MOVE W-KVKOLLI-SKP   TO SAVE-KVKOLLI-SKP                     
151700             MOVE W-VKORDBTO-SKP  TO SAVE-VKORDBTO-SKP                    
151800             MOVE W-VLORDBTO-SKP  TO SAVE-VLORDBTO-SKP                    
151900             MOVE W-VKORDNTO-SKP  TO SAVE-VKORDNTO-SKP                    
152000             MOVE W-SUORDV-SKEPPN TO SAVE-SUORDV-SKEPPN                   
152100             MOVE W-SUORDV-LOCPREL-SKP TO SAVE-SUORDV-LOCPREL-SKP         
152200                                                                          
152300           END-IF                                                         
152400           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
152500           CALL WMEDKONV USING MED-WMEDAREA                               
152600           MOVE MED-TEMFSINF TO MOD-TEMFSINF                              
152700         ELSE                                                             
152800           MOVE SAVE-IDDISTR-ENTER    TO SAVE-IDDISTR-NEXT                
152900           MOVE SAVE-IDKUNDNR-ENTER   TO SAVE-IDKUNDNR-NEXT               
153000           MOVE SAVE-IDPRODNR-ENTER   TO SAVE-IDPRODNR-NEXT               
153100           MOVE SAVE-IDKOLLI-ENTER    TO SAVE-IDKOLLI-NEXT                
153200           MOVE INF-LAST-PAGE         TO MED-IDMFSINF                     
153300           CALL WMEDKONV USING MED-WMEDAREA                               
153400           MOVE MED-TEMFSINF TO MOD-TEMFSINF                              
153500           PERFORM S05-SAVE-SUM                                           
153600         END-IF                                                           
153700                                                                          
153800         MOVE '002'      TO MSGI-KDCALL                                   
153900         MOVE '4624'     TO SAVE-IDTRANS                                  
154000         MOVE SAVE-AREA  TO MSGI-SPAR-AREA                                
154100         CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                       
154200       END-IF                                                             
154300         MOVE SAVE-KVKOLLI-SKP        TO MOD-KVKOLLI-SKP                  
154400         MOVE SAVE-VKORDBTO-SKP       TO MOD-VKORDBTO-SKP                 
154500         MOVE SAVE-VLORDBTO-SKP       TO MOD-VLORDBTO-SKP                 
154600         MOVE SAVE-VKORDNTO-SKP       TO MOD-VKORDNTO-SKP                 
154700         MOVE SAVE-SUORDV-SKEPPN      TO MOD-SUORDV-SKEPPN                
154800     END-IF                                                               
154900     .                                                                    
155000     EJECT                                                                
155100 FCA-WDE1-SUM SECTION.                                                    
155200     PERFORM UNTIL SEGMENT-MISSING                                        
155300       PERFORM UNTIL SEGMENT-MISSING                                      
155400         PERFORM S04-WDE121-SUM                                           
155500         PERFORM IMS-GNP-WDE121-INCL-WDE111                               
155600       END-PERFORM                                                        
155700       PERFORM IMS-GNP-WDE111                                             
155800       IF SEGMENT-FOUND                                                   
155900           MOVE SGMT-IDDISTR     TO W-WDE111-IDDISTR-MIN                  
156000           MOVE SGMT-IDKUNDNR    TO W-WDE111-IDKUNDNR-MIN                 
156100         PERFORM IMS-GNP-WDE121-INCL-WDE111                               
156200         PERFORM S01-FIND-PRKURS                                          
156300       END-IF                                                             
156400     END-PERFORM                                                          
156500     .                                                                    
156600     EJECT                                                                
156700                                                                          
156800 S01-FIND-PRKURS SECTION.                                                 
156900                                                                          
157000     MOVE SKOLLI-KDVALISO TO CURR-KDVALISO-ROW                            
157100     MOVE SGMT-IDDISTR  TO W-WDB201-IDDISTR                               
157200                           W-WDB201-IDDISTR-MIN                           
157300                           W-WDB201-IDDISTR-MAX                           
157400     MOVE SGMT-IDKUNDNR TO W-WDB201-IDKUNDNR                              
157500                                                                          
157600** I/P  CURR-KDVALISO-ROW     W-WDB201-IDDISTR   W-WDB201-IDKUNDNR        
157700**      W-WDB201-IDDISTR-MIN  W-WDB201-IDDISTR-MAX                        
157800** O/P  SAVE-PRKURS                                                       
157900                                                                          
158000     IF CURR-KDVALISO-ROW       > SPACE                                   
158100       CONTINUE                                                           
158200     ELSE                                                                 
158300       PERFORM S07-FIND-KDVALISO                                          
158400     END-IF                                                               
158500                                                                          
158700     MOVE W-DATE-AAMM            TO CURR-TIAAMM                           
158800     MOVE WS-KDVALISO-HUV        TO CURR-KDVALISO-HUV                     
158900     MOVE 'M'                    TO CURR-KDVALTYP                         
159000     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
159100     IF CURR-KDSVAR = ' '                                                 
159200       MOVE CURR-PRKURS-NEW      TO SAVE-PRKURS                           
159300     ELSE                                                                 
159400       MOVE 1                    TO SAVE-PRKURS                           
159500     END-IF                                                               
159600     .                                                                    
159700     EJECT                                                                
159800                                                                          
159900 S02-CONVERT-TO-SEK SECTION.                                              
160000** I/P  SAVE-PRKURS  W-SUORDV-LOC        W-SUORDV-LOCPREL                 
160100** O/P               W-SUORDV-LOC-SEK    W-SUORDV-LOCPREL-SEK             
160200                                                                          
160300     MOVE SAVE-PRKURS        TO EXCH-PRKURS                               
160400**   +1 KDCALL = LOCAL CURRENCY TO SEK                                    
160500     MOVE +1                 TO EXCH-KDCALL                               
160600     MOVE W-SUORDV-LOC       TO EXCH-SUORDV-IN                            
160700     MOVE +0                 TO EXCH-PRARTNTO-IN                          
160800     CALL W411EXCH USING EXCH-W411EXCH                                    
160900     MOVE EXCH-SUORDV-UT     TO W-SUORDV-LOC-SEK                          
161000                                                                          
161100     MOVE +1                 TO EXCH-KDCALL                               
161200     MOVE W-SUORDV-LOCPREL   TO EXCH-SUORDV-IN                            
161300     MOVE +0                 TO EXCH-PRARTNTO-IN                          
161400     CALL W411EXCH USING EXCH-W411EXCH                                    
161500     MOVE EXCH-SUORDV-UT     TO W-SUORDV-LOCPREL-SEK                      
161600     .                                                                    
161700     EJECT                                                                
161800                                                                          
161900 S03-NOLLA-SAVE  SECTION.                                                 
162000                                                                          
162100     PERFORM S03-NOLLA-SAVE-TRP                                           
162200     PERFORM S03-NOLLA-SAVE-SHIPM                                         
162300     .                                                                    
162400     EJECT                                                                
162500 S03-NOLLA-SAVE-TRP  SECTION.                                             
162600     MOVE ZERO                   TO SAVE-IDTRPTNR-ENTER                   
162700                                    SAVE-DARFS-ENTER                      
162800                                    SAVE-ADKOLLI-ENTER                    
162900                                    SAVE-ADFLOMR-ENTER                    
163000                                    SAVE-ADRUTNIV-ENTER                   
163100                                    SAVE-ADVMODUL-ENTER                   
163200                                    SAVE-KVKOLLI-SKP                      
163300                                    SAVE-VKORDBTO-SKP                     
163400                                    SAVE-VLORDBTO-SKP                     
163500                                    SAVE-VKORDNTO-SKP                     
163600                                    SAVE-SUORDV-SKEPPN                    
163700                                    SAVE-SUORDV-LOCPREL-SKP               
163800     MOVE SPACE                  TO SAVE-IDDC-ENTER                       
163900                                    SAVE-ADFLGEO-ENTER                    
164000     MOVE ZERO                   TO SAVE-TRP-IDPRODNR-ENTER               
164100                                    SAVE-TRP-IDKOLLI-ENTER                
164200     MOVE +1                     TO SAVE-PRKURS                           
164300     .                                                                    
164400     EJECT                                                                
164500                                                                          
164600 S03-NOLLA-SAVE-SHIPM  SECTION.                                           
164700     MOVE ZERO                   TO SAVE-IDPRODNR-ENTER                   
164800                                    SAVE-IDPRODNR-NEXT                    
164900                                    SAVE-IDKUNDNR-ENTER                   
165000                                    SAVE-IDKUNDNR-NEXT                    
165100                                    SAVE-IDKOLLI-ENTER                    
165200                                    SAVE-IDKOLLI-NEXT                     
165300                                    SAVE-IDDISTR-ENTER                    
165400                                    SAVE-IDDISTR-NEXT                     
165500     MOVE +1                     TO SAVE-PRKURS                           
165600     .                                                                    
165700     EJECT                                                                
165800                                                                          
165900 S04-WDE121-SUM SECTION.                                                  
166000                                                                          
166100     IF  DIST79-DEALER-PRICE OR                                           
166120         DIST79-ECOM-PRICE                                                
166200         MOVE SKOLLI-SUORDV-LOC     TO W-SUORDV-LOC                       
166300         MOVE SKOLLI-SUORDV-LOCPREL TO W-SUORDV-LOCPREL                   
166400         PERFORM S02-CONVERT-TO-SEK                                       
166500     ELSE                                                                 
166600        MOVE SKOLLI-SUORDV-LOC   TO W-SUORDV-LOC-SEK                      
166700        MOVE SKOLLI-SUORDV-LOCPREL                                        
166800                                 TO W-SUORDV-LOCPREL-SEK                  
166900     END-IF                                                               
167000     COMPUTE W-SUORDV-TOT-SEK  = W-SUORDV-LOC-SEK     +                   
167100                                 W-SUORDV-LOCPREL-SEK +                   
167200                                 SKOLLI-SUORDV                            
167300                                                                          
167400     COMPUTE W-KVKOLLI-SKP   = W-KVKOLLI-SKP  +  1                        
167500     COMPUTE W-VKORDBTO-SKP  = W-VKORDBTO-SKP +                           
167600                               SKOLLI-VKORDBTO-KOLLI                      
167700     COMPUTE W-VLORDBTO-SKP  = W-VLORDBTO-SKP +                           
167800                               SKOLLI-VLORDBTO-KOLLI                      
167900     COMPUTE W-VKORDNTO-SKP  = W-VKORDNTO-SKP +                           
168000                               SKOLLI-VKORDNTO-KOLLI                      
168100     COMPUTE W-SUORDV-SKEPPN = W-SUORDV-SKEPPN +                          
168200                               W-SUORDV-TOT-SEK                           
168300     COMPUTE W-SUORDV-LOCPREL-SKP                                         
168400                             = W-SUORDV-LOCPREL-SKP +                     
168500                               W-SUORDV-LOCPREL-SEK                       
168600     .                                                                    
168700     EJECT                                                                
168800 S05-SAVE-SUM SECTION.                                                    
168900                                                                          
169000     IF W-KVKOLLI-SKP             > ZERO                                  
169100        MOVE W-KVKOLLI-SKP       TO MOD-KVKOLLI-SKP                       
169200                                  SAVE-KVKOLLI-SKP                        
169300     ELSE                                                                 
169400        MOVE MFS-ERASE-FIELD     TO MOD-KVKOLLI-SKP                       
169500        MOVE ZERO                TO SAVE-KVKOLLI-SKP                      
169600     END-IF                                                               
169700     IF W-VKORDBTO-SKP            > ZERO                                  
169800        MOVE W-VKORDBTO-SKP      TO MOD-VKORDBTO-SKP                      
169900                                  SAVE-VKORDBTO-SKP                       
170000     ELSE                                                                 
170100        MOVE MFS-ERASE-FIELD     TO MOD-VKORDBTO-SKP                      
170200        MOVE ZERO                TO SAVE-VKORDBTO-SKP                     
170300     END-IF                                                               
170400     IF W-VLORDBTO-SKP           > ZERO                                   
170500        MOVE W-VLORDBTO-SKP     TO MOD-VLORDBTO-SKP                       
170600                                 SAVE-VLORDBTO-SKP                        
170700     ELSE                                                                 
170800        MOVE MFS-ERASE-FIELD    TO MOD-VLORDBTO-SKP                       
170900        MOVE ZERO               TO SAVE-VLORDBTO-SKP                      
171000     END-IF                                                               
171100                                                                          
171200     IF W-VKORDNTO-SKP           > ZERO                                   
171300        MOVE W-VKORDNTO-SKP     TO MOD-VKORDNTO-SKP                       
171400                                 SAVE-VKORDNTO-SKP                        
171500     ELSE                                                                 
171600        MOVE MFS-ERASE-FIELD    TO MOD-VKORDNTO-SKP                       
171700        MOVE ZERO               TO SAVE-VKORDNTO-SKP                      
171800     END-IF                                                               
171900                                                                          
172000     IF W-SUORDV-SKEPPN       NUMERIC                                     
172100        MOVE W-SUORDV-SKEPPN    TO MOD-SUORDV-SKEPPN                      
172200                                 SAVE-SUORDV-SKEPPN                       
172300     ELSE                                                                 
172400        MOVE MFS-ERASE-FIELD    TO MOD-SUORDV-SKEPPN                      
172500        MOVE ZERO               TO SAVE-SUORDV-SKEPPN                     
172600     END-IF                                                               
172700                                                                          
172800     IF W-SUORDV-LOCPREL-SKP    > ZERO                                    
172900       MOVE '*'                 TO MOD-TEASTRIX-TRP                       
173000       MOVE W-SUORDV-LOCPREL-SKP                                          
173100                              TO SAVE-SUORDV-LOCPREL-SKP                  
173200     ELSE                                                                 
173300       MOVE SPACE               TO MOD-TEASTRIX-TRP                       
173400       MOVE ZERO                TO SAVE-SUORDV-LOCPREL-SKP                
173500     END-IF                                                               
173600     .                                                                    
173700     EJECT                                                                
173800                                                                          
173900 S06-FIND-PRKURS-FOR-FB SECTION.                                          
174000                                                                          
174100** I/P  CURR-KDVALISO-ROW     W-WDB201-IDDISTR   W-WDB201-IDKUNDNR        
174200**      W-WDB201-IDDISTR-MIN  W-WDB201-IDDISTR-MAX                        
174300** O/P  SAVE-PRKURS                                                       
174400                                                                          
174500     IF CURR-KDVALISO-ROW       > SPACE                                   
174600       CONTINUE                                                           
174700     ELSE                                                                 
174800       PERFORM S07-FIND-KDVALISO                                          
174900                                                                          
175000     END-IF                                                               
175100                                                                          
175200     MOVE W-DATE-AAMM            TO CURR-TIAAMM                           
175300     MOVE WS-KDVALISO-HUV        TO CURR-KDVALISO-HUV                     
175400     MOVE 'M'                    TO CURR-KDVALTYP                         
175500     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
175600     IF CURR-KDSVAR = ' '                                                 
175700       MOVE CURR-PRKURS-NEW      TO SAVE-PRKURS                           
175800     ELSE                                                                 
175900       MOVE 1                    TO SAVE-PRKURS                           
176000     END-IF                                                               
176100     .                                                                    
176200     EJECT                                                                
176300                                                                          
176400                                                                          
176500 S07-FIND-KDVALISO SECTION.                                               
176600                                                                          
176700     PERFORM IMS-GU-WDB201-UNIQ                                           
176800     IF SEGMENT-FOUND                                                     
176900       CONTINUE                                                           
177000     ELSE                                                                 
177100       PERFORM IMS-GU-WDB201                                              
177200     END-IF                                                               
177300     MOVE GMT-IDPARTNR         TO W-WDB101-IDPARTNR                       
177400     MOVE GMT-IDFTG            TO W-WDB101-IDFTG                          
177500     IF DIST35-NONVCC-NONVCC-REFILL  OR                                   
177510        DIST35-NONVCC-NONVCC-TRANSFER                                     
177600        MOVE W-DCS-IDPARTNR-EXP TO W-WDB101-IDPARTNR                      
177700     END-IF                                                               
178000     PERFORM IMS-GU-WDB101                                                
179000     MOVE BET-KDVALISO         TO CURR-KDVALISO-ROW                       
180000     .                                                                    
190000     SKIP3                                                                
191000                                                                          
192000 MFS-ERASE-FIELD-OUT SECTION.                                             
193000                                                                          
194000*    --- ALLA UTDATA-FÄLT                                                 
195000*    --- INCL. SCROLL KEYS                                                
196000     MOVE MFS-ERASE-FIELD   TO  MOD-IDDISTR-UT                            
197000                                MOD-IDKUNDNR-UT                           
198000                                MOD-IDTRPTNR-UT                           
198100                                MOD-IDSHIPM-UT                            
198200                                MOD-IDDC-UT                               
198300     .                                                                    
198400     SKIP3                                                                
198500                                                                          
198600 MFS-ERASE-LINE-FIELD-OUT SECTION.                                        
198700                                                                          
198800*    --- OUTDATA-FIELD ON SCROLL KEYS                                     
198900     MOVE MFS-ERASE-FIELD     TO  MOD-KVKOLLI-SKP                         
199000                                  MOD-VKORDBTO-SKP                        
199100                                  MOD-VLORDBTO-SKP                        
199200                                  MOD-SUORDV-SKEPPN                       
199300                                  MOD-TEASTRIX-TRP                        
199400                                  MOD-VKORDNTO-SKP                        
199500                                                                          
199600     MOVE  1                  TO  INDX                                    
199700     PERFORM  UNTIL INDX > MAX-INDX                                       
199800                                                                          
199900        MOVE MFS-ERASE-FIELD  TO  MOD-IDKUNDNR (INDX)                     
200000                                  MOD-IDORDNR5 (INDX)                     
200100                                  MOD-IDKOLLI  (INDX)                     
200200                                  MOD-IDTULL   (INDX)                     
200300                                  MOD-DIKOLLIL (INDX)                     
200400                                  MOD-DIKOLLIB (INDX)                     
200500                                  MOD-DIKOLLIH (INDX)                     
200600                                  MOD-VKORDBTO-KOLLI (INDX)               
200700                                  MOD-VLORDBTO-KOLLI (INDX)               
200800                                  MOD-SUORDV-KOLLI (INDX)                 
200900                                  MOD-TEASTRIX-KLI (INDX)                 
201000                                  MOD-VKORDNTO-KOLLI (INDX)               
201100                                  MOD-FLFARLIG (INDX)                     
201200                                                                          
201300        ADD 1                 TO  INDX                                    
201400                                                                          
201500     END-PERFORM                                                          
201600     .                                                                    
201700     SKIP3                                                                
201800 MFS-ERASE-FIELD-IN SECTION.                                              
201900                                                                          
202000*    --- ALLA INDATA-FÄLT                                                 
202100     MOVE MFS-ERASE-FIELD   TO  MOD-IDDISTR-IN                            
202200                                MOD-IDKUNDNR-IN                           
202300                                MOD-IDTRPTNR-IN                           
202400                                MOD-IDSHIPM-IN                            
202500                                MOD-IDDC-IN                               
202600     .                                                                    
202700     EJECT                                                                
202800 IMS-GET-MSG SECTION.                                                     
202900                                                                          
203000     MOVE '  QC' TO GOOD-STATUSCODES                                      
203100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
203200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
203300     PERFORM IMS-STATUSCHECK                                              
203400     .                                                                    
203500     SKIP3                                                                
203600 IMS-INSERT-MSG SECTION.                                                  
203700                                                                          
203800* FORMAT IN SWEDISH IS NOT PRESENT                                        
203900*    IF MSGI-IDLAND-SPR = 'SE'                                            
204000*      MOVE '0' TO MFS-KDHUVOMR                                           
204100*    END-IF                                                               
204200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
204300     MOVE SPACE TO GOOD-STATUSCODES                                       
204400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
204500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
204600     PERFORM IMS-STATUSCHECK                                              
204700     .                                                                    
204800     EJECT                                                                
204900 IMS-GHU-WDE101 SECTION.                                                  
205000                                                                          
205100     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
205200          DELIMITED BY SIZE INTO SSA1                                     
205300     MOVE '  GE' TO GOOD-STATUSCODES                                      
205400     CALL CBLTDLI USING GHU WDE1-PCB DLI-IO-WDE101 SSA1                   
205500     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
205600     PERFORM IMS-STATUSCHECK                                              
205700     .                                                                    
205800     EJECT                                                                
205900 IMS-GU-WDE111 SECTION.                                                   
206000                                                                          
206100     STRING 'WDE101  (IDSHIPM  =' W-WDE101-IDSHIPM-X ')'                  
206200          DELIMITED BY SIZE INTO SSA1                                     
206300     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X                            
206400                    '&IDDC     =' W-IDDC-X        ')'                     
206500          DELIMITED BY SIZE INTO SSA2                                     
206600     MOVE '  GE' TO GOOD-STATUSCODES                                      
206700     CALL CBLTDLI USING GU WDE1-PCB DLI-IO-WDE111 SSA1 SSA2               
206800     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
206900     PERFORM IMS-STATUSCHECK                                              
207000     .                                                                    
207100     EJECT                                                                
207200 IMS-GNP-WDE111 SECTION.                                                  
207300                                                                          
207400     STRING 'WDE101  (IDSHIPM  =' W-WDE101-IDSHIPM-X ')'                  
207500          DELIMITED BY SIZE INTO SSA1                                     
207600     STRING 'WDE111  (WDE111KY=>' W-WDE111KY-MIN                          
207700                    '&WDE111KY=<' W-WDE111KY-MAX  ')'                     
207800          DELIMITED BY SIZE INTO SSA2                                     
207900     MOVE '  GE' TO GOOD-STATUSCODES                                      
208000     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE111 SSA1 SSA2              
208100     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
208200     PERFORM IMS-STATUSCHECK                                              
208300     .                                                                    
208400     EJECT                                                                
208500 IMS-GNP-WDE121 SECTION.                                                  
208600                                                                          
208700     STRING 'WDE121  (WDE121KY>=' W-WDE121KY-MIN                          
208800                    '&WDE121KY<=' W-WDE121KY-MAX  ')'                     
208900          DELIMITED BY SIZE INTO SSA1                                     
209000     MOVE '  GE' TO GOOD-STATUSCODES                                      
209100     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE121 SSA1                   
209200     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
209300     PERFORM IMS-STATUSCHECK                                              
209400     .                                                                    
209500     EJECT                                                                
209600 IMS-GNP-WDE121-INCL-WDE111 SECTION.                                      
209700                                                                          
209800     STRING 'WDE111  (WDE111KY=>' W-WDE111KY-MIN                          
209900                    '&WDE111KY=<' W-WDE111KY-MAX  ')'                     
210000                    '&IDDC     =' W-IDDC-X        ')'                     
210100          DELIMITED BY SIZE INTO SSA1                                     
210200     STRING 'WDE121  (WDE121KY=>' W-WDE121KY-MIN                          
210300                    '&WDE121KY=<' W-WDE121KY-MAX  ')'                     
210400          DELIMITED BY SIZE INTO SSA2                                     
210500     MOVE '  GE' TO GOOD-STATUSCODES                                      
210600     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE121 SSA1 SSA2              
210700     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
210800     PERFORM IMS-STATUSCHECK                                              
210900     .                                                                    
211000     EJECT                                                                
211100 IMS-GU-WDE611 SECTION.                                                   
211200                                                                          
211300     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
211400          DELIMITED BY SIZE INTO SSA1                                     
211500     STRING 'WDE611  (IDKOLLI  =' W-WDE611-IDKOLLI-X ')'                  
211600          DELIMITED BY SIZE INTO SSA2                                     
211700     MOVE '  GE' TO GOOD-STATUSCODES                                      
211800     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-WDE611 SSA1 SSA2               
211900     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
212000     PERFORM IMS-STATUSCHECK                                              
212100     .                                                                    
212200     EJECT                                                                
212300 IMS-GU-WDE611-CSEQ SECTION.                                              
212400                                                                          
212500     STRING 'WDE611  (WDE6CSEQ>=' W-WDE6CSEQ-MIN                          
212600                    '&WDE6CSEQ<=' W-WDE6CSEQ-MAX                          
212700                    '&IDDC     =' W-IDDC-X                                
212800                    '&IDDISTR  =' W-IDDISTR-X                             
212900                    '&IDKUNDNR =' W-IDKUNDNR-X  ')'                       
213000          DELIMITED BY SIZE INTO SSA1                                     
213100     MOVE '  GE' TO GOOD-STATUSCODES                                      
213200     CALL CBLTDLI USING GU WDE6CSQ-PCB DLI-IO-WDE611 SSA1                 
213300     MOVE WDE6CSQ-STATUS-CODE TO STATUS-WS                                
213400     PERFORM IMS-STATUSCHECK                                              
213500     .                                                                    
213600     EJECT                                                                
213700 IMS-GN-WDE611-CSEQ SECTION.                                              
213800                                                                          
213900     STRING 'WDE611  (WDE6CSEQ>=' W-WDE6CSEQ-MIN                          
214000                    '&WDE6CSEQ<=' W-WDE6CSEQ-MAX                          
214100                    '&IDDC     =' W-IDDC-X                                
214200                    '&IDDISTR  =' W-IDDISTR-X                             
214300                    '&IDKUNDNR =' W-IDKUNDNR-X  ')'                       
214400          DELIMITED BY SIZE INTO SSA1                                     
214500     MOVE '  GE' TO GOOD-STATUSCODES                                      
214600     CALL CBLTDLI USING GN WDE6CSQ-PCB DLI-IO-WDE611 SSA1                 
214700     MOVE WDE6CSQ-STATUS-CODE TO STATUS-WS                                
214800     PERFORM IMS-STATUSCHECK                                              
214900     .                                                                    
215000     EJECT                                                                
215100 IMS-GNP-WDE601-CSEQ SECTION.                                             
215200                                                                          
215300     MOVE 'WDE601  *F'        TO SSA1                                     
215400     MOVE '  ' TO GOOD-STATUSCODES                                        
215500     CALL CBLTDLI USING GNP WDE6CSQ-PCB DLI-IO-WDE601 SSA1                
215600     MOVE WDE6CSQ-STATUS-CODE TO STATUS-WS                                
215700     PERFORM IMS-STATUSCHECK                                              
215800     .                                                                    
215900     EJECT                                                                
216000 IMS-GU-WDE611-CSEQ-UNIK SECTION.                                         
216100                                                                          
216200     STRING 'WDE611  *PD(WDE6CSEQ>=' W-WDE6CSEQ-MIN                       
216300                       '&WDE6CSEQ<=' W-WDE6CSEQ-MAX                       
216400                       '&IDDC     =' W-IDDC-X                             
216500                       '&IDDISTR  =' W-IDDISTR-X                          
216600                       '&IDKUNDNR =' W-IDKUNDNR-X                         
216700                       '&IDKOLLI  =' W-IDKOLLI-X  ')'                     
216800          DELIMITED BY SIZE INTO SSA1                                     
216900     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
217000          DELIMITED BY SIZE INTO SSA2                                     
217100     MOVE '  GE' TO GOOD-STATUSCODES                                      
217200     CALL CBLTDLI USING GU WDE6CSQ-PCB DLI-IO-WDE611-01 SSA1 SSA2         
217300     MOVE WDE6CSQ-STATUS-CODE TO STATUS-WS                                
217400     PERFORM IMS-STATUSCHECK                                              
217500     .                                                                    
217600     EJECT                                                                
217700 IMS-GU-WDE4F1   SECTION.                                                 
217800                                                                          
217900     STRING 'WDE4F1  (WDE4F1KY>=' W-WDE4F1KY-MIN-X                        
218000                    '&WDE4F1KY<=' W-WDE4F1KY-MAX-X ')'                    
218100          DELIMITED BY SIZE INTO SSA1                                     
218200     MOVE '  GE' TO GOOD-STATUSCODES                                      
218300     CALL CBLTDLI USING GU WDE4F-PCB DLI-IO-WDE4F1 SSA1                   
218400     MOVE WDE4F-STATUS-CODE TO STATUS-WS                                  
218500     PERFORM IMS-STATUSCHECK                                              
218600     .                                                                    
218700 IMS-GU-WDB201 SECTION.                                                   
218800                                                                          
218900     STRING 'WDB201  (IDGMT   >=' W-WDB201-IDGMT-MIN-X                    
219000                    '&IDGMT   <=' W-WDB201-IDGMT-MAX-X ')'                
219100            DELIMITED BY SIZE INTO SSA1                                   
219200     MOVE '  ' TO GOOD-STATUSCODES                                        
219300     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
219400     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
219500     PERFORM IMS-STATUSCHECK                                              
219600     .                                                                    
219700     EJECT                                                                
219800 IMS-GU-WDB201-UNIQ SECTION.                                              
219900                                                                          
220000     STRING 'WDB201  (IDGMT    =' W-WDB201-IDGMT-X ')'                    
220100            DELIMITED BY SIZE INTO SSA1                                   
220200                                                                          
220300     MOVE '  GE' TO GOOD-STATUSCODES                                      
220400     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
220500     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
220600     PERFORM IMS-STATUSCHECK                                              
220700     .                                                                    
220800     EJECT                                                                
220900 IMS-GU-WDB101 SECTION.                                                   
221000                                                                          
221100     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
221200          DELIMITED BY SIZE INTO SSA1                                     
221300     MOVE '  ' TO GOOD-STATUSCODES                                        
221400     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
221500     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
221600     PERFORM IMS-STATUSCHECK                                              
221700     .                                                                    
221800     EJECT                                                                
223100 IMS-GU-WDB601    SECTION.                                                
223200     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
223300          DELIMITED BY SIZE INTO SSA1                                     
223400     MOVE '  GE' TO GOOD-STATUSCODES                                      
223500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
223600     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
223700     PERFORM IMS-STATUSCHECK                                              
223800     IF SEGMENT-MISSING                                                   
223900         MOVE SPACE TO DCS-KDDC                                           
224000     END-IF                                                               
224100     .                                                                    
224200 IMS-STATUSCHECK SECTION.                                                 
224300                                                                          
224400     SET STATUS-IX TO 1                                                   
224500     SEARCH GOOD-STATUS                                                   
224600       AT END                                                             
224700         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
224800         DELIMITED BY SIZE INTO ERROR-TEXT                                
224900         CALL FELLOG                                                      
225000       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
225100         CONTINUE                                                         
225200     END-SEARCH                                                           
225300     .                                                                    
