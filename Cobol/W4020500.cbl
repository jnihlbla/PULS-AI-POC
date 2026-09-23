000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4020500.                                                
000400 AUTHOR.         LARS THELL      (MG).                                    
000500 DATE-WRITTEN.   90/08/21.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        PROGRAMMET ANVÄNDS FÖR ATT SÄTTA OM TRANSPORT-                   
001100*        AVGÅNGSTID ELLER READY-FOR-SHIPMENTTID PÅ EN                     
001200*        ORDER                                                            
001300*                                                                         
001400*        ÄR DET EN B ELLER C ORDER SOM HAR DIREKTLEVERANS                 
001500*        RADER SOM MAN SÄTTER TID PÅ FÖR FÖRSTA GÅNGEN,                   
001600*        STARTAS PGM W40695. DET PROGRAMMET FLYTTAR RADERNA               
001700*        FRÅN WDQ4 TILL WDE4/WDE6 OCH SKRIVER UT PU.                      
001800*                                                                         
001900*        VISAR ENDAST ORDER SOM MAN FÅR UPPDATERA, DVS                    
002000*        ORDER SOM HAR RADER.                                             
002100*                                                                         
002200*        VID NYA NYCKLAR OCH UPPDATERING LÄSER MAN IGENOM                 
002300*        HELA BASEN FÖR ATT RÄKNA FRAM EN TOTAL SOM LIGGER                
002400*        LÄNGST NER PÅ SIDAN, VID ÖVRIGA FALL, PFK7, PFK8                 
002500*        OCH ENTER FLYTTAS TOTALEN IFRÅN MID TILL MOD.                    
002600*                                                                         
002700*                                                                         
002800*        PROGRAMMET ÄR EN UPPDATERINGS-MPP                                
002900*        PROGRAMMET UPPDATERAR WLORQI (WDQ2)                              
003000*                              WLORQA (WDQ3)                              
003100*                                                                         
003200*                                                                         
003300*    INDATA.                                                              
003400*        TRANSAKTION: W4T205                                              
003500*        MID:         W4I20501                                            
003600*                                                                         
003700*    UTDATA.                                                              
003800*        MOD:         W4O20501                                            
003900*                                                                         
004000* HÖSTEN 2004 GÖRAN KJELLSON                                              
004100* ETRACKER 887753                                                         
004200*                                                                         
004300*  SEPT 2005 LINDA NILSSON                                                
004400*  ETRACKER 1334295                                                       
004500*                                                                         
004600                                                                          
004700     SKIP3                                                                
004800 ENVIRONMENT DIVISION.                                                    
004900     EJECT                                                                
005000 DATA DIVISION.                                                           
005100 WORKING-STORAGE SECTION.                                                 
005200                                                                          
005300*    -COPY WY2000WB                                                       
005400     SKIP3                                                                
005500 77  IDPGM                       PIC X(08)   VALUE 'W4020500'.            
005600                                                                          
005700 77  JA                          PIC X       VALUE 'J'.                   
005800 77  NEJ                         PIC X       VALUE 'N'.                   
005900                                                                          
006000 77  FELTEXT                     PIC X(64)  VALUE SPACE.                  
006100 77  RKOD-ABEND                  PIC S9(4)  VALUE +33   COMP SYNC.        
006200 77  IX                          PIC S9(9)  VALUE ZERO  COMP SYNC.        
006300 77  IX-MAX                      PIC S9(9)  VALUE +7    COMP SYNC.        
006400                                                                          
006500*    --- INDEX FÖR BLÄDDRINGSRADER                                        
006600 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
006900 77  MAX-INDX                    PIC S9(4)  VALUE +11   COMP SYNC.        
007000 77  MAX-INDX-PLUS-ETT           PIC S9(4)  VALUE +12   COMP SYNC.        
007100 77  MAX-INDX-PLUS-TVA           PIC S9(4)  VALUE +13   COMP SYNC.        
007200                                                                          
007300 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
007400 77  MAX-MOD4205-LAENGD          PIC S9(4)  VALUE +977  COMP SYNC.        
007500                                                                          
007600*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
007700 77  WS-IDTRP                    PIC X(05)   VALUE SPACE.                 
007800 77  WS-TITRPAVG                 PIC X(07)   VALUE SPACE.                 
007900 77  WS-IDDISTR                  PIC X(04)   VALUE SPACE.                 
008000 77  WS-IDKUNDNR                 PIC X(06)   VALUE SPACE.                 
008100 77  WS-KDFRAKT                  PIC X(02)   VALUE SPACE.                 
008200 77  WS-IDORDNR7                 PIC X(07)   VALUE SPACE.                 
008300                                                                          
008400 77  WS-KDVALISO                 PIC X(3)    VALUE SPACE.                 
008500 77  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
008600 77  WS-KDVALISO-HUV             PIC X(3)    VALUE 'SEK'.                 
008700*    --- ARBETSFÄLT                                                       
008800 77  W-KDFRAKT                   PIC S9(3)   VALUE ZERO COMP-3.           
008900 77  W-DIRL-VLORDNTO             PIC  9(4)V9(3) VALUE ZERO.               
009000 77  W-DIRL-VKORDNTO             PIC  9(6)V9    VALUE ZERO.               
009100 77  W-DIRL-SUORDV               PIC  9(9)V9(2) VALUE ZERO.               
009200 77  W-DIRL-SUORDV-LOC           PIC  9(9)V9(2) VALUE ZERO.               
009300 77  W-DIRL-SUORDV-LOCPREL       PIC  9(9)V9(2) VALUE ZERO.               
009400 77  W-VLORDNTO-TOT              PIC  9(4)V9(3) VALUE ZERO.               
009500 77  W-VKORDNTO-TOT              PIC  9(6)V9    VALUE ZERO.               
009600 77  W-SUORDV-TOT                PIC  9(9)V9(2) VALUE ZERO.               
009700 77  W-SUORDV-LOC-TOT            PIC  9(9)V9(2) VALUE ZERO.               
009800 77  W-SUORDV-LOCPREL-TOT        PIC  9(9)V9(2) VALUE ZERO.               
009900 77  W-MOD-IDORDNR7              PIC  9(7)      VALUE ZERO.               
010000*                                                                         
010100 77  W-RAD-VLORDNTO-TOT          PIC  9(3)V9(3)  VALUE ZERO.              
010200 77  W-RAD-VKORDNTO-TOT          PIC  9(5)V9     VALUE ZERO.              
010300 77  W-RAD-SUORDV-TOT            PIC  9(9)V9(2)  VALUE ZERO.              
010400 77  W-RAD-SUORDV-LOC-TOT        PIC  9(9)V9(2)  VALUE ZERO.              
010500 77  W-RAD-SUORDV-LOCPREL-TOT    PIC  9(9)V9(2)  VALUE ZERO.              
010600*                                                                         
010700 77  W-ARB-TIAA                  PIC  9(02)  VALUE ZERO.                  
010800 77  W-ARB-TIHHMM                PIC  9(04)  VALUE ZERO.                  
010900 77  W-DAT-TIAA                  PIC  9(02)  VALUE ZERO.                  
011000 77  W-TITRPAVG                  PIC  9(07)  VALUE ZERO.                  
011100 77  W-ARB-DATRPAVD              PIC  9(08)  VALUE ZERO.                  
011200 77  W-ARB-TIRFS                 PIC  9(10)  VALUE ZERO.                  
011300 77  WS-TIVV                     PIC  9(02)  VALUE ZERO.                  
011400 77  W-ASTTOT                    PIC  X(01)  VALUE SPACE.                 
011500*                                                                         
011600 77  W-SPAR-KDTRPKAT             PIC  X(01)  VALUE SPACE.                 
011700*                                                                         
011800 01  W-VVDHHMM.                                                           
011900   05 FILLER                     PIC  X(03).                              
012000   05 W-TIMMA                    PIC  9(02).                              
012100      88 TIMMA-OK                            VALUE 00 THRU 23.            
012200   05 W-MINUT                    PIC  9(02).                              
012300      88 MINUT-OK                             VALUE 00 THRU 59.           
012400*                                                                         
012500 01  W-TITRPAVT.                                                          
012600   05 W-TIAAMMDD                 PIC  S9(7)  VALUE ZERO COMP-3.           
012700   05 W-TIHHMM                   PIC  S9(5)  VALUE ZERO COMP-3.           
012800*                                                                         
012900 01  W-DATRPAVT.                                                          
013000   05 W-DAAAMMDD                 PIC   9(8)  VALUE ZERO.                  
013100   05 W-TIHHMM                   PIC  S9(5)  VALUE ZERO COMP-3.           
013200*                                                                         
013300 01  W-TIAAVVD.                                                           
013400   05 W-TIAA                     PIC   9(2)  VALUE ZERO.                  
013500   05 W-TIVVD.                                                            
013600      07 W-TIVV                  PIC   9(2)  VALUE ZERO.                  
013700      07 W-TID                   PIC   9(1)  VALUE ZERO.                  
013800 01  W-TIAAVVD-NUM REDEFINES W-TIAAVVD PIC 9(5).                          
013900*                                                                         
014000 01  W-TIRFS.                                                             
014100   05 W-TIAAMMDD                       PIC   9(7).                        
014200   05 W-TIHHMM                         PIC   9(4).                        
014300 01  W-TIRFS-NUM REDEFINES W-TIRFS     PIC   9(11).                       
014400*                                                                         
014500*    --- FLAGGOR                                                          
014600 77  INDATA-SW                   PIC X       VALUE 'J'.                   
014700     88  INDATA-OK                           VALUE 'J'.                   
014800     88  INDATA-FEL                          VALUE 'N'.                   
014900                                                                          
015000 77  TID-SW                      PIC X       VALUE 'J'.                   
015100     88  TID-OK                              VALUE 'J'.                   
015200     88  TID-FEL                             VALUE 'N'.                   
015300                                                                          
015400 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
015500     88  NYCKLAR-OK                          VALUE 'J'.                   
015600     88  NYCKLAR-FEL                         VALUE 'N'.                   
015700                                                                          
015800 77  ALLT-SW                     PIC X       VALUE 'J'.                   
015900     88  ALLT-OK                             VALUE 'J'.                   
016000                                                                          
016100 77  IDTRP-IFYLLT-SW             PIC X       VALUE 'N'.                   
016200     88  IDTRP-IFYLLT                        VALUE 'J'.                   
016300                                                                          
016400 77  CMD-IFYLLT-SW               PIC X       VALUE 'N'.                   
016500     88  CMD-IFYLLT                          VALUE 'J'.                   
016600                                                                          
016700 77  TITRPAVG-IFYLLT-SW          PIC X       VALUE 'N'.                   
016800     88  TITRPAVG-IFYLLT                     VALUE 'J'.                   
016900                                                                          
017000 77  LAS-BARA-EN-SIDA-SW         PIC X       VALUE 'N'.                   
017100     88  LAS-BARA-EN-SIDA                    VALUE 'J'.                   
017200                                                                          
017300 77  FORSTA-WDQ2-LAST-SW         PIC X       VALUE 'N'.                   
017400     88  FORSTA-WDQ2-LAST                    VALUE 'J'.                   
017500                                                                          
017600 77  ODEL-FINNS-SW               PIC X       VALUE 'N'.                   
017700     88  ODEL-FINNS                          VALUE 'J'.                   
017800     88  ODEL-SAKNAS                         VALUE 'N'.                   
017900                                                                          
018000 77  RADER-FINNS-SW              PIC X       VALUE 'N'.                   
018100     88  RADER-FINNS                         VALUE 'J'.                   
018200     88  RADER-SAKNAS                        VALUE 'N'.                   
018300                                                                          
018400 77  DIRL-RADER-FINNS-SW         PIC X       VALUE 'N'.                   
018500     88  DIRL-RADER-FINNS                    VALUE 'J'.                   
018600                                                                          
018702 77  Q221-FINNS-SW               PIC X       VALUE 'N'.                   
018802     88  Q221-FINNS                          VALUE 'J'.                   
018902                                                                          
019002                                                                          
019102 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
019202     88  EGEN-MID                            VALUE '4205'.                
019302     88  GODK-MID                            VALUE '4205'.                
019402     EJECT                                                                
019502                                                                          
019602 01  TEST-IDDISTR                PIC S9(5)   COMP-3.                      
019702*01  FILLER -COPY WWDIST79  -RED TEST-IDDISTR.                            
019802     EJECT                                                                
019902                                                                          
020002*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
020102                                                                          
020202 01  GENERELLA-SUBPROGRAM.                                                
020302     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
020402     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
020502     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
020602     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
020702     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
020802     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
020902     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
021002                                                                          
021102 01  GEMENSAMMA-SUBPROGRAM.                                               
021202     03  W413AVSO                PIC X(8)    VALUE 'W413AVSO'.            
021302*                                                                         
021402     03  W411EXCH                PIC X(8)    VALUE 'W411EXCH'.            
021502*            RÄKNA OM VALUTA DDI                                          
021602     EJECT                                                                
021702*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
021802*01 -COPY WMSGINIT                                                        
021902     EJECT                                                                
022002*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
022102*   -COPY WMEDAREA                                                        
022202     EJECT                                                                
022302*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
022402*   -COPY WDATAREA                                                        
022502     EJECT                                                                
022602*    --- PARAMETRAR TILL SUBPROGRAM W413AVSO                              
022702*   -COPY W413AVSO                                                        
022802     EJECT                                                                
022902*    --- PARAMETRAR TILL SUBPROGRAM W411EXCH                              
023002*01 -COPY W411EXCH                                                        
023102     EJECT                                                                
023202*    --- PARAMETRAR TILL SUBPROGRAM W510CURR                              
023302*01 -COPY W510CURR                                                        
023402     EJECT                                                                
023502 01  MESSAGE-CODES.                                                       
023602     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
023702     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
023802     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
023902     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
024002     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
024102     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
024202     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
024302     03  ERR-KUND-SPAERRAD       PIC X(3)    VALUE '213'.                 
024402     EJECT                                                                
024502*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
024602*                                                                         
024702 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
024802     SKIP3                                                                
024902*01  MID -COPY W4I20501                                                   
025002     EJECT                                                                
025102 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
025202     SKIP3                                                                
025302*01  -COPY WMSGAREA                                                       
025402     EJECT                                                                
025502     03  MOD REDEFINES MSG-AREA.                                          
025602*      05  -COPY W4O20501                                                 
025702     EJECT                                                                
025802******************************************************************        
025902*---MSG-AREOR FÖR HOPP TILL 4695  PU DIREKTLEVERANTÖR                     
026002******************************************************************        
026102                                                                          
026202 01  FILLER                  PIC X(16)  VALUE '4695-MSG-IO-AREA'.         
026302 01  4695-MSG-IO-AREA.                                                    
026402     03  4695-LL               PIC S9(4)  VALUE +26  COMP SYNC.           
026502     03  4695-Z1               PIC X.                                     
026602     03  4695-Z2               PIC X.                                     
026702     03  4695-TRANSKOD         PIC X(8)   VALUE 'W4T695X '.               
026802     03  4695-IDTRANS          PIC X(4)   VALUE '4695'.                   
026902     03  4695-SPRAK            PIC X      VALUE '1'.                      
027002     03  4695-IDORDER          PIC 9(7).                                  
027102     03  4695-IDDC             PIC X(2).                                  
027202     EJECT                                                                
027302 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
027402     SKIP3                                                                
027502*01  -COPY WMFSAREA                                                       
027602     EJECT                                                                
027702*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
027802*                                                                         
027902 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
028002     SKIP3                                                                
028102 01  NYCKLAR-TILL-DLI.                                                    
028202*------ TILL WDQ201                                                       
028302   03  W-IDORDER-X.                                                       
028402       05  W-OHUV-IDORDER           PIC S9(07) VALUE ZERO COMP-3.         
028502                                                                          
028602   03  W-IDORDER-MIN-X.                                                   
028702       05  W-OHUV-IDORDER-MIN       PIC S9(07) VALUE ZERO COMP-3.         
028802                                                                          
028902   03  W-IDORDER-MAX-X.                                                   
029002       05  W-OHUV-IDORDER-MAX       PIC S9(07) VALUE ZERO COMP-3.         
029102                                                                          
029202*------ TILL WDQ212                                                       
029302                                                                          
029402   03  W-IDDC-X.                                                          
029502       05  W-IDDC                   PIC  X(02) VALUE SPACE.               
029602                                                                          
029702   03  W-WDQ2B1KY-X.                                                      
029802       05  W-Q2B1KY-IDDC            PIC  X(02) VALUE SPACE.               
029902       05  W-Q2B1KY-IDTRP.                                                
030002          07 W-Q2B1KY-IDTRPLOS      PIC  X(03) VALUE SPACE.               
030102          07 W-Q2B1KY-IDTRPVAR      PIC  X(02) VALUE SPACE.               
030202       05  W-Q2B1KY-DATRPAVT.                                             
030302          07 W-Q2B1KY-DATRPAVD      PIC  9(08) VALUE ZERO.                
030402          07 W-Q2B1KY-TIHHMM        PIC S9(05) VALUE ZERO COMP-3.         
030502       05  W-Q2B1KY-IDORDER         PIC S9(07) VALUE ZERO COMP-3.         
030602                                                                          
030702   03  W-WDQ2B1KY-MIN-X.                                                  
030802       05  W-Q2B1KY-IDDC-MIN        PIC  X(02) VALUE SPACE.               
030902       05  W-Q2B1KY-IDTRP-MIN.                                            
031002          07 W-Q2B1KY-IDTRPLOS-MIN  PIC  X(03) VALUE SPACE.               
031102          07 W-Q2B1KY-IDTRPVAR-MIN  PIC  X(02) VALUE SPACE.               
031202       05  W-Q2B1KY-DATRPAVT-MIN.                                         
031302          07 W-Q2B1KY-DATRPAVD-MIN  PIC  9(08) VALUE ZERO.                
031402          07 W-Q2B1KY-TIHHMM-MIN    PIC S9(05) VALUE ZERO COMP-3.         
031502       05  W-Q2B1KY-IDORDER-MIN     PIC S9(07) VALUE ZERO COMP-3.         
031602                                                                          
031702   03  W-WDQ2B1KY-MAX-X.                                                  
031802       05  W-Q2B1KY-IDDC-MAX        PIC  X(02) VALUE SPACE.               
031902       05  W-Q2B1KY-IDTRP-MAX.                                            
032002          07 W-Q2B1KY-IDTRPLOS-MAX  PIC  X(03) VALUE SPACE.               
032102          07 W-Q2B1KY-IDTRPVAR-MAX  PIC  X(02) VALUE SPACE.               
032202       05  W-Q2B1KY-DATRPAVT-MAX.                                         
032302          07 W-Q2B1KY-DATRPAVD-MAX  PIC  9(08) VALUE ZERO.                
032402          07 W-Q2B1KY-TIHHMM-MAX    PIC S9(05) VALUE ZERO COMP-3.         
032502       05  W-Q2B1KY-IDORDER-MAX     PIC S9(07) VALUE ZERO COMP-3.         
032602                                                                          
032702   03  W-DATRPAVT-MIN-X.                                                  
032802       05 W-ARB-DATRPAVD-MIN        PIC  9(08) VALUE ZERO.                
032902       05 W-ARB-TIHHMM-MIN          PIC S9(05) VALUE ZERO COMP-3.         
033002                                                                          
033102   03  W-DATRPAVT-MAX-X.                                                  
033202       05 W-ARB-DATRPAVD-MAX        PIC  9(08) VALUE ZERO.                
033302       05 W-ARB-TIHHMM-MAX          PIC S9(05) VALUE ZERO COMP-3.         
033402                                                                          
033502   03  W-IDDC-B6-X.                                                       
033602       05 W-IDDC-B6                  PIC X(2).                            
033702                                                                          
033802     EJECT                                                                
033902*------ TILL WDQ201  VIA WDQ2C1(SEK.INDX)                                 
034002   03  W-WDQ2CSEQ-X.                                                      
034102       05  W-Q2CSEQ-IDDISTR         PIC S9(05) VALUE ZERO COMP-3.         
034202       05  W-Q2CSEQ-IDKUNDNR        PIC S9(07) VALUE ZERO COMP-3.         
034302       05  W-Q2CSEQ-IDKUNDRF.                                             
034402          07 W-Q2CSEQ-IDORDNR7      PIC  9(07) VALUE ZERO.                
034502          07 FILLER                 PIC  X(03) VALUE SPACE.               
034602                                                                          
034702   03  W-WDQ2CSEQ-MIN-X.                                                  
034802       05  W-Q2CSEQ-IDDISTR-MIN     PIC S9(05) VALUE ZERO COMP-3.         
034902       05  W-Q2CSEQ-IDKUNDNR-MIN    PIC S9(07) VALUE ZERO COMP-3.         
035002       05  W-Q2CSEQ-IDKUNDRF-MIN.                                         
035102          07 W-Q2CSEQ-IDORDNR7-MIN  PIC  9(07) VALUE ZERO.                
035202          07 FILLER                 PIC  X(03) VALUE SPACE.               
035302                                                                          
035402   03  W-WDQ2CSEQ-MAX-X.                                                  
035502       05  W-Q2CSEQ-IDDISTR-MAX     PIC S9(05) VALUE ZERO COMP-3.         
035602       05  W-Q2CSEQ-IDKUNDNR-MAX    PIC S9(07) VALUE ZERO COMP-3.         
035702       05  W-Q2CSEQ-IDKUNDRF-MAX.                                         
035802          07 W-Q2CSEQ-IDORDNR7-MAX  PIC  9(07) VALUE ZERO.                
035902          07 FILLER                 PIC  X(03) VALUE SPACE.               
036002                                                                          
036102   03  W-SEQC-IDKUNDRF-MIN-X.                                             
036202          07 W-SEQC-IDORDNR7-MIN    PIC  9(07) VALUE ZERO.                
036302          07 FILLER                 PIC  X(03) VALUE SPACE.               
036402                                                                          
036502   03  W-SEQC-IDKUNDRF-MAX-X.                                             
036602          07 W-SEQC-IDORDNR7-MAX    PIC  9(07) VALUE ZERO.                
036702          07 FILLER                 PIC  X(03) VALUE SPACE.               
036802                                                                          
036902*------ TILL WDQ301                                                       
037002     03  W-WDQ301KY-X.                                                    
037102         05  W-Q301KY-IDORDER       PIC S9(07) VALUE ZERO COMP-3.         
037202         05  W-Q301KY-IDDC          PIC  X(02) VALUE SPACE.               
037302         05  W-Q301KY-IDPRODNR      PIC S9(07) VALUE ZERO COMP-3.         
037402         05  W-Q301KY-IDPLKLST      PIC S9(03) VALUE ZERO COMP-3.         
037502                                                                          
037602     03  W-WDQ301KY-MIN-X.                                                
037702         05  W-Q301KY-IDORDER-MIN   PIC S9(07) VALUE ZERO COMP-3.         
037802         05  W-Q301KY-IDDC-MIN      PIC  X(02) VALUE SPACE.               
037902         05  W-Q301KY-IDPRODNR-MIN  PIC S9(07) VALUE ZERO COMP-3.         
038002         05  W-Q301KY-IDPLKLST-MIN  PIC S9(03) VALUE ZERO COMP-3.         
038102                                                                          
038202     03  W-WDQ301KY-MAX-X.                                                
038302         05  W-Q301KY-IDORDER-MAX   PIC S9(07) VALUE ZERO COMP-3.         
038402         05  W-Q301KY-IDDC-MAX      PIC  X(02) VALUE SPACE.               
038502         05  W-Q301KY-IDPRODNR-MAX  PIC S9(07) VALUE ZERO COMP-3.         
038602         05  W-Q301KY-IDPLKLST-MAX  PIC S9(03) VALUE ZERO COMP-3.         
038702                                                                          
038802     03  W-IDKUNDNR-MIN-X.                                                
038902         05 W-SEQC-IDKUNDNR-MIN     PIC S9(07) VALUE ZERO COMP-3.         
039002                                                                          
039102     03  W-IDKUNDNR-MAX-X.                                                
039202         05 W-SEQC-IDKUNDNR-MAX     PIC S9(07) VALUE ZERO COMP-3.         
039302                                                                          
039402*------ TILL WDB201                                                       
039502   03  W-IDGMT-X.                                                         
039602       05  W-IDDISTR                PIC S9(05) VALUE ZERO COMP-3.         
039702       05  W-IDKUNDNR               PIC S9(07) VALUE ZERO COMP-3.         
039802                                                                          
039902   03  W-IDGMT-MIN-X.                                                     
040002       05  W-IDDISTR-WDB2-MIN       PIC S9(05) VALUE ZERO COMP-3.         
040102       05  W-IDKUNDNR-WDB2-MIN      PIC S9(07) VALUE ZERO COMP-3.         
040202                                                                          
040302   03  W-IDGMT-MAX-X.                                                     
040402       05  W-IDDISTR-WDB2-MAX       PIC S9(05) VALUE ZERO COMP-3.         
040502       05  W-IDKUNDNR-WDB2-MAX      PIC S9(07) VALUE ZERO COMP-3.         
040602                                                                          
040702   03  W-WDB101KY-X.                                                      
040802       05  W-WDB1-IDPARTNR              PIC X(9)   VALUE SPACE.           
040902       05  W-WDB1-IDFTG                 PIC 9(2)   VALUE ZERO.            
041002                                                                          
041200     EJECT                                                                
041300*    --- STATUS-KOD FRÅN IMS                                              
041400 01  STATUS-WS                   PIC XX.                                  
041500     88  SEGMENT-FINNS                       VALUE '  '.                  
041600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
041700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
041800     88  END-OF-DATA                         VALUE 'GB'.                  
041900     SKIP2                                                                
042000 01  GODK-STATUSKODER.                                                    
042100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
042200     SKIP3                                                                
042300 01  SSA1                        PIC X(128).                              
042400 01  SSA2                        PIC X(96).                               
042500     EJECT                                                                
042600                                                                          
042700*    --- IMS FUNKTIONSKODER                                               
042800*01  -COPY W0003                                                          
042900     EJECT                                                                
043000*    ---  DLI INPUT-OUTPUT AREA                                           
043100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
043200     SKIP3                                                                
043300 01  DLI-IO-AREA1.                                                        
043400     SKIP3                                                                
043500*        03  -COPY WDQ201                                                 
043600*        03  -COPY WDQ212                                                 
043700     EJECT                                                                
043800 01  DLI-IO-AREA3.                                                        
043900     03  IO-AREA3                PIC X(32)    VALUE SPACE.                
044000     SKIP3                                                                
044100     03  WLORQK01 REDEFINES IO-AREA3.                                     
044200*        05  -COPY WDQ2B1                                                 
044300     EJECT                                                                
044400 01  DLI-IO-AREA4.                                                        
044500     03  IO-AREA4                PIC X(192)   VALUE SPACE.                
044600     SKIP3                                                                
044700     03  WLORQA01 REDEFINES IO-AREA4.                                     
044800*        05  -COPY WDQ301                                                 
044902 01  FILLER                      PIC X(16)   VALUE 'WDQ211-AREA'.         
045002 01  DLI-IO-Q211.                                                         
045402*    03  -COPY WDQ211                                                     
045502     SKIP1                                                                
046002 01  FILLER                      PIC X(16)   VALUE 'WDQ221-AREA'.         
046102 01  DLI-IO-Q221.                                                         
046202*    03  -COPY WDQ221                                                     
046302     SKIP1                                                                
046402 01  FILLER                      PIC X(16)   VALUE 'WDB201-AREA'.         
046502 01  DLI-IO-AREA-WDB2.                                                    
046602     03  WDB201.                                                          
046702*        05  -COPY WDB201                                                 
046802     EJECT                                                                
046902 01  FILLER                      PIC X(16)   VALUE 'WDB101-AREA'.         
047002 01  DLI-IO-AREA-WDB1.                                                    
047102     03  WDB101.                                                          
047202*        05  -COPY WDB101                                                 
047302     EJECT                                                                
047402 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
047502 01   DLI-IO-AREA-B601.                                                   
047602*     03  -COPY WDB601                                                    
047702     EJECT                                                                
047802 LINKAGE SECTION.                                                         
047902                                                                          
048002*01  -COPY W0009      -PRE MSG-                                           
048102     EJECT                                                                
048202*01  -COPY W0009      -PRE ALT-                                           
048302     EJECT                                                                
048402*01  -COPY W0008      -PRE USEA-                                          
048502     05  FILLER                  PIC X.                                   
048602     EJECT                                                                
048702*01  -COPY W0008      -PRE ORQA-                                          
048802     05  FILLER                  PIC X.                                   
048902     EJECT                                                                
049002*01  -COPY W0008      -PRE WDQ2-                                          
049102     05  FILLER                  PIC X.                                   
049202     EJECT                                                                
049302*01  -COPY W0008      -PRE ORQK-                                          
049402     05  FILLER                  PIC X.                                   
049502     EJECT                                                                
049602*01  -COPY W0008      -PRE ORQL-                                          
049702     05  FILLER                  PIC X.                                   
049802     EJECT                                                                
049902*01  -COPY W0008      -PRE WDB2-                                          
050002     05  FILLER                  PIC X.                                   
050102     EJECT                                                                
050202*01  -COPY W0008      -PRE WDB1-                                          
050302     05  FILLER                  PIC X.                                   
050402     EJECT                                                                
050502*01  -COPY W0008      -PRE WDG2-                                          
050602     05  FILLER                  PIC X.                                   
050702     EJECT                                                                
050802**---PCB'ER FÖR SUBMODUL W413AVSO                                         
050902                                                                          
051002*01  -COPY W0008      -PRE WDE6-                                          
051102     05  FILLER                  PIC X.                                   
051202     SKIP2                                                                
051302*01  -COPY W0008      -PRE ORQA1-                                         
051402     05  FILLER                  PIC X.                                   
051502     EJECT                                                                
051602                                                                          
051702*01  -COPY W0008      -PRE WDQ21-                                         
051802     05  FILLER                  PIC X.                                   
051902     SKIP2                                                                
052002*01  -COPY W0008      -PRE GMTB1-                                         
052102     05  FILLER                  PIC X.                                   
052202     SKIP2                                                                
052302*01  -COPY W0008      -PRE XXKA-                                          
052402     05  FILLER                  PIC X.                                   
052502     EJECT                                                                
052602                                                                          
052702*01  -COPY W0008      -PRE 4437-                                          
052802     05  FILLER                  PIC X.                                   
052902     SKIP2                                                                
053002*01  -COPY W0008      -PRE XXKE-                                          
053102     05  FILLER                  PIC X.                                   
053202     EJECT                                                                
053302                                                                          
053402*01  -COPY W0008      -PRE XXKF-                                          
053502     05  FILLER                  PIC X.                                   
053602     SKIP2                                                                
053702*01  -COPY W0008      -PRE XXKG-                                          
053802     05  FILLER                  PIC X.                                   
053902     EJECT                                                                
054002                                                                          
054102*01  -COPY W0008      -PRE XXKH-                                          
054202     05  FILLER                  PIC X.                                   
054302     SKIP2                                                                
054402*01  -COPY W0008      -PRE XXKI-                                          
054502     05  FILLER                  PIC X.                                   
054602     EJECT                                                                
054702                                                                          
054802*01  -COPY W0008      -PRE XXKP-                                          
054902     05  FILLER                  PIC X.                                   
055002     EJECT                                                                
055102                                                                          
055202*01  -COPY W0008      -PRE AVSO-WDB2-                                     
055302     05  FILLER                  PIC X.                                   
055402     EJECT                                                                
055502                                                                          
055602*01  -COPY W0008      -PRE AVSO-WDB6-                                     
055702     05  FILLER                  PIC X.                                   
055802     EJECT                                                                
055902*01  -COPY W0008      -PRE TRAN-XXKB-                                     
056002     05  FILLER                  PIC X.                                   
056102     EJECT                                                                
056202 01  ORDN-ORQL-PCB               PIC X.                                   
056302 01  ORDN-PROC-PCB               PIC X.                                   
056402 01  ORDN-ORQI-PCB               PIC X.                                   
056502 01  ORDN-WDQ3-PCB               PIC X.                                   
056602*01  -COPY W0008      -PRE WDB6-                                          
056702     05  FILLER                  PIC X.                                   
056802     EJECT                                                                
056902                                                                          
057002 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB USEA-PCB ORQA-PCB              
057102                           WDQ2-PCB ORQK-PCB ORQL-PCB                     
057202                           WDB2-PCB WDB1-PCB WDG2-PCB                     
057302                           WDE6-PCB ORQA1-PCB WDQ21-PCB                   
057402                           GMTB1-PCB XXKA-PCB 4437-PCB XXKE-PCB           
057502                           XXKF-PCB XXKG-PCB XXKH-PCB                     
057602                           XXKI-PCB XXKP-PCB AVSO-WDB2-PCB                
057702                           AVSO-WDB6-PCB TRAN-XXKB-PCB                    
057802                           ORDN-ORQL-PCB ORDN-PROC-PCB                    
057902                           ORDN-ORQI-PCB ORDN-WDQ3-PCB                    
058002                           WDB6-PCB.                                      
058102                                                                          
058202     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB ORQA-PCB              
058302                           WDQ2-PCB ORQK-PCB ORQL-PCB                     
058402                           WDB2-PCB WDB1-PCB WDG2-PCB                     
058502                           WDE6-PCB ORQA1-PCB WDQ21-PCB                   
058602                           GMTB1-PCB XXKA-PCB 4437-PCB XXKE-PCB           
058702                           XXKF-PCB XXKG-PCB XXKH-PCB                     
058802                           XXKI-PCB XXKP-PCB AVSO-WDB2-PCB                
058902                           AVSO-WDB6-PCB TRAN-XXKB-PCB                    
059002                           ORDN-ORQL-PCB ORDN-PROC-PCB                    
059102                           ORDN-ORQI-PCB ORDN-WDQ3-PCB                    
059202                           WDB6-PCB.                                      
059302                                                                          
059402     PERFORM IMS-GET-MSG                                                  
059502     IF SEGMENT-FINNS                                                     
059602       PERFORM A-INIT                                                     
059702       PERFORM C-KOLLA-NYCKLAR                                            
059802       IF NYCKLAR-OK                                                      
059902         IF MFS-UPDATE                                                    
060002           PERFORM H-KOLLA-INPUT                                          
060102           IF INDATA-OK                                                   
060202             PERFORM I-UPPDATERA                                          
060302           END-IF                                                         
060402         ELSE                                                             
060502           IF MFS-FIRST                                                   
060602             PERFORM D-FOERSTA-SIDA                                       
060702           ELSE                                                           
060802             IF MFS-NEXT                                                  
060902               PERFORM E-NAESTA-SIDA                                      
061002             ELSE                                                         
061102               PERFORM F-SAMMA-SIDA                                       
061202             END-IF                                                       
061302           END-IF                                                         
061402         END-IF                                                           
061502         IF ALLT-OK AND INDATA-OK                                         
061602           PERFORM G-LAES-VISA-INFO                                       
061702           PERFORM J-FLYTTA-TOT-TILL-MOD                                  
061802         END-IF                                                           
061902       END-IF                                                             
062002       PERFORM IMS-INSERT-MSG                                             
062102     END-IF                                                               
062202                                                                          
062302     MOVE ZERO TO RETURN-CODE                                             
062402     GOBACK                                                               
062502     .                                                                    
062602     EJECT                                                                
062702                                                                          
062802 A-INIT SECTION.                                                          
062902                                                                          
063002     MOVE 'IDAG  '                   TO  DAT-KDDATFORM                    
063102                                                                          
063202     CALL WDATKONV USING             DAT-KDDATFORM                        
063302                                     DAT-I-TIDATUM                        
063402                                     DAT-O-TIDATUM                        
063502                                     DAT-KDSVAR                           
063602     MOVE DAT-TIAAMMDD (1:2)         TO  W-TIAA                           
063702     MOVE DAT-TIVV                   TO  WS-TIVV                          
063802                                                                          
063902     MOVE SPACE                      TO  W-SPAR-KDTRPKAT                  
064002     MOVE NEJ                        TO  TITRPAVG-IFYLLT-SW               
064102                                         IDTRP-IFYLLT-SW                  
064202                                         FORSTA-WDQ2-LAST-SW              
064302                                         ODEL-FINNS-SW                    
064402                                         RADER-FINNS-SW                   
064502     MOVE JA                         TO  INDATA-SW                        
064602                                         TID-SW                           
064702     MOVE ZERO                       TO  W-KDFRAKT                        
064802                                                                          
064902     IF MSG-DUBBLA-TRANSKODER                                             
065002       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I20501                 
065102       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
065202       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
065302     ELSE                                                                 
065402       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W4I20501                 
065502       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
065602       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
065702     END-IF                                                               
065802                                                                          
065902     MOVE MSG-KDTRTYP                     TO MFS-KDTRTYP                  
066002     MOVE MSG-IDPFK                       TO MFS-IDPFK                    
066102     MOVE MFS-IDTRANS                     TO W-IDTRANS                    
066202                                                                          
066302     MOVE LOW-VALUE                       TO MSG-AREA                     
066402     MOVE 'W4O205N1'                      TO MFS-IDMOD                    
066502     MOVE '4205'                          TO MOD-IDTRANS                  
066602     MOVE MFS-RENSA-FAELT                 TO MOD-TEMFSFEL                 
066702                                             MOD-TEMFSINF                 
066802                                                                          
066902      MOVE MAX-MOD4205-LAENGD             TO MSG-KVLL                     
067002     IF NOT EGEN-MID                                                      
067102       MOVE SPACE                         TO  MFS-KDTRTYP                 
067202       MOVE '7'                           TO  MFS-IDPFK                   
067302     END-IF                                                               
067402                                                                          
067502     IF ENGLISH-TEXT                                                      
067602       MOVE +2                            TO SPRAK-IX                     
067702       MOVE 'GB '                         TO MED-IDSKYLT                  
067802     ELSE                                                                 
067902       MOVE +1                            TO SPRAK-IX                     
068002       MOVE 'S  '                         TO MED-IDSKYLT                  
068102     END-IF                                                               
068202                                                                          
068302     MOVE JA                            TO LAS-BARA-EN-SIDA-SW            
068402     MOVE FUNCTION CURRENT-DATE (3:2)   TO W-DATE-AAMM(1:2)               
068502     MOVE FUNCTION CURRENT-DATE (5:2)   TO W-DATE-AAMM(3:2)               
068602     .                                                                    
068702     EJECT                                                                
068802                                                                          
068902 C-KOLLA-NYCKLAR SECTION.                                                 
069002                                                                          
069102     MOVE ALL '+'           TO MSGI-WMSGINIT                              
069202     MOVE '013'             TO MSGI-KDCALL                                
069302     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
069402     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
069502     MOVE '4205'            TO MSGI-IDTRANS                               
069602     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
069702     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
069802                                                                          
069902     MOVE JA                   TO NYCKLAR-SW                              
070002     MOVE LOW-VALUE            TO W-WDQ2B1KY-MIN-X                        
070102                                  W-DATRPAVT-MIN-X                        
070202                                  W-WDQ2CSEQ-MIN-X                        
070302                                  W-IDKUNDNR-MIN-X                        
070402                                  W-SEQC-IDKUNDRF-MIN-X                   
070502                                  W-WDQ301KY-MIN-X                        
070602                                                                          
070702     MOVE HIGH-VALUE           TO W-WDQ2B1KY-MAX-X                        
070802                                  W-DATRPAVT-MAX-X                        
070902                                  W-WDQ2CSEQ-MAX-X                        
071002                                  W-IDKUNDNR-MAX-X                        
071102                                  W-SEQC-IDKUNDRF-MAX-X                   
071202                                  W-WDQ301KY-MAX-X                        
071302                                                                          
071402     MOVE MFS-RENSA-FAELT      TO MOD-IDDC-IN                             
071502                                  MOD-IDTRP-IN                            
071602                                  MOD-TITRPAVG-UPDATE                     
071702                                  MOD-IDDISTR-IN                          
071802                                  MOD-IDKUNDNR-IN                         
071902                                  MOD-KDFRAKT-IN                          
072002                                  MOD-IDORDNR7-IN                         
072102                                  MOD-TITRPAVG-UPDATE                     
072202                                  MOD-TIRFS-UPDATE                        
072302                                                                          
072402     PERFORM CA-KOLLA-IDDC                                                
072502     PERFORM CB-KOLLA-WDQ2B1-NYCKEL                                       
072602     PERFORM CC-KOLLA-WDQ2C1-NYCKEL                                       
072702                                                                          
072802     IF NYCKLAR-FEL                                                       
072902         MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                               
073002         CALL WMEDKONV USING MED-WMEDAREA                                 
073102         MOVE MED-MFSFEL       TO MOD-TEMFSFEL                            
073202         PERFORM MFS-RENSA-FAELT-IN                                       
073302         PERFORM MFS-RENSA-FAELT-UT                                       
073402     END-IF                                                               
073502     .                                                                    
073602     EJECT                                                                
073702                                                                          
073802 CA-KOLLA-IDDC       SECTION.                                             
073902                                                                          
074002     IF MID-IDDC-IN           NOT  = ALL '+'                              
074102         MOVE MID-IDDC-IN     TO W-IDDC-B6                                
074202         MOVE '7'              TO MFS-IDPFK                               
074302         MOVE NEJ              TO LAS-BARA-EN-SIDA-SW                     
074402         MOVE SPACE            TO MFS-KDTRTYP                             
074502     ELSE                                                                 
074602         MOVE MID-IDDC-UT      TO W-IDDC-B6                               
074702     END-IF                                                               
074802     PERFORM IMS-GU-WDB601                                                
074902                                                                          
075002     IF DCS-KDDC = SPACE OR DCS-DDC                                       
075102        MOVE MSGI-IDDC         TO W-IDDC-B6                               
075202        PERFORM IMS-GU-WDB601                                             
075302        MOVE NEJ               TO LAS-BARA-EN-SIDA-SW                     
075402     END-IF                                                               
075502                                                                          
075602     MOVE DCS-IDDC             TO MOD-IDDC-UT                             
075702                                  W-IDDC                                  
075802                                  W-Q2B1KY-IDDC                           
075902                                  W-Q2B1KY-IDDC-MIN                       
076002                                  W-Q2B1KY-IDDC-MAX                       
076102                                  W-Q301KY-IDDC-MIN                       
076202                                  W-Q301KY-IDDC-MAX                       
076302                                  W-Q301KY-IDDC                           
076402                                                                          
076502     IF GODK-MID OR NYCKLAR-OK                                            
076602         MOVE WS-TITRPAVG      TO MOD-TITRPAVG-UT                         
076702         INSPECT MOD-TITRPAVG-UT  REPLACING LEADING ZERO BY SPACE         
076802     ELSE                                                                 
076902         MOVE MFS-RENSA-FAELT  TO MOD-TITRPAVG-UT                         
077002     END-IF                                                               
077102     .                                                                    
077202     EJECT                                                                
077302                                                                          
077402 CB-KOLLA-WDQ2B1-NYCKEL   SECTION.                                        
077502                                                                          
077602     PERFORM CBA-KOLLA-TRANSPORT-ID                                       
077702     PERFORM CBB-KOLLA-AVGANGSTID                                         
077802                                                                          
077902     IF GODK-MID OR NYCKLAR-OK                                            
078002         MOVE WS-IDTRP         TO MOD-IDTRP-UT                            
078102         INSPECT MOD-IDTRP-UT  REPLACING LEADING ZERO BY SPACE            
078202     ELSE                                                                 
078302         MOVE MFS-RENSA-FAELT  TO MOD-IDTRP-UT                            
078402     END-IF                                                               
078502     .                                                                    
078602     EJECT                                                                
078702                                                                          
078802 CBA-KOLLA-TRANSPORT-ID  SECTION.                                         
078902                                                                          
079002     IF MID-IDTRP-IN           = ALL '+'                                  
079102         MOVE MID-IDTRP-UT     TO WS-IDTRP                                
079202         INSPECT WS-IDTRP   REPLACING LEADING SPACE BY ZERO               
079302     ELSE                                                                 
079402         MOVE MID-IDTRP-IN     TO WS-IDTRP                                
079502         INSPECT WS-IDTRP   REPLACING LEADING SPACE BY ZERO               
079602         MOVE '7'              TO MFS-IDPFK                               
079702         MOVE NEJ              TO LAS-BARA-EN-SIDA-SW                     
079802         MOVE SPACE            TO MFS-KDTRTYP                             
079902     END-IF                                                               
080002                                                                          
080102     IF WS-IDTRP NUMERIC AND WS-IDTRP > ZERO                              
080202         MOVE JA               TO IDTRP-IFYLLT-SW                         
080302         MOVE WS-IDTRP         TO W-Q2B1KY-IDTRP                          
080402         MOVE WS-IDTRP         TO W-Q2B1KY-IDTRP-MIN                      
080502         MOVE WS-IDTRP         TO W-Q2B1KY-IDTRP-MAX                      
080602     END-IF                                                               
080702     .                                                                    
080802     EJECT                                                                
080902                                                                          
081002 CBB-KOLLA-AVGANGSTID    SECTION.                                         
081102                                                                          
081202     IF MID-TITRPAVG-IN        = ALL '+'                                  
081302         MOVE MID-TITRPAVG-UT  TO WS-TITRPAVG                             
081402         INSPECT WS-TITRPAVG REPLACING LEADING SPACE BY ZERO              
081502     ELSE                                                                 
081602         MOVE MID-TITRPAVG-IN  TO WS-TITRPAVG                             
081702         INSPECT WS-TITRPAVG REPLACING LEADING SPACE BY ZERO              
081802         MOVE '7'              TO MFS-IDPFK                               
081902         MOVE NEJ              TO LAS-BARA-EN-SIDA-SW                     
082002         MOVE SPACE            TO MFS-KDTRTYP                             
082102     END-IF                                                               
082202                                                                          
082302     IF WS-TITRPAVG            =  ZERO                                    
082402         MOVE LOW-VALUE        TO W-DATRPAVT-MIN-X                        
082502         MOVE HIGH-VALUE       TO W-DATRPAVT-MAX-X                        
082602      ELSE                                                                
082702         IF WS-TITRPAVG NUMERIC AND WS-TITRPAVG > ZERO                    
082802             MOVE WS-TITRPAVG        TO  W-VVDHHMM                        
082902             MOVE WS-TITRPAVG (1:3)  TO  W-TIVVD                          
083002             MOVE WS-TITRPAVG (4:4)  TO  W-TIHHMM                         
083102                                     IN  W-TITRPAVT                       
083202             MOVE WS-TITRPAVG (4:4)  TO  W-TIHHMM                         
083302                                     IN  W-DATRPAVT                       
083402             MOVE '20'               TO  W-DATRPAVT(1:2)                  
083502             PERFORM S05-KONTR-KONV-TITRPAVG                              
083602                                                                          
083702             IF TID-OK                                                    
083802                 MOVE W-DATRPAVT TO W-DATRPAVT-MIN-X                      
083902                                    W-DATRPAVT-MAX-X                      
084002              ELSE                                                        
084102                 MOVE NEJ      TO NYCKLAR-SW                              
084202             END-IF                                                       
084302          ELSE                                                            
084402             MOVE NEJ          TO NYCKLAR-SW                              
084502         END-IF                                                           
084602     END-IF                                                               
084702     .                                                                    
084802     EJECT                                                                
084902                                                                          
085002 CC-KOLLA-WDQ2C1-NYCKEL   SECTION.                                        
085102                                                                          
085202     PERFORM CCA-KOLLA-KUNDNUMMER                                         
085302     PERFORM CCB-KOLLA-FRAKTKOD                                           
085402     PERFORM CCC-KOLLA-ORDERNUMMER                                        
085502     PERFORM CCD-KOLLA-DISTRIKT                                           
085602                                                                          
085702     IF GODK-MID OR NYCKLAR-OK                                            
085802         MOVE WS-IDKUNDNR      TO MOD-IDKUNDNR-UT                         
085902         INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE          
086002         IF MOD-IDKUNDNR-UT    =  SPACE                                   
086102             MOVE '     0'     TO MOD-IDKUNDNR-UT                         
086202         END-IF                                                           
086302         MOVE WS-KDFRAKT       TO MOD-KDFRAKT-UT                          
086402         INSPECT MOD-KDFRAKT-UT REPLACING LEADING ZERO BY SPACE           
086502         MOVE WS-IDORDNR7      TO MOD-IDORDNR7-UT                         
086602         INSPECT MOD-IDORDNR7-UT REPLACING LEADING ZERO BY SPACE          
086702         MOVE WS-IDDISTR       TO MOD-IDDISTR-UT                          
086802         INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE           
086902     ELSE                                                                 
087002         MOVE MFS-RENSA-FAELT  TO MOD-IDKUNDNR-UT                         
087102                                  MOD-KDFRAKT-UT                          
087202                                  MOD-IDORDNR7-UT                         
087302                                  MOD-IDDISTR-UT                          
087402     END-IF                                                               
087502     .                                                                    
087602     EJECT                                                                
087702                                                                          
087802 CCA-KOLLA-KUNDNUMMER    SECTION.                                         
087902                                                                          
088002     IF MID-IDKUNDNR-IN        = ALL '+'                                  
088102         MOVE MID-IDKUNDNR-UT  TO WS-IDKUNDNR                             
088202         INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO              
088302     ELSE                                                                 
088402         MOVE MID-IDKUNDNR-IN  TO WS-IDKUNDNR                             
088502         IF WS-IDKUNDNR        =  SPACE                                   
088602             CONTINUE                                                     
088702          ELSE                                                            
088802             INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO          
088902         END-IF                                                           
089002         MOVE '7'              TO MFS-IDPFK                               
089102         MOVE NEJ              TO LAS-BARA-EN-SIDA-SW                     
089202         MOVE SPACE            TO MFS-KDTRTYP                             
089302     END-IF                                                               
089402                                                                          
089502     IF WS-IDKUNDNR   NUMERIC                                             
089602         MOVE WS-IDKUNDNR      TO W-Q2CSEQ-IDKUNDNR-MIN                   
089702                                  W-Q2CSEQ-IDKUNDNR-MAX                   
089802      ELSE                                                                
089902         MOVE ZERO             TO W-Q2CSEQ-IDKUNDNR-MIN                   
090002                                  W-Q2CSEQ-IDKUNDNR-MAX                   
090102     END-IF                                                               
090202     .                                                                    
090302     EJECT                                                                
090402                                                                          
090502 CCB-KOLLA-FRAKTKOD      SECTION.                                         
090602                                                                          
090702     IF MID-KDFRAKT-IN         = ALL '+'                                  
090802         MOVE MID-KDFRAKT-UT   TO WS-KDFRAKT                              
090902         INSPECT WS-KDFRAKT REPLACING LEADING SPACE BY ZERO               
091002     ELSE                                                                 
091102         MOVE MID-KDFRAKT-IN   TO WS-KDFRAKT                              
091202         INSPECT WS-KDFRAKT REPLACING LEADING SPACE BY ZERO               
091302         MOVE '7'              TO MFS-IDPFK                               
091402         MOVE NEJ              TO LAS-BARA-EN-SIDA-SW                     
091502         MOVE SPACE            TO MFS-KDTRTYP                             
091602     END-IF                                                               
091702                                                                          
091802     IF WS-KDFRAKT    NUMERIC AND WS-KDFRAKT > 0                          
091902         MOVE WS-KDFRAKT       TO W-KDFRAKT                               
092002      ELSE                                                                
092102         MOVE ZERO             TO W-KDFRAKT                               
092202     END-IF                                                               
092302     .                                                                    
092402     EJECT                                                                
092502                                                                          
092602 CCC-KOLLA-ORDERNUMMER   SECTION.                                         
092702                                                                          
092802     MOVE SPACE                TO W-Q2CSEQ-IDKUNDRF-MIN                   
092902                                  W-Q2CSEQ-IDKUNDRF-MAX                   
093002                                                                          
093102     IF MID-IDORDNR7-IN        = ALL '+'                                  
093202         MOVE MID-IDORDNR7-UT  TO WS-IDORDNR7                             
093302         INSPECT WS-IDORDNR7   REPLACING LEADING SPACE BY ZERO            
093402     ELSE                                                                 
093502         MOVE MID-IDORDNR7-IN  TO WS-IDORDNR7                             
093602         INSPECT WS-IDORDNR7   REPLACING LEADING SPACE BY ZERO            
093702         MOVE '7'              TO MFS-IDPFK                               
093802         MOVE NEJ              TO LAS-BARA-EN-SIDA-SW                     
093902         MOVE SPACE            TO MFS-KDTRTYP                             
094002     END-IF                                                               
094102                                                                          
094202     IF WS-IDORDNR7   NUMERIC AND  WS-IDORDNR7 > ZERO                     
094302         MOVE SPACE            TO W-Q2CSEQ-IDKUNDRF-MIN                   
094402                                  W-Q2CSEQ-IDKUNDRF-MAX                   
094502         MOVE WS-IDORDNR7      TO W-Q2CSEQ-IDORDNR7-MIN                   
094602                                  W-Q2CSEQ-IDORDNR7-MAX                   
094702      ELSE                                                                
094802         MOVE LOW-VALUE        TO W-Q2CSEQ-IDKUNDRF-MIN                   
094902         MOVE HIGH-VALUE       TO W-Q2CSEQ-IDKUNDRF-MAX                   
095002     END-IF                                                               
095102                                                                          
095202     .                                                                    
095302     EJECT                                                                
095402                                                                          
095502 CCD-KOLLA-DISTRIKT     SECTION.                                          
095602                                                                          
095702     IF MID-IDDISTR-IN         = ALL '+'                                  
095802         MOVE MID-IDDISTR-UT   TO WS-IDDISTR                              
095902         INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO               
096002     ELSE                                                                 
096102         MOVE MID-IDDISTR-IN   TO WS-IDDISTR                              
096202         INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO               
096302         MOVE '7'              TO MFS-IDPFK                               
096402         MOVE NEJ              TO LAS-BARA-EN-SIDA-SW                     
096502         MOVE SPACE            TO MFS-KDTRTYP                             
096602     END-IF                                                               
096702                                                                          
096802     IF WS-IDDISTR  NUMERIC  AND WS-IDDISTR > ZERO                        
096902         IF WS-IDTRP           > ZERO                                     
097002             MOVE NEJ          TO NYCKLAR-SW                              
097102          ELSE                                                            
097202             MOVE WS-IDDISTR   TO W-Q2CSEQ-IDDISTR-MIN                    
097302                                  W-Q2CSEQ-IDDISTR-MAX                    
097402                                  W-Q2CSEQ-IDDISTR                        
097502         END-IF                                                           
097602      ELSE                                                                
097702         IF IDTRP-IFYLLT                                                  
097802             IF WS-IDDISTR     > ZERO                                     
097902                 MOVE NEJ      TO NYCKLAR-SW                              
098002             END-IF                                                       
098102          ELSE                                                            
098202             MOVE NEJ              TO NYCKLAR-SW                          
098302         END-IF                                                           
098402     END-IF                                                               
098502     .                                                                    
098602     EJECT                                                                
098702                                                                          
098802 D-FOERSTA-SIDA SECTION.                                                  
098902                                                                          
099002     MOVE INF-FIRST-PAGE       TO MED-IDMFSINF                            
099102     CALL WMEDKONV USING MED-WMEDAREA                                     
099202     MOVE MED-MFSINF           TO MOD-TEMFSFEL                            
099302                                                                          
099402*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
099502     MOVE ZERO                 TO MOD-TIAAMMDD-ENTER                      
099602                                  MOD-TIHHMM-ENTER                        
099702                                  MOD-TIAAMMDD-NEXT                       
099802                                  MOD-TIHHMM-NEXT                         
099902                                  MOD-IDORDER-ENTER                       
100002                                  MOD-IDORDER-NEXT                        
100102                                  MOD-IDORDNR7-ENTER                      
100202                                  MOD-IDORDNR7-NEXT                       
100302                                  MOD-IDPRODNR-ENTER                      
100402                                  MOD-IDPRODNR-NEXT                       
100502                                  MOD-IDPLKLST-ENTER                      
100602                                  MOD-IDPLKLST-NEXT                       
100702     MOVE SPACE                TO MOD-IDKUNDNR-ENTER                      
100802                                  MOD-IDKUNDNR-NEXT                       
100902     MOVE JA                   TO ALLT-SW                                 
101002     .                                                                    
101102     EJECT                                                                
101202                                                                          
101302 E-NAESTA-SIDA SECTION.                                                   
101402                                                                          
101502     MOVE MID-TIAAMMDD-NEXT    TO  W-Q2B1KY-DATRPAVD                      
101602     IF MID-TIAAMMDD-NEXT NOT = ZERO                                      
101702       IF MID-TIAAMMDD-NEXT < 500000                                      
101802         MOVE 20               TO  W-Q2B1KY-DATRPAVD (1:2)                
101902       ELSE                                                               
102002         IF MID-TIAAMMDD-NEXT < 999999                                    
102102           MOVE 19             TO  W-Q2B1KY-DATRPAVD (1:2)                
102202         ELSE                                                             
102302           MOVE 99999999       TO  W-Q2B1KY-DATRPAVD                      
102402         END-IF                                                           
102502       END-IF                                                             
102602     END-IF                                                               
102702     MOVE MID-TIHHMM-NEXT      TO  W-Q2B1KY-TIHHMM                        
102802                                                                          
102902     MOVE MID-IDORDER-NEXT     TO W-Q2B1KY-IDORDER                        
103002     MOVE MID-IDKUNDNR-NEXT    TO W-Q2CSEQ-IDKUNDNR                       
103102     MOVE SPACE                TO W-Q2CSEQ-IDKUNDRF                       
103202     MOVE MID-IDORDNR7-NEXT    TO W-Q2CSEQ-IDORDNR7                       
103302     MOVE JA                   TO ALLT-SW                                 
103402     .                                                                    
103502     EJECT                                                                
103602                                                                          
103702 F-SAMMA-SIDA SECTION.                                                    
103802                                                                          
103902     IF MID-INPUT                  = ALL '+'                              
104002       MOVE MID-TIAAMMDD-ENTER   TO W-Q2B1KY-DATRPAVD                     
104102       IF MID-TIAAMMDD-ENTER NOT = ZERO                                   
104202         IF MID-TIAAMMDD-ENTER < 500000                                   
104302           MOVE 20                 TO  W-Q2B1KY-DATRPAVD (1:2)            
104402         ELSE                                                             
104502           IF MID-TIAAMMDD-ENTER < 999999                                 
104602             MOVE 19               TO  W-Q2B1KY-DATRPAVD (1:2)            
104702           ELSE                                                           
104802             MOVE 99999999         TO  W-Q2B1KY-DATRPAVD                  
104902           END-IF                                                         
105002         END-IF                                                           
105102       END-IF                                                             
105202         MOVE MID-TIHHMM-ENTER     TO W-Q2B1KY-TIHHMM                     
105302         MOVE MID-IDORDER-ENTER    TO W-Q2B1KY-IDORDER                    
105402         MOVE MID-IDKUNDNR-ENTER   TO W-Q2CSEQ-IDKUNDNR                   
105502         MOVE SPACE                TO W-Q2CSEQ-IDKUNDRF                   
105602         MOVE MID-IDORDNR7-ENTER   TO W-Q2CSEQ-IDORDNR7                   
105702         MOVE JA                   TO ALLT-SW                             
105802      ELSE                                                                
105902         MOVE NEJ                  TO ALLT-SW                             
106002         MOVE INF-PRESS-PF11       TO MED-IDMFSINF                        
106102         CALL WMEDKONV USING MED-WMEDAREA                                 
106202         MOVE MED-MFSINF           TO MOD-TEMFSINF                        
106302         PERFORM MFS-ROR-EJ-FAELT-IN                                      
106402         PERFORM MFS-ROR-EJ-FAELT-UT                                      
106502         PERFORM MFS-LAS-IN-IGEN                                          
106602     END-IF                                                               
106702     .                                                                    
106802     EJECT                                                                
106902                                                                          
107002 G-LAES-VISA-INFO SECTION.                                                
107102                                                                          
107202     IF IDTRP-IFYLLT                                                      
107302         PERFORM  S01-LAES-OHUV-VIA-WDQ2B1KY                              
107402      ELSE                                                                
107502         PERFORM  S02-LAES-OHUV-VIA-WDQ2CSEQ                              
107602     END-IF                                                               
107702                                                                          
107802     IF SEGMENT-FINNS                                                     
107902         MOVE +1 TO INDX                                                  
108002         PERFORM GA-SPARA-NYCKEL-ENTER                                    
108102         MOVE OHUV-IDDISTR      TO TEST-IDDISTR                           
108202         IF DIST79-DEALER-PRICE                                           
108302           IF ENGLISH-TEXT                                                
108402             MOVE 'DEALPRICE'     TO MOD-TEDDI                            
108502           ELSE                                                           
108602             MOVE ' ÅF PRIS'      TO MOD-TEDDI                            
108702           END-IF                                                         
108802         ELSE                                                             
108902           MOVE ' '             TO MOD-TEDDI                              
109002         END-IF                                                           
109102**                                                                        
109202** ANTINGEN SKALL HELA BASEN LÄSAS ELLER OCKSÅ                            
109302** BARA EN SIDA, MEN MAN MÅSTE ÄVEN DÅ KOLLA SÅ ATT                       
109402** BASEN EJ TAR SLUT                                                      
109502**                                                                        
109602         PERFORM UNTIL (SEGMENT-SAKNAS OR END-OF-DATA) OR                 
109702                       ((LAS-BARA-EN-SIDA        AND                      
109802                         INDX      >  MAX-INDX-PLUS-ETT) OR               
109902                        SEGMENT-SAKNAS OR END-OF-DATA)                    
110002             IF (W-KDFRAKT > ZERO AND                                     
110102                 W-KDFRAKT NOT = ARB-KDFRAKT)                             
110202                  CONTINUE                                                
110302              ELSE                                                        
110402                 PERFORM GB-KOLLA-OM-RADER-FINNS                          
110502                 IF OHUV-FLKLAR = JA AND OHUV-FLBORT = NEJ AND            
110602                    RADER-FINNS                                           
110702                     MOVE NEJ            TO ODEL-FINNS-SW                 
110802                     MOVE OHUV-IDDISTR   TO W-IDDISTR                     
110902                                            W-IDDISTR-WDB2-MIN            
111002                                            W-IDDISTR-WDB2-MAX            
111102                                            TEST-IDDISTR                  
111202                     MOVE OHUV-IDKUNDNR  TO W-IDKUNDNR                    
111302                     PERFORM IMS-GET-ORQA-ODEL-OKVAL-GU                   
111402                     IF SEGMENT-FINNS                                     
111502                         MOVE JA  TO ODEL-FINNS-SW                        
111602                     END-IF                                               
111702                     IF INDX      <  MAX-INDX-PLUS-ETT                    
111802                         PERFORM GC-FIXA-NYCKEL-NEXT                      
111902                     END-IF                                               
112002                     PERFORM UNTIL SEGMENT-SAKNAS OR END-OF-DATA          
112102                                OR ODEL-KDODELSTA NOT = 'R'               
112202                          IF INDX =  MAX-INDX-PLUS-ETT                    
112302                              PERFORM GD-SPARA-NYCKEL-NEXT                
112402                              ADD +1      TO  INDX                        
112502                          END-IF                                          
112602                          PERFORM GE-ADDERA-SID-TOTAL                     
112702                          IF INDX <  MAX-INDX-PLUS-ETT                    
112802                               PERFORM GF-ADDERA-RAD-TOTAL                
112902                          END-IF                                          
113002                          PERFORM IMS-GET-ORQA-ODEL-OKVAL-GN              
113102                     END-PERFORM                                          
113202                     IF ODEL-SAKNAS                                       
113302                         PERFORM GG-BEHANDLA-ARB-SUMMOR                   
113402                         IF DIRL-RADER-FINNS                              
113502                             PERFORM GH-BEHANDLA-DIRL-SUMMOR              
113602                         END-IF                                           
113702                     END-IF                                               
113802                     IF ODEL-KDODELSTA =    'R' OR ODEL-SAKNAS            
113902                         IF INDX         <   MAX-INDX-PLUS-ETT            
114002                             PERFORM GI-REDIGERA-RAD                      
114102                             ADD +1      TO  INDX                         
114202                             PERFORM S10-NOLLSTALL-RAD-TOTAL              
114302                         END-IF                                           
114402                         IF MFS-UPDATE                                    
114502                             CONTINUE                                     
114602                          ELSE                                            
114702                             IF INDX  =   MAX-INDX-PLUS-TVA               
114802                                 MOVE '105' TO MED-IDMFSINF               
114902                                 CALL WMEDKONV USING MED-WMEDAREA         
115002                                 MOVE MED-TEMFSINF TO MOD-TEMFSINF        
115102                                 ADD +1  TO  INDX                         
115202                             END-IF                                       
115302                         END-IF                                           
115402                     ELSE                                                 
115502                         PERFORM GJ-SUBTRAHERA-SID-TOTAL                  
115602                         PERFORM S10-NOLLSTALL-RAD-TOTAL                  
115702                     END-IF                                               
115802                     PERFORM GK-REDIGERA-TOT                              
115902                 END-IF                                                   
116002             END-IF                                                       
116102             IF IDTRP-IFYLLT                                              
116202                  PERFORM  S01-LAES-OHUV-VIA-WDQ2B1KY                     
116302                ELSE                                                      
116402                  PERFORM  S02-LAES-OHUV-VIA-WDQ2CSEQ                     
116502             END-IF                                                       
116602         END-PERFORM                                                      
116702         IF INDX               =  +1                                      
116802             MOVE '005'        TO MED-IDMFSFEL                            
116902             CALL WMEDKONV USING MED-WMEDAREA                             
117002             MOVE MED-MFSFEL TO MOD-TEMFSFEL                              
117102             PERFORM MFS-RENSA-FAELT-UT                                   
117202         END-IF                                                           
117302     ELSE                                                                 
117402         IF MFS-NEXT                                                      
117502             MOVE '056'        TO MED-IDMFSFEL                            
117602             CALL WMEDKONV USING MED-WMEDAREA                             
117702             MOVE MED-MFSFEL   TO MOD-TEMFSFEL                            
117802             PERFORM MFS-RENSA-FAELT-UT                                   
117902          ELSE                                                            
118002             MOVE '005'        TO MED-IDMFSFEL                            
118102             CALL WMEDKONV USING MED-WMEDAREA                             
118202             MOVE MED-MFSFEL   TO MOD-TEMFSFEL                            
118302             PERFORM MFS-RENSA-FAELT-UT                                   
118402         END-IF                                                           
118502     END-IF                                                               
118602                                                                          
118702     PERFORM MFS-RENSA-FAELT-IN                                           
118802     .                                                                    
118902     EJECT                                                                
119002                                                                          
119102 GA-SPARA-NYCKEL-ENTER          SECTION.                                  
119202     IF IDTRP-IFYLLT                                                      
119302        PERFORM GAA-SPAR-Q2B-Q301-NYCKEL-ENTER                            
119402     ELSE                                                                 
119502        PERFORM GAB-SPAR-Q2C-Q301-NYCKEL-ENTER                            
119602     END-IF                                                               
119702     .                                                                    
119802     EJECT                                                                
119902                                                                          
120002 GAA-SPAR-Q2B-Q301-NYCKEL-ENTER  SECTION.                                 
120102                                                                          
120202     MOVE ARB-DATRPAVD (3:6)   TO MOD-TIAAMMDD-ENTER                      
120302     MOVE ARB-TIHHMM           TO MOD-TIHHMM-ENTER                        
120402     MOVE OHUV-IDORDER         TO MOD-IDORDER-ENTER                       
120502     .                                                                    
120602     EJECT                                                                
120702                                                                          
120802 GAB-SPAR-Q2C-Q301-NYCKEL-ENTER  SECTION.                                 
120902                                                                          
121002     MOVE OHUV-IDORDER         TO MOD-IDORDER-ENTER                       
121102     MOVE OHUV-IDKUNDNR        TO MOD-IDKUNDNR-ENTER                      
121202     MOVE W-MOD-IDORDNR7       TO MOD-IDORDNR7-ENTER                      
121302     .                                                                    
121402     EJECT                                                                
121502                                                                          
121602 GB-KOLLA-OM-RADER-FINNS        SECTION.                                  
121702                                                                          
121902     MOVE NEJ                  TO RADER-FINNS-SW                          
123100                                                                          
123200     MOVE ZERO                 TO W-DIRL-VLORDNTO                         
123300                                  W-DIRL-VKORDNTO                         
123400                                  W-DIRL-SUORDV                           
123500                                  W-DIRL-SUORDV-LOC                       
123600                                  W-DIRL-SUORDV-LOCPREL                   
123702                                                                          
123802* WITH THE IDORDER FROM S01 OR S02 ESTABLISHING PARENTAGE                 
123902* FOR WDQ2-PCB TO MAKE POSSIBLE GNP OF Q211 AND Q221                      
124002     PERFORM IMS-GHU-WDQ201                                               
124102     PERFORM IMS-GNP-WDQ211                                               
124202     PERFORM UNTIL SEGMENT-SAKNAS OR END-OF-DATA                          
124302         IF DIRL-KVRADER       >  ZERO                                    
124402             MOVE JA           TO RADER-FINNS-SW                          
124502                                  DIRL-RADER-FINNS-SW                     
124602             COMPUTE W-DIRL-VLORDNTO  =                                   
124702                     W-DIRL-VLORDNTO  +  DIRL-VLORDNTO                    
124802             COMPUTE W-DIRL-VKORDNTO  =                                   
124902                     W-DIRL-VKORDNTO  +  DIRL-VKORDNTO                    
125002             COMPUTE W-DIRL-SUORDV    =                                   
125102                     W-DIRL-SUORDV    +  DIRL-SUORDV                      
125202             COMPUTE W-DIRL-SUORDV-LOC     =                              
125302                     W-DIRL-SUORDV-LOC     + DIRL-SUORDV-LOC              
125402             COMPUTE W-DIRL-SUORDV-LOCPREL =                              
125502                     W-DIRL-SUORDV-LOCPREL + DIRL-SUORDV-LOCPREL          
125602             MOVE DIRL-KDVALISO  TO WS-KDVALISO                           
125702         END-IF                                                           
125802         PERFORM IMS-GNP-WDQ211                                           
125902     END-PERFORM                                                          
126002                                                                          
126104     MOVE NEJ                  TO Q221-FINNS-SW                           
126202     PERFORM IMS-GNP-WDQ221                                               
126302     IF SEGMENT-FINNS                                                     
126404        MOVE JA                TO RADER-FINNS-SW                          
126504                                  Q221-FINNS-SW                           
126604        MOVE LOR-KDVALISO      TO WS-KDVALISO                             
126702     END-IF                                                               
126802     .                                                                    
126902     EJECT                                                                
127002                                                                          
127102 GC-FIXA-NYCKEL-NEXT   SECTION.                                           
127202                                                                          
127302**   FIXA TILL NYCKLAR FÖR ATT VISA EN TOM SIDA                           
127402**   OM MAN TRYCKER PF8 PÅ SISTA SIDAN                                    
127502                                                                          
127602     MOVE 999999               TO MOD-TIAAMMDD-NEXT                       
127702     MOVE 9999                 TO MOD-TIHHMM-NEXT                         
127802     MOVE 9999999              TO MOD-IDORDER-NEXT                        
127902                                  MOD-IDPRODNR-NEXT                       
128002                                  MOD-IDPLKLST-NEXT                       
128102                                  MOD-IDORDNR7-NEXT                       
128202     MOVE HIGH-VALUE           TO MOD-IDKUNDNR-NEXT                       
128302     .                                                                    
128402     EJECT                                                                
128502                                                                          
128602 GD-SPARA-NYCKEL-NEXT   SECTION.                                          
128702     IF IDTRP-IFYLLT                                                      
128802         PERFORM GDA-SPAR-Q2B-Q301-NYCKEL-NEXT                            
128902      ELSE                                                                
129002         PERFORM GDB-SPAR-Q2C-Q301-NYCKEL-NEXT                            
129102     END-IF                                                               
129202     .                                                                    
129302     EJECT                                                                
129402                                                                          
129502 GDA-SPAR-Q2B-Q301-NYCKEL-NEXT   SECTION.                                 
129602                                                                          
129702     MOVE ARB-DATRPAVD (3:6)   TO MOD-TIAAMMDD-NEXT                       
129802     MOVE ARB-TIHHMM           TO MOD-TIHHMM-NEXT                         
129902     MOVE OHUV-IDORDER         TO MOD-IDORDER-NEXT                        
130002     MOVE OHUV-IDKUNDNR        TO MOD-IDKUNDNR-NEXT                       
130102     .                                                                    
130202     EJECT                                                                
130302                                                                          
130402 GDB-SPAR-Q2C-Q301-NYCKEL-NEXT   SECTION.                                 
130502                                                                          
130602     MOVE OHUV-IDORDER         TO MOD-IDORDER-NEXT                        
130702     MOVE OHUV-IDKUNDNR        TO MOD-IDKUNDNR-NEXT                       
130802     MOVE W-MOD-IDORDNR7       TO MOD-IDORDNR7-NEXT                       
130902     .                                                                    
131002     EJECT                                                                
131102                                                                          
131202 GE-ADDERA-SID-TOTAL     SECTION.                                         
131302                                                                          
131402     COMPUTE W-VLORDNTO-TOT ROUNDED   =  W-VLORDNTO-TOT +                 
131502                                         ODEL-VLORDNTO                    
131602     COMPUTE W-VKORDNTO-TOT ROUNDED   =  W-VKORDNTO-TOT +                 
131702                                         ODEL-VKORDNTO                    
131802     COMPUTE W-SUORDV-TOT   ROUNDED   =  W-SUORDV-TOT +                   
131902                                         ODEL-SUORDV                      
132002     COMPUTE W-SUORDV-LOC-TOT     ROUNDED =  W-SUORDV-LOC-TOT +           
132102                                         ODEL-SUORDV-LOC                  
132202     COMPUTE W-SUORDV-LOCPREL-TOT ROUNDED = W-SUORDV-LOCPREL-TOT +        
132302                                         ODEL-SUORDV-LOCPREL              
132402     .                                                                    
132502     EJECT                                                                
132602                                                                          
132702 GF-ADDERA-RAD-TOTAL     SECTION.                                         
132802                                                                          
132902     COMPUTE W-RAD-VLORDNTO-TOT ROUNDED = W-RAD-VLORDNTO-TOT +            
133002                                          ODEL-VLORDNTO                   
133102     COMPUTE W-RAD-VKORDNTO-TOT ROUNDED = W-RAD-VKORDNTO-TOT +            
133202                                          ODEL-VKORDNTO                   
133302     COMPUTE W-RAD-SUORDV-TOT   ROUNDED = W-RAD-SUORDV-TOT +              
133402                                          ODEL-SUORDV                     
133502     COMPUTE W-RAD-SUORDV-LOC-TOT ROUNDED = W-RAD-SUORDV-LOC-TOT +        
133602                                          ODEL-SUORDV-LOC                 
133702     COMPUTE W-RAD-SUORDV-LOCPREL-TOT ROUNDED =                           
133802             W-RAD-SUORDV-LOCPREL-TOT + ODEL-SUORDV-LOCPREL               
133902     .                                                                    
134002     EJECT                                                                
134102                                                                          
134202 GG-BEHANDLA-ARB-SUMMOR  SECTION.                                         
134302                                                                          
134402     IF Q221-FINNS                                                        
134502       PERFORM UNTIL SEGMENT-SAKNAS                                       
134602         IF INDX                        < MAX-INDX-PLUS-ETT               
134702           COMPUTE W-RAD-VLORDNTO-TOT ROUNDED =                           
134802                   W-RAD-VLORDNTO-TOT +  LOR-VLORDNTO                     
134902           COMPUTE W-RAD-VKORDNTO-TOT ROUNDED =                           
135002                   W-RAD-VKORDNTO-TOT +  LOR-VKORDNTO                     
135102           COMPUTE W-RAD-SUORDV-TOT ROUNDED =                             
135202                   W-RAD-SUORDV-TOT   +  LOR-SUORDV                       
135302           COMPUTE W-RAD-SUORDV-LOC-TOT ROUNDED =                         
135402                   W-RAD-SUORDV-LOC-TOT + LOR-SUORDV-LOC                  
135502           COMPUTE W-RAD-SUORDV-LOCPREL-TOT ROUNDED =                     
135602                   W-RAD-SUORDV-LOCPREL-TOT + LOR-SUORDV-LOCPREL          
135702         END-IF                                                           
135802                                                                          
135902         COMPUTE W-VLORDNTO-TOT ROUNDED =                                 
136002                 W-VLORDNTO-TOT +  LOR-VLORDNTO                           
136102         COMPUTE W-VKORDNTO-TOT ROUNDED =                                 
136202                 W-VKORDNTO-TOT +  LOR-VKORDNTO                           
136302         COMPUTE W-SUORDV-TOT ROUNDED   =                                 
136402                 W-SUORDV-TOT   +  LOR-SUORDV                             
136502         COMPUTE W-SUORDV-LOC-TOT ROUNDED   =                             
136602                 W-SUORDV-LOC-TOT   +  LOR-SUORDV-LOC                     
136702         COMPUTE W-SUORDV-LOCPREL-TOT ROUNDED   =                         
136802                 W-SUORDV-LOCPREL-TOT  + LOR-SUORDV-LOCPREL               
136902                                                                          
137302         PERFORM IMS-GNP-WDQ221                                           
137502       END-PERFORM                                                        
137602     END-IF                                                               
137700     .                                                                    
137800     EJECT                                                                
137900                                                                          
138000 GH-BEHANDLA-DIRL-SUMMOR   SECTION.                                       
138100                                                                          
138200     IF INDX                   <  MAX-INDX-PLUS-ETT                       
138300         COMPUTE W-RAD-VLORDNTO-TOT ROUNDED =                             
138400                 W-RAD-VLORDNTO-TOT + W-DIRL-VLORDNTO                     
138500         COMPUTE W-RAD-VKORDNTO-TOT ROUNDED =                             
138600                 W-RAD-VKORDNTO-TOT + W-DIRL-VKORDNTO                     
138700         COMPUTE W-RAD-SUORDV-TOT ROUNDED       =                         
138800                 W-RAD-SUORDV-TOT   + W-DIRL-SUORDV                       
138900         COMPUTE W-RAD-SUORDV-LOC-TOT ROUNDED       =                     
139000                 W-RAD-SUORDV-LOC-TOT + W-DIRL-SUORDV-LOC                 
139100         COMPUTE W-RAD-SUORDV-LOCPREL-TOT ROUNDED       =                 
139200              W-RAD-SUORDV-LOCPREL-TOT + W-DIRL-SUORDV-LOCPREL            
139300     END-IF                                                               
139400                                                                          
139500     COMPUTE W-VLORDNTO-TOT ROUNDED =                                     
139600             W-VLORDNTO-TOT + W-DIRL-VLORDNTO                             
139700     COMPUTE W-VKORDNTO-TOT ROUNDED =                                     
139800             W-VKORDNTO-TOT + W-DIRL-VKORDNTO                             
139900     COMPUTE W-SUORDV-TOT ROUNDED   =                                     
140000             W-SUORDV-TOT   + W-DIRL-SUORDV                               
140100     COMPUTE W-SUORDV-LOC-TOT ROUNDED   =                                 
140200             W-SUORDV-LOC-TOT   + W-DIRL-SUORDV-LOC                       
140300     COMPUTE W-SUORDV-LOCPREL-TOT ROUNDED   =                             
140400             W-SUORDV-LOCPREL-TOT   + W-DIRL-SUORDV-LOCPREL               
140500     .                                                                    
140600     EJECT                                                                
140700                                                                          
140800 GI-REDIGERA-RAD SECTION.                                                 
140900                                                                          
141000     IF DIST79-DEALER-PRICE                                               
141100       COMPUTE W-RAD-SUORDV-LOC-TOT = W-RAD-SUORDV-LOC-TOT +              
141200                                  W-RAD-SUORDV-LOCPREL-TOT                
141300                                                                          
141400       PERFORM S08-KOLLA-HAMTA-KDVALISO                                   
141500       MOVE W-DATE-AAMM           TO CURR-TIAAMM                          
141600       MOVE WS-KDVALISO-HUV       TO CURR-KDVALISO-HUV                    
141700       MOVE 'M'                   TO CURR-KDVALTYP                        
141800       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
141900       IF CURR-KDSVAR = ' '                                               
142000          MOVE CURR-PRKURS-NEW    TO EXCH-PRKURS                          
142100       ELSE                                                               
142200          MOVE 1                  TO EXCH-PRKURS                          
142300       END-IF                                                             
142400****   +1 KDCALL = LOKAL VALUTA TILL SEK                                  
142500       MOVE +1                   TO EXCH-KDCALL                           
142600       MOVE W-RAD-SUORDV-LOC-TOT TO EXCH-SUORDV-IN                        
142700       MOVE +0                   TO EXCH-PRARTNTO-IN                      
142800                                                                          
142900       CALL W411EXCH USING EXCH-W411EXCH                                  
143000                                                                          
143100       MOVE EXCH-SUORDV-UT     TO  MOD-SUORDV-RAD (INDX)                  
143200                                                                          
143300       IF W-RAD-SUORDV-LOCPREL-TOT > +0                                   
143400         MOVE '*'              TO  MOD-TEASTRIX-RAD (INDX)                
143500       ELSE                                                               
143600         MOVE ' '              TO  MOD-TEASTRIX-RAD (INDX)                
143700       END-IF                                                             
143800     ELSE                                                                 
143900       MOVE ' '                TO  MOD-TEASTRIX-RAD (INDX)                
144000       MOVE W-RAD-SUORDV-TOT   TO  MOD-SUORDV-RAD (INDX)                  
144100     END-IF                                                               
144200     MOVE OHUV-IDDISTR         TO  MOD-IDDISTR-RAD (INDX)                 
144300     MOVE OHUV-IDKUNDNR        TO  MOD-IDKUNDNR-RAD (INDX)                
144400     MOVE W-MOD-IDORDNR7       TO  MOD-IDORDNR7-RAD (INDX)                
144500     MOVE ARB-KDFRAKT          TO  MOD-KDFRAKT-RAD (INDX)                 
144600     MOVE OHUV-KDORDKL         TO  MOD-KDORDKL-RAD (INDX)                 
144700     MOVE ARB-KDTRPKAT         TO  MOD-KDTRPKAT-RAD (INDX)                
144800     MOVE W-RAD-VLORDNTO-TOT   TO  MOD-VLORDNTO-RAD (INDX)                
144900     MOVE W-RAD-VKORDNTO-TOT   TO  MOD-VKORDNTO-RAD (INDX)                
145000     MOVE ARB-IDTRP            TO  MOD-IDTRP-RAD (INDX)                   
145100                                                                          
145200     IF ARB-DATRPAVD           =   ZERO                                   
145300         MOVE ZERO             TO  MOD-TITRPAVG-RAD(INDX)                 
145400      ELSE                                                                
145500         PERFORM GIA-KONV-TITRPAVT                                        
145600     END-IF                                                               
145700     .                                                                    
145800     EJECT                                                                
145900                                                                          
146000 GIA-KONV-TITRPAVT           SECTION.                                     
146100                                                                          
146200     MOVE ARB-TIHHMM           TO  W-ARB-TIHHMM                           
146300     MOVE W-ARB-TIHHMM         TO  MOD-TITRPAVG-RAD(INDX) (4:4)           
146400     MOVE 'AAMMDD'             TO  DAT-KDDATFORM                          
146500     MOVE ARB-DATRPAVD (3:6)   TO  DAT-I-TIDATUM                          
146600                                                                          
146700     CALL WDATKONV USING       DAT-KDDATFORM                              
146800                               DAT-I-TIDATUM                              
146900                               DAT-O-TIDATUM                              
147000                               DAT-KDSVAR                                 
147100                                                                          
147200     MOVE DAT-TIAAVVD (3:3)    TO  MOD-TITRPAVG-RAD(INDX)  (1:3)          
147300     .                                                                    
147400     EJECT                                                                
147500                                                                          
147600 GJ-SUBTRAHERA-SID-TOTAL   SECTION.                                       
147700                                                                          
147800*-----                                                                    
147900*     HAR NÅGON ORDERDEL EJ STATUS 'R' SKALL                              
148000*     DENNA ORDER EJ VISAS PÅ BILDEN. SUBTRARHERAR                        
148100*     DÄRFÖR BORT EV TIDIGARE ORDERDELARS VÄRDE                           
148200*     FRÅN TOTALEN.                                                       
148300*-----                                                                    
148400     COMPUTE W-VLORDNTO-TOT    =  W-VLORDNTO-TOT -                        
148500                                  W-RAD-VLORDNTO-TOT                      
148600     COMPUTE W-VKORDNTO-TOT    =  W-VKORDNTO-TOT -                        
148700                                  W-RAD-VKORDNTO-TOT                      
148800     COMPUTE W-SUORDV-TOT      =  W-SUORDV-TOT  -                         
148900                                  W-RAD-SUORDV-TOT                        
149000     COMPUTE W-SUORDV-LOC-TOT  =  W-SUORDV-LOC-TOT  -                     
149100                                  W-RAD-SUORDV-LOC-TOT                    
149200     COMPUTE W-SUORDV-LOCPREL-TOT  =  W-SUORDV-LOCPREL-TOT  -             
149300                                      W-RAD-SUORDV-LOCPREL-TOT            
149400     .                                                                    
149500     EJECT                                                                
149600                                                                          
149700 GK-REDIGERA-TOT SECTION.                                                 
149800                                                                          
149900     IF DIST79-DEALER-PRICE                                               
150000       COMPUTE W-SUORDV-LOC-TOT = W-SUORDV-LOC-TOT +                      
150100                                  W-SUORDV-LOCPREL-TOT                    
150200       IF W-SUORDV-LOCPREL-TOT   > +0 AND W-ASTTOT = ' '                  
150300         MOVE '*'                TO W-ASTTOT                              
150400       END-IF                                                             
150500                                                                          
150600       PERFORM S08-KOLLA-HAMTA-KDVALISO                                   
150700       MOVE W-DATE-AAMM           TO CURR-TIAAMM                          
150800       MOVE WS-KDVALISO-HUV       TO CURR-KDVALISO-HUV                    
150900       MOVE 'M'                   TO CURR-KDVALTYP                        
151000       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
151100       IF CURR-KDSVAR = ' '                                               
151200          MOVE CURR-PRKURS-NEW    TO EXCH-PRKURS                          
151300       ELSE                                                               
151400          MOVE 1                  TO EXCH-PRKURS                          
151500       END-IF                                                             
151600*      +1 KDCALL = LOKAL VALUTA TILL SEK                                  
151700       MOVE +1                   TO EXCH-KDCALL                           
151800       MOVE W-SUORDV-LOC-TOT     TO EXCH-SUORDV-IN                        
151900       MOVE +0                   TO EXCH-PRARTNTO-IN                      
152000                                                                          
152100       CALL W411EXCH USING EXCH-W411EXCH                                  
152200                                                                          
152300       COMPUTE W-SUORDV-TOT ROUNDED =                                     
152400               W-SUORDV-TOT   + EXCH-SUORDV-UT                            
152500                                                                          
152600     END-IF                                                               
152700     .                                                                    
152800     EJECT                                                                
152900                                                                          
153000 H-KOLLA-INPUT SECTION.                                                   
153100                                                                          
153200     MOVE JA                   TO INDATA-SW                               
153300     IF MID-INPUT              =  ALL '+'                                 
153400         MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                        
153500         CALL WMEDKONV USING MED-WMEDAREA                                 
153600         MOVE MED-MFSFEL       TO MOD-TEMFSFEL                            
153700         PERFORM MFS-ROR-EJ-FAELT-IN                                      
153800         PERFORM MFS-ROR-EJ-FAELT-UT                                      
153900         MOVE NEJ              TO INDATA-SW                               
154000     ELSE                                                                 
154100         PERFORM HA-KOLLA-CMD-KDTRPKAT                                    
154200         PERFORM HB-KOLLA-TITRPAVG-TIRFS                                  
154300         IF INDATA-FEL OR NOT CMD-IFYLLT                                  
154400             MOVE NEJ                  TO INDATA-SW                       
154500             CALL WMEDKONV USING MED-WMEDAREA                             
154600             MOVE MED-MFSFEL TO MOD-TEMFSFEL                              
154700             PERFORM MFS-ROR-EJ-FAELT-UT                                  
154800             PERFORM MFS-ROR-EJ-FAELT-IN                                  
154900         END-IF                                                           
155000     END-IF                                                               
155100     .                                                                    
155200     EJECT                                                                
155300                                                                          
155400 HA-KOLLA-CMD-KDTRPKAT   SECTION.                                         
155500                                                                          
155600*---- NÅGOT COMMAND FÄLT MÅSTE VARA IFYLLT PÅ SIDAN VID PF11              
155700*                                                                         
155800*---- SAMTLIGA MARKERADE RADER MÅSTE HA TRANSPORTKAT.                     
155900*     A, ELLER SAMTLIGA MÅSTE HA TRANSPORTKAT. B ELLER C                  
156000*                                                                         
156100     MOVE +1                     TO  INDX                                 
156200     PERFORM UNTIL               INDX  > MAX-INDX                         
156300       IF MID-CMD-UPDATE(INDX)    = ALL '+' OR SPACE                      
156400         CONTINUE                                                         
156500       ELSE                                                               
156600         IF MID-CMD-UPDATE(INDX) = 'X'                                    
156700             MOVE MFS-ALFA-FAELT-RAETT TO                                 
156800                                 MOD-CMD-UPDATE-ATTR(INDX)                
156900          ELSE                                                            
157000             MOVE MFS-ALFA-FAELT-FEL TO                                   
157100                                 MOD-CMD-UPDATE-ATTR(INDX)                
157200             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
157300             MOVE NEJ                      TO INDATA-SW                   
157400         END-IF                                                           
157500         MOVE JA                 TO CMD-IFYLLT-SW                         
157600         IF MID-KDTRPKAT-RAD(INDX)       = 'A' OR 'B' OR 'C'              
157700           IF W-SPAR-KDTRPKAT            = ' '                            
157800              MOVE MID-KDTRPKAT-RAD(INDX) TO W-SPAR-KDTRPKAT              
157900           ELSE                                                           
158000             IF (W-SPAR-KDTRPKAT                = 'A' AND                 
158100                 MID-KDTRPKAT-RAD(INDX) = 'A')                OR          
158200                ((W-SPAR-KDTRPKAT           = 'B' OR 'C') AND             
158300                 (MID-KDTRPKAT-RAD(INDX) = 'B' OR 'C'))                   
158400                  CONTINUE                                                
158500             ELSE                                                         
158600               IF MOD-TEMFSFEL                 = MFS-RENSA-FAELT          
158700                   MOVE '044'            TO MED-IDMFSFEL                  
158800                ELSE                                                      
158900                   MOVE ERR-CORR-HILITE-FLDS                              
159000                               TO MED-IDMFSFEL                            
159100               END-IF                                                     
159200               MOVE MFS-ALFA-FAELT-FEL TO                                 
159300                           MOD-CMD-UPDATE-ATTR(INDX)                      
159400               MOVE NEJ                  TO INDATA-SW                     
159500             END-IF                                                       
159600           END-IF                                                         
159700           IF MID-KDTRPKAT-RAD(INDX)      = 'B' OR 'C'                    
159800             MOVE MID-IDDISTR-RAD(INDX)  TO W-IDDISTR                     
159900             MOVE MID-IDKUNDNR-RAD(INDX) TO W-IDKUNDNR                    
160000***** LÄSES FÖR ATT FÅ NYCKEL TILL WDB101 ( BETALAR-REG. ****             
160100             PERFORM IMS-GU-GMTA-WDB201                                   
160200             IF SEGMENT-FINNS                                             
160300               MOVE GMT-IDPARTNR    TO W-WDB1-IDPARTNR                    
160400               MOVE GMT-IDFTG       TO W-WDB1-IDFTG                       
160500               PERFORM IMS-GU-BETC-WDB101                                 
160600               IF SEGMENT-FINNS                                           
160700                 IF BET-KDKREDSP = '1'                                    
160800                   IF MOD-TEMFSFEL = MFS-RENSA-FAELT                      
160900                     MOVE '213'              TO MED-IDMFSFEL              
161000                   ELSE                                                   
161100                     MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL           
161200                   END-IF                                                 
161300                   MOVE MFS-ALFA-FAELT-FEL TO                             
161400                               MOD-CMD-UPDATE-ATTR(INDX)                  
161500                   MOVE NEJ    TO INDATA-SW                               
161600                 END-IF                                                   
161700               END-IF                                                     
161800             END-IF                                                       
161900           END-IF                                                         
162000         ELSE                                                             
162100           IF MOD-TEMFSFEL                = MFS-RENSA-FAELT               
162200               MOVE '048'                       TO MED-IDMFSFEL           
162300           ELSE                                                           
162400               MOVE ERR-CORR-HILITE-FLDS        TO MED-IDMFSFEL           
162500           END-IF                                                         
162600           MOVE MFS-ALFA-FAELT-FEL TO                                     
162700                             MOD-CMD-UPDATE-ATTR(INDX)                    
162800           MOVE NEJ              TO INDATA-SW                             
162900         END-IF                                                           
163000       END-IF                                                             
163100       ADD +1                    TO  INDX                                 
163200     END-PERFORM                                                          
163300     IF CMD-IFYLLT                                                        
163400         CONTINUE                                                         
163500     ELSE                                                                 
163600         MOVE NEJ                       TO  INDATA-SW                     
163700         MOVE MFS-ALFA-FAELT-FEL        TO  MOD-CMD-UPDATE-ATTR(1)        
163800         IF MOD-TEMFSFEL                =   MFS-RENSA-FAELT               
163900             MOVE '049'                 TO  MED-IDMFSFEL                  
164000         ELSE                                                             
164100             MOVE ERR-CORR-HILITE-FLDS  TO  MED-IDMFSFEL                  
164200         END-IF                                                           
164300     END-IF                                                               
164400     .                                                                    
164500     EJECT                                                                
164600                                                                          
164700 HB-KOLLA-TITRPAVG-TIRFS SECTION.                                         
164800                                                                          
164900     IF (MID-TITRPAVG-UPDATE    NOT = ALL '+' AND                         
165000         MID-TITRPAVG-UPDATE    NUMERIC)             AND                  
165100        (MID-TIRFS-UPDATE       NOT = ALL '+' AND                         
165200         MID-TIRFS-UPDATE       NUMERIC)                                  
165300          MOVE MFS-NUM-FAELT-FEL       TO MOD-TITRPAVG-UPDATE-ATTR        
165400          MOVE MFS-NUM-FAELT-FEL       TO MOD-TIRFS-UPDATE-ATTR           
165500          IF MOD-TEMFSFEL              =   MFS-RENSA-FAELT                
165600              MOVE '046'               TO  MED-IDMFSFEL                   
165700           ELSE                                                           
165800              MOVE ERR-CORR-HILITE-FLDS TO  MED-IDMFSFEL                  
165900          END-IF                                                          
166000          MOVE NEJ            TO INDATA-SW                                
166100      ELSE                                                                
166200         IF MID-TITRPAVG-UPDATE NOT = ALL '+' AND                         
166300            MID-TITRPAVG-UPDATE NUMERIC                                   
166400             PERFORM HBA-KOLLA-TITRPAVG                                   
166500             MOVE JA           TO  TITRPAVG-IFYLLT-SW                     
166600          ELSE                                                            
166700             IF MID-TIRFS-UPDATE NOT = ALL '+' AND                        
166800                MID-TIRFS-UPDATE NUMERIC                                  
166900                 PERFORM HBB-KOLLA-TIRFS                                  
167000              ELSE                                                        
167100                 MOVE MFS-NUM-FAELT-FEL TO                                
167200                                        MOD-TITRPAVG-UPDATE-ATTR          
167300                 MOVE MFS-NUM-FAELT-FEL TO MOD-TIRFS-UPDATE-ATTR          
167400                 IF MOD-TEMFSFEL        =   MFS-RENSA-FAELT               
167500                     MOVE '045'         TO  MED-IDMFSFEL                  
167600                  ELSE                                                    
167700                     MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL            
167800                 END-IF                                                   
167900                 MOVE NEJ        TO INDATA-SW                             
168000             END-IF                                                       
168100         END-IF                                                           
168200     END-IF                                                               
168300                                                                          
168400     .                                                                    
168500     EJECT                                                                
168600                                                                          
168700 HBA-KOLLA-TITRPAVG      SECTION.                                         
168800                                                                          
168900     MOVE MID-TITRPAVG-UPDATE        TO  W-VVDHHMM                        
169000     MOVE MID-TITRPAVG-UPDATE (1:3)  TO  W-TIVVD                          
169100     MOVE MID-TITRPAVG-UPDATE (4:4)  TO  W-TIHHMM                         
169200                                     IN  W-TITRPAVT                       
169300     MOVE MID-TITRPAVG-UPDATE (4:4)  TO  W-TIHHMM                         
169400                                     IN  W-DATRPAVT                       
169500                                                                          
169600     PERFORM S05-KONTR-KONV-TITRPAVG                                      
169700     IF TID-FEL                                                           
169800         MOVE NEJ               TO INDATA-SW                              
169900         MOVE MFS-NUM-FAELT-FEL TO  MOD-TITRPAVG-UPDATE-ATTR              
170000         IF MOD-TEMFSFEL        =   MFS-RENSA-FAELT                       
170100             MOVE '047'         TO  MED-IDMFSFEL                          
170200          ELSE                                                            
170300             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
170400         END-IF                                                           
170500      ELSE                                                                
170600         MOVE MFS-NUM-FAELT-RAETT TO  MOD-TITRPAVG-UPDATE-ATTR            
170700         MOVE +1               TO  INDX                                   
170800         PERFORM UNTIL INDX    >   MAX-INDX                               
170900             IF MID-CMD-UPDATE(INDX) = ALL '+' OR SPACE                   
171000                 CONTINUE                                                 
171100              ELSE                                                        
171200                 IF MID-KDTRPKAT-RAD(INDX)   = 'A'                        
171300                    PERFORM S07-SKAPA-WDQ2SEQ-NYCKEL                      
171400                    PERFORM IMS-GET-ORQL-OHUV-ARB-KVAL                    
171500                    MOVE ARB-DATRPAVD TO W-ARB-DATRPAVD                   
171600                    IF W-DATRPAVT < ARB-DATRPAVT                          
171700                          MOVE MFS-NUM-FAELT-FEL TO                       
171800                                       MOD-TITRPAVG-UPDATE-ATTR           
171900                          IF MOD-TEMFSFEL =    MFS-RENSA-FAELT            
172000                              MOVE '043' TO    MED-IDMFSFEL               
172100                           ELSE                                           
172200                              MOVE ERR-CORR-HILITE-FLDS                   
172300                                           TO MED-IDMFSFEL                
172400                          END-IF                                          
172500                          MOVE NEJ           TO INDATA-SW                 
172600                    END-IF                                                
172700                  ELSE                                                    
172800                     MOVE MFS-NUM-FAELT-FEL TO                            
172900                                         MOD-TITRPAVG-UPDATE-ATTR         
173000                     IF MOD-TEMFSFEL         =  MFS-RENSA-FAELT           
173100                         MOVE '049'          TO MED-IDMFSFEL              
173200                      ELSE                                                
173300                         MOVE ERR-CORR-HILITE-FLDS TO                     
173400                                        MED-IDMFSFEL                      
173500                     END-IF                                               
173600                     MOVE NEJ               TO INDATA-SW                  
173700                 END-IF                                                   
173800             END-IF                                                       
173900             ADD +1            TO INDX                                    
174000         END-PERFORM                                                      
174100     END-IF                                                               
174200     .                                                                    
174300     EJECT                                                                
174400                                                                          
174500 HBB-KOLLA-TIRFS         SECTION.                                         
174600                                                                          
174700     MOVE MID-TIRFS-UPDATE           TO  W-VVDHHMM                        
174800     MOVE MID-TIRFS-UPDATE (1:3)     TO  W-TIVVD                          
174900     MOVE MID-TIRFS-UPDATE (4:4)     TO  W-TIHHMM                         
175000                                     IN  W-TIRFS                          
175100                                                                          
175200     PERFORM S06-KONTR-KONV-TIRFS                                         
175300                                                                          
175400     IF TID-FEL                                                           
175500         MOVE NEJ               TO  INDATA-SW                             
175600         MOVE MFS-NUM-FAELT-FEL TO  MOD-TIRFS-UPDATE-ATTR                 
175700         IF MOD-TEMFSFEL        =   MFS-RENSA-FAELT                       
175800             MOVE '047'         TO  MED-IDMFSFEL                          
175900          ELSE                                                            
176000             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
176100         END-IF                                                           
176200      ELSE                                                                
176300         MOVE MFS-NUM-FAELT-RAETT TO  MOD-TIRFS-UPDATE-ATTR               
176400         MOVE +1               TO  INDX                                   
176500         PERFORM UNTIL INDX    >   MAX-INDX                               
176600             IF MID-CMD-UPDATE(INDX) = ALL '+' OR SPACE                   
176700                 CONTINUE                                                 
176800              ELSE                                                        
176900                 IF (MID-KDTRPKAT-RAD(INDX) =  'B' OR 'C') OR             
177000                    (MID-KDTRPKAT-RAD(INDX) =  'A' AND                    
177100                     MID-TITRPAVG-RAD(INDX) =  ZERO)                      
177200                    PERFORM S07-SKAPA-WDQ2SEQ-NYCKEL                      
177300                    PERFORM IMS-GET-ORQL-OHUV-ARB-KVAL                    
177400                    MOVE ARB-TIRFS TO W-ARB-TIRFS                         
177500                    MOVE W-TIRFS-NUM   TO TMP1-YYMMDDHHMM                 
177600                    MOVE ARB-TIRFS     TO TMP2-YYMMDDHHMM                 
177700                    PERFORM WY2000PB                                      
177800                    IF TMP1-YYMMDDHHMM < TMP2-YYMMDDHHMM                  
177900                        MOVE MFS-NUM-FAELT-FEL TO                         
178000                                     MOD-TIRFS-UPDATE-ATTR                
178100                        IF MOD-TEMFSFEL =      MFS-RENSA-FAELT            
178200                            MOVE '043' TO      MED-IDMFSFEL               
178300                         ELSE                                             
178400                            MOVE ERR-CORR-HILITE-FLDS                     
178500                                         TO MED-IDMFSFEL                  
178600                        END-IF                                            
178700                        MOVE NEJ             TO INDATA-SW                 
178800                    END-IF                                                
178900                  ELSE                                                    
179000                     MOVE MFS-NUM-FAELT-FEL TO                            
179100                                         MOD-TIRFS-UPDATE-ATTR            
179200                     IF MOD-TEMFSFEL        =  MFS-RENSA-FAELT            
179300                         MOVE '049'                TO MED-IDMFSFEL        
179400                      ELSE                                                
179500                         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL        
179600                     END-IF                                               
179700                     MOVE NEJ                      TO INDATA-SW           
179800                 END-IF                                                   
179900             END-IF                                                       
180000             ADD +1            TO INDX                                    
180100         END-PERFORM                                                      
180200     END-IF                                                               
180300     .                                                                    
180400     EJECT                                                                
180500                                                                          
180600 I-UPPDATERA SECTION.                                                     
180700                                                                          
180800     MOVE +1                   TO  INDX                                   
180900     PERFORM UNTIL INDX        >   MAX-INDX                               
181000         IF MID-CMD-UPDATE(INDX) = ALL '+' OR SPACE                       
181100             CONTINUE                                                     
181200          ELSE                                                            
181300             PERFORM IA-UPPDATERA-FLKLAR-WDQ2                             
181400             PERFORM IB-CALL-W413AVSO                                     
181602             PERFORM IC-KOLLA-OM-DIRL-RADER-FINNS                         
182105                                                                          
182200             IF (MID-KDTRPKAT-RAD(INDX)  = 'B' OR 'C') AND                
182300                 W-ARB-TIRFS             = ZERO        AND                
182400                 DIRL-RADER-FINNS                                         
182500                  PERFORM IE-STARTA-W4T695                                
182600             END-IF                                                       
182700         END-IF                                                           
182800         ADD +1                TO  INDX                                   
182900     END-PERFORM                                                          
183000                                                                          
183100     MOVE MID-TIAAMMDD-ENTER   TO W-Q2B1KY-DATRPAVD                       
183200     IF MID-TIAAMMDD-ENTER NOT = ZERO                                     
183300       IF MID-TIAAMMDD-ENTER < 500000                                     
183400         MOVE 20               TO W-Q2B1KY-DATRPAVD (1:2)                 
183500       ELSE                                                               
183600         IF MID-TIAAMMDD-ENTER < 999999                                   
183700           MOVE 19             TO W-Q2B1KY-DATRPAVD (1:2)                 
183800         ELSE                                                             
183900           MOVE 99999999       TO W-Q2B1KY-DATRPAVD                       
184000         END-IF                                                           
184100       END-IF                                                             
184200     END-IF                                                               
184300     MOVE MID-TIHHMM-ENTER     TO W-Q2B1KY-TIHHMM                         
184400     MOVE MID-IDORDER-ENTER    TO W-Q2B1KY-IDORDER                        
184500     MOVE MID-IDKUNDNR-ENTER   TO W-Q2CSEQ-IDKUNDNR                       
184600     MOVE SPACE                TO W-Q2CSEQ-IDKUNDRF                       
184700     MOVE MID-IDORDNR7-ENTER   TO W-Q2CSEQ-IDORDNR7                       
184800                                                                          
184900     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
185000     CALL WMEDKONV USING MED-WMEDAREA                                     
185100     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
185200     PERFORM MFS-FORM-ATTR                                                
185300     PERFORM MFS-RENSA-FAELT-IN                                           
185400     .                                                                    
185500     EJECT                                                                
185600                                                                          
185700 IA-UPPDATERA-FLKLAR-WDQ2    SECTION.                                     
185800                                                                          
185900     MOVE MID-IDDISTR-RAD(INDX)   TO  W-Q2CSEQ-IDDISTR                    
186000     MOVE MID-IDKUNDNR-RAD(INDX)  TO  W-Q2CSEQ-IDKUNDNR                   
186100     MOVE SPACE                   TO  W-Q2CSEQ-IDKUNDRF                   
186200     INSPECT MID-IDORDNR7-RAD(INDX)                                       
186300                            REPLACING LEADING SPACE BY ZERO               
186400     MOVE MID-IDORDNR7-RAD(INDX)  TO  W-Q2CSEQ-IDKUNDRF                   
186500     PERFORM IMS-GET-ORQL-OHUV-ARB-KVAL                                   
186600     MOVE ARB-TIRFS               TO  W-ARB-TIRFS                         
186700                                                                          
186800     MOVE OHUV-IDORDER            TO  W-OHUV-IDORDER                      
186902     PERFORM IMS-GHU-WDQ201                                               
187000                                                                          
187102     MOVE 'J'                     TO  OHUV-FLKLAR                         
187103     MOVE 'R'                     TO  ARB-KDORDSTA                        
187200     PERFORM IMS-REPL-WDQ2-OHUV-ARB                                       
187300     .                                                                    
187400     EJECT                                                                
187500                                                                          
187600 IB-CALL-W413AVSO            SECTION.                                     
187700                                                                          
187800     MOVE MID-IDDISTR-RAD(INDX)   TO  AVSO-IDDISTR                        
187900     MOVE MID-IDKUNDNR-RAD(INDX)  TO  AVSO-IDKUNDNR                       
188000     MOVE SPACE                   TO  AVSO-IDKUNDRF                       
188100     MOVE MID-IDORDNR7-RAD(INDX)  TO  AVSO-IDORDNR7                       
188200     MOVE OHUV-IDORDER            TO  AVSO-IDORDER                        
188300     MOVE DCS-IDDC                TO  AVSO-IDDC                           
188400     MOVE W-IDTRANS               TO  AVSO-IDTRANS                        
188500                                                                          
188600     IF TITRPAVG-IFYLLT                                                   
188700         MOVE W-TITRPAVT          TO  AVSO-TITRPAVT                       
188800         MOVE ZERO                TO  AVSO-TIRFS                          
188900      ELSE                                                                
189000         MOVE W-TIRFS-NUM         TO  AVSO-TIRFS                          
189100         MOVE ZERO                TO  AVSO-TIAAMMDD                       
189200         MOVE ZERO                TO  AVSO-TIHHMM                         
189300     END-IF                                                               
189400                                                                          
189500                                                                          
189600     CALL W413AVSO USING          AVSO-W413AVSO WDE6-PCB ORQA1-PCB        
189700                                  WDQ21-PCB GMTB1-PCB                     
189800                                  XXKA-PCB 4437-PCB XXKE-PCB              
189900                                  XXKF-PCB XXKG-PCB XXKH-PCB              
190000                                  XXKI-PCB XXKP-PCB AVSO-WDB2-PCB         
190100                                  AVSO-WDB6-PCB                           
190200                                  USEA-PCB TRAN-XXKB-PCB                  
190300                                  ORDN-ORQL-PCB ORDN-PROC-PCB             
190400                                  ORDN-ORQI-PCB ORDN-WDQ3-PCB             
190500                                                                          
190600     .                                                                    
190700     EJECT                                                                
190800                                                                          
193002 IC-KOLLA-OM-DIRL-RADER-FINNS  SECTION.                                   
193100                                                                          
193200     MOVE NEJ                  TO DIRL-RADER-FINNS-SW                     
193302     PERFORM IMS-GHU-WDQ201                                               
193303     PERFORM IMS-GNP-WDQ211                                               
193402     PERFORM UNTIL SEGMENT-SAKNAS OR DIRL-RADER-FINNS                     
193500         IF DIRL-KVRADER       >  ZERO                                    
193600             MOVE JA           TO DIRL-RADER-FINNS-SW                     
193700         END-IF                                                           
193802         PERFORM IMS-GNP-WDQ211                                           
193900     END-PERFORM                                                          
194000     .                                                                    
194100     EJECT                                                                
194200                                                                          
196800 IE-STARTA-W4T695            SECTION.                                     
196900                                                                          
197000     MOVE    OHUV-IDORDER         TO 4695-IDORDER                         
197100     MOVE    DIRL-IDDC            TO 4695-IDDC                            
197200                                                                          
197300     PERFORM IMS-ISRT-ALT-MSG-4695                                        
197400     .                                                                    
197500     EJECT                                                                
197600                                                                          
197700 J-FLYTTA-TOT-TILL-MOD       SECTION.                                     
197800                                                                          
197900     IF LAS-BARA-EN-SIDA                                                  
198000         MOVE MID-VLORDNTO-TOT    TO  MOD-VLORDNTO-TOT-X                  
198100         MOVE MID-VKORDNTO-TOT    TO  MOD-VKORDNTO-TOT-X                  
198200         MOVE MID-SUORDV-TOT      TO  MOD-SUORDV-TOT-X                    
198300         MOVE MID-TEASTRIX-TOT    TO  MOD-TEASTRIX-TOT-X                  
198400      ELSE                                                                
198500         MOVE W-VLORDNTO-TOT      TO  MOD-VLORDNTO-TOT                    
198600         MOVE W-VKORDNTO-TOT      TO  MOD-VKORDNTO-TOT                    
198700         MOVE W-SUORDV-TOT        TO  MOD-SUORDV-TOT                      
198800         MOVE W-ASTTOT            TO  MOD-TEASTRIX-TOT                    
198900     END-IF                                                               
199000                                                                          
199100     .                                                                    
199200     EJECT                                                                
199300                                                                          
199400 S01-LAES-OHUV-VIA-WDQ2B1KY  SECTION.                                     
199500                                                                          
199600     IF MFS-FIRST  OR FORSTA-WDQ2-LAST                                    
199700         PERFORM IMS-GET-ORQK-ARB-OKVAL                                   
199800      ELSE                                                                
199900         PERFORM IMS-GET-ORQK-ARB-KVAL                                    
200000         MOVE JA         TO FORSTA-WDQ2-LAST-SW                           
200100     END-IF                                                               
200200                                                                          
200300     IF SEGMENT-FINNS                                                     
200400         MOVE SEQB-IDORDER     TO  W-OHUV-IDORDER                         
200500                                   W-Q301KY-IDORDER-MIN                   
200600                                   W-Q301KY-IDORDER-MAX                   
200700         PERFORM IMS-GET-WDQ2-OHUV-ARB-KVAL                               
200900         MOVE OHUV-IDORDNR7     TO  W-MOD-IDORDNR7                        
201000     END-IF                                                               
201100                                                                          
201200     .                                                                    
201300     EJECT                                                                
201400                                                                          
201500 S02-LAES-OHUV-VIA-WDQ2CSEQ  SECTION.                                     
201600                                                                          
201700     IF MFS-FIRST OR FORSTA-WDQ2-LAST                                     
201800         PERFORM IMS-GET-ORQL-OHUV-ARB-OKVAL                              
201900      ELSE                                                                
202000         PERFORM IMS-GET-ORQL-OHUV-ARB-KVAL                               
202100         MOVE JA               TO FORSTA-WDQ2-LAST-SW                     
202200     END-IF                                                               
202300                                                                          
202400     IF SEGMENT-FINNS                                                     
202500         MOVE OHUV-IDORDNR7    TO W-MOD-IDORDNR7                          
202600         MOVE OHUV-IDORDER     TO W-OHUV-IDORDER                          
202700                                  W-Q301KY-IDORDER-MIN                    
202800                                  W-Q301KY-IDORDER-MAX                    
202900     END-IF                                                               
203000                                                                          
203100     .                                                                    
203200     EJECT                                                                
203300                                                                          
203400 S05-KONTR-KONV-TITRPAVG     SECTION.                                     
203500                                                                          
203600     IF TIMMA-OK                                                          
203700         CONTINUE                                                         
203800      ELSE                                                                
203900         MOVE NEJ              TO TID-SW                                  
204000     END-IF                                                               
204100                                                                          
204200     IF MINUT-OK                                                          
204300         CONTINUE                                                         
204400      ELSE                                                                
204500         MOVE NEJ              TO TID-SW                                  
204600     END-IF                                                               
204700                                                                          
204800     IF W-TIVV                 <   WS-TIVV                                
204900         ADD +1                TO  W-TIAA                                 
205000     END-IF                                                               
205100     MOVE 'AAVVD '             TO  DAT-KDDATFORM                          
205200     MOVE W-TIAAVVD-NUM        TO  DAT-I-TIDATUM                          
205300                                                                          
205400     CALL WDATKONV USING       DAT-KDDATFORM                              
205500                               DAT-I-TIDATUM                              
205600                               DAT-O-TIDATUM                              
205700                               DAT-KDSVAR                                 
205800     IF DAT-KDSVAR-FEL                                                    
205900         MOVE NEJ              TO  TID-SW                                 
206000      ELSE                                                                
206100         MOVE DAT-TIAAMMDD     TO  W-TIAAMMDD                             
206200                               IN  W-TITRPAVT                             
206300         MOVE DAT-TIAAMMDD     TO  W-DAAAMMDD                             
206400                               IN  W-DATRPAVT                             
206500         ADD 20000000 TO  W-DAAAMMDD                                      
206600                               IN  W-DATRPAVT                             
206700     END-IF                                                               
206800     .                                                                    
206900     EJECT                                                                
207000                                                                          
207100 S06-KONTR-KONV-TIRFS        SECTION.                                     
207200                                                                          
207300     IF TIMMA-OK                                                          
207400         CONTINUE                                                         
207500      ELSE                                                                
207600         MOVE NEJ              TO TID-SW                                  
207700     END-IF                                                               
207800                                                                          
207900     IF MINUT-OK                                                          
208000         CONTINUE                                                         
208100      ELSE                                                                
208200         MOVE NEJ              TO TID-SW                                  
208300     END-IF                                                               
208400                                                                          
208500     IF W-TIVV                 <   WS-TIVV                                
208600         ADD +1                TO  W-TIAA                                 
208700     END-IF                                                               
208800                                                                          
208900     MOVE 'AAVVD '             TO  DAT-KDDATFORM                          
209000     MOVE W-TIAAVVD-NUM        TO  DAT-I-TIDATUM                          
209100                                                                          
209200     CALL WDATKONV USING       DAT-KDDATFORM                              
209300                               DAT-I-TIDATUM                              
209400                               DAT-O-TIDATUM                              
209500                               DAT-KDSVAR                                 
209600                                                                          
209700     IF DAT-KDSVAR-FEL                                                    
209800         MOVE NEJ              TO  TID-SW                                 
209900      ELSE                                                                
210000         MOVE DAT-TIAAMMDD     TO  W-TIAAMMDD                             
210100                                   IN W-TIRFS                             
210200     END-IF                                                               
210300     .                                                                    
210400     EJECT                                                                
210500                                                                          
210600 S07-SKAPA-WDQ2SEQ-NYCKEL    SECTION.                                     
210700                                                                          
210800     MOVE MID-IDDISTR-RAD(INDX)   TO W-Q2CSEQ-IDDISTR                     
210900     MOVE MID-IDKUNDNR-RAD(INDX)  TO W-Q2CSEQ-IDKUNDNR                    
211000     MOVE SPACE                   TO W-Q2CSEQ-IDKUNDRF                    
211100     INSPECT MID-IDORDNR7-RAD(INDX)                                       
211200                            REPLACING LEADING SPACE BY ZERO               
211300     MOVE MID-IDORDNR7-RAD(INDX)  TO W-Q2CSEQ-IDORDNR7                    
211400     .                                                                    
211500     EJECT                                                                
211600                                                                          
211700 S08-KOLLA-HAMTA-KDVALISO SECTION.                                        
211800                                                                          
211900     IF WS-KDVALISO = SPACE                                               
212000       IF DIST79-DEALER-PRICE                                             
212100         PERFORM IMS-GU-GMTA-WDB201                                       
212200         IF SEGMENT-FINNS                                                 
212300           CONTINUE                                                       
212400         ELSE                                                             
212500           PERFORM IMS-GET-GMTA-WDB201                                    
212600         END-IF                                                           
212700         MOVE GMT-IDPARTNR       TO W-WDB1-IDPARTNR                       
212800         MOVE GMT-IDFTG          TO W-WDB1-IDFTG                          
212900         PERFORM IMS-GU-BETC-WDB101                                       
213000         MOVE BET-KDVALISO       TO CURR-KDVALISO-ROW                     
213100                                    WS-KDVALISO                           
213200       ELSE                                                               
213300         MOVE 'SEK'              TO CURR-KDVALISO-ROW                     
213400                                    WS-KDVALISO                           
213500       END-IF                                                             
213600     ELSE                                                                 
213700       MOVE WS-KDVALISO          TO CURR-KDVALISO-ROW                     
213800     END-IF                                                               
213900     .                                                                    
214000     EJECT                                                                
214100                                                                          
214200 S10-NOLLSTALL-RAD-TOTAL  SECTION.                                        
214300                                                                          
214400     MOVE ZERO                 TO W-RAD-VLORDNTO-TOT                      
214500                                  W-RAD-VKORDNTO-TOT                      
214600                                  W-RAD-SUORDV-TOT                        
214700     .                                                                    
214800     EJECT                                                                
214900                                                                          
215000 MFS-RENSA-FAELT-UT SECTION.                                              
215100                                                                          
215200*    --- ALLA UTDATA-FÄLT                                                 
215300*    --- INKL. BLÄDDRINGSNYCKLAR                                          
215400     MOVE MFS-RENSA-FAELT      TO MOD-TIAAMMDD-ENTER                      
215500                                  MOD-TIAAMMDD-NEXT                       
215600                                  MOD-TIHHMM-ENTER                        
215700                                  MOD-TIHHMM-NEXT                         
215800                                  MOD-IDORDER-ENTER                       
215900                                  MOD-IDORDER-NEXT                        
216000                                  MOD-IDKUNDNR-ENTER                      
216100                                  MOD-IDKUNDNR-NEXT                       
216200                                  MOD-IDORDNR7-ENTER                      
216300                                  MOD-IDORDNR7-NEXT                       
216400                                  MOD-IDPRODNR-ENTER                      
216500                                  MOD-IDPRODNR-NEXT                       
216600                                  MOD-VLORDNTO-TOT                        
216700                                  MOD-VKORDNTO-TOT                        
216800                                  MOD-SUORDV-TOT                          
216900                                                                          
217000     MOVE +1                   TO INDX                                    
217100     PERFORM UNTIL INDX        >  MAX-INDX                                
217200         PERFORM MFS-RENSA-RAD-FAELT-UT                                   
217300         ADD +1                TO INDX                                    
217400     END-PERFORM                                                          
217500     .                                                                    
217600     EJECT                                                                
217700 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
217800                                                                          
217900*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
218000     MOVE MFS-RENSA-FAELT      TO MOD-IDDISTR-RAD  (INDX)                 
218100                                  MOD-IDKUNDNR-RAD (INDX)                 
218200                                  MOD-IDORDNR7-RAD (INDX)                 
218300                                  MOD-KDFRAKT-RAD (INDX)                  
218400                                  MOD-KDORDKL-RAD (INDX)                  
218500                                  MOD-KDTRPKAT-RAD (INDX)                 
218600                                  MOD-VLORDNTO-RAD (INDX)                 
218700                                  MOD-VKORDNTO-RAD (INDX)                 
218800                                  MOD-SUORDV-RAD (INDX)                   
218900                                  MOD-TEASTRIX-RAD (INDX)                 
219000                                  MOD-IDTRP-RAD (INDX)                    
219100                                  MOD-TITRPAVG-RAD (INDX)                 
219200     .                                                                    
219300     SKIP2                                                                
219400 MFS-RENSA-FAELT-IN SECTION.                                              
219500                                                                          
219600*    --- ALLA INDATA-FÄLT                                                 
219700     MOVE MFS-RENSA-FAELT         TO MOD-TITRPAVG-UPDATE                  
219800                                     MOD-TIRFS-UPDATE                     
219900                                                                          
220000     MOVE +1                      TO INDX                                 
220100     PERFORM UNTIL INDX           >  MAX-INDX                             
220200         MOVE MFS-RENSA-FAELT     TO MOD-CMD-UPDATE (INDX)                
220300         ADD +1                   TO INDX                                 
220400     END-PERFORM                                                          
220500     .                                                                    
220600     EJECT                                                                
220700                                                                          
220800 MFS-ROR-EJ-FAELT-UT  SECTION.                                            
220900                                                                          
221000*    --- ALLA UTDATA-FÄLT                                                 
221100*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
221200     MOVE MFS-ROER-EJ-FAELT    TO MOD-TIAAMMDD-ENTER                      
221300                                  MOD-TIAAMMDD-NEXT                       
221400                                  MOD-TIHHMM-ENTER                        
221500                                  MOD-TIHHMM-NEXT                         
221600                                  MOD-IDORDER-ENTER                       
221700                                  MOD-IDORDER-NEXT                        
221800                                  MOD-IDKUNDNR-ENTER                      
221900                                  MOD-IDKUNDNR-NEXT                       
222000                                  MOD-IDORDNR7-ENTER                      
222100                                  MOD-IDORDNR7-NEXT                       
222200                                  MOD-IDPRODNR-ENTER                      
222300                                  MOD-IDPRODNR-NEXT                       
222400                                  MOD-VLORDNTO-TOT                        
222500                                  MOD-VKORDNTO-TOT                        
222600                                  MOD-SUORDV-TOT                          
222700                                                                          
222800     MOVE +1                   TO INDX                                    
222900     PERFORM UNTIL INDX        > MAX-INDX                                 
223000       PERFORM MFS-ROR-EJ-RAD-FAELT-UT                                    
223100       ADD +1                  TO INDX                                    
223200     END-PERFORM                                                          
223300     .                                                                    
223400     SKIP2                                                                
223500 MFS-ROR-EJ-RAD-FAELT-UT  SECTION.                                        
223600                                                                          
223700*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
223800     MOVE MFS-ROER-EJ-FAELT    TO MOD-IDDISTR-RAD  (INDX)                 
223900                                  MOD-IDKUNDNR-RAD (INDX)                 
224000                                  MOD-IDORDNR7-RAD (INDX)                 
224100                                  MOD-KDFRAKT-RAD  (INDX)                 
224200                                  MOD-KDORDKL-RAD  (INDX)                 
224300                                  MOD-KDTRPKAT-RAD (INDX)                 
224400                                  MOD-VLORDNTO-RAD (INDX)                 
224500                                  MOD-VKORDNTO-RAD (INDX)                 
224600                                  MOD-SUORDV-RAD   (INDX)                 
224700                                  MOD-TEASTRIX-RAD (INDX)                 
224800                                  MOD-IDTRP-RAD    (INDX)                 
224900                                  MOD-TITRPAVG-RAD (INDX)                 
225000     .                                                                    
225100     SKIP2                                                                
225200 MFS-ROR-EJ-FAELT-IN  SECTION.                                            
225300                                                                          
225400*    --- ALLA INDATA-FÄLT                                                 
225500     MOVE MFS-ROER-EJ-FAELT       TO MOD-TITRPAVG-UPDATE                  
225600                                     MOD-TIRFS-UPDATE                     
225700                                                                          
225800     MOVE +1                      TO INDX                                 
225900     PERFORM UNTIL INDX           >  MAX-INDX                             
226000         MOVE MFS-ROER-EJ-FAELT   TO MOD-CMD-UPDATE (INDX)                
226100         ADD +1                   TO INDX                                 
226200     END-PERFORM                                                          
226300     .                                                                    
226400     EJECT                                                                
226500                                                                          
226600 MFS-FORM-ATTR SECTION.                                                   
226700                                                                          
226800*    --- ALLA INDATA-FÄLT                                                 
226900     MOVE MFS-FORMATETS-ATTR      TO MOD-TITRPAVG-UPDATE-ATTR             
227000                                     MOD-TIRFS-UPDATE-ATTR                
227100                                                                          
227200     MOVE +1                      TO INDX                                 
227300     PERFORM UNTIL INDX           >  MAX-INDX                             
227400         MOVE MFS-FORMATETS-ATTR  TO MOD-CMD-UPDATE-ATTR (INDX)           
227500         ADD +1                   TO INDX                                 
227600     END-PERFORM                                                          
227700     .                                                                    
227800     EJECT                                                                
227900                                                                          
228000 MFS-LAS-IN-IGEN SECTION.                                                 
228100                                                                          
228200*    --- ALLA INDATA-FÄLT                                                 
228300     MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-TITRPAVG-UPDATE-ATTR             
228400                                     MOD-TIRFS-UPDATE-ATTR                
228500                                                                          
228600     MOVE +1                         TO INDX                              
228700     PERFORM UNTIL INDX              >  MAX-INDX                          
228800         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-CMD-UPDATE-ATTR (INDX)        
228900         ADD +1                      TO INDX                              
229000     END-PERFORM                                                          
229100     .                                                                    
229200     EJECT                                                                
229300                                                                          
229400* --- IMS SEKTIONER ---                                                   
229500     SKIP3                                                                
229600 IMS-GET-MSG SECTION.                                                     
229700                                                                          
229800     MOVE '  QC' TO GODK-STATUSKODER                                      
229900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
230000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
230100     PERFORM IMS-STATUSKONTROLL                                           
230200     .                                                                    
230300     SKIP3                                                                
230400 IMS-INSERT-MSG SECTION.                                                  
230500                                                                          
230600     IF NOT ENGLISH-TEXT                                                  
230700       MOVE '0' TO MFS-KDHUVOMR                                           
230800     END-IF                                                               
230900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
231000     MOVE SPACE TO GODK-STATUSKODER                                       
231100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
231200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
231300     PERFORM IMS-STATUSKONTROLL                                           
231400     .                                                                    
231500     SKIP3                                                                
231600 IMS-ISRT-ALT-MSG-4695  SECTION.                                          
231700     MOVE SPACE TO GODK-STATUSKODER                                       
231800     CALL  CBLTDLI  USING ISRT ALT-PCB 4695-MSG-IO-AREA                   
231900     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
232000     PERFORM IMS-STATUSKONTROLL                                           
232100     .                                                                    
232200     EJECT                                                                
233400 IMS-GET-ORQL-OHUV-ARB-KVAL SECTION.                                      
233500                                                                          
233600     STRING 'WDQ201  *D(WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                      
233700          DELIMITED BY SIZE INTO SSA1                                     
233800     STRING 'WDQ212  (IDDC     =' W-IDDC-X ')'                            
233900          DELIMITED BY SIZE INTO SSA2                                     
234000     MOVE '  GE' TO GODK-STATUSKODER                                      
234100     CALL CBLTDLI USING GU ORQL-PCB DLI-IO-AREA1 SSA1 SSA2                
234200     MOVE ORQL-STATUS-CODE TO STATUS-WS                                   
234300     PERFORM IMS-STATUSKONTROLL                                           
234400     .                                                                    
234500     SKIP2                                                                
234600 IMS-GET-ORQL-OHUV-ARB-OKVAL SECTION.                                     
234700                                                                          
234800     STRING 'WDQ201  *D(WDQ2CSEQ>=' W-WDQ2CSEQ-MIN-X                      
234900                      '&WDQ2CSEQ<=' W-WDQ2CSEQ-MAX-X ')'                  
235000          DELIMITED BY SIZE INTO SSA1                                     
235100     STRING 'WDQ212  (IDDC     =' W-IDDC-X ')'                            
235200          DELIMITED BY SIZE INTO SSA2                                     
235300     MOVE '  GE' TO GODK-STATUSKODER                                      
235400     CALL CBLTDLI USING GN ORQL-PCB DLI-IO-AREA1 SSA1 SSA2                
235500     MOVE ORQL-STATUS-CODE TO STATUS-WS                                   
235600     PERFORM IMS-STATUSKONTROLL                                           
235700     .                                                                    
235800     SKIP2                                                                
235900 IMS-GET-ORQK-ARB-KVAL         SECTION.                                   
236000     STRING 'WLORQK01(WDQ2B1KY =' W-WDQ2B1KY-X ')'                        
236100          DELIMITED BY SIZE INTO SSA1                                     
236200     MOVE '  GE' TO GODK-STATUSKODER                                      
236300     CALL CBLTDLI USING GU ORQK-PCB DLI-IO-AREA3 SSA1                     
236400     MOVE ORQK-STATUS-CODE TO STATUS-WS                                   
236500     PERFORM IMS-STATUSKONTROLL                                           
236600     .                                                                    
236700     SKIP2                                                                
236800 IMS-GET-ORQK-ARB-OKVAL        SECTION.                                   
236900     STRING 'WLORQK01(WDQ2B1KY>=' W-WDQ2B1KY-MIN-X                        
237000                    '&WDQ2B1KY<=' W-WDQ2B1KY-MAX-X                        
237100                    '&DATRPAVT>=' W-DATRPAVT-MIN-X                        
237200                    '&DATRPAVT<=' W-DATRPAVT-MAX-X ')'                    
237300          DELIMITED BY SIZE INTO SSA1                                     
237400     MOVE '  GE' TO GODK-STATUSKODER                                      
237500     CALL CBLTDLI USING GN ORQK-PCB DLI-IO-AREA3 SSA1                     
237600     MOVE ORQK-STATUS-CODE TO STATUS-WS                                   
237700     PERFORM IMS-STATUSKONTROLL                                           
237800     .                                                                    
237900     SKIP2                                                                
238000 IMS-GET-WDQ2-OHUV-ARB-KVAL    SECTION.                                   
238100                                                                          
238200     STRING 'WDQ201  *D(IDORDER  =' W-IDORDER-X ')'                       
238300          DELIMITED BY SIZE INTO SSA1                                     
238400     STRING 'WDQ212  (IDDC     =' W-IDDC-X ')'                            
238500          DELIMITED BY SIZE INTO SSA2                                     
238600     MOVE '  GE' TO GODK-STATUSKODER                                      
238700     CALL CBLTDLI USING GU WDQ2-PCB DLI-IO-AREA1 SSA1 SSA2                
238800     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
238900     PERFORM IMS-STATUSKONTROLL                                           
239000     .                                                                    
239100     SKIP2                                                                
239202 IMS-GHU-WDQ201    SECTION.                                               
239302                                                                          
239402     STRING 'WDQ201  (IDORDER  =' W-IDORDER-X ')'                         
239502          DELIMITED BY SIZE INTO SSA1                                     
239602     MOVE '  ' TO GODK-STATUSKODER                                        
239702     CALL CBLTDLI USING GHU WDQ2-PCB DLI-IO-AREA1 SSA1                    
239802     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
239902     PERFORM IMS-STATUSKONTROLL                                           
240002     .                                                                    
240102     SKIP2                                                                
240202 IMS-REPL-WDQ2-OHUV-ARB  SECTION.                                         
240302                                                                          
240402     MOVE '  ' TO GODK-STATUSKODER                                        
240502     CALL CBLTDLI USING REPL WDQ2-PCB DLI-IO-AREA1                        
240602     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
240702     PERFORM IMS-STATUSKONTROLL                                           
240802     .                                                                    
240902     EJECT                                                                
241002 IMS-GNP-WDQ211         SECTION.                                          
241102                                                                          
241202     MOVE 'WDQ211  ' TO SSA1                                              
241302     MOVE '  GE'               TO GODK-STATUSKODER                        
241402     CALL CBLTDLI USING GNP WDQ2-PCB DLI-IO-Q211 SSA1                     
241502     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
241602     PERFORM IMS-STATUSKONTROLL                                           
241702     .                                                                    
241802     SKIP3                                                                
243802                                                                          
243902 IMS-GNP-WDQ221                SECTION.                                   
244002                                                                          
244102     STRING 'WDQ212  (IDDC     =' W-IDDC-X ')'                            
244202          DELIMITED BY SIZE INTO SSA1                                     
244302     MOVE 'WDQ221 '           TO SSA2                                     
244402     MOVE '  GE' TO GODK-STATUSKODER                                      
244502     CALL CBLTDLI USING GNP WDQ2-PCB DLI-IO-Q221 SSA1 SSA2                
244602     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
244702     PERFORM IMS-STATUSKONTROLL                                           
244802     .                                                                    
244902     SKIP2                                                                
245002 IMS-GET-ORQA-ODEL-OKVAL-GU SECTION.                                      
245102                                                                          
245202     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN-X                        
245302                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
245402          DELIMITED BY SIZE INTO SSA1                                     
245502     MOVE '  GE' TO GODK-STATUSKODER                                      
245602     CALL CBLTDLI USING GU ORQA-PCB DLI-IO-AREA4 SSA1                     
245702     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
245802     PERFORM IMS-STATUSKONTROLL                                           
245902     .                                                                    
246002     EJECT                                                                
246102 IMS-GET-ORQA-ODEL-OKVAL-GN SECTION.                                      
246202                                                                          
246302     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN-X                        
246402                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
246502          DELIMITED BY SIZE INTO SSA1                                     
246602     MOVE '  GE' TO GODK-STATUSKODER                                      
246702     CALL CBLTDLI USING GN ORQA-PCB DLI-IO-AREA4 SSA1                     
246802     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
246902     PERFORM IMS-STATUSKONTROLL                                           
247002     .                                                                    
247102     EJECT                                                                
247200 IMS-GET-GMTA-WDB201 SECTION.                                             
247300                                                                          
247400     STRING 'WDB201  (IDGMT   >=' W-IDGMT-MIN-X                           
247500                    '&IDGMT   <=' W-IDGMT-MAX-X ')'                       
247600            DELIMITED BY SIZE INTO SSA1                                   
247700                                                                          
247800     MOVE '  ' TO GODK-STATUSKODER                                        
247900     CALL CBLTDLI USING                                                   
248000           GU WDB2-PCB DLI-IO-AREA-WDB2 SSA1                              
248100     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
248200     PERFORM IMS-STATUSKONTROLL                                           
248300     .                                                                    
248400     EJECT                                                                
248500 IMS-GU-GMTA-WDB201 SECTION.                                              
248600                                                                          
248700     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
248800          DELIMITED BY SIZE INTO SSA1                                     
248900     MOVE '  GE'              TO GODK-STATUSKODER                         
249000                                                                          
249100     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-AREA-WDB2 SSA1                 
249200     MOVE WDB2-STATUS-CODE    TO STATUS-WS                                
249300     PERFORM IMS-STATUSKONTROLL                                           
249400     .                                                                    
249500     EJECT                                                                
249600                                                                          
249700 IMS-GU-BETC-WDB101 SECTION.                                              
249800                                                                          
249900     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
250000          DELIMITED BY SIZE INTO SSA1                                     
250100     MOVE '  GE'              TO GODK-STATUSKODER                         
250200                                                                          
250300     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-AREA-WDB1 SSA1                 
250400     MOVE WDB1-STATUS-CODE    TO STATUS-WS                                
250500     PERFORM IMS-STATUSKONTROLL                                           
250600     .                                                                    
250700     EJECT                                                                
250800                                                                          
250900 IMS-GU-WDB601    SECTION.                                                
251000     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
251100          DELIMITED BY SIZE INTO SSA1                                     
251200     MOVE '  GE' TO GODK-STATUSKODER                                      
251300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
251400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
251500     PERFORM IMS-STATUSKONTROLL                                           
251600     IF SEGMENT-SAKNAS                                                    
251700         MOVE SPACE TO DCS-KDDC                                           
251800     END-IF                                                               
251900     .                                                                    
252000 IMS-STATUSKONTROLL SECTION.                                              
252100                                                                          
252200     SET STATUS-IX TO 1                                                   
252300     SEARCH GODK-STATUS                                                   
252400       AT END CALL FELLOG                                                 
252500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
252600     END-SEARCH                                                           
252700     .                                                                    
253000     EJECT                                                                
260000*    -COPY WY2000PB                                                       
