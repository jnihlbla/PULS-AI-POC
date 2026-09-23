000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W476TRPT.                                                
000300 AUTHOR.         STINA MOGREN.                                            
000400 DATE-WRITTEN.   05/09/23.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        SUBPROGRAM TO WRITE 'LOADING REPORT' TRANSPORT                   
000900*        DOCUMENT. IT IS CALLED BY A PROGRAM W40631. DOCUMENT SHOW        
001000*        EACH DEALER,ADDRESS AND EACH KOLLI. FOR EACH KOLLI IT            
001100*        SHOWS PACKAGE SIZE,WEIGHT,VOLUME AND IS THAT CONTAINS            
001200*        HAZARDOUS GOODS. AFTER EACH DEALER IT SHOWS TOTAL LINE           
001300*        FOR TOTAL KOLLI,WEIGHT,VOLUME.AT THE END OF THE                  
001400*        DOCUMENT IT SHOWS THE TOTAL FOR THE FULL SHIPMENT/DIST           
001500*                                                                         
001600*        THE PROGRAM READS     WDE1                                       
001700*        THE PROGRAM READS     WDQ2                                       
001800*        THE PROGRAM READS     WDR1                                       
001900*                                                                         
002000*    ABENDCODES:                                                          
002100*        U0016 -  . . . .                                                 
002200*        U1000 -  . . . .                                                 
002300*                                                                         
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP2                                                                
002700 INPUT-OUTPUT SECTION.                                                    
002800                                                                          
002900 FILE-CONTROL.                                                            
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP2                                                                
003300 FILE SECTION.                                                            
003400     EJECT                                                                
003500 WORKING-STORAGE SECTION.                                                 
003600                                                                          
003700 77  IDPGM                       PIC X(8)   VALUE 'W476TRPT'.             
003800 77  YES                         PIC X      VALUE 'J'.                    
003900 77  NOO                         PIC X      VALUE 'N'.                    
004000 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004100 77  WS-IX                       PIC S9(4)  VALUE +0    COMP SYNC.        
004200 77  FLSAMFAK-SW                 PIC X(01)  VALUE 'N'.                    
004300     88 FLSAMFAK                            VALUE 'J'.                    
004400                                                                          
004500 77  W-PAGE-NO                   PIC S9(3)  COMP-3 VALUE ZERO.            
004600     EJECT                                                                
004700 01  W-YYMMDD                    PIC 9(06)  VALUE ZERO.                   
004800 01  CURR-SECTION                PIC X(32)  VALUE SPACE.                  
004900 01  CARRIER-TEXT                PIC X(18)  VALUE SPACE.                  
005000 01  WS-IDTRPTNR                 PIC X(3)   VALUE SPACE.                  
005100                                                                          
005200*    --- STYRTECKEN PRINTER                                               
005300 01  WS-PAGESKIP                   PIC X      VALUE '1'.                  
005400 01  WS-SKIP1                      PIC X      VALUE ' '.                  
005500 01  WS-SKIP2                      PIC X      VALUE '0'.                  
005600 01  WS-SKIP3                      PIC X      VALUE '-'.                  
005700                                                                          
005800 01  WS-TYP-IDSHIP                 PIC X(19) VALUE                        
005900                                   'LOADING REPORT     '.                 
006000 01  WS-RAD                        PIC S9(3) VALUE ZERO COMP-3.           
006100 01  W-LINE.                                                              
006200     03  W-LINE-COUNT            PIC 9(02)  VALUE ZERO.                   
006300     03  W-LINE-MAX              PIC 9(02)  VALUE 43.                     
006400     03  W-LINE-HEAD             PIC 9(02)  VALUE 5.                      
006500     03  W-LINE-GRTOTAL          PIC 9(02)  VALUE 5.                      
006600                                                                          
006700 01  TODAYS-DATE                 PIC 9(6)   VALUE ZERO.                   
006800 01  FILLER REDEFINES TODAYS-DATE.                                        
006900     03  TODAYS-DATE-YEAR        PIC 9(2).                                
007000     03  TODAYS-DATE-MONTH       PIC 9(2).                                
007100     03  TODAYS-DATE-DAY         PIC 9(2).                                
007200     EJECT                                                                
007300                                                                          
007400 01  ARB-RAD                     PIC X(132)  VALUE SPACE.                 
007500                                                                          
007600 01  RAD-HEAD.                                                            
007700     03  FILLER                    PIC X(15).                             
007800     03  RAD1H-IMPORTER-TEXT       PIC X(13).                             
007900     03  FILLER                    PIC X(2).                              
008000     03  RAD1H-IMPORTER            PIC X(35).                             
008100     03  FILLER                    PIC X(2).                              
008200     03  RAD-TYP-IDSHIP            PIC X(25).                             
008300                                                                          
008400 01  RAD2-HEAD.                                                           
008500     03  FILLER                    PIC X(30).                             
008600     03  RAD2H-IMPORTER            PIC X(35).                             
008700                                                                          
008800 01  RAD3-HEAD.                                                           
008900     03  FILLER                    PIC X(30).                             
009000     03  RAD3H-IMPORTER            PIC X(35).                             
009100                                                                          
009200 01  RAD5-HEAD.                                                           
009300     03  FILLER                    PIC X(30).                             
009400     03  RAD5H-IMPORTER            PIC X(35).                             
009500     03  FILLER                    PIC X(25).                             
009600     03  RAD5H-CARRIER-TEXT        PIC X(18).                             
009700                                                                          
009800 01  RAD1.                                                                
009900     03  FILLER                    PIC X(30).                             
010000     03  RAD4H-IMPORTER            PIC X(35).                             
010100     03  FILLER                    PIC X(02).                             
010200     03  RAD1-TIAAMMDD             PIC 9(06).                             
010300     03  RAD1-IDDISTR              PIC Z(4)9.                             
010400     03  FILLER                    PIC X(01).                             
010500     03  RAD1-IDSHIPM              PIC Z(06)9.                            
010600     03  FILLER                    PIC X(04).                             
010700     03  RAD1-IDTRPTNR             PIC Z(02)9.                            
010800     03  FILLER                    PIC X(01).                             
010900     03  RAD1-IDLBBET              PIC X(12).                             
011000     03  FILLER                    PIC X(02).                             
011100     03  RAD1-PAGE-NO              PIC Z(03).                             
011200                                                                          
011300 01  RAD2.                                                                
011400     03 FILLER             PIC X(8)    VALUE SPACE.                       
011500     03 RAD2-IDKUNDNR-LEDTEXT                                             
011600                           PIC X(6).                                      
011700     03 FILLER             PIC X(01).                                     
011800     03 RAD2-IDKUNDNR      PIC Z(7).                                      
011900     03 FILLER             PIC X(2)    VALUE SPACE.                       
012000     03 RAD2-BEGMT1        PIC X(35).                                     
012100     03 FILLER             PIC X(2)    VALUE SPACE.                       
012200     03 RAD2-ADGMT-GATA    PIC X(35).                                     
012300                                                                          
012400 01  RAD3.                                                                
012500     03 FILLER             PIC X(24)   VALUE SPACE.                       
012600     03 RAD3-BEGMT2        PIC X(35).                                     
012700     03 FILLER             PIC X(2)    VALUE SPACE.                       
012800     03 RAD3-ADGMT-PADR    PIC X(35).                                     
012900                                                                          
013000 01  RAD4.                                                                
013100     03 FILLER             PIC X(61)   VALUE SPACE.                       
013200     03 RAD4-ADGMT-LAND    PIC X(35).                                     
013300                                                                          
013400 01  RAD5.                                                                
013500     03  FILLER                   PIC X(1).                               
013600     03  RAD5-LINE-X              PIC X(5).                               
013700     03  FILLER                   PIC X(2).                               
013800     03  RAD5-IDORDNR5-X          PIC X(6).                               
013900     03  FILLER                   PIC X(1).                               
014000     03  RAD5-IDKOLLI-X           PIC X(5).                               
014100     03  FILLER                   PIC X(2).                               
014200     03  RAD5-BEEMBTYP-X          PIC X(12).                              
014300     03  FILLER                   PIC X(1).                               
014400     03  RAD5-DIKOLLIL-X          PIC X(5).                               
014500     03  FILLER                   PIC X(1).                               
014600     03  RAD5-DIKOLLIB-X          PIC X(3).                               
014700     03  FILLER                   PIC X(1).                               
014800     03  RAD5-DIKOLLIH-X          PIC X(3).                               
014900     03  FILLER                   PIC X(1).                               
015000     03  RAD5-VKORDBTO-X          PIC X(8).                               
015102**** 03  FILLER                   PIC X(1).                               
015202     03  FILLER                   PIC X(3).                               
015302     03  RAD5-VKORDNTO-X          PIC X(8)  JUSTIFIED RIGHT.              
015402     03  FILLER                   PIC X(1).                               
015502     03  RAD5-VLORDBTO-X          PIC X(8)  JUSTIFIED RIGHT.              
015602     03  FILLER                   PIC X(5).                               
015702     03  FILLER                   PIC X(27).                              
015802                                                                          
015902 01  RAD6.                                                                
016002     03  FILLER                   PIC X(1).                               
016102     03  RAD6-LINE                PIC Z(4).                               
016202     03  FILLER                   PIC X(3).                               
016302     03  RAD6-IDORDNR7-IDKOLLI-X.                                         
016402       05  RAD6-IDORDNR7-X.                                               
016502         07  RAD6-IDORDNR7        PIC Z(5)9.                              
016602       05  RAD6-IDKOLLI-SEP       PIC X(1)  VALUE ' '.                    
016702       05  RAD6-IDKOLLI-X.                                                
016802         07  RAD6-IDKOLLI         PIC Z9(3).                              
016902     03  FILLER                   PIC X(3).                               
017002     03  RAD6-BEEMBTYP            PIC X(12).                              
017102     03  FILLER                   PIC X(1).                               
017202     03  RAD6-DIKOLLIL-X.                                                 
017302       05  RAD6-DIKOLLIL          PIC Z(5).                               
017402     03  FILLER                   PIC X(1).                               
017502     03  RAD6-DIKOLLIB-X.                                                 
017602       05  RAD6-DIKOLLIB          PIC Z(3).                               
017702     03  FILLER                   PIC X(1).                               
017802     03  RAD6-DIKOLLIH-X.                                                 
017902       05  RAD6-DIKOLLIH          PIC Z(3).                               
018002     03  FILLER                   PIC X(1).                               
018102     03  RAD6-VKORDBTO-W.                                                 
018202       05  RAD6-VKORDBTO          PIC Z(5)9.9.                            
018302     03  FILLER  REDEFINES RAD6-VKORDBTO-W.                               
018402       05  RAD6-VKORDBTO-X        PIC X(8)  JUSTIFIED RIGHT.              
018502     03  FILLER                   PIC X(1).                               
018602     03  RAD6-VKORDNTO-W.                                                 
018702****   05  RAD6-VKORDNTO          PIC Z(5)9.9.                            
018802       05  RAD6-VKORDNTO          PIC Z(5)9.999.                          
018902     03  FILLER  REDEFINES RAD6-VKORDNTO-W.                               
019002****   05  RAD6-VKORDNTO-X        PIC X(8)  JUSTIFIED RIGHT.              
019102       05  RAD6-VKORDNTO-X        PIC X(10) JUSTIFIED RIGHT.              
019202     03  FILLER                   PIC X(1).                               
019302     03  RAD6-VLORDBTO-W.                                                 
019402       05  RAD6-VLORDBTO          PIC Z(3)9.999.                          
019502     03  FILLER  REDEFINES RAD6-VLORDBTO-W.                               
019602       05  RAD6-VLORDBTO-X        PIC X(8)  JUSTIFIED RIGHT.              
019702     03  FILLER                   PIC X(2).                               
019802     03  RAD6-FARLIG.                                                     
019902       05 RAD6-BEFARLIG           PIC X(17).                              
020002       05 FILLER                  PIC X(1).                               
020102       05 RAD6-KVFLAMP-X          PIC X(6).                               
020202       05 RAD6-KVFLAMP            PIC ZZ9.                                
020302     03  FILLER REDEFINES RAD6-FARLIG.                                    
020402       05 FILLER                  PIC X(13).                              
020502       05 RAD6-BEFARLIG-SDC       PIC X(4).                               
020602       05 FILLER                  PIC X(1).                               
020702       05 RAD6-KVFLAMP-X-SDC      PIC X(6).                               
020802       05 RAD6-KVFLAMP-SDC        PIC ZZ9.                                
020902                                                                          
021002 01  RAD7.                                                                
021102     03  FILLER                   PIC X(01).                              
021202     03  FILLER                   PIC X(7).                               
021302     03  RAD7-TOTAL-TEXT          PIC X(12).                              
021402     03  FILLER                   PIC X(1)     VALUE  ':'.                
021502     03  RAD7-KOLLI-TOTAL-X.                                              
021602         05  RAD7-KOLLI-TOTAL     PIC Z(6)9.                              
021702     03  FILLER                   PIC X(01).                              
021802     03  RAD7-KOLLI-TEXT          PIC X(7).                               
021902     03  FILLER                   PIC X(13).                              
022002     03  RAD7-TOTAL-VKORDBTO-W.                                           
022102       05  RAD7-TOTAL-VKORDBTO    PIC Z(5)9.9.                            
022202     03  FILLER                   PIC X(1).                               
022302     03  RAD7-TOTAL-VKORDNTO-W.                                           
022402*****  05  RAD7-TOTAL-VKORDNTO    PIC Z(5)9.9.                            
022502       05  RAD7-TOTAL-VKORDNTO    PIC Z(5)9.999.                          
022602     03  FILLER                   PIC X(1).                               
022702     03  RAD7-TOTAL-VLORDBTO-W.                                           
022802       05  RAD7-TOTAL-VLORDBTO    PIC Z(3)9.999.                          
022902     03  FILLER                   PIC X(1).                               
023002                                                                          
023102 01  RAD8.                                                                
023202     03  FILLER                   PIC X(01).                              
023302     03  FILLER                   PIC X(7).                               
023402     03  RAD8-TOTAL-TEXT          PIC X(25).                              
023502                                                                          
023602 01  RAD9.                                                                
023702     03  FILLER                   PIC X(26)   VALUE SPACES.               
023802     03  RAD9-END-TEXT            PIC X(25)                               
023902                               VALUE '* * * END OF INFO * * * '.          
024002 01  RAD10.                                                               
024102     03  FILLER                   PIC X(08)   VALUE SPACES.               
024202     03  RAD10-TELEF-TEXT         PIC X(10).                              
024302     03  RAD10-TELEFON            PIC X(25).                              
024402     03  FILLER                   PIC X(16)   VALUE SPACES.               
024502     03  RAD10-FAX-TEXT           PIC X(5).                               
024602     03  RAD10-FAX                PIC X(25).                              
024702                                                                          
024802 77  W-KDSPRAK                 PIC S9    VALUE +2  COMP-3.                
024902 77  W-SUORDV                  PIC S9(9)V9(2) VALUE ZERO COMP-3.          
025002 77  W-KDVALISO                PIC X(3)    VALUE SPACE.                   
025102 77  DUMMY-AREA                PIC X(50)   VALUE SPACE.                   
025202                                                                          
025302 01  W-PRINT-DOC-FLAG            PIC X(01)   VALUE 'N'.                   
025402     88  W-PRINT-NEW-DOC                     VALUE 'J'.                   
025502                                                                          
025602 01  W-TOTAL-FLAG                PIC X(01)   VALUE 'N'.                   
025702     88  W-PRINT-TOTAL                       VALUE 'J'.                   
025802                                                                          
025902 01  W-GRTOTAL-FLAG              PIC X(01)   VALUE 'N'.                   
026002     88  W-PRINT-GRTOTAL                     VALUE 'J'.                   
026102                                                                          
026202 01  W-PRINT-DEALER-FLAG         PIC X(01)   VALUE 'N'.                   
026302     88  W-PRINT-DEALER                      VALUE 'J'.                   
026402                                                                          
026502 01  W-VKORDNTO-KOLLI            PIC S9(6)V9(3).                          
026602                                                                          
026702 01  W-TOT.                                                               
026802     03  W-TOTAL   OCCURS 2.                                              
026902         05  W-TOTAL-SUORDV            PIC S9(9)V9(2).                    
027002         05  W-TOTAL-VKORDBTO          PIC S9(6)V9(1).                    
027102******   05  W-TOTAL-VKORDNTO          PIC S9(6)V9(1).                    
027202         05  W-TOTAL-VKORDNTO          PIC S9(6)V9(3).                    
027302         05  W-TOTAL-VLORDBTO          PIC S9(6)V9(3).                    
027402         05  W-TOTAL-KOLLI-COUNT       PIC S9(6).                         
027502*                                                                         
027602 01  W-FARLIG-TEXT-SDC22        PIC X(4)   VALUE 'DANG'.                  
027702 01  W-FARLIG-TEXT-SDC24        PIC X(4)   VALUE 'PER.'.                  
027802 01  W-FARLIG-TEXT-SDC25        PIC X(4)   VALUE 'PER.'.                  
027902*                                                                         
028002 01  TEST-IDDISTR               PIC 9(5)   COMP-3.                        
028102*01  FILLER   -COPY WWDIST34    -RED TEST-IDDISTR.                        
028202     EJECT                                                                
028302                                                                          
028402 01  FILLER          PIC X(32)  VALUE  'LEDTEXT TABLE'.                   
028502* 01 -COPY W475W552                                                       
028602     EJECT                                                                
028702* 01 -COPY W475W553                                                       
028802     EJECT                                                                
028902* 01 -COPY W476W001                                                       
029002     EJECT                                                                
029102                                                                          
029202 01  GENERAL-SUBPROGRAMS.                                                 
029302*                                                                         
029402     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
029502     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
029602     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
029702     03  W006PRS1                PIC X(8)    VALUE 'W006PRS1'.            
029802     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
029902     SKIP2                                                                
030002*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
030102                                                                          
030202 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
030302 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
030402 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
030502     SKIP2                                                                
030602 01  ERRTEXT.                                                             
030702     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
030802     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
030902 77  KDRC-DISPLAY                PIC Z(5).                                
031002     EJECT                                                                
031102*    --- PARAMETERS FOR SUBPROGRAM W006PRS1                               
031202     EJECT                                                                
031302*01  -COPY W006PRAR                                                       
031402 01  W-NYSIDA-RAD10          PIC S9(3)   VALUE +910  COMP-3.              
031502*                                        WRITE ON NEW LINE 10             
031602 01  W-IDPRTLST                  PIC X(8).                                
031702     SKIP2                                                                
031802*    --- AREAS FOR IMS-SECTIONS                                           
031902*                                                                         
032002     EJECT                                                                
032102 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
032202     SKIP3                                                                
032302 01  KEYS-TO-DLI.                                                         
032402                                                                          
032502     03  W-IDSHIPM-X.                                                     
032602         05  W-IDSHIPM           PIC 9(7)    VALUE ZERO.                  
032702                                                                          
032802     03  W-WDE111KY-X.                                                    
032902         05  W-WDE111-IDDISTR    PIC S9(05)  VALUE ZERO COMP-3.           
033002         05  W-WDE111-IDKUNDNR   PIC S9(07)  VALUE ZERO COMP-3.           
033102                                                                          
033202     03  W-WDE111KY-MIN.                                                  
033302         05  W-WDE111-IDDISTR-MIN PIC S9(05)  VALUE ZERO COMP-3.          
033402         05  FILLER               PIC X(04)   VALUE LOW-VALUES.           
033502                                                                          
033602     03  W-WDE111KY-MAX.                                                  
033702         05  W-WDE111-IDDISTR-MAX PIC S9(05)  VALUE ZERO COMP-3.          
033802         05  FILLER               PIC X(04)   VALUE HIGH-VALUES.          
033902                                                                          
034002     03  W-WDE121KY-X.                                                    
034102         05  W-WDE121-IDPRODNR   PIC S9(07)  VALUE ZERO COMP-3.           
034202         05  W-WDE121-IDKOLLI    PIC S9(05)  VALUE ZERO COMP-3.           
034302                                                                          
034402                                                                          
034502     03  W-IDORDER-X.                                                     
034602         05  W-IDORDER           PIC S9(7)   VALUE ZERO COMP-3.           
034702                                                                          
034802     03  W-WDGXKEY-4738-X.                                                
034902         05  FILLER              PIC X(4)     VALUE '4738'.               
035002         05  W-KDEMBTYP          PIC S9(3)    VALUE +0  COMP-3.           
035102         05  FILLER              PIC X(24)    VALUE LOW-VALUE.            
035202                                                                          
035302     03  W-WDB101KY-X.                                                    
035402         05  W-WDB101-IDPARTNR   PIC X(09)   VALUE SPACE.                 
035502         05  W-WDB101-IDFTG      PIC 9(02)   VALUE ZERO.                  
035602                                                                          
035702     03  W-WDB201KY-X.                                                    
035802         05  W-WDB201-IDDISTR    PIC S9(05)  VALUE ZERO COMP-3.           
035902         05  W-WDB201-IDKUNDNR   PIC S9(07)  VALUE ZERO COMP-3.           
036002                                                                          
036102     03  W-IDDC-B6-X.                                                     
036202         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
036302                                                                          
036402                                                                          
036502     SKIP2                                                                
036602*    --- STATUS-KOD FRÅN IMS                                              
036702 01  STATUS-WS                   PIC XX.                                  
036802     88  SEGMENT-FOUND                       VALUE '  '.                  
036902     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
037002     88  SEGMENT-MISSING                     VALUE 'GE'.                  
037102     SKIP2                                                                
037202 01  GOOD-STATUSCODES.                                                    
037302     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
037402     SKIP3                                                                
037502 01  SSA1                        PIC X(64).                               
037602 01  SSA2                        PIC X(64).                               
037702 01  SSA3                        PIC X(64).                               
037802 01  SSA4                        PIC X(64).                               
037902     EJECT                                                                
038002                                                                          
038102 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
038202 01  SEND-AREA.                                                           
038302*    03  -COPY WZ01SEND                                                   
038402                                                                          
038502 01  SEND-RAD-STYRTECKEN.                                                 
038602     03  STYRTECKEN-RAD          PIC X.                                   
038702     03  SEND-RAD                PIC X(120)  VALUE SPACE.                 
038802                                                                          
038902 01  DAP-AREA-START              PIC X(24)   VALUE                        
039002                                             'DAP-AREA-START'.            
039102                                                                          
039202 01  FILLER                 PIC X(16)   VALUE 'HDR-AREA'.                 
039302 01  HDR-AREA.                                                            
039402*    03  -COPY WZ01REQU  -PRE HDR-                                        
039502*    03  -COPY WZ04HDR                                                    
039602                                                                          
039702*    --- IMS FUNCTION CODES                                               
039802*01  -COPY W0003                                                          
039902     EJECT                                                                
040002*    ---  DLI INPUT-OUTPUT AREA                                           
040102 01  FILLER         PIC X(20) VALUE 'DLI-IO-WDE101-WDE111'.               
040202 01  DLI-IO-WDE101-11.                                                    
040302     03  DLI-IO-WDE101.                                                   
040402*        05  -COPY WDE101                                                 
040502     03  DLI-IO-WDE111.                                                   
040602*        05  -COPY WDE111                                                 
040702     EJECT                                                                
040802 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE121'.                      
040902 01  DLI-IO-WDE121.                                                       
041002*    03  -COPY WDE121                                                     
041003     EJECT                                                                
041004 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE131'.                      
041005 01  DLI-IO-WDE131.                                                       
041006*    03  -COPY WDE131                                                     
041102 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ201'.                      
041202 01  DLI-IO-WDQ201.                                                       
041302*    03  -COPY WDQ201                                                     
041402 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4738'.                    
041502 01  DLI-IO-WDGX4738.                                                     
041602*    03  -COPY WDGX4738                                                   
041702 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB101'.                      
041802 01  DLI-IO-WDB101.                                                       
041902*    03  -COPY WDB101                                                     
042002 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
042102 01  DLI-IO-WDB201.                                                       
042202*    03  -COPY WDB201                                                     
042302 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
042402 01  DLI-IO-WDB601.                                                       
042502*    03  -COPY WDB601                                                     
042602     EJECT                                                                
042702                                                                          
042802     EJECT                                                                
042902 LINKAGE SECTION.                                                         
043002                                                                          
043102*01  -COPY W476TRPD                                                       
043202                                                                          
043302 01  ALT-PCB                     PIC X(32).                               
043402                                                                          
043502*01  -COPY W0008  -PRE WDE1-                                              
043602     05  FILLER                  PIC X.                                   
043702                                                                          
043802*01  -COPY W0008  -PRE WDQ2-                                              
043902     05  FILLER                  PIC X.                                   
044002                                                                          
044102*01  -COPY W0008  -PRE WDR1-                                              
044202     05  FILLER                  PIC X.                                   
044302                                                                          
044402*01  -COPY W0008  -PRE WDB1-                                              
044502     05  FILLER                  PIC X.                                   
044602                                                                          
044702*01  -COPY W0008  -PRE WDB2-                                              
044802     05  FILLER                  PIC X.                                   
044902                                                                          
045002*01  -COPY W0008  -PRE WDB6-                                              
045102     05  FILLER                  PIC X.                                   
045202     EJECT                                                                
045302 PROCEDURE DIVISION  USING TRPD-W476TRPD ALT-PCB                          
045402                           WDE1-PCB WDQ2-PCB WDR1-PCB                     
045502                           WDB2-PCB WDB1-PCB WDB6-PCB.                    
045602 MAIN SECTION.                                                            
045702     ENTRY 'DLITCBL' USING TRPD-W476TRPD ALT-PCB                          
045802                           WDE1-PCB WDQ2-PCB WDR1-PCB                     
045902                           WDB2-PCB WDB1-PCB WDB6-PCB.                    
046002                                                                          
046102     PERFORM A-INIT                                                       
046202                                                                          
046302     PERFORM IMS-GU-WDE111-DIST                                           
046402     MOVE SHIP-IDDC             TO W-IDDC-B6                              
046502     PERFORM IMS-GU-WDB601                                                
046602     PERFORM S02-TELEFON                                                  
046702                                                                          
046802     MOVE YES       TO  W-PRINT-DOC-FLAG                                  
046902                                                                          
047002     PERFORM UNTIL NOT SEGMENT-FOUND                                      
047102       IF WS-IX = ZERO                                                    
047202         PERFORM B-IMPORTER                                               
047302       END-IF                                                             
047402       PERFORM B-DEALER-DETAIL                                            
047502       PERFORM IMS-GNP-WDE111-DIST                                        
047602     END-PERFORM                                                          
047702                                                                          
047802     IF W-PRINT-GRTOTAL                                                   
047902       PERFORM S24-PRINT-GRTOTAL                                          
048002     END-IF                                                               
048102                                                                          
048202     MOVE ZERO TO RETURN-CODE                                             
048302     GOBACK                                                               
048402     .                                                                    
048502     EJECT                                                                
048602 A-INIT SECTION.                                                          
048702                                                                          
048802     MOVE LENGTH OF SEND-RAD           TO SEND-KVDLEN                     
048902     ACCEPT TODAYS-DATE  FROM DATE                                        
049002                                                                          
049102     MOVE TRPD-IDPRTLST   TO W-IDPRTLST                                   
049202     MOVE TRPD-IDSHIPM    TO W-IDSHIPM                                    
049302     MOVE TRPD-PFDEF-OVR  TO PRT-PFDEF-OVR                                
049402     MOVE TRPD-IDDISTR    TO W-WDE111-IDDISTR                             
049502                             W-WDB201-IDDISTR                             
049602                             W-WDE111-IDDISTR-MIN                         
049702                             W-WDE111-IDDISTR-MAX                         
049802                             TEST-IDDISTR                                 
049902                                                                          
050002     MOVE NOO             TO W-PRINT-DOC-FLAG                             
050102                             W-PRINT-DEALER-FLAG                          
050202                             W-TOTAL-FLAG                                 
050302                                                                          
050402     MOVE 2               TO W-KDSPRAK                                    
050502     MOVE ZERO            TO W-PAGE-NO                                    
050602     MOVE SPACE           TO RAD-HEAD                                     
050702                             RAD2-HEAD                                    
050802                             RAD3-HEAD                                    
050902                             RAD5-HEAD                                    
051002                                                                          
051102     PERFORM S22-INIT-TOTAL                                               
051202** BUILD HEADER                                                           
051302     MOVE RADNR-LEDTEXT (W-KDSPRAK)    TO RAD5-LINE-X                     
051402     MOVE IDKUNDRF-LEDT-1-5 (W-KDSPRAK)                                   
051502                                       TO RAD5-IDORDNR5-X                 
051602     MOVE IDKOLLI-LEDTEXT (W-KDSPRAK)  TO RAD5-IDKOLLI-X                  
051702     MOVE BEEMBTYP-LEDTEXT (W-KDSPRAK) TO RAD5-BEEMBTYP-X                 
051802     MOVE DIKOLLIL-LEDTEXT (W-KDSPRAK) TO RAD5-DIKOLLIL-X                 
051902     MOVE DIKOLLIB-LEDTEXT (W-KDSPRAK) TO RAD5-DIKOLLIB-X                 
052002     MOVE DIKOLLIH-LEDTEXT (W-KDSPRAK) TO RAD5-DIKOLLIH-X                 
052102     MOVE VKORDBTO-LEDTEXT (W-KDSPRAK) TO RAD5-VKORDBTO-X                 
052202     MOVE VKORDNTO-LEDTEXT (W-KDSPRAK) TO RAD5-VKORDNTO-X                 
052302     MOVE VLORDBTO-LEDTEXT (W-KDSPRAK) TO RAD5-VLORDBTO-X                 
052402     MOVE TELEF-LEDTEXT (W-KDSPRAK)    TO RAD10-TELEF-TEXT                
052502     MOVE FAX-LEDTEXT (W-KDSPRAK)      TO RAD10-FAX-TEXT                  
052602     MOVE ZERO         TO WS-IX                                           
052702                                                                          
052802     .                                                                    
052902     EJECT                                                                
053002 B-IMPORTER SECTION.                                                      
053102                                                                          
053202     MOVE SHIP-IDTRPTNR  TO WS-IDTRPTNR                                   
053302     PERFORM S01-CARRIER-TEXT                                             
053402     MOVE SGMT-IDKUNDNR  TO W-WDE111-IDKUNDNR                             
053502                            W-WDB201-IDKUNDNR                             
053602     PERFORM IMS-GU-WDB201                                                
053702     MOVE SGMT-IDPARTNR  TO W-WDB101-IDPARTNR                             
053802     MOVE GMT-IDFTG      TO W-WDB101-IDFTG                                
053902     PERFORM IMS-GU-WDB101                                                
054002     MOVE BET-BEBETRAD-1 TO RAD1H-IMPORTER                                
054102     MOVE BET-BEBETRAD-2 TO RAD2H-IMPORTER                                
054202     MOVE BET-ADBETRAD-1 TO RAD3H-IMPORTER                                
054302     MOVE BET-ADBETRAD-2 TO RAD4H-IMPORTER                                
054402     MOVE BET-BELAND-SVE TO RAD5H-IMPORTER                                
054502     MOVE CARRIER-TEXT   TO RAD5H-CARRIER-TEXT                            
054602     ADD +1 TO WS-IX                                                      
054702     .                                                                    
054802     EJECT                                                                
054902                                                                          
055002 B-DEALER-DETAIL SECTION.                                                 
055102                                                                          
055202     MOVE SPACE         TO RAD1                                           
055302                           RAD2                                           
055402                           RAD3                                           
055502                           RAD4                                           
055602                           RAD6                                           
055702                           RAD7                                           
055802                                                                          
055902     MOVE YES           TO W-PRINT-DEALER-FLAG                            
056002     MOVE IDKUNDNR-LEDTEXT (W-KDSPRAK)                                    
056102                        TO RAD2-IDKUNDNR-LEDTEXT                          
056202     MOVE SGMT-IDKUNDNR TO RAD2-IDKUNDNR                                  
056302     PERFORM S20-HAMTA-WDB2                                               
056402     PERFORM BA-PACKAGE-DETAIL                                            
056502     .                                                                    
056602     EJECT                                                                
056702                                                                          
056802 BA-PACKAGE-DETAIL SECTION.                                               
056902                                                                          
057002     MOVE NOO            TO W-TOTAL-FLAG                                  
057102     MOVE SGMT-IDKUNDNR  TO W-WDE111-IDKUNDNR                             
057202     PERFORM IMS-GNP-WDE121                                               
057302     PERFORM UNTIL NOT SEGMENT-FOUND                                      
057402                                                                          
057502        PERFORM BAA-WRITE-LINE                                            
057602        PERFORM IMS-GNP-WDE121                                            
057702                                                                          
057802     END-PERFORM                                                          
057902                                                                          
058002     IF W-PRINT-TOTAL                                                     
058102        MOVE NOO                    TO W-TOTAL-FLAG                       
058202        MOVE SPACES                 TO RAD7                               
058302        MOVE PRODGRP-SUMMA-LEDTEXT (W-KDSPRAK)                            
058402                                    TO RAD7-TOTAL-TEXT                    
058502        MOVE IDKUNDRF-IDKOLLI-TOT-LEDTEXT (W-KDSPRAK)                     
058602                                    TO RAD7-KOLLI-TEXT                    
058702                                                                          
058802        MOVE W-TOTAL-VKORDBTO (1)   TO RAD7-TOTAL-VKORDBTO                
058902        MOVE W-TOTAL-VKORDNTO (1)   TO RAD7-TOTAL-VKORDNTO                
059002        MOVE W-TOTAL-VLORDBTO (1)   TO RAD7-TOTAL-VLORDBTO                
059102        MOVE W-TOTAL-KOLLI-COUNT (1)                                      
059202                                    TO RAD7-KOLLI-TOTAL                   
059302                                                                          
059402        MOVE ZERO                   TO W-TOTAL-VKORDBTO (1)               
059502                                       W-TOTAL-VKORDNTO (1)               
059602                                       W-TOTAL-VLORDBTO (1)               
059702                                       W-TOTAL-KOLLI-COUNT (1)            
059802        MOVE RAD7                TO ARB-RAD                               
059902                                    SEND-RAD                              
060002        ADD 1                    TO W-LINE-COUNT                          
060102        MOVE PRT-AFTER-1         TO PRT-RADSKIP                           
060202        MOVE WS-SKIP1            TO STYRTECKEN-RAD                        
060302        PERFORM S21-PRINT-LINE                                            
060402        MOVE WS-SKIP1            TO STYRTECKEN-RAD                        
060502                                                                          
060602     END-IF                                                               
060702     .                                                                    
060802     EJECT                                                                
060902                                                                          
061002 BAA-WRITE-LINE SECTION.                                                  
061102     IF W-PRINT-NEW-DOC                                                   
061202        MOVE 0              TO W-PAGE-NO                                  
061302        PERFORM S25-PRINT-DISTR                                           
061402     END-IF                                                               
061502                                                                          
061602     IF W-PRINT-DEALER                                                    
061702        MOVE NOO                 TO W-PRINT-DEALER-FLAG                   
061802        MOVE SGMT-IDKUNDNR       TO RAD2-IDKUNDNR                         
061902        MOVE SKOLLI-IDORDER      TO W-IDORDER                             
062002        PERFORM IMS-GU-WDQ201                                             
062102        IF SEGMENT-FOUND                                                  
062202           MOVE OHUV-BEGMT-RAD1  TO RAD2-BEGMT1                           
062302           MOVE OHUV-ADGMT-GATA  TO RAD2-ADGMT-GATA                       
062402           MOVE OHUV-BEGMT-RAD2  TO RAD3-BEGMT2                           
062502           MOVE OHUV-ADGMT-PADR  TO RAD3-ADGMT-PADR                       
062602           MOVE OHUV-ADGMT-LAND  TO RAD4-ADGMT-LAND                       
062702        ELSE                                                              
062802           MOVE SPACE            TO RAD2-BEGMT1                           
062902                                    RAD2-ADGMT-GATA                       
063002                                    RAD3-BEGMT2                           
063102                                    RAD3-ADGMT-PADR                       
063202                                    RAD4-ADGMT-LAND                       
063302        END-IF                                                            
063402                                                                          
063502        IF W-LINE-COUNT >  W-LINE-MAX - W-LINE-HEAD                       
063602           PERFORM S25-PRINT-DISTR                                        
063702        END-IF                                                            
063802                                                                          
063902        MOVE RAD2                TO ARB-RAD                               
064002                                    SEND-RAD                              
064102        ADD 2                    TO W-LINE-COUNT                          
064202        MOVE PRT-AFTER-2         TO PRT-RADSKIP                           
064302        MOVE WS-SKIP2            TO STYRTECKEN-RAD                        
064402        PERFORM S21-PRINT-LINE                                            
064502        MOVE WS-SKIP1            TO STYRTECKEN-RAD                        
064602                                                                          
064702        MOVE RAD3                TO ARB-RAD                               
064802                                    SEND-RAD                              
064902        ADD 1                    TO W-LINE-COUNT                          
065002        MOVE PRT-AFTER-1         TO PRT-RADSKIP                           
065102        MOVE WS-SKIP1            TO STYRTECKEN-RAD                        
065202        PERFORM S21-PRINT-LINE                                            
065302        MOVE WS-SKIP1            TO STYRTECKEN-RAD                        
065402                                                                          
065502        MOVE RAD4                TO ARB-RAD                               
065602                                    SEND-RAD                              
065702        ADD 1                    TO W-LINE-COUNT                          
065802        MOVE PRT-AFTER-1         TO PRT-RADSKIP                           
065902        MOVE WS-SKIP1            TO STYRTECKEN-RAD                        
066002        PERFORM S21-PRINT-LINE                                            
066102        MOVE WS-SKIP1            TO STYRTECKEN-RAD                        
066202                                                                          
066302        MOVE PRT-AFTER-1         TO PRT-RADSKIP                           
066402        MOVE WS-SKIP1            TO STYRTECKEN-RAD                        
066502        ADD 1                    TO W-LINE-COUNT                          
066602        PERFORM S23-PRINT-HEADER                                          
066702        MOVE WS-SKIP1            TO STYRTECKEN-RAD                        
066802     END-IF                                                               
066902                                                                          
067002     IF W-LINE-COUNT > W-LINE-MAX   -  1                                  
067102        PERFORM S25-PRINT-DISTR                                           
067202        MOVE PRT-AFTER-2         TO PRT-RADSKIP                           
067302        MOVE WS-SKIP2            TO STYRTECKEN-RAD                        
067402        ADD 2                    TO W-LINE-COUNT                          
067502        PERFORM S23-PRINT-HEADER                                          
067602        MOVE WS-SKIP1            TO STYRTECKEN-RAD                        
067702     END-IF                                                               
067802     MOVE SPACES             TO RAD6                                      
067902     ADD 1                   TO WS-RAD                                    
068002     MOVE WS-RAD             TO RAD6-LINE                                 
068102     MOVE SKOLLI-IDORDNR7    TO RAD6-IDORDNR7                             
068202     MOVE ' '                TO RAD6-IDKOLLI-SEP                          
068302     MOVE SKOLLI-IDKOLLI     TO RAD6-IDKOLLI                              
068402     MOVE SKOLLI-KDEMBTYP    TO W-KDEMBTYP                                
068502     PERFORM IMS-GU-WDGX4738                                              
068602     IF SEGMENT-FOUND                                                     
068702        MOVE EMBTYP-BEEMBTYP(W-KDSPRAK) TO RAD6-BEEMBTYP                  
068802     END-IF                                                               
068902     MOVE SKOLLI-DIKOLLIL    TO RAD6-DIKOLLIL                             
069002     MOVE SKOLLI-DIKOLLIB    TO RAD6-DIKOLLIB                             
069102     MOVE SKOLLI-DIKOLLIH    TO RAD6-DIKOLLIH                             
069202     MOVE SKOLLI-VKORDBTO-KOLLI    TO RAD6-VKORDBTO                       
069302                                                                          
069402**** MOVE SKOLLI-VKORDNTO-KOLLI    TO RAD6-VKORDNTO                       
069502     PERFORM BAAC-BERAKNA-KOLLITS-NETTOVIKT                               
069602     MOVE W-VKORDNTO-KOLLI         TO RAD6-VKORDNTO                       
069702                                                                          
069802     MOVE SKOLLI-VLORDBTO-KOLLI    TO RAD6-VLORDBTO                       
069902                                                                          
070002                                                                          
070102     PERFORM BAAA-BUILD-TOTAL                                             
070202     PERFORM BAAB-KDFARLIG-CHECK                                          
070302     MOVE RAD6                TO ARB-RAD                                  
070402                                 SEND-RAD                                 
070502     ADD 1                    TO W-LINE-COUNT                             
070602     MOVE PRT-AFTER-1         TO PRT-RADSKIP                              
070702     MOVE WS-SKIP1            TO STYRTECKEN-RAD                           
070802     PERFORM S21-PRINT-LINE                                               
070902     MOVE WS-SKIP1            TO STYRTECKEN-RAD                           
071002     .                                                                    
071102     EJECT                                                                
071202                                                                          
071302 BAAA-BUILD-TOTAL SECTION.                                                
071402                                                                          
071502     ADD W-SUORDV               TO W-TOTAL-SUORDV (1)                     
071602                                   W-TOTAL-SUORDV (2)                     
071702     ADD SKOLLI-VKORDBTO-KOLLI  TO W-TOTAL-VKORDBTO (1)                   
071802                                   W-TOTAL-VKORDBTO (2)                   
071902**** ADD SKOLLI-VKORDNTO-KOLLI  TO W-TOTAL-VKORDNTO (1)                   
072002     ADD W-VKORDNTO-KOLLI       TO W-TOTAL-VKORDNTO (1)                   
072102                                   W-TOTAL-VKORDNTO (2)                   
072202     ADD SKOLLI-VLORDBTO-KOLLI  TO W-TOTAL-VLORDBTO (1)                   
072302                                   W-TOTAL-VLORDBTO (2)                   
072402     ADD 1                      TO W-TOTAL-KOLLI-COUNT (1)                
072502                                   W-TOTAL-KOLLI-COUNT (2)                
072602     MOVE YES                   TO W-TOTAL-FLAG                           
072702                                   W-GRTOTAL-FLAG                         
072802     .                                                                    
072902     EJECT                                                                
073002                                                                          
073102 BAAB-KDFARLIG-CHECK SECTION.                                             
073202                                                                          
073302     IF SKOLLI-KDFARLIG-KOLLI = +4                                        
073402     OR SKOLLI-KDFARLIG-KOLLI = +7                                        
073502       IF DIST34-FRANKRIKE-SDC OR                                         
073602          DIST34-ITALIEN-SDC OR                                           
073702          DIST34-SPANIEN-SDC                                              
073802         IF SKOLLI-KVFLAMP-KOLLI NOT = ZERO                               
073902           MOVE KVFLAMP-LEDTEXT (W-KDSPRAK)                               
074002                                      TO RAD6-KVFLAMP-X-SDC               
074102           MOVE SKOLLI-KVFLAMP-KOLLI  TO RAD6-KVFLAMP-SDC                 
074202         END-IF                                                           
074302       END-IF                                                             
074402       IF DIST34-FRANKRIKE-SDC                                            
074502         MOVE W-FARLIG-TEXT-SDC22     TO RAD6-BEFARLIG-SDC                
074602       ELSE                                                               
074702         IF DIST34-ITALIEN-SDC                                            
074802           MOVE W-FARLIG-TEXT-SDC25   TO RAD6-BEFARLIG-SDC                
074902         ELSE                                                             
075002           IF DIST34-SPANIEN-SDC                                          
075102             MOVE W-FARLIG-TEXT-SDC24 TO RAD6-BEFARLIG-SDC                
075202           ELSE                                                           
075302             MOVE BEFARLIG-TEXT (W-KDSPRAK) TO RAD6-BEFARLIG              
075402             IF SKOLLI-KVFLAMP-KOLLI  NOT = ZERO                          
075502               MOVE KVFLAMP-LEDTEXT (W-KDSPRAK)                           
075602                                            TO RAD6-KVFLAMP-X             
075702               MOVE SKOLLI-KVFLAMP-KOLLI    TO RAD6-KVFLAMP               
075802             END-IF                                                       
075902           END-IF                                                         
076002         END-IF                                                           
076102       END-IF                                                             
076202     END-IF                                                               
076302     .                                                                    
076402     EJECT                                                                
076502                                                                          
076602 BAAC-BERAKNA-KOLLITS-NETTOVIKT SECTION.                                  
076702                                                                          
076802     MOVE SGMT-IDKUNDNR    TO W-WDE111-IDKUNDNR                           
076902     MOVE SKOLLI-IDPRODNR  TO W-WDE121-IDPRODNR                           
077002     MOVE SKOLLI-IDKOLLI   TO W-WDE121-IDKOLLI                            
077102     MOVE ZERO             TO W-VKORDNTO-KOLLI                            
077202                                                                          
077302     PERFORM IMS-GNP-WDE131                                               
077402                                                                          
077502     PERFORM UNTIL NOT SEGMENT-FOUND                                      
077602        COMPUTE W-VKORDNTO-KOLLI =                                        
077702                W-VKORDNTO-KOLLI +                                        
077802**             (SRAD-VKARTNTO * SRAD-KVLEVART)                            
077803               (SRAD-VKART-NTO-KG * SRAD-KVLEVART)                        
077902                                                                          
078002        PERFORM IMS-GNP-WDE131                                            
078102     END-PERFORM                                                          
078202     .                                                                    
078302     EJECT                                                                
078402 S01-CARRIER-TEXT   SECTION.                                              
078502                                                                          
078602***   TRANSPORTISTA: JOSE MARTINEZ CONDIGO: FRAKTKOD ****                 
078702                                                                          
078802     MOVE SPACE                TO CARRIER-TEXT                            
078902                                                                          
079002     IF DCS-SDC AND DCS-IDLANDX2 = 'ES'                                   
079102**      SDC-ES                                                            
079202       EVALUATE TRUE                                                      
079302       WHEN WS-IDTRPTNR = '100'                                           
079402          MOVE 'AZKAR          '                                          
079502                               TO CARRIER-TEXT                            
079602       WHEN WS-IDTRPTNR = '200'                                           
079702          MOVE 'AZKAR MADRID   '                                          
079802                               TO CARRIER-TEXT                            
079902       WHEN WS-IDTRPTNR = '300'                                           
080002          MOVE 'CTE GRAN CANARIA'                                         
080102                               TO CARRIER-TEXT                            
080202       WHEN WS-IDTRPTNR = '400'                                           
080302          MOVE 'SUS MEDIOS     '                                          
080402                               TO CARRIER-TEXT                            
080502       WHEN WS-IDTRPTNR = '500'                                           
080602          MOVE 'GALLIKER       '                                          
080702                               TO CARRIER-TEXT                            
080802       WHEN WS-IDTRPTNR = '600'                                           
080902          MOVE 'TAXI           '                                          
081002                               TO CARRIER-TEXT                            
081102       WHEN WS-IDTRPTNR = '700'                                           
081202          MOVE 'IBEXPRESS      '                                          
081302                               TO CARRIER-TEXT                            
081402       WHEN WS-IDTRPTNR = '800'                                           
081502          MOVE 'A.V.E.         '                                          
081602                               TO CARRIER-TEXT                            
081702                                                                          
081802       WHEN WS-IDTRPTNR = '900'                                           
081902          MOVE 'LUALGA         '                                          
082002                               TO CARRIER-TEXT                            
082102       WHEN OTHER                                                         
082202          MOVE SPACE           TO CARRIER-TEXT                            
082302       END-EVALUATE                                                       
082402     ELSE                                                                 
082502       IF DCS-SDC AND DCS-IDLANDX2 = 'AT'                                 
082602**       SDC-AT                                                           
082702         MOVE 'ENGLMAYER'      TO CARRIER-TEXT                            
082802       ELSE                                                               
082902                                                                          
083002         IF DCS-NDC-PF AND DCS-IDLANDX2 = 'JP'                            
083102**         NDC-JP                                                         
083202           EVALUATE TRUE                                                  
083302           WHEN WS-IDTRPTNR = '100'                                       
083402             MOVE 'SEINO          '                                       
083502                               TO CARRIER-TEXT                            
083602           WHEN WS-IDTRPTNR = '200'                                       
083702             MOVE 'MEITETSU       '                                       
083802                               TO CARRIER-TEXT                            
083902           WHEN OTHER                                                     
084002             MOVE SPACE                                                   
084102                               TO CARRIER-TEXT                            
084202           END-EVALUATE                                                   
084302         ELSE                                                             
084402           IF DCS-NDC-PF AND DCS-IDLANDX2 = 'AU'                          
084502**           NDC-AU                                                       
084602             MOVE 'TNT'        TO CARRIER-TEXT                            
084702           ELSE                                                           
084802             IF DCS-CDC                                                   
084902**            CDC-SE                                                      
085002               MOVE '      '   TO CARRIER-TEXT                            
085102             END-IF                                                       
085202           END-IF                                                         
085302         END-IF                                                           
085402       END-IF                                                             
085502     END-IF                                                               
085602*      MOVE RUB-8 (TEXT-IX) TO LIST-RAD                                   
085702*      MOVE PRT-AFTER-2     TO PRT-RADSKIP                                
085802     .                                                                    
085902     EJECT                                                                
086002 S02-TELEFON  SECTION.                                                    
086102                                                                          
086202     MOVE SPACES                 TO RAD10-TELEFON                         
086302                                    RAD10-FAX                             
086402     IF DCS-SDC AND DCS-IDLANDX2 = 'ES'                                   
086502       MOVE '949-26 60 66     '  TO RAD10-TELEFON                         
086602       MOVE '949-26 60 31     '  TO RAD10-FAX                             
086702     END-IF                                                               
086802     IF DCS-SDC AND DCS-IDLANDX2 = 'AT'                                   
086902       MOVE '+43 2162 68 841  '  TO RAD10-TELEFON                         
087002       MOVE '+43 2162 68 4111 '  TO RAD10-FAX                             
087102     END-IF                                                               
087202     IF DCS-NDC-PF AND DCS-IDLANDX2 = 'JP'                                
087302       MOVE '+81(5675)5-2879  '  TO RAD10-TELEFON                         
087402       MOVE '+81(5675)5-2336  '  TO RAD10-FAX                             
087502     END-IF                                                               
087602     IF DCS-NDC-PF AND DCS-IDLANDX2 = 'AU'                                
087702       MOVE '+61-2-9827-3100  '  TO RAD10-TELEFON                         
087802       MOVE '+61-2-9827-3143  '  TO RAD10-FAX                             
087902     END-IF                                                               
088002     IF DCS-CDC                                                           
088102       MOVE SPACES               TO RAD10-TELEFON                         
088202                                    RAD10-FAX                             
088302     END-IF                                                               
088402     .                                                                    
088502     EJECT                                                                
088602                                                                          
088702 S20-HAMTA-WDB2  SECTION.                                                 
088802                                                                          
088902     MOVE SGMT-IDKUNDNR      TO W-WDB201-IDKUNDNR                         
089002     PERFORM IMS-GU-WDB201                                                
089102     IF GMT-KDSPRAK  <  ZERO OR > +5                                      
089202       MOVE +2 TO W-KDSPRAK                                               
089302     ELSE                                                                 
089402       COMPUTE W-KDSPRAK = GMT-KDSPRAK + +1                               
089502     END-IF                                                               
089602     IF W-KDSPRAK NOT = +2                                                
089702       MOVE RADNR-LEDTEXT (W-KDSPRAK)  TO RAD5-LINE-X                     
089802       MOVE IDKUNDRF-LEDT-1-5 (W-KDSPRAK)                                 
089902                                       TO RAD5-IDORDNR5-X                 
090002       MOVE IDKOLLI-LEDTEXT (W-KDSPRAK)  TO RAD5-IDKOLLI-X                
090102       MOVE BEEMBTYP-LEDTEXT (W-KDSPRAK) TO RAD5-BEEMBTYP-X               
090202       MOVE DIKOLLIL-LEDTEXT (W-KDSPRAK) TO RAD5-DIKOLLIL-X               
090302       MOVE DIKOLLIB-LEDTEXT (W-KDSPRAK) TO RAD5-DIKOLLIB-X               
090402       MOVE DIKOLLIH-LEDTEXT (W-KDSPRAK) TO RAD5-DIKOLLIH-X               
090502       MOVE VKORDBTO-LEDTEXT (W-KDSPRAK) TO RAD5-VKORDBTO-X               
090602       MOVE VKORDNTO-LEDTEXT (W-KDSPRAK) TO RAD5-VKORDNTO-X               
090702       MOVE VLORDBTO-LEDTEXT (W-KDSPRAK) TO RAD5-VLORDBTO-X               
090802     END-IF                                                               
090902     .                                                                    
091002     EJECT                                                                
091102                                                                          
091202 S21-PRINT-LINE SECTION.                                                  
091302                                                                          
091402*    -- PRINT TO ON-DEMAND IF SO SPECIFIED ON 4456                        
091502     PERFORM S90-PUT-DOC-LINE                                             
091602                                                                          
091702*    -- PRINT TO PAPER IF SO SPECIFIED ON 4664 (FLSKRIV-NU = YES)         
091802*    -- OR ALWAYS IF WE COME FROM 4622                                    
091902*    -- OR ALWAYS IF IT IS A WEB DC (WHERE FLSKRIV-NU IS N AND            
092002*    -- CANNOT BE SET TO J)                                               
092102     IF SHIP-FLSKRIV-NU = YES OR TRPD-IDPGM = 'W4062200'                  
092202     OR TRPD-FLLDCKND = YES                                               
092302       CALL W006PRS1 USING PRT-SPOOL-OVR                                  
092402                           PRT-WRITE                                      
092502                           W-IDPRTLST                                     
092602                           ALT-PCB                                        
092702                           PRT-RADSKIP                                    
092802                           ARB-RAD                                        
092902     END-IF                                                               
093002     .                                                                    
093102     EJECT                                                                
093202                                                                          
093302 S22-INIT-TOTAL SECTION.                                                  
093402                                                                          
093502     MOVE 1 TO INDX                                                       
093602     PERFORM UNTIL INDX > 2                                               
093702       MOVE ZERO TO W-TOTAL-SUORDV (INDX)                                 
093802                    W-TOTAL-VKORDBTO (INDX)                               
093902                    W-TOTAL-VKORDNTO (INDX)                               
094002                    W-TOTAL-VLORDBTO (INDX)                               
094102                    W-TOTAL-KOLLI-COUNT (INDX)                            
094202       ADD 1     TO INDX                                                  
094302     END-PERFORM                                                          
094402                                                                          
094502     .                                                                    
094602     EJECT                                                                
094702                                                                          
094802 S23-PRINT-HEADER SECTION.                                                
094902                                                                          
095002     MOVE RAD5                         TO ARB-RAD                         
095102                                          SEND-RAD                        
095202     PERFORM S21-PRINT-LINE                                               
095302     MOVE WS-SKIP1            TO STYRTECKEN-RAD                           
095402     .                                                                    
095502     EJECT                                                                
095602                                                                          
095702 S24-PRINT-GRTOTAL SECTION.                                               
095802                                                                          
095902     MOVE NOO                   TO W-GRTOTAL-FLAG                         
096002     MOVE SPACES                TO RAD8                                   
096102                                   ARB-RAD                                
096202                                   SEND-RAD                               
096302     MOVE IDSHIPM-TOTAL-LEDTEXT (W-KDSPRAK)                               
096402                                TO RAD8-TOTAL-TEXT                        
096502     MOVE PRT-AFTER-2           TO PRT-RADSKIP                            
096602     MOVE WS-SKIP2              TO STYRTECKEN-RAD                         
096702     ADD 2                      TO W-LINE-COUNT                           
096802     IF W-LINE-COUNT > W-LINE-MAX - W-LINE-GRTOTAL                        
096902       PERFORM S25-PRINT-DISTR                                            
097002       MOVE PRT-AFTER-1         TO PRT-RADSKIP                            
097102       MOVE WS-SKIP1            TO STYRTECKEN-RAD                         
097202       ADD 1                    TO W-LINE-COUNT                           
097302     END-IF                                                               
097402     MOVE RAD8                  TO ARB-RAD                                
097502                                   SEND-RAD                               
097602     PERFORM S21-PRINT-LINE                                               
097702     MOVE WS-SKIP1              TO STYRTECKEN-RAD                         
097802                                                                          
097902     MOVE SPACES                TO RAD7                                   
098002                                   ARB-RAD                                
098102                                   SEND-RAD                               
098202     MOVE IDKUNDRF-IDKOLLI-TOT-LEDTEXT (W-KDSPRAK)                        
098302                                TO RAD7-KOLLI-TEXT                        
098402     MOVE W-TOTAL-VKORDBTO (2)  TO RAD7-TOTAL-VKORDBTO                    
098502     MOVE W-TOTAL-VKORDNTO (2)  TO RAD7-TOTAL-VKORDNTO                    
098602     MOVE W-TOTAL-VLORDBTO (2)  TO RAD7-TOTAL-VLORDBTO                    
098702     MOVE W-TOTAL-KOLLI-COUNT (2)                                         
098802                                TO RAD7-KOLLI-TOTAL                       
098902     MOVE RAD7                  TO ARB-RAD                                
099002                                   SEND-RAD                               
099102     MOVE PRT-AFTER-1           TO PRT-RADSKIP                            
099202     MOVE WS-SKIP1              TO STYRTECKEN-RAD                         
099302     ADD 1                      TO W-LINE-COUNT                           
099402     PERFORM S21-PRINT-LINE                                               
099502     MOVE WS-SKIP1              TO STYRTECKEN-RAD                         
099602                                                                          
099702     ADD 2                      TO W-LINE-COUNT                           
099802     MOVE SPACES                TO RAD8                                   
099902     MOVE 'DRIVER NAME:'        TO RAD8-TOTAL-TEXT                        
100002     MOVE RAD8                  TO ARB-RAD                                
100102                                   SEND-RAD                               
100202     MOVE PRT-AFTER-2           TO PRT-RADSKIP                            
100302     MOVE WS-SKIP2              TO STYRTECKEN-RAD                         
100402     ADD 2                      TO W-LINE-COUNT                           
100502     PERFORM S21-PRINT-LINE                                               
100602     MOVE RAD9                  TO ARB-RAD                                
100702                                   SEND-RAD                               
100802     MOVE PRT-AFTER-2           TO PRT-RADSKIP                            
100902     MOVE WS-SKIP2              TO STYRTECKEN-RAD                         
101002     ADD 2                      TO W-LINE-COUNT                           
101102     PERFORM S21-PRINT-LINE                                               
101202                                                                          
101302     PERFORM S22-INIT-TOTAL                                               
101402     MOVE WS-SKIP1              TO STYRTECKEN-RAD                         
101502     .                                                                    
101602     EJECT                                                                
101702                                                                          
101802 S25-PRINT-DISTR SECTION.                                                 
101902                                                                          
102002     MOVE NOO              TO W-PRINT-DOC-FLAG                            
102102                                                                          
102202     MOVE SPACE                      TO SEND-RAD                          
102302     MOVE WS-PAGESKIP                TO STYRTECKEN-RAD                    
102402     PERFORM S90-PUT-DOC-LINE                                             
102502     MOVE WS-SKIP3                   TO STYRTECKEN-RAD                    
102602     PERFORM S90-PUT-DOC-LINE                                             
102702     MOVE WS-SKIP2                   TO STYRTECKEN-RAD                    
102802     PERFORM S90-PUT-DOC-LINE                                             
102902                                                                          
103002     MOVE BEVARREF-LEDTEXT (W-KDSPRAK) TO RAD1H-IMPORTER-TEXT             
103102     MOVE WS-TYP-IDSHIP              TO  RAD-TYP-IDSHIP                   
103202     MOVE  RAD-HEAD                  TO  ARB-RAD                          
103302                                         SEND-RAD                         
103402     MOVE WS-SKIP1                   TO  STYRTECKEN-RAD                   
103502     MOVE  PRT-NYSIDA-RAD7           TO  PRT-RADSKIP                      
103602     MOVE  7                         TO  W-LINE-COUNT                     
103702     PERFORM S21-PRINT-LINE                                               
103802                                                                          
103902     MOVE SPACE                      TO SEND-RAD                          
104002     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
104102*    PERFORM S90-PUT-DOC-LINE                                             
104202*    PERFORM S90-PUT-DOC-LINE                                             
104302                                                                          
104402     MOVE RAD2-HEAD                  TO ARB-RAD                           
104502                                        SEND-RAD                          
104602     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
104702     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
104802     ADD 1                           TO W-LINE-COUNT                      
104902     PERFORM S21-PRINT-LINE                                               
105002     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
105102                                                                          
105202     MOVE RAD3-HEAD                  TO ARB-RAD                           
105302                                        SEND-RAD                          
105402     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
105502     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
105602     ADD 1                           TO W-LINE-COUNT                      
105702     PERFORM S21-PRINT-LINE                                               
105802     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
105902                                                                          
106002     ADD 1                     TO W-PAGE-NO                               
106102     MOVE SPACE                TO RAD1                                    
106202     MOVE BET-ADBETRAD-2       TO RAD4H-IMPORTER                          
106302     MOVE SHIP-TISKEPPN        TO W-YYMMDD                                
106402     MOVE W-YYMMDD             TO RAD1-TIAAMMDD                           
106502     MOVE SGMT-IDDISTR         TO RAD1-IDDISTR                            
106602     MOVE SHIP-IDSHIPM         TO RAD1-IDSHIPM                            
106702     MOVE SHIP-IDTRPTNR        TO RAD1-IDTRPTNR                           
106802     MOVE SHIP-IDLBBET         TO RAD1-IDLBBET                            
106902     MOVE W-PAGE-NO            TO RAD1-PAGE-NO                            
107002     MOVE RAD1                 TO ARB-RAD                                 
107102                                  SEND-RAD                                
107202     MOVE PRT-AFTER-1          TO PRT-RADSKIP                             
107302     MOVE WS-SKIP1             TO STYRTECKEN-RAD                          
107402     ADD 1                     TO W-LINE-COUNT                            
107502     PERFORM S21-PRINT-LINE                                               
107602     MOVE WS-SKIP1             TO STYRTECKEN-RAD                          
107702                                                                          
107802     MOVE RAD5-HEAD        TO ARB-RAD                                     
107902                              SEND-RAD                                    
108002     MOVE PRT-AFTER-1      TO PRT-RADSKIP                                 
108102     MOVE WS-SKIP1         TO STYRTECKEN-RAD                              
108202     ADD 1                 TO W-LINE-COUNT                                
108302     PERFORM S21-PRINT-LINE                                               
108402                                                                          
108502     MOVE RAD10            TO ARB-RAD                                     
108602                              SEND-RAD                                    
108702     MOVE PRT-AFTER-2      TO PRT-RADSKIP                                 
108802     MOVE WS-SKIP2         TO STYRTECKEN-RAD                              
108902     ADD 2                 TO W-LINE-COUNT                                
109002     PERFORM S21-PRINT-LINE                                               
109102                                                                          
109202     MOVE WS-SKIP1         TO STYRTECKEN-RAD                              
109302     .                                                                    
109402     EJECT                                                                
109502                                                                          
109602 S90-PUT-DOC-LINE SECTION.                                                
109702     IF TRPD-IDPGM = 'W4063600' AND TRPD-KVCOPIES = '1'                   
109802       IF TRPD-FLSKRIV-ONDEM = YES                                        
109902         MOVE +1                          TO SEND-IDCOM                   
110002         MOVE 'PUT'                       TO SEND-KDFUNC                  
110102         MOVE LENGTH OF SEND-RAD-STYRTECKEN TO SEND-KVDLEN                
110202         CALL WZ01SEND USING SEND-CONTROL-AREA                            
110302                             SEND-KVDLEN                                  
110402                             SEND-RAD-STYRTECKEN                          
110502         IF SEND-KDRC > ZERO                                              
110602           MOVE SEND-KDRC                 TO KDRC-DISPLAY                 
110702           STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                   
110802           DELIMITED BY SIZE INTO ERRTEXT-STR                             
110902           CALL ABEND USING RKOD-ABEND-WITH-DUMP                          
111002         END-IF                                                           
111102       END-IF                                                             
111202     END-IF                                                               
111302     .                                                                    
111402     EJECT                                                                
111502* --- IMS SECTIONS  ---                                                   
111602                                                                          
111702     EJECT                                                                
111802 IMS-GU-WDE111-DIST SECTION.                                              
111902                                                                          
112002     STRING 'WDE101  *PD(IDSHIPM  =' W-IDSHIPM-X ')'                      
112102          DELIMITED BY SIZE INTO SSA1                                     
112202     STRING 'WDE111  (WDE111KY>=' W-WDE111KY-MIN                          
112302                    '&WDE111KY<=' W-WDE111KY-MAX ')'                      
112402          DELIMITED BY SIZE INTO SSA2                                     
112502     MOVE '  GE' TO GOOD-STATUSCODES                                      
112602     CALL CBLTDLI USING GU WDE1-PCB DLI-IO-WDE101-11 SSA1 SSA2            
112702     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
112802     PERFORM IMS-STATUSCHECK                                              
112902     .                                                                    
113002     EJECT                                                                
113102 IMS-GNP-WDE111-DIST SECTION.                                             
113202                                                                          
113302     STRING 'WDE111  (WDE111KY>=' W-WDE111KY-MIN                          
113402                    '&WDE111KY<=' W-WDE111KY-MAX ')'                      
113502          DELIMITED BY SIZE INTO SSA1                                     
113602     MOVE '  GE' TO GOOD-STATUSCODES                                      
113702     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE111 SSA1                   
113802     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
113902     PERFORM IMS-STATUSCHECK                                              
114002     .                                                                    
114102     EJECT                                                                
114202 IMS-GNP-WDE121 SECTION.                                                  
114302                                                                          
114402     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
114502          DELIMITED BY SIZE INTO SSA1                                     
114602     MOVE 'WDE121  '          TO SSA2                                     
114702     MOVE '  GE' TO GOOD-STATUSCODES                                      
114802     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE121 SSA1 SSA2              
114902     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
115002     PERFORM IMS-STATUSCHECK                                              
115102     .                                                                    
115202     EJECT                                                                
115302 IMS-GNP-WDE131 SECTION.                                                  
115402                                                                          
115502     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
115602          DELIMITED BY SIZE INTO SSA1                                     
115702     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
115802          DELIMITED BY SIZE INTO SSA2                                     
115902     STRING 'WDE121  (WDE121KY =' W-WDE121KY-X ')'                        
116002          DELIMITED BY SIZE INTO SSA3                                     
116102     MOVE 'WDE131  '          TO SSA4                                     
116202     MOVE '  GE' TO GOOD-STATUSCODES                                      
116302     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE131 SSA1 SSA2              
116402                                                   SSA3 SSA4              
116502     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
116602     PERFORM IMS-STATUSCHECK                                              
116702     .                                                                    
116802     EJECT                                                                
116902 IMS-GU-WDQ201 SECTION.                                                   
117002                                                                          
117102     STRING 'WDQ201  (IDORDER  =' W-IDORDER-X ')'                         
117202          DELIMITED BY SIZE INTO SSA1                                     
117302     MOVE '  GE' TO GOOD-STATUSCODES                                      
117402     CALL CBLTDLI USING GU WDQ2-PCB DLI-IO-WDQ201 SSA1                    
117502     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
117602     PERFORM IMS-STATUSCHECK                                              
117702     .                                                                    
117802     EJECT                                                                
117902 IMS-GU-WDGX4738 SECTION.                                                 
118002                                                                          
118102     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-4738-X ')'                    
118202          DELIMITED BY SIZE INTO SSA1                                     
118302     MOVE   'WDGX4738'     TO SSA2                                        
118402     MOVE '  GE' TO GOOD-STATUSCODES                                      
118502     CALL CBLTDLI USING GU WDR1-PCB DLI-IO-WDGX4738 SSA1 SSA2             
118602     MOVE WDR1-STATUS-CODE TO STATUS-WS                                   
118702     PERFORM IMS-STATUSCHECK                                              
118802     .                                                                    
118902     EJECT                                                                
119002 IMS-GU-WDB101 SECTION.                                                   
119102                                                                          
119202     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
119302          DELIMITED BY SIZE INTO SSA1                                     
119402     MOVE '  GE' TO GOOD-STATUSCODES                                      
119502     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
119602     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
119702     PERFORM IMS-STATUSCHECK                                              
119802     .                                                                    
119902     EJECT                                                                
120002 IMS-GU-WDB201 SECTION.                                                   
120102                                                                          
120202     STRING 'WDB201  (IDGMT    =' W-WDB201KY-X ')'                        
120302          DELIMITED BY SIZE INTO SSA1                                     
120402     MOVE '  GE' TO GOOD-STATUSCODES                                      
120502     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
120602     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
120702     PERFORM IMS-STATUSCHECK                                              
120802     .                                                                    
120902     EJECT                                                                
121002 IMS-GU-WDB601          SECTION.                                          
121102                                                                          
121202     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
121302            DELIMITED BY SIZE INTO SSA1                                   
121402     MOVE '  ' TO GOOD-STATUSCODES                                        
121502     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
121602     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
121702     PERFORM IMS-STATUSCHECK                                              
121802     .                                                                    
121902     EJECT                                                                
122002 IMS-STATUSCHECK SECTION.                                                 
122102                                                                          
122202     SET STATUS-IX TO 1                                                   
122302     SEARCH GOOD-STATUS                                                   
122402       AT END                                                             
122502         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
122602           DELIMITED BY SIZE INTO ERRTEXT                                 
122702         DISPLAY ERRTEXT                                                  
122802         CALL FELLOG                                                      
122902       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
123002         CONTINUE                                                         
123102     END-SEARCH                                                           
124001     .                                                                    
130001                                                                          
