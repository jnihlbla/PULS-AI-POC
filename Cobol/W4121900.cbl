000100 ID DIVISION.                                                             
000200 PROGRAM-ID.                 W4121900.                                    
000300 AUTHOR.                     P. GALBAO.                                   
000400 DATE-WRITTEN.               JAN 2020.                                    
000500                                                                          
000600*    REMARKS.                                                             
000700*                                                                         
001300*    FUNCTION.                                                            
001400*                                                                         
001500*    DELETING BACK ORDER LINES FROM WDA5 WITH                             
001600*    1. READY FOR SHIPMENT DATE EQUAL TO CURRENT DATE                     
001601*    2. IDDISTR SHOULD NOT BE EQUAL TO 1378, 1822                         
001602*    3. RECORDS NOT HAVING NOTE                                           
001602*    4. IDSYSTEM LDC AND ECOM                                             
001610*                                                                         
001800*                                                                         
001810*        THE PROGRAM READS     WDA5                                       
001820*                              WDR5                                       
001830*                              WDB2                                       
001891*                              WDB6                                       
001892*                              WDK6                                       
001893*                              WDQ1                                       
001894*                              WDQ2                                       
001895*                              WDD3                                       
001896*                              WDD9                                       
001897*                              WDG6                                       
001898*                                                                         
001899*                  UPDATES     EVENT REGISTER (CHKPOINT)                  
001901*                              WDK6                                       
001902*                              WDK7                                       
001903*                              WDA5                                       
001904*                              WDQ1                                       
001905*                              WDG6                                       
001906     EJECT                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 DATA DIVISION.                                                           
002800 FILE SECTION.                                                            
002900                                                                          
003600 WORKING-STORAGE SECTION.                                                 
003700*    -COPY WY2000W1                                                       
003800                                                                          
003900 77  PROGRAM-NAME               PIC X(8)    VALUE 'W4121900'.             
003901 77  CURRENT-SECTION            PIC X(20)   VALUE SPACE.                  
003902 77  CURRENT-IMS-SECTION        PIC X(20)   VALUE SPACE.                  
003903 77  DATUM-MED-ARHUNDR          PIC 9(8)    VALUE ZERO.                   
003904 77  JA                         PIC X       VALUE 'J'.                    
003905 77  NEJ                        PIC X       VALUE 'N'.                    
003908 77  RKOD-ABEND-NO-DUMP         PIC S9(4)   COMP   VALUE +16.             
003909 77  RKOD-ABEND-WITH-DUMP       PIC S9(4)   COMP   VALUE +1000.           
003910 77  RFS-IX                     PIC S9(4)   VALUE +0  COMP SYNC.          
003911 77  MAX-RFS-IX                 PIC S9(4)   VALUE +4  COMP SYNC.          
003912 77  WS-TIRFS                   PIC 9(6).                                 
003913 77  VERKSTADSORDER             PIC X(2)    VALUE 'PW'.                   
003914 77  BUTIKSORDER                PIC X(2)    VALUE 'PC'.                   
003915 77  SPAR-KVART                 PIC 9(7).                                 
003916 77  W-SAVE-TILEVBSK            PIC S9(7)   COMP-3 VALUE ZERO.            
003917 77  WZ04-SEND-IDCOM            PIC S9(9)   COMP VALUE +0.                
003918 77  WS-ADRESS                  PIC X(50)                                 
003919                           VALUE 'CARPARTS.DAP.DISTRDOC'.                 
003920 77  WS-DEL-COUNT               PIC S9(9)   VALUE +0   COMP-3.            
003921                                                                          
003922 01  CHKP-VAR.                                                            
003923     03 CHKP-MSG-IO-AREA-LENGTH PIC S9(9)   VALUE +32 COMP SYNC.          
003924     03 CHKP-MSG-IO-AREA        PIC X(32)   VALUE SPACE.                  
003925     03 CHKP-AREA-LENGTH        PIC S9(9)   VALUE +32 COMP SYNC.          
003926     03 CHKP-AREA               PIC X(32)   VALUE SPACE.                  
003927     03 CHKP-ANT                PIC S9(3)   VALUE +0   COMP-3.            
003928     03 CHKP-MAX                PIC S9(3)   VALUE +5   COMP-3.            
003929                                                                          
003930 01  WS-IDDISTR-X               PIC 9(4)    VALUE ZERO.                   
003931 01  WS-IDKUNDNR-X              PIC 9(6)    VALUE ZERO.                   
003934 01  WS-HELPDAT                 PIC 9(6).                                 
003935 01  WS-TODAY-PKD-DEC-X.                                                  
003935     03 WS-TODAY-PKD-DEC        PIC S9(7) COMP-3.                         
003935 01  WS-TODAY                   PIC 9(6).                                 
003936 01  FILLER REDEFINES WS-TODAY.                                           
003937     03 WS-YEAR                 PIC 9(2).                                 
003938     03 WS-MONTH                PIC 9(2).                                 
003939     03 WS-DAYS                 PIC 9(2).                                 
003935 01  WS-TIME                    PIC 9(8).                                 
003942 01  ERROR-TEXT.                                                          
003943     03  FILLER                 PIC X(11)   VALUE 'ERROR-TEXT '.          
003944     03  ERROR-TEXT-STR         PIC X(72)   VALUE SPACE.                  
003945 01  KDRC-DISPLAY               PIC Z(5).                                 
003946                                                                          
004000*---- SUBPROGRAMS AND PARAMETER AREAS                                     
004200 01  GENERAL-SUBPROGRAMS.                                                 
004400   03  CBLTDLI                  PIC X(8)    VALUE 'CBLTDLI'.              
004500   03  FELLOG                   PIC X(8)    VALUE 'FELLOG '.              
004501   03  ABEND                    PIC X(8)    VALUE 'ABEND   '.             
004520   03  WORKDAY                  PIC X(8)    VALUE 'WORKDAY '.             
004520   03  WDATKONV                 PIC X(8)    VALUE 'WDATKONV'.             
004530   03  WZ01SEND                 PIC X(8)    VALUE 'WZ01SEND'.             
004600                                                                          
004630*---- PARAMETER TO SUBPROGRAM WDATKONV                                    
004701*01 -COPY WDATAREA                                                        
004630*---- PARAMETER TO SUBPROGRAM WORKDAY                                     
004701*01 -COPY WORKAREA                                                        
005100                                                                          
005101*    --- AREA FOR COMMUNICATION                                           
005102 01  FILLER                     PIC X(16)   VALUE 'SEND-CONTROL'.         
005103*01  -COPY WZ01SEND                                                       
005104                                                                          
005105 01  FILLER                     PIC X(16)   VALUE 'HDR-AREA'.             
005106                                                                          
005107 01  HDR-AREA.                                                            
005108*    03 -COPY WZ01REQU   -PRE HDR-                                        
005700*    03 -COPY WZ04HDR                                                     
005701                                                                          
005702 01  TEST-IDDISTR               PIC S9(5) COMP-3.                         
005704                                                                          
005705*01  FILLER -COPY WWDIST03 -RED TEST-IDDISTR.                             
005705*                                                                         
005705*01  FILLER -COPY WWDIST12 -RED TEST-IDDISTR.                             
005706*                                                                         
005707*01  FILLER -COPY WWBYT03                                                 
005708                                                                          
005709                                                                          
005710*---- IMS-SECTION WORK AREAS                                              
005711 01  FILLER                     PIC X(8)    VALUE 'IMS-WS  '.             
005712                                                                          
005713 01  KEYS-TO-DLI.                                                         
005714     03  W-WDA5KEY-MIN-X.                                                 
005715         05  W-A5-IDDISTR-MIN   PIC S9(5)   VALUE ZERO COMP-3.            
005716         05  W-A5-IDKUNDNR-MIN  PIC S9(7)   VALUE ZERO COMP-3.            
005717         05  W-A5-IDKUNDRF-MIN  PIC X(10)   VALUE SPACE.                  
005718         05  W-A5-IDARTNR-MIN   PIC S9(9)   VALUE ZERO COMP-3.            
005719         05  W-A5-IDLOPNR-MIN   PIC S9(3)   VALUE ZERO COMP-3.            
005720                                                                          
005721     03  W-WDA5KEY-MAX-X.                                                 
005722         05  W-A5-IDDISTR-MAX   PIC S9(5)   VALUE ZERO COMP-3.            
005723         05  W-A5-IDKUNDNR-MAX  PIC S9(7)   VALUE ZERO COMP-3.            
005724         05  W-A5-IDKUNDRF-MAX  PIC X(10)   VALUE SPACE.                  
005725         05  W-A5-IDARTNR-MAX   PIC S9(9)   VALUE ZERO COMP-3.            
005726         05  W-A5-IDLOPNR-MAX   PIC S9(3)   VALUE ZERO COMP-3.            
005727                                                                          
005728     03  W-WDA5KEY-X.                                                     
005729         05  W-A5-IDDISTR-KEY   PIC S9(5)   VALUE ZERO COMP-3.            
005730         05  W-A5-IDKUNDNR-KEY  PIC S9(7)   VALUE ZERO COMP-3.            
005731         05  W-A5-IDKUNDRF-KEY  PIC X(10)   VALUE SPACE.                  
005732         05  FILLER REDEFINES W-A5-IDKUNDRF-KEY.                          
005733             07  W-A5-IDORDNR-KEY    PIC 9(5).                            
005734             07  FILLER              PIC X(5).                            
005735         05  W-A5-IDARTNR-KEY   PIC S9(9)   VALUE ZERO COMP-3.            
005736         05  W-A5-IDLOPNR-KEY   PIC S9(3)   VALUE ZERO COMP-3.            
005737                                                                          
005738     03  FILLER                 PIC X(16)   VALUE 'WDA501KY'.             
005763                                                                          
005764     03  W-KDORDKL-X.                                                     
005765         05  W-KDORDKL          PIC S9      VALUE 3    COMP-3.            
005766                                                                          
005767     03  W-IDSYSTX3-LDC-X.                                                
005768         05  W-IDSYSTX3-LDC     PIC X(3)    VALUE 'LDC'.                  
005769                                                                          
005767     03  W-IDSYSTX3-ECO-X.                                                
005768         05  W-IDSYSTX3-ECO     PIC X(3)    VALUE 'ECO'.                  
005769                                                                          
005770     03  W-KDSTARAD-X.                                                    
005771         05  W-KDSTARAD         PIC X(1)    VALUE '2'.                    
005772                                                                          
005776     03  W-4563KEY-X.                                                     
005777         05  W-4563-IDHTYP      PIC X(4)    VALUE '4563'.                 
005778         05  FILLER             PIC X(26)   VALUE LOW-VALUE.              
005779     03  W-4564KEY-X.                                                     
005780         05  W-IDGMTREF-4564    PIC X(17)   VALUE SPACE.                  
005781         05  W-IDARTNR-4564     PIC S9(9)   VALUE ZERO COMP-3.            
005782         05  W-IDLOPNR-4564     PIC S9(3)   VALUE ZERO COMP-3.            
005783                                                                          
005784     03  W-IDGMT-X.                                                       
005785         05 W-IDDISTR-WDB2      PIC S9(5)   VALUE ZERO COMP-3.            
005786         05 W-IDKUNDNR-WDB2     PIC S9(7)   VALUE ZERO COMP-3.            
005787                                                                          
005788     03  W-IDARTNR-X.                                                     
005789         05  W-IDARTNR          PIC S9(9)   VALUE ZERO COMP-3.            
005790                                                                          
005791     03  W-IDDC-B6-X.                                                     
005792         05 W-IDDC-B6           PIC X(2).                                 
005793                                                                          
005794     03  W-IDSKYLT-X.                                                     
005795         05  W-IDSKYLT          PIC X(3)    VALUE SPACE.                  
005796                                                                          
005797     03  W-WDD901KY-X.                                                    
005798         05  W-IDARTNR-D9       PIC S9(9)   VALUE ZERO COMP-3.            
005799         05  W-IDDC-D9          PIC X(2)    VALUE SPACE.                  
005800                                                                          
005801     03  W-IDLEVNR-X.                                                     
005802         05  W-IDLEVNR          PIC X(5)    VALUE SPACE.                  
005803                                                                          
005804     03  W-IDARTNR-WDK601-X.                                              
005805         05  W-IDARTNR-WDK601   PIC S9(9)   VALUE ZERO COMP-3.            
005806                                                                          
005807     03  W-IDARTNR-K7-X.                                                  
005808         05  W-IDARTNR-K7       PIC S9(9)   VALUE ZERO COMP-3.            
005809                                                                          
005810     03  W-IDDC-K7-X.                                                     
005811         05 W-IDDC-K7           PIC X(2).                                 
005812                                                                          
005813     03  W-IDGMTREF-X.                                                    
005814         05  W-IDDISTR-N9       PIC S9(5)   VALUE ZERO COMP-3.            
005815         05  W-IDKUNDNR-N9      PIC S9(7)   VALUE ZERO COMP-3.            
005816         05  W-IDKUNDRF-N9      PIC X(10)   VALUE SPACE.                  
005817                                                                          
005823 01  DAP-MAIL-BODY-AREA1.                                                 
005824      03  DAP-MAIL-LINE1-AREA1.                                           
005825          05 DAP-MAIL-LINE-TEXT-1 PIC X(47) VALUE                         
005826               'FROM  VOLVO CAR CUSTOMER SERVICE CDC GOTHENBURG'.         
005827                                                                          
005828      03  DAP-MAIL-LINE2-AREA1.                                           
005829          05 DAP-MAIL-LINE-TEXT-2 PIC X(57) VALUE                         
005830     'FOR THE URGENT ATTENTION OF THE PARTS ORDERING DEPARTMENT'.         
005831                                                                          
005832      03  DAP-MAIL-LINE3-AREA1.                                           
005833          05 DAP-MAIL-LINE-TEXT-3 PIC X(23) VALUE                         
005834                                       'PLEASE SEE THE DETAILS:'.         
005835                                                                          
005836 01  DAP-MAIL-BODY-AREA2.                                                 
005837      03  DAP-MAIL-LINE1-AREA2.                                           
005838          05 DAP-MAIL-LINE-TEXT-4      PIC X(14).                         
005839      03  DAP-MAIL-LINE2-AREA2.                                           
005840          05 DAP-MAIL-LINE-TEXT-5      PIC X(10).                         
005841      03  DAP-MAIL-LINE3-AREA2.                                           
005842          05 DAP-MAIL-LINE-TEXT-6      PIC X(10).                         
005843      03  DAP-MAIL-LINE4-AREA2.                                           
005844          05 DAP-MAIL-LINE-TEXT-7      PIC X(27).                         
005845      03  DAP-MAIL-LINE5-AREA2.                                           
005846          05 DAP-MAIL-LINE-TEXT-8      PIC X(10).                         
005847      03  DAP-MAIL-LINE6-AREA2.                                           
005848          05 DAP-MAIL-LINE-TEXT-9      PIC X(12).                         
005849      03  DAP-MAIL-LINE7-AREA2.                                           
005850          05 DAP-MAIL-LINE-TEXT-10     PIC X(13).                         
005851      03  DAP-MAIL-LINE8-AREA2.                                           
005852          05 DAP-MAIL-LINE-TEXT-11     PIC X(17).                         
005853                                                                          
005854 01  DAP-MAIL-BODY-AREA3.                                                 
005855      03  DAP-MAIL-LINE1-AREA3.                                           
005856          05 DAP-MAIL-LINE-FILLER1     PIC X(8) VALUE SPACE.              
005857          05 DAP-MAIL-DISTRICT-NO      PIC Z(3)9.                         
005858          05 DAP-MAIL-LINE-FILLER2     PIC X(2) VALUE SPACE.              
005859      03  DAP-MAIL-LINE2-AREA3.                                           
005860          05 DAP-MAIL-LINE-FILLER3     PIC X(2) VALUE SPACE.              
005861          05 DAP-MAIL-CUSTOMER-NO      PIC Z(5)9.                         
005862          05 DAP-MAIL-LINE-FILLER4     PIC X(2) VALUE SPACE.              
005863      03  DAP-MAIL-LINE3-AREA3.                                           
005864          05 DAP-MAIL-PART-NO          PIC Z(7)9.                         
005865          05 DAP-MAIL-LINE-FILLER5     PIC X(2) VALUE SPACE.              
005866      03  DAP-MAIL-LINE4-AREA3.                                           
005867          05 DAP-MAIL-PART-DESC        PIC X(25).                         
005868          05 DAP-MAIL-LINE-FILLER6     PIC X(2) VALUE SPACE.              
005869      03  DAP-MAIL-LINE5-AREA3.                                           
005870          05 DAP-MAIL-LINE-FILLER7     PIC X(1) VALUE SPACE.              
005871          05 DAP-MAIL-QTY              PIC Z(6)9.                         
005872          05 DAP-MAIL-LINE-FILLER8     PIC X(2) VALUE SPACE.              
005873      03  DAP-MAIL-LINE6-AREA3.                                           
005874          05 DAP-MAIL-ORDER-NO         PIC X(10).                         
005875          05 DAP-MAIL-LINE-FILLER9     PIC X(2) VALUE SPACE.              
005876      03  DAP-MAIL-LINE7-AREA3.                                           
005877          05 DAP-MAIL-LINE-FILLERA     PIC X(5) VALUE SPACE.              
005878          05 DAP-MAIL-REPAIR-DATE      PIC X(6).                          
005879          05 DAP-MAIL-LINE-FILLERB     PIC X(2) VALUE SPACE.              
005880      03  DAP-MAIL-LINE8-AREA3.                                           
005881          05 DAP-MAIL-LINE-FILLERC     PIC X(5) VALUE SPACE.              
005882          05 DAP-MAIL-WRKSHOP-ORDER    PIC X(10).                         
005883          05 DAP-MAIL-LINE-FILLERD     PIC X(2) VALUE SPACE.              
005884                                                                          
005885                                                                          
005886 01  DAP-MAIL-BODY-AREA4.                                                 
005887      03  DAP-MAIL-LINE1-AREA4.                                           
005888          05 DAP-MAIL-LINE-TEXT-12-1   PIC X(44) VALUE                    
005889     'WE WISH TO ADVISE YOU THAT THIS PART IS NOT '.                      
005890          05 DAP-MAIL-LINE-TEXT-12-2   PIC X(17) VALUE                    
005891                                             'AVAILABLE AND WE '.         
005892                                                                          
005893      03  DAP-MAIL-LINE2-AREA4.                                           
005894          05 DAP-MAIL-LINE-TEXT-13-1   PIC X(55) VALUE                    
005895       'WILL BE UNABLE TO OBTAIN YOUR ORDERED QUANTITY IN TIME '.         
005896          05 DAP-MAIL-LINE-TEXT-13-2   PIC X(04) VALUE    'FOR '.         
005897                                                                          
005898      03  DAP-MAIL-LINE3-AREA4.                                           
005899          05 DAP-MAIL-LINE-TEXT-14     PIC X(17) VALUE                    
005900                         'YOUR REPAIR DATE.'.                             
005901                                                                          
005902      03  DAP-MAIL-LINE4-AREA4.                                           
005903          05 DAP-MAIL-LINE-TEXT-15-1   PIC X(57) VALUE                    
005904     'YOUR ORDER IS CANCELLED OR KEPT DEPENDING ON THE CURRENT '.         
005905          05 DAP-MAIL-LINE-TEXT-15-2   PIC X(42) VALUE                    
005906     'RULES FOR YOUR MARKET. PLEASE TAKE ACTION.'.                        
005907                                                                          
005913      03  DAP-MAIL-LINE5-AREA4.                                           
005914          05 DAP-MAIL-LINE-TEXT-16     PIC X(43) VALUE                    
005915                   'EXPECTED TIME OF ARRIVAL AT DC 11 EARLIEST '.         
005916          05 DAP-MAIL-LINE-VALUE-WW    PIC 9(2) VALUE 0.                  
005917          05 DAP-MAIL-LINE-CONST       PIC X(1) VALUE ':'.                
005918          05 DAP-MAIL-LINE-VALUE-D     PIC 9(1) VALUE 0.                  
005919                                                                          
005920      03  DAP-MAIL-LINE6-AREA4.                                           
005921          05 DAP-MAIL-LINE-TEXT-17-1   PIC X(46) VALUE                    
005922                'IF THERE ARE ANY QUESTIONS PLEASE CONTACT THE '.         
005923          05 DAP-MAIL-LINE-TEXT-17-2   PIC X(22) VALUE                    
005924                                        'DEALER SERVICE OFFICE.'.         
005925      03  DAP-MAIL-LINE7-AREA4.                                           
005926          05 DAP-MAIL-LINE-TEXT-18     PIC X(46) VALUE                    
005927                        '(DO NOT REPLY ON THIS E-MAIL MESSAGE.)'.         
005928                                                                          
005929      03  DAP-MAIL-LINE-AREA.                                             
005930          05 DAP-MAIL-LINE-TEXT-SPACE  PIC X(80) VALUE SPACES.            
005931                                                                          
005932                                                                          
005933*---- STATUS CODE FROM IMS                                                
005934 01  STATUS-WS                  PIC XX.                                   
005935     88  SEGMENT-FOUND                      VALUE '  '.                   
005936     88  SEGMENT-ADDED                      VALUE '  '.                   
005937     88  SEGMENT-EXISTS-ALREADY             VALUE 'II'.                   
005938     88  SEGMENT-MISSING                    VALUE 'GE'.                   
005939     88  SEGMENT-NOMORE                     VALUE 'GB'.                   
005940     88  IMS-NOT-OK                         VALUE 'XD'.                   
005941                                                                          
005942 01  GOOD-STATUSCODES.                                                    
005943     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
005944                                                                          
005945 01  ALL-SSA.                                                             
005946     03  SSA1                   PIC X(600).                               
005947     03  SSA2                   PIC X(160).                               
005948                                                                          
005950*---- IMS FUNCTIONS                                                       
006800*01      -COPY W0003.                                                     
006900     EJECT                                                                
006901*---- DLI INPUT-OUTPUT AREA                                               
007000 01  FILLER                     PIC X(16)   VALUE                         
007100                                            'DLI-IO-WDA501'.              
007200 01  DLI-IO-WDA501.                                                       
007400*                                                                         
007500*    03  -COPY WDA501                                                     
007600 01  FILLER                     PIC X(16)   VALUE                         
007601                                            'DLI-IO-WDR501'.              
007610 01  DLI-IO-WDR501.                                                       
007611*                                                                         
007620*    03  -COPY WDGX4564                                                   
007630 01  FILLER                     PIC X(16)   VALUE                         
007631                                            'DLI-IO-WDB201'.              
007632 01  DLI-IO-WDB201.                                                       
007633*                                                                         
007640*    03  -COPY WDB201                                                     
007700                                                                          
007701 01  FILLER                     PIC X(16)   VALUE                         
007702                                            'DLI-IO-WDB601'.              
007703 01  DLI-IO-WDB601.                                                       
007704*                                                                         
007705*    03  -COPY WDB601                                                     
007706                                                                          
007707 01  FILLER                     PIC X(16)   VALUE                         
007708                                            'DLI-IO-WDK611'.              
007709 01  DLI-IO-WDK611.                                                       
007710*                                                                         
007711*    03  -COPY WDK611                                                     
007712                                                                          
007713 01  FILLER                     PIC X(16) VALUE                           
007714                                            'DLI-IO-WDK711'.              
007715 01  DLI-IO-WDK711.                                                       
007716*                                                                         
007717*    03  -COPY WDK711                                                     
007718                                                                          
007719 01  FILLER                     PIC X(16) VALUE                           
007720                                            'DLI-IO-WDQ201  '.            
007721 01  DLI-IO-WDQ201.                                                       
007722*                                                                         
007723*    03  -COPY WDQ201                                                     
007724                                                                          
007725 01  FILLER                     PIC X(16) VALUE                           
007726                                            'DLI-IO-WDQ101'.              
007727 01  DLI-IO-WDQ101.                                                       
007728*                                                                         
007729*    03  -COPY WDQ101                                                     
007730                                                                          
007737 01  FILLER                     PIC X(16)   VALUE                         
007738                                            'DLI-IO-WDD311'.              
007739 01  DLI-IO-WDD311.                                                       
007741*    03  -COPY WDD311                                                     
007742                                                                          
007743 01  FILLER                     PIC X(16)   VALUE                         
007744                                            'DLI-IO-WDD901'.              
007745 01  DLI-IO-WDD901.                                                       
007746*    03  -COPY WDD901.                                                    
007747                                                                          
007748 01  FILLER                     PIC X(16)   VALUE                         
007749                                            'DLI-IO-WDD902'.              
007750 01  DLI-IO-WDD902.                                                       
007751*    03  -COPY WDD902                                                     
007752                                                                          
007753 01  FILLER                     PIC X(16)   VALUE                         
007754                                            'DLI-IO-WDD924'.              
007755 01  DLI-IO-WDD924.                                                       
007756*    03  -COPY WDD924                                                     
007757                                                                          
007758 01  FILLER                     PIC X(16)   VALUE                         
007759                                            'DLI-IO-WDG601'.              
007760 01  DLI-IO-WDG601.                                                       
007761*    03  -COPY WDGZ01 -PRE ZZAC-                                          
007762                                                                          
007763*  ---COPYTEXT TO RY9-TRANS                                               
007764                                                                          
007765*01    -COPY WDGZRY9                                                      
007766*01    -COPY WDGZRY9S                                                     
007767                                                                          
007768                                                                          
007769 01  FILLER                     PIC X(16)   VALUE                         
007770                                            'MSG-KOM-AREA'.               
007771*01  -COPY WMSGKOM                                                        
007772    EJECT                                                                 
007773                                                                          
007774 01  FILLER                     PIC X(16)   VALUE                         
007775                                            'MSG-IO-AREA'.                
007776                                                                          
007777*01  -COPY WMSGAREA                                                       
007778                                                                          
007779*    MSG-AREA FÖR HOPP TILL W20109                                        
007780 01  FILLER            PIC X(16)   VALUE '2109-MSG-IO-AREA'.              
007781 01  W-PROG-TO-PROG-SW-1.                                                 
007782     03  2109-KVLL                 PIC S9(4) COMP SYNC.                   
007783     03  2109-Z1                   PIC X.                                 
007784     03  2109-Z2                   PIC X.                                 
007785     03  2109-TRANSKOD             PIC X(8)  VALUE 'W2T109X '.            
007786     03  2109-IDTRANS              PIC X(4)  VALUE '4572'.                
007787     03  2109-KDMFSFOR             PIC X.                                 
007788*    03  -COPY W2I10902    -PRE 2109-                                     
007789                                                                          
007800 LINKAGE SECTION.                                                         
008000*01  -COPY W0009                -PRE MSG-                                 
008211                                                                          
008212*01  -COPY W0009                -PRE DISTRDOC-                            
008213*01  -COPY W0009                -PRE 2109-                                
008214                                                                          
008215*01  -COPY W0008                -PRE WDA5-                                
008216     05  FILLER                 PIC X.                                    
008217                                                                          
008220*01  -COPY W0008                -PRE WDR5-                                
008230     05  FILLER                 PIC X.                                    
008240                                                                          
008250*01  -COPY W0008                -PRE WDB2-                                
008260     05  FILLER                 PIC X.                                    
008300                                                                          
008301*01  -COPY W0008                -PRE WDB6-                                
008302     05  FILLER                 PIC X.                                    
008303                                                                          
008304*01  -COPY W0008                -PRE WDK6-                                
008305     05  FILLER                 PIC X.                                    
008306                                                                          
008310*01  -COPY W0008                -PRE WDQ2-                                
008311     05  FILLER                 PIC X.                                    
008312                                                                          
008313*01  -COPY W0008                -PRE WDQ1-                                
008314     05  FILLER                 PIC X.                                    
008315                                                                          
008316*01  -COPY W0008                -PRE WDG6-                                
008317     05  FILLER                 PIC X.                                    
008318                                                                          
008319*01  -COPY W0008                -PRE WDD3-                                
008320     05  FILLER                 PIC X.                                    
008321                                                                          
008322*01  -COPY W0008                -PRE WDK7-                                
008323     05  FILLER                  PIC X.                                   
008324                                                                          
008325*01  -COPY W0008                -PRE WDD9-                                
008326     05  FILLER                 PIC X.                                    
008327                                                                          
008400 PROCEDURE DIVISION  USING MSG-PCB                                        
008401                           DISTRDOC-PCB                                   
008402                           2109-PCB                                       
008403                           WDA5-PCB WDR5-PCB WDB2-PCB                     
008404                           WDB6-PCB                                       
008405                           WDK6-PCB                                       
008407                           WDQ2-PCB                                       
008408                           WDQ1-PCB                                       
008409                           WDG6-PCB                                       
008410                           WDD3-PCB                                       
008411                           WDK7-PCB                                       
008412                           WDD9-PCB.                                      
008620 MAIN SECTION.                                                            
008621     ENTRY 'DLITCBL' USING MSG-PCB                                        
008622                           DISTRDOC-PCB                                   
008623                           2109-PCB                                       
008624                           WDA5-PCB WDR5-PCB WDB2-PCB                     
008630                           WDB6-PCB                                       
008631                           WDK6-PCB                                       
008633                           WDQ2-PCB                                       
008634                           WDQ1-PCB                                       
008635                           WDG6-PCB                                       
008636                           WDD3-PCB                                       
008637                           WDK7-PCB                                       
008638                           WDD9-PCB.                                      
008640                                                                          
008700     PERFORM A-INIT                                                       
008701                                                                          
011200*    SELECT RAD THAT IS A SPECIAL ORDER KDORDKL = 3 AND                   
011201*    REPAIR DATE SHOULD BE GREATER THAN THE CURRENT DATE AND              
011202*    LINE STATUS CODE KDSTARAD SHOULD BE BO(2) AND                        
011203*    ORDER TYPE KDORDTYP='LDC' OR 'ECOM'                                  
011203*    SHOULD BE EITHER                                                     
011204*    WAREHOUSE ORDER(PW) OR STORE ORDER(PC)                               
011205     PERFORM IMS-GHN-WDA501                                               
011206                                                                          
011207     PERFORM UNTIL SEGMENT-NOMORE OR SEGMENT-MISSING                      
011208       PERFORM B-VALIDATE-RAD                                             
011209       IF CHKP-ANT >= CHKP-MAX                                            
011210         PERFORM X-TAKE-CHECKPOINT                                        
011211       END-IF                                                             
011212       MOVE RAD-IDDISTR            TO W-A5-IDDISTR-MIN                    
011212       MOVE RAD-IDKUNDNR           TO W-A5-IDKUNDNR-MIN                   
011212       MOVE RAD-IDKUNDRF           TO W-A5-IDKUNDRF-MIN                   
011212       MOVE RAD-IDARTNR            TO W-A5-IDARTNR-MIN                    
011212       MOVE RAD-IDLOPNR            TO W-A5-IDLOPNR-MIN                    
011212       PERFORM IMS-GHN-WDA501                                             
011213     END-PERFORM                                                          
011214                                                                          
011214     DISPLAY 'DELETED COUNT OF WDA5 BO LINES: ' WS-DEL-COUNT              
011216     MOVE ZERO                     TO RETURN-CODE                         
011217     GOBACK                                                               
011218     .                                                                    
011219                                                                          
011220 A-INIT SECTION.                                                          
011221     MOVE 'A-INIT              '   TO CURRENT-SECTION                     
011222                                                                          
011222     PERFORM IMS-RESTART                                                  
011222                                                                          
011223     MOVE LOW-VALUE                TO W-WDA5KEY-MIN-X                     
011224     MOVE HIGH-VALUE               TO W-WDA5KEY-MAX-X                     
011225                                                                          
011226     ACCEPT WS-TODAY             FROM DATE                                
011226     MOVE WS-TODAY                 TO WS-TODAY-PKD-DEC                    
011250     .                                                                    
011251                                                                          
011252 B-VALIDATE-RAD SECTION.                                                  
011253     MOVE 'B-VALIDATE-RAD      '   TO CURRENT-SECTION                     
011254                                                                          
011255     MOVE RAD-IDDISTR              TO TEST-IDDISTR                        
011255                                      WS-IDDISTR-X                        
011257     MOVE RAD-IDKUNDNR             TO WS-IDKUNDNR-X                       
011256                                                                          
011256                                                                          
011256                                                                          
011260     IF ( RAD-KDORDKL = 3  AND                                            
011261          RAD-TIREPDAT > WS-TODAY-PKD-DEC    AND                          
011262        ( RAD-IDSYSTEM =('LDC' OR 'ECOM' ))  AND                          
011263          RAD-KDSTARAD = '2' AND                                          
011263          RAD-KDTPOTYP = +0  AND                                          
011264          ( RAD-KDORDTYP-LDC = 'PW' OR 'PC' ))                            
011265                                                                          
011266*    SKIP RAD RECORD HAVING IDDISTR EQUAL TO 98, 1378, 1822               
011267       IF NOT DIST12-NO-DLET                                              
011268         MOVE RAD-IDGMTREF         TO W-IDGMTREF-4564                     
011269         MOVE RAD-IDARTNR          TO W-IDARTNR-4564                      
011270         MOVE RAD-IDLOPNR          TO W-IDLOPNR-4564                      
011271         PERFORM IMS-GU-WDR501                                            
011272         IF SEGMENT-MISSING                                               
011273           MOVE SPACE              TO 4564-BETEXT-010                     
011274         END-IF                                                           
011275*    SKIP RAD RECORD HAVING A NOTE. SELECT 4564-BETEXT-010 = SPACE        
011276         IF (4564-BETEXT-010 = SPACE)                                     
011277           MOVE RAD-IDDISTR        TO W-IDDISTR-WDB2                      
011278           MOVE RAD-IDKUNDNR       TO W-IDKUNDNR-WDB2                     
011279           PERFORM IMS-GU-WDB201                                          
011280           PERFORM BA-SKAPA-RFSDATE                                       
011281           IF WS-TIRFS = WS-TODAY                                         
011282             PERFORM BB-DELETE-ORDER                                      
011283             PERFORM BC-SEND-DAP                                          
011284           END-IF                                                         
011285         END-IF                                                           
011286       END-IF                                                             
011287     END-IF                                                               
011288     .                                                                    
011289                                                                          
011290 BA-SKAPA-RFSDATE SECTION.                                                
011291     MOVE 'BA-SKAPA-RFSDATE    '   TO CURRENT-SECTION                     
011292                                                                          
011293     MOVE RAD-IDDC                 TO WORK-IDDC                           
011294     MOVE +002                     TO WORK-KDCALL                         
011295     MOVE +001                     TO WORK-KVWORKD                        
011296     IF RAD-TIREPDAT = ZERO                                               
011297       MOVE WS-TODAY               TO WORK-TIAAMMDD-FOM                   
011298     ELSE                                                                 
011299       MOVE RAD-TIREPDAT           TO WORK-TIAAMMDD-FOM                   
011300     END-IF                                                               
011301     CALL WORKDAY               USING WORK-KDCALL                         
011302                                      WORK-DATE-AREA                      
011303                                      WORK-KDSVAR                         
011304     IF WORK-KDSVAR-FEL                                                   
011305        MOVE 'SECT BA-1, DATUM SAKNAS I WORKDAY'                          
011306                                   TO ERROR-TEXT-STR                      
011307        CALL ABEND              USING RKOD-ABEND-NO-DUMP                  
011308     ELSE                                                                 
011309       MOVE +003                   TO WORK-KDCALL                         
011310       MOVE GMT-KVDAGAR-RFS-DEF    TO WORK-KVWORKD                        
011311       PERFORM                                                            
011312       VARYING RFS-IX FROM 1 BY 1                                         
011313         UNTIL RFS-IX > MAX-RFS-IX                                        
011314         IF GMT-IDDC-RFS (RFS-IX) = WORK-IDDC                             
011315           MOVE GMT-KVDAGAR-RFS (RFS-IX)                                  
011316                                   TO WORK-KVWORKD                        
011317         END-IF                                                           
011318       END-PERFORM                                                        
011319       ADD +1                      TO WORK-KVWORKD                        
011320*      +1 FÖR ATT VARIABELN SKALL KUNNA INNEHÅLLA                         
011321*      ANTAL DAGAR FÖRE RFS.                                              
011322*      0 GER DÅ SAMMA DAG, 1 GER FÖRSTA ARBETSDAG FÖRE OSV...             
011323*      OM VI INTE ADDERAR +1 SKULLE VARIABELN SÄTTAS SÅ                   
011324*      1 GER SAMMA DAG, 2 FÖRSTA ARBETSDAG FÖRE OSV...                    
011325*                                                                         
011326                                                                          
011327       CALL WORKDAY             USING WORK-KDCALL                         
011328                                      WORK-DATE-AREA                      
011329                                      WORK-KDSVAR                         
011330       IF WORK-KDSVAR-FEL                                                 
011331          MOVE 'SECT BA-2, DATUM SAKNAS I WORKDAY'                        
011332                                   TO ERROR-TEXT-STR                      
011333          CALL ABEND            USING RKOD-ABEND-NO-DUMP                  
011334       ELSE                                                               
011335         IF WORK-TIAAMMDD-FOM < WS-TODAY                                  
011336           MOVE GMT-IDDC-BULK(1)   TO WORK-IDDC                           
011337           MOVE +002               TO WORK-KDCALL                         
011338           MOVE +001               TO WORK-KVWORKD                        
011339           MOVE WS-TODAY           TO WORK-TIAAMMDD-FOM                   
011340           CALL WORKDAY         USING WORK-KDCALL                         
011341                                       WORK-DATE-AREA                     
011342                                       WORK-KDSVAR                        
011343           IF WORK-KDSVAR-FEL                                             
011344              MOVE 'SECT BA-3, DATUM SAKNAS I WORKDAY'                    
011345                                   TO ERROR-TEXT-STR                      
011346              CALL ABEND        USING RKOD-ABEND-NO-DUMP                  
011347           ELSE                                                           
011348              MOVE WORK-TIAAMMDD-TOM                                      
011349                                   TO WS-TIRFS                            
011350           END-IF                                                         
011351         ELSE                                                             
011352           MOVE WORK-TIAAMMDD-FOM  TO WS-TIRFS                            
011353         END-IF                                                           
011354       END-IF                                                             
011355     END-IF                                                               
011356     .                                                                    
011357                                                                          
011358 BB-DELETE-ORDER SECTION.                                                 
011359     MOVE 'BB-DELETE-ORDER     '   TO CURRENT-SECTION                     
011360                                                                          
011378     MOVE RAD-KVART                TO SPAR-KVART                          
011379                                                                          
011380     MOVE RAD-IDDC                 TO W-IDDC-B6                           
011381     PERFORM IMS-GU-WDB601                                                
011382                                                                          
011383     IF DCS-CDC                                                           
011384       PERFORM S10-MINSKA-KVROS-WDK6                                      
011385     ELSE                                                                 
011387       PERFORM S11-MINSKA-KVROS-WDK7                                      
011389     END-IF                                                               
011390                                                                          
011391     PERFORM IMS-DLET-WDA501                                              
011392     ADD +1                        TO CHKP-ANT                            
011393                                      WS-DEL-COUNT                        
011394                                                                          
011395     PERFORM BBA-UPPDATERA-WDQ1                                           
011396                                                                          
011397     MOVE ZERO                     TO RY9-KVART                           
011398                                                                          
011399     IF RAD-KDSTARAD = 2                                                  
011400        MOVE SPAR-KVART            TO RY9-KVART                           
011401     END-IF                                                               
011402                                                                          
011403     PERFORM BBB-SKAPA-RY9-TRANS                                          
011404     PERFORM BBC-SKRIV-LOGG                                               
011405                                                                          
011406     IF RAD-KDSTARAD = 2                                                  
011407        PERFORM BBD-SKAPA-2109-TRANS                                      
011408     END-IF                                                               
011410     .                                                                    
011411                                                                          
011412 BBA-UPPDATERA-WDQ1 SECTION.                                              
011413     MOVE 'BBA-UPPDATERA-WDQ1   '  TO CURRENT-SECTION                     
011414                                                                          
011415     MOVE RAD-IDDISTR              TO W-IDDISTR-N9                        
011416     MOVE RAD-IDKUNDNR             TO W-IDKUNDNR-N9                       
011417     MOVE '00'                     TO W-IDKUNDRF-N9(1:2)                  
011418     MOVE RAD-IDKUNDRF             TO W-IDKUNDRF-N9(3:5)                  
011419     PERFORM IMS-GU-WDQ201                                                
011420                                                                          
011421     MOVE SPACE                    TO DLI-IO-WDQ101                       
011422     MOVE OHUV-IDORDER             TO OBKR-IDORDER                        
011423     MOVE RAD-IDARTNR              TO OBKR-IDARTNR                        
011424     MOVE +1                       TO OBKR-IDLOPNR                        
011425                                      OBKR-IDSEKVNR                       
011426     MOVE RAD-IDDC                 TO OBKR-IDDC                           
011427     MOVE RAD-IDDC-RO              TO OBKR-IDDC-RO                        
011428     MOVE 85                       TO OBKR-KDORDBEK                       
011429     MOVE SPACE                    TO OBKR-BEERS                          
011430     MOVE SPACE                    TO OBKR-IDBIL                          
011431     MOVE RAD-BEKUNDRF             TO OBKR-BEKUNDRF                       
011432     MOVE RAD-BERADREF             TO OBKR-BERADREF                       
011433     MOVE RAD-BEVOLREF             TO OBKR-BEVOLREF                       
011434     MOVE RAD-IDKAMPRF             TO OBKR-IDKAMPRF                       
011435     MOVE ZERO                     TO OBKR-DIERS-KVOT                     
011436     MOVE NEJ                      TO OBKR-FLAKPLOC                       
011437     MOVE RAD-FLINVEST             TO OBKR-FLINVEST                       
011438     MOVE JA                       TO OBKR-FLOBOK                         
011439     MOVE NEJ                      TO OBKR-FLOBTRAN                       
011440                                      OBKR-FLOBPRT                        
011441     MOVE RAD-FLPRTILL             TO OBKR-FLPRTILL                       
011442     MOVE JA                       TO OBKR-FLRESTN                        
011443     MOVE NEJ                      TO OBKR-FLSLATT                        
011444     MOVE RAD-FLERS                TO OBKR-FLTILLK                        
011445     MOVE ZERO                     TO OBKR-IDARTNR-TILLK                  
011446     MOVE RAD-IDDISTR              TO OBKR-IDDISTR                        
011447     MOVE RAD-IDKUNDNR             TO OBKR-IDKUNDNR                       
011448     MOVE '00'                     TO OBKR-IDKUNDRF(1:2)                  
011449     MOVE RAD-IDORDNR5             TO OBKR-IDKUNDRF(3:5)                  
011450     MOVE '0000000   '             TO OBKR-IDKUNDRF-RO                    
011451     MOVE RAD-IDLEVNR              TO OBKR-IDLEVNR                        
011452     MOVE RAD-IDLOPNR              TO OBKR-IDLOPNR-RO                     
011453     MOVE PROGRAM-NAME             TO OBKR-IDPGM                          
011454     MOVE RAD-IDSYSTEM             TO OBKR-IDSYSTEM                       
011455     MOVE RAD-KDDSP                TO OBKR-KDDSP                          
011456     MOVE ZERO                     TO OBKR-KDERS                          
011457     MOVE RAD-KDKVBRYT             TO OBKR-KDKVBRYT                       
011458     MOVE RAD-KDPRTYP              TO OBKR-KDPRTYP                        
011459     MOVE RAD-KDTPOTYP             TO OBKR-KDTPOTYP                       
011460     MOVE RAD-KDVRINFO             TO OBKR-KDVRINFO                       
011461     MOVE SPAR-KVART               TO OBKR-KVANNANT                       
011462     MOVE ZERO                     TO OBKR-KVAVBART                       
011463     MOVE RAD-KVART                TO OBKR-KVBEART                        
011464                                      OBKR-KVBEART-Q                      
011465     MOVE ZERO                     TO OBKR-KVBEART-TILLK                  
011466                                      OBKR-KVPREAVB                       
011467                                      OBKR-KVPRERO                        
011468                                      OBKR-KVQPACK                        
011469                                      OBKR-KVRO                           
011470                                      OBKR-KVSLATT                        
011471     MOVE RAD-PRARTNTO             TO OBKR-PRARTNTO                       
011472     MOVE RAD-DEAL-PR-LINE         TO OBKR-DEAL-PR-LINE                   
011473     MOVE ZERO                     TO OBKR-PRBPRIS                        
011474     MOVE RAD-REKSIFFR             TO OBKR-REKSIFFR                       
011475     MOVE ZERO                     TO OBKR-REKSIFFR-TILLK                 
011476                                      OBKR-RERF-RAD                       
011477                                      OBKR-TIDISPIN                       
011478     MOVE OHUV-TIREGDAT            TO OBKR-TIORDREG                       
011479     MOVE ZERO                     TO OBKR-TIPRIS                         
011480                                      OBKR-TIRODAT                        
011481     ACCEPT OBKR-TIREGDAT        FROM DATE                                
011482     ACCEPT WS-TIME              FROM TIME                                
011482     MOVE   WS-TIME(1:6)           TO OBKR-TIREGTID                       
011483                                                                          
011484     MOVE FUNCTION CURRENT-DATE (1:8) TO DATUM-MED-ARHUNDR                
011485     SUBTRACT DATUM-MED-ARHUNDR  FROM +999999999 GIVING                   
011486                                      OBKR-TITIREGD-9KOMPL                
011487     MOVE RAD-TITPO                TO OBKR-TITPO                          
011488     IF  OHUV-TIREGDAT > +500000                                          
011489         ADD +19000000             TO OHUV-TIREGDAT GIVING                
011490                                      DATUM-MED-ARHUNDR                   
011491     ELSE                                                                 
011492         ADD +20000000             TO OHUV-TIREGDAT GIVING                
011493                                     DATUM-MED-ARHUNDR                    
011494     END-IF                                                               
011495     SUBTRACT DATUM-MED-ARHUNDR  FROM +999999999 GIVING                   
011496                                      OBKR-TITIORDD-9KOMPL                
011497     MOVE RAD-KDFRAKT              TO OBKR-KDFRAKT                        
011498     MOVE OHUV-KDORDKL             TO OBKR-KDORDKL                        
011499                                                                          
011500     MOVE RAD-KDORDTYP-LDC         TO OBKR-KDORDTYP-LDC                   
011501     MOVE RAD-TIREPDAT             TO OBKR-TIREPDAT                       
011502     MOVE RAD-IDKUNDRF-WIP         TO OBKR-IDKUNDRF-WIP                   
011503     MOVE ZERO                     TO OBKR-TIDLEVDAT                      
011504     MOVE RAD-PRAVCOST             TO OBKR-PRAVCOST                       
011505     MOVE RAD-KDVALISO             TO OBKR-KDVALISO                       
011506                                                                          
011517     PERFORM IMS-ISRT-WDQ101                                              
011519     PERFORM UNTIL SEGMENT-FOUND                                          
011520        ADD +1                     TO OBKR-IDLOPNR                        
011523        PERFORM IMS-ISRT-WDQ101                                           
011524     END-PERFORM                                                          
011527     .                                                                    
011528                                                                          
011529 BBB-SKAPA-RY9-TRANS  SECTION.                                            
011530     MOVE 'BBB-SKAPA-RY9-TRANS  '  TO CURRENT-SECTION                     
011531                                                                          
011532     MOVE 'RY9'                    TO RY9-IDPTYP                          
011533     MOVE RAD-BERADREF             TO RY9-BERADREF                        
011534     MOVE RAD-BEVOLREF             TO RY9-BEVOLREF                        
011535     MOVE RAD-FLERS                TO RY9-FLERS                           
011536     MOVE RAD-IDDISTR              TO W-IDDISTR-WDB2                      
011537     MOVE RAD-IDKUNDNR             TO W-IDKUNDNR-WDB2                     
011538     PERFORM IMS-GU-WDB201                                                
011539     MOVE GMT-FLNC                 TO RY9-FLNC                            
011540     MOVE RAD-IDARTNR              TO RY9-IDARTNR                         
011541     MOVE ZERO                     TO RY9-IDDIVORD                        
011542     MOVE RAD-IDKUNDRF             TO RY9-IDKUNDRF                        
011543     MOVE RAD-IDLOPNR              TO RY9-IDLOPNR                         
011544     MOVE MSG-LTERM-NAME           TO RY9-IDUSER                          
011545     MOVE RAD-KDFAKTYP             TO RY9-KDFAKTYP                        
011546     MOVE RAD-KDKVBRYT             TO RY9-KDKVBRYT                        
011547     MOVE RAD-KDORDKL              TO RY9-KDORDKL                         
011548     MOVE RAD-KDDSP                TO RY9-KDDSP                           
011549     MOVE RAD-KDRAPRIO             TO RY9-KDRAPRIO                        
011550     MOVE RAD-KDSTARAD             TO RY9-KDSTARAD                        
011551     MOVE RAD-KDTPOTYP             TO RY9-KDTPOTYP                        
011552     MOVE SPACE                    TO RY9-KDUART                          
011553     MOVE RAD-KDVRINFO             TO RY9-KDVRINFO                        
011554     IF RAD-KDTPOTYP NOT = 1                                              
011555       MOVE 0                      TO RY9-KDVRTPO                         
011556     END-IF                                                               
011557     MOVE RAD-PRARTNTO             TO RY9-PRARTNTO                        
011558     MOVE RAD-TIREGDAT             TO RY9-TIREGDAT                        
011559     MOVE RAD-TIRES                TO RY9-TIRES                           
011560     MOVE RAD-TITPO                TO RY9-TITPO                           
011561     MOVE RAD-DARODAT (3:6)        TO RY9-TIRODAT                         
011562     MOVE GMT-FLVR                 TO RY9-FLVR                            
011563                                                                          
011564     MOVE RY9-WDGZRY9              TO ZZAC-LOGGPOST                       
011565                                                                          
011566     MOVE SPACE                    TO RY9S-WDGZRY9S                       
011567     MOVE RAD-IDDISTR              TO RY9S-IDDISTR                        
011568     MOVE RAD-IDKUNDNR             TO RY9S-IDKUNDNR                       
011569     MOVE OHUV-IDORDER             TO RY9S-IDORDER                        
011570     MOVE RAD-IDDC                 TO RY9S-IDDC                           
011571     MOVE RAD-KDFRAKT              TO RY9S-KDFRAKT                        
011572     MOVE RAD-KDORDKL              TO RY9S-KDORDKL                        
011573     IF RAD-FLTPOBEK = NEJ                                                
011574        MOVE 83                    TO RY9S-KDORDBEK                       
011575     ELSE                                                                 
011576        MOVE 85                    TO RY9S-KDORDBEK                       
011577     END-IF                                                               
011578                                                                          
011579     MOVE RY9S-WDGZRY9S            TO ZZAC-SORTPOST                       
011580     .                                                                    
011581                                                                          
011582 BBC-SKRIV-LOGG SECTION.                                                  
011583     MOVE 'BBC-SKRIV-LOGG       '  TO CURRENT-SECTION                     
011584                                                                          
011585     ACCEPT ZZAC-TIAAMMDD        FROM DATE                                
011586     ACCEPT ZZAC-TIKLOCK         FROM TIME                                
011587     MOVE  +1                      TO ZZAC-IDLOGLOP                       
011588                                                                          
011589     PERFORM IMS-ISRT-WDG6                                                
011590     IF SEGMENT-EXISTS-ALREADY                                            
011591       PERFORM UNTIL SEGMENT-ADDED                                        
011592         ADD +1                    TO ZZAC-IDLOGLOP                       
011593         PERFORM IMS-ISRT-WDG6                                            
011594       END-PERFORM                                                        
011595     END-IF                                                               
011596     .                                                                    
011597                                                                          
011598 BBD-SKAPA-2109-TRANS SECTION.                                            
011599     MOVE 'BBD-SKAPA-2109-TRANS '  TO CURRENT-SECTION                     
011600                                                                          
011601*    FÖR BYYTESARTIKLAR SKALL INGEN ORDERINGÅNG SKAPAS                    
011602     MOVE RAD-IDARTNR              TO BYT03-IDARTNR                       
011603     IF NOT BYT03-OBJEKT                                                  
011604                                                                          
011605        IF RAD-KDOI NOT = SPACE                                           
011606          MOVE SPACE               TO 2109-MID2-W2I10902                  
011607          MOVE 1                   TO 2109-MID2-KVANTART                  
011608          MOVE RAD-IDARTNR         TO 2109-MID2-IDARTNR (1)               
011609          MOVE OHUV-IDDC-PRIM      TO 2109-MID2-IDDC(1)                   
011611          MOVE '-'                 TO 2109-MID2-KDTECKEN (1)              
011612          MOVE RAD-KDOI            TO 2109-MID2-KDOI (1)                  
011613          MOVE RAD-CLEARGROUP      TO 2109-MID2-CLEARGROUP (1)            
011614          MOVE SPAR-KVART          TO 2109-MID2-KVOI (1)                  
011615                                                                          
011616          IF RAD-KDTPOTYP = 0                                             
011617              MOVE RAD-TIREGDAT    TO 2109-MID2-TIUPPDAT (1)              
011618          END-IF                                                          
011619                                                                          
011620          COMPUTE 2109-KVLL = LENGTH OF 2109-MID2-W2I10902 + 17           
011621                                                                          
011622          PERFORM IMS-PURG-ALT-MSG-2109                                   
011623        END-IF                                                            
011624     END-IF                                                               
011625     .                                                                    
011626                                                                          
011627 BC-SEND-DAP SECTION.                                                     
011628     MOVE 'BC-SEND-DAP         '   TO CURRENT-SECTION                     
011629                                                                          
011630     IF WZ04-SEND-IDCOM = ZERO                                            
011631        PERFORM S90-SEND-OPEN                                             
011632        MOVE SEND-IDCOM            TO WZ04-SEND-IDCOM                     
011633     END-IF                                                               
011634                                                                          
011635     MOVE 001                      TO HDR-REQU-IDMSGVER                   
011636     MOVE SPACE                    TO HDR-REQU-KDPGMACT                   
011637     MOVE PROGRAM-NAME             TO HDR-REQU-IDUSER                     
011638     MOVE 'BO-RELEASE-DOC'         TO HDR-IDOUTTYPE                       
011639     MOVE SPACE                    TO HDR-IDOUTREC                        
011640     MOVE WS-IDDISTR-X             TO HDR-IDOUTREC(1:4)                   
011641     MOVE WS-IDKUNDNR-X            TO HDR-IDOUTREC(5:6)                   
011642     MOVE 'W41219'                 TO HDR-IDLIST                          
011643     PERFORM S90-PUT-HEADER                                               
011644     PERFORM S90-PUT-MAIL-LINE-AREA1                                      
011645                                                                          
011646     MOVE 'District No.  '         TO DAP-MAIL-LINE-TEXT-4                
011647     MOVE 'Customer  '             TO DAP-MAIL-LINE-TEXT-5                
011648     MOVE 'Part No.  '             TO DAP-MAIL-LINE-TEXT-6                
011649     MOVE 'Part Description           '                                   
011650                                   TO DAP-MAIL-LINE-TEXT-7                
011651     MOVE 'Quantity  '             TO DAP-MAIL-LINE-TEXT-8                
011652     MOVE 'Order No.  '            TO DAP-MAIL-LINE-TEXT-9                
011653     MOVE 'Repair Date  '          TO DAP-MAIL-LINE-TEXT-10               
011654     MOVE 'Work Shop Order  '      TO DAP-MAIL-LINE-TEXT-11               
011655     MOVE RAD-IDDISTR              TO DAP-MAIL-DISTRICT-NO                
011656                                      TEST-IDDISTR                        
011657     MOVE RAD-IDKUNDNR             TO DAP-MAIL-CUSTOMER-NO                
011658     MOVE RAD-IDARTNR              TO DAP-MAIL-PART-NO                    
011659                                      W-IDARTNR                           
011660     IF DIST03-SVERIGE                                                    
011661        MOVE 'S '                  TO W-IDSKYLT                           
011662     ELSE                                                                 
011663        MOVE 'GB'                  TO W-IDSKYLT                           
011664     END-IF                                                               
011665     PERFORM IMS-GU-WDD311                                                
011666     MOVE TEXT-BEART               TO DAP-MAIL-PART-DESC                  
011667     MOVE RAD-KVART                TO DAP-MAIL-QTY                        
011668     MOVE RAD-IDKUNDRF             TO DAP-MAIL-ORDER-NO                   
011669     MOVE RAD-TIREPDAT             TO WS-HELPDAT                          
011670     MOVE WS-HELPDAT               TO DAP-MAIL-REPAIR-DATE                
011671     MOVE RAD-IDKUNDRF-WIP         TO DAP-MAIL-WRKSHOP-ORDER              
011672     PERFORM S90-PUT-MAIL-LINE-AREA2                                      
011673     PERFORM S90-PUT-MAIL-LINE-AREA3                                      
011674     PERFORM BCA-REDIGERA-WDD9-INLA                                       
011675     PERFORM S90-PUT-MAIL-LINE-AREA4                                      
011676                                                                          
011677     IF WZ04-SEND-IDCOM > ZERO                                            
011678        PERFORM S90-SEND-CLOSE                                            
011679        MOVE ZERO                  TO WZ04-SEND-IDCOM                     
011680     END-IF                                                               
011681     .                                                                    
011682                                                                          
011683 BCA-REDIGERA-WDD9-INLA SECTION.                                          
011684                                                                          
011685     MOVE 999999                   TO W-SAVE-TILEVBSK                     
011686                                                                          
011687     MOVE W-IDARTNR                TO W-IDARTNR-D9                        
011688     MOVE RAD-IDDC                 TO W-IDDC-D9                           
011689     PERFORM IMS-GU-WDD901                                                
011690     IF SEGMENT-FOUND                                                     
011691       PERFORM IMS-GNP-WDD902                                             
011692       PERFORM UNTIL SEGMENT-MISSING                                      
011693         MOVE IDLEVNR              TO W-IDLEVNR                           
011694         PERFORM IMS-GNP-WDD924                                           
011695         IF SEGMENT-FOUND                                                 
011696            MOVE LEV-TILEVBSK-INL  TO TMP1-YYMMDD                         
011697            MOVE W-SAVE-TILEVBSK   TO TMP2-YYMMDD                         
011698            PERFORM WY2000P1                                              
011699            IF TMP1-YYMMDD < TMP2-YYMMDD AND                              
011700               LEV-FLSENLEV = NEJ                                         
011701                MOVE LEV-TILEVBSK-INL        TO W-SAVE-TILEVBSK           
011702            END-IF                                                        
011703         END-IF                                                           
011704         PERFORM IMS-GNP-WDD902                                           
011705       END-PERFORM                                                        
011706     END-IF                                                               
011707                                                                          
011708     IF W-SAVE-TILEVBSK < 999999                                          
011709         MOVE W-SAVE-TILEVBSK      TO DAT-I-TIDATUM                       
011710         MOVE 'AAMMDD'             TO DAT-KDDATFORM                       
011711         CALL WDATKONV          USING DAT-KDDATFORM,                      
011712                                      DAT-I-TIDATUM,                      
011713                                      DAT-O-TIDATUM,                      
011714                                      DAT-KDSVAR                          
011715         IF DAT-KDSVAR-OK                                                 
011716            MOVE DAT-TIAAVVD(3:2)  TO DAP-MAIL-LINE-VALUE-WW              
011717            MOVE DAT-TIAAVVD(5:1)  TO DAP-MAIL-LINE-VALUE-D               
011718         ELSE                                                             
011719            MOVE ZERO              TO DAP-MAIL-LINE-VALUE-WW              
011720            MOVE ZERO              TO DAP-MAIL-LINE-VALUE-D               
011721         END-IF                                                           
011722     END-IF                                                               
011723     .                                                                    
011724                                                                          
011725 S10-MINSKA-KVROS-WDK6  SECTION.                                          
011726     MOVE 'S10-MINSKA-KVROS-WDK6 ' TO CURRENT-SECTION                     
011727                                                                          
011728     MOVE RAD-IDARTNR TO W-IDARTNR-WDK601                                 
011729     PERFORM IMS-GHU-WDK611                                               
011730     COMPUTE CLAG-KVROS = CLAG-KVROS - SPAR-KVART                         
011731     PERFORM IMS-REPL-WDK611                                              
011732     .                                                                    
011733                                                                          
011734 S11-MINSKA-KVROS-WDK7  SECTION.                                          
011735     MOVE 'S11-MINSKA-KVROS-WDK7 ' TO CURRENT-SECTION                     
011736                                                                          
011737     MOVE RAD-IDARTNR TO W-IDARTNR-K7                                     
011738     MOVE RAD-IDDC    TO W-IDDC-K7                                        
011739                                                                          
011740     PERFORM IMS-GHU-WDK711                                               
011741     IF SEGMENT-FOUND                                                     
011742        IF RAD-KDORDKL = +3                                               
011743           COMPUTE SLAG-KVROS-BULK =                                      
011744                   SLAG-KVROS-BULK - SPAR-KVART                           
011745        END-IF                                                            
011746        PERFORM IMS-REPL-WDK711                                           
011747     END-IF                                                               
011748     .                                                                    
011749                                                                          
011750 S90-SEND-OPEN SECTION.                                                   
011751                                                                          
011752     MOVE WS-ADRESS                TO SEND-ADDISPABS                      
011753     MOVE 'OPEN'                   TO SEND-KDFUNC                         
011754     CALL WZ01SEND USING SEND-CONTROL-AREA                                
011755                         SEND-OPEN-AREA                                   
011756     IF SEND-KDRC > ZERO                                                  
011757       MOVE SEND-KDRC              TO KDRC-DISPLAY                        
011758       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
011759       DELIMITED BY SIZE INTO ERROR-TEXT-STR                              
011760       CALL ABEND               USING RKOD-ABEND-WITH-DUMP                
011761     END-IF                                                               
011762     .                                                                    
011763                                                                          
011764 S90-PUT-HEADER SECTION.                                                  
011765     MOVE 'PUT'                    TO SEND-KDFUNC                         
011766     MOVE WZ04-SEND-IDCOM          TO SEND-IDCOM                          
011767     MOVE LENGTH OF HDR-AREA       TO SEND-KVDLEN                         
011768     CALL WZ01SEND              USING SEND-CONTROL-AREA                   
011769                                      SEND-KVDLEN                         
011770                                      HDR-AREA                            
011771     IF SEND-KDRC > ZERO                                                  
011772       MOVE SEND-KDRC              TO KDRC-DISPLAY                        
011773       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
011774       DELIMITED BY SIZE INTO ERROR-TEXT-STR                              
011775       CALL ABEND               USING RKOD-ABEND-WITH-DUMP                
011776     END-IF                                                               
011777     .                                                                    
011778                                                                          
011779 S90-PUT-MAIL-LINE-AREA1 SECTION.                                         
011780                                                                          
011781     MOVE 'PUT'                    TO SEND-KDFUNC                         
011782     MOVE WZ04-SEND-IDCOM          TO SEND-IDCOM                          
011783     MOVE LENGTH OF DAP-MAIL-LINE1-AREA1  TO SEND-KVDLEN                  
011784     CALL WZ01SEND              USING SEND-CONTROL-AREA                   
011785                                      SEND-KVDLEN                         
011786                                      DAP-MAIL-LINE1-AREA1                
011787     IF SEND-KDRC > ZERO                                                  
011788       MOVE SEND-KDRC              TO KDRC-DISPLAY                        
011789       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
011790       DELIMITED BY SIZE INTO ERROR-TEXT-STR                              
011791       CALL ABEND               USING RKOD-ABEND-WITH-DUMP                
011792     END-IF                                                               
011793     MOVE WZ04-SEND-IDCOM          TO SEND-IDCOM                          
011794     MOVE LENGTH OF DAP-MAIL-LINE-TEXT-SPACE                              
011795                                   TO SEND-KVDLEN                         
011796     CALL WZ01SEND              USING SEND-CONTROL-AREA                   
011797                                      SEND-KVDLEN                         
011798                                      DAP-MAIL-LINE-TEXT-SPACE            
011799     IF SEND-KDRC > ZERO                                                  
011800       MOVE SEND-KDRC                   TO KDRC-DISPLAY                   
011801       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
011802       DELIMITED BY SIZE INTO ERROR-TEXT-STR                              
011803       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
011804     END-IF                                                               
011805     MOVE WZ04-SEND-IDCOM               TO SEND-IDCOM                     
011806     MOVE LENGTH OF DAP-MAIL-LINE2-AREA1                                  
011807                                        TO SEND-KVDLEN                    
011808     CALL WZ01SEND                   USING SEND-CONTROL-AREA              
011809                                           SEND-KVDLEN                    
011810                                           DAP-MAIL-LINE2-AREA1           
011811     IF SEND-KDRC > ZERO                                                  
011812       MOVE SEND-KDRC                   TO KDRC-DISPLAY                   
011813       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
011814       DELIMITED BY SIZE INTO ERROR-TEXT-STR                              
011815       CALL ABEND                    USING RKOD-ABEND-WITH-DUMP           
011816     END-IF                                                               
011817     MOVE WZ04-SEND-IDCOM               TO SEND-IDCOM                     
011818     MOVE LENGTH OF DAP-MAIL-LINE-TEXT-SPACE                              
011819                                        TO SEND-KVDLEN                    
011820     CALL WZ01SEND                   USING SEND-CONTROL-AREA              
011821                                           SEND-KVDLEN                    
011822                                          DAP-MAIL-LINE-TEXT-SPACE        
011823     IF SEND-KDRC > ZERO                                                  
011824       MOVE SEND-KDRC                   TO KDRC-DISPLAY                   
011825       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
011826       DELIMITED BY SIZE INTO ERROR-TEXT-STR                              
011827       CALL ABEND                    USING RKOD-ABEND-WITH-DUMP           
011828     END-IF                                                               
011829     MOVE WZ04-SEND-IDCOM               TO SEND-IDCOM                     
011830     MOVE LENGTH OF DAP-MAIL-LINE3-AREA1                                  
011831                                        TO SEND-KVDLEN                    
011832     CALL WZ01SEND                   USING SEND-CONTROL-AREA              
011833                                           SEND-KVDLEN                    
011834                                           DAP-MAIL-LINE3-AREA1           
011835     IF SEND-KDRC > ZERO                                                  
011836       MOVE SEND-KDRC                   TO KDRC-DISPLAY                   
011837       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
011838       DELIMITED BY SIZE INTO ERROR-TEXT-STR                              
011839       CALL ABEND                    USING RKOD-ABEND-WITH-DUMP           
011840     END-IF                                                               
011841     MOVE WZ04-SEND-IDCOM               TO SEND-IDCOM                     
011842     MOVE LENGTH OF DAP-MAIL-LINE-TEXT-SPACE                              
011843                                        TO SEND-KVDLEN                    
011844     CALL WZ01SEND                   USING SEND-CONTROL-AREA              
011845                                           SEND-KVDLEN                    
011846                                          DAP-MAIL-LINE-TEXT-SPACE        
011847     IF SEND-KDRC > ZERO                                                  
011848       MOVE SEND-KDRC                   TO KDRC-DISPLAY                   
011849       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
011850       DELIMITED BY SIZE INTO ERROR-TEXT-STR                              
011851       CALL ABEND                    USING RKOD-ABEND-WITH-DUMP           
011852     END-IF                                                               
011853     .                                                                    
011854                                                                          
011855 S90-PUT-MAIL-LINE-AREA2 SECTION.                                         
011856                                                                          
011857     MOVE 'PUT'                         TO SEND-KDFUNC                    
011858     MOVE WZ04-SEND-IDCOM               TO SEND-IDCOM                     
011859     MOVE LENGTH OF DAP-MAIL-BODY-AREA2 TO SEND-KVDLEN                    
011860     CALL WZ01SEND                   USING SEND-CONTROL-AREA              
011861                                           SEND-KVDLEN                    
011862                                           DAP-MAIL-BODY-AREA2            
011863     IF SEND-KDRC > ZERO                                                  
011864       MOVE SEND-KDRC                   TO KDRC-DISPLAY                   
011865       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
011866       DELIMITED BY SIZE INTO ERROR-TEXT-STR                              
011867       CALL ABEND                    USING RKOD-ABEND-WITH-DUMP           
011868     END-IF                                                               
011869     .                                                                    
011870                                                                          
011871 S90-PUT-MAIL-LINE-AREA3 SECTION.                                         
011872                                                                          
011873     MOVE 'PUT'                         TO SEND-KDFUNC                    
011874     MOVE WZ04-SEND-IDCOM               TO SEND-IDCOM                     
011875     MOVE LENGTH OF DAP-MAIL-BODY-AREA3 TO SEND-KVDLEN                    
011876     CALL WZ01SEND                   USING SEND-CONTROL-AREA              
011877                                           SEND-KVDLEN                    
011878                                           DAP-MAIL-BODY-AREA3            
011879     IF SEND-KDRC > ZERO                                                  
011880       MOVE SEND-KDRC                   TO KDRC-DISPLAY                   
011881       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
011882       DELIMITED BY SIZE INTO ERROR-TEXT-STR                              
011883       CALL ABEND                    USING RKOD-ABEND-WITH-DUMP           
011884     END-IF                                                               
011885     MOVE WZ04-SEND-IDCOM               TO SEND-IDCOM                     
011886     MOVE LENGTH OF DAP-MAIL-LINE-TEXT-SPACE                              
011887                                        TO SEND-KVDLEN                    
011888     CALL WZ01SEND                   USING SEND-CONTROL-AREA              
011889                                           SEND-KVDLEN                    
011890                                          DAP-MAIL-LINE-TEXT-SPACE        
011891     IF SEND-KDRC > ZERO                                                  
011892       MOVE SEND-KDRC                   TO KDRC-DISPLAY                   
011893       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
011894       DELIMITED BY SIZE INTO ERROR-TEXT-STR                              
011895       CALL ABEND                    USING RKOD-ABEND-WITH-DUMP           
011896     END-IF                                                               
011897     .                                                                    
011898                                                                          
011899 S90-PUT-MAIL-LINE-AREA4 SECTION.                                         
011900                                                                          
011901     MOVE 'PUT'                         TO SEND-KDFUNC                    
011902     MOVE WZ04-SEND-IDCOM               TO SEND-IDCOM                     
011903     MOVE LENGTH OF DAP-MAIL-LINE1-AREA4                                  
011904                                        TO SEND-KVDLEN                    
011905     CALL WZ01SEND                   USING SEND-CONTROL-AREA              
011906                                           SEND-KVDLEN                    
011907                                           DAP-MAIL-LINE1-AREA4           
011908     IF SEND-KDRC > ZERO                                                  
011909       MOVE SEND-KDRC                   TO KDRC-DISPLAY                   
011910       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
011911       DELIMITED BY SIZE INTO ERROR-TEXT-STR                              
011912       CALL ABEND                    USING RKOD-ABEND-WITH-DUMP           
011913     END-IF                                                               
011914     MOVE WZ04-SEND-IDCOM               TO SEND-IDCOM                     
011915     MOVE LENGTH OF DAP-MAIL-LINE2-AREA4                                  
011916                                        TO SEND-KVDLEN                    
011917     CALL WZ01SEND                   USING SEND-CONTROL-AREA              
011918                                           SEND-KVDLEN                    
011919                                           DAP-MAIL-LINE2-AREA4           
011920     IF SEND-KDRC > ZERO                                                  
011921       MOVE SEND-KDRC                   TO KDRC-DISPLAY                   
011922       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
011923       DELIMITED BY SIZE INTO ERROR-TEXT-STR                              
011924       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
011925     END-IF                                                               
011926     MOVE WZ04-SEND-IDCOM               TO SEND-IDCOM                     
011927     MOVE LENGTH OF DAP-MAIL-LINE3-AREA4                                  
011928                                        TO SEND-KVDLEN                    
011929     CALL WZ01SEND                   USING SEND-CONTROL-AREA              
011930                                           SEND-KVDLEN                    
011931                                           DAP-MAIL-LINE3-AREA4           
011932     IF SEND-KDRC > ZERO                                                  
011933       MOVE SEND-KDRC                   TO KDRC-DISPLAY                   
011934       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
011935       DELIMITED BY SIZE INTO ERROR-TEXT-STR                              
011936       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
011937     END-IF                                                               
011938     MOVE WZ04-SEND-IDCOM               TO SEND-IDCOM                     
011939     MOVE LENGTH OF DAP-MAIL-LINE4-AREA4                                  
011940                                        TO SEND-KVDLEN                    
011941     CALL WZ01SEND                   USING SEND-CONTROL-AREA              
011942                                           SEND-KVDLEN                    
011943                                           DAP-MAIL-LINE4-AREA4           
011944     IF SEND-KDRC > ZERO                                                  
011945       MOVE SEND-KDRC                   TO KDRC-DISPLAY                   
011946       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
011947       DELIMITED BY SIZE INTO ERROR-TEXT-STR                              
011948       CALL ABEND                    USING RKOD-ABEND-WITH-DUMP           
011949     END-IF                                                               
011950     MOVE WZ04-SEND-IDCOM               TO SEND-IDCOM                     
011951     MOVE LENGTH OF DAP-MAIL-LINE-TEXT-SPACE                              
011952                                        TO SEND-KVDLEN                    
011953     CALL WZ01SEND                   USING SEND-CONTROL-AREA              
011954                                           SEND-KVDLEN                    
011955                                          DAP-MAIL-LINE-TEXT-SPACE        
011956     IF SEND-KDRC > ZERO                                                  
011957       MOVE SEND-KDRC                   TO KDRC-DISPLAY                   
011958       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
011959       DELIMITED BY SIZE INTO ERROR-TEXT-STR                              
011960       CALL ABEND                    USING RKOD-ABEND-WITH-DUMP           
011961     END-IF                                                               
011962     MOVE WZ04-SEND-IDCOM               TO SEND-IDCOM                     
011963     MOVE LENGTH OF DAP-MAIL-LINE5-AREA4                                  
011964                                        TO SEND-KVDLEN                    
011965     CALL WZ01SEND                   USING SEND-CONTROL-AREA              
011966                                           SEND-KVDLEN                    
011967                                           DAP-MAIL-LINE5-AREA4           
011968     IF SEND-KDRC > ZERO                                                  
011969       MOVE SEND-KDRC                   TO KDRC-DISPLAY                   
011970       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
011971       DELIMITED BY SIZE INTO ERROR-TEXT-STR                              
011972       CALL ABEND                    USING RKOD-ABEND-WITH-DUMP           
011973     END-IF                                                               
011974     MOVE WZ04-SEND-IDCOM               TO SEND-IDCOM                     
011975     MOVE LENGTH OF DAP-MAIL-LINE-TEXT-SPACE                              
011976                                        TO SEND-KVDLEN                    
011977     CALL WZ01SEND                   USING SEND-CONTROL-AREA              
011978                                           SEND-KVDLEN                    
011979                         DAP-MAIL-LINE-TEXT-SPACE                         
011980     IF SEND-KDRC > ZERO                                                  
011981       MOVE SEND-KDRC                   TO KDRC-DISPLAY                   
011982       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
011983       DELIMITED BY SIZE INTO ERROR-TEXT-STR                              
011984       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
011985     END-IF                                                               
011986     MOVE WZ04-SEND-IDCOM               TO SEND-IDCOM                     
011987     MOVE LENGTH OF DAP-MAIL-LINE6-AREA4                                  
011988                                        TO SEND-KVDLEN                    
011989     CALL WZ01SEND                   USING SEND-CONTROL-AREA              
011990                                           SEND-KVDLEN                    
011991                                           DAP-MAIL-LINE6-AREA4           
011992     IF SEND-KDRC > ZERO                                                  
011993       MOVE SEND-KDRC                   TO KDRC-DISPLAY                   
011994       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
011995       DELIMITED BY SIZE INTO ERROR-TEXT-STR                              
011996       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
011997     END-IF                                                               
011998     MOVE WZ04-SEND-IDCOM               TO SEND-IDCOM                     
011999     MOVE LENGTH OF DAP-MAIL-LINE7-AREA4                                  
012000                                        TO SEND-KVDLEN                    
012001     CALL WZ01SEND                   USING SEND-CONTROL-AREA              
012002                                           SEND-KVDLEN                    
012003                                           DAP-MAIL-LINE7-AREA4           
012004     IF SEND-KDRC > ZERO                                                  
012005       MOVE SEND-KDRC                   TO KDRC-DISPLAY                   
012006       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
012007       DELIMITED BY SIZE INTO ERROR-TEXT-STR                              
012008       CALL ABEND                    USING RKOD-ABEND-WITH-DUMP           
012009     END-IF                                                               
012010     .                                                                    
012011                                                                          
012012 S90-SEND-CLOSE SECTION.                                                  
012013                                                                          
012014     MOVE 'CLOSE'                       TO SEND-KDFUNC                    
012015     MOVE WZ04-SEND-IDCOM               TO SEND-IDCOM                     
012016     CALL WZ01SEND                   USING SEND-CONTROL-AREA              
012017     .                                                                    
012018                                                                          
012019 X-TAKE-CHECKPOINT   SECTION.                                             
012020                                                                          
012021     PERFORM IMS-CHECKPOINT                                               
012022     MOVE ZERO                     TO CHKP-ANT                            
012034     .                                                                    
012035                                                                          
012050*---- IMS SECTIONS                                                        
012035 IMS-RESTART SECTION.                                                     
012035                                                                          
012035     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
012035     MOVE '  ' TO GOOD-STATUSCODES                                        
012035     CALL CBLTDLI USING XRST MSG-PCB                                      
012035                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
012035                        CHKP-AREA-LENGTH CHKP-AREA                        
012035     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
012035     PERFORM IMS-STATUSCHECK                                              
012035     .                                                                    
012035                                                                          
012073 IMS-CHECKPOINT SECTION.                                                  
012074                                                                          
012075     MOVE PROGRAM-NAME             TO CHKP-MSG-IO-AREA                    
012076     MOVE '  XD'                   TO GOOD-STATUSCODES                    
012077     CALL CBLTDLI USING CHKP MSG-PCB                                      
012078                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
012079                        CHKP-AREA-LENGTH CHKP-AREA                        
012080     MOVE MSG-STATUS-CODE          TO STATUS-WS                           
012081     PERFORM IMS-STATUSCHECK                                              
012082                                                                          
012083     IF IMS-NOT-OK                                                        
012084       MOVE 'IMS CONTROL REGION IS NOT ACCESSIBLE'                        
012085                                   TO ERROR-TEXT-STR                      
012086       DISPLAY ERROR-TEXT                                                 
012087       CALL FELLOG                                                        
012088     END-IF                                                               
012089     .                                                                    
012090                                                                          
012091 IMS-PURG-ALT-MSG-2109 SECTION.                                           
012092     MOVE LOW-VALUE                TO 2109-Z1 2109-Z2                     
012093     MOVE SPACE                    TO GOOD-STATUSCODES                    
012094     CALL CBLTDLI USING PURG 2109-PCB W-PROG-TO-PROG-SW-1                 
012095     MOVE 2109-STATUS-CODE         TO STATUS-WS                           
012096     PERFORM IMS-STATUSCHECK                                              
012097     .                                                                    
012098                                                                          
012099 IMS-GHN-WDA501 SECTION.                                                  
012100     MOVE 'IMS-GN-WDA501       '   TO CURRENT-IMS-SECTION                 
012101                                                                          
012102     STRING 'WDA501  (WDA501KY=>' W-WDA5KEY-MIN-X                         
012103                    '&WDA501KY=<' W-WDA5KEY-MAX-X                         
012104                    '&KDORDKL = ' W-KDORDKL-X                             
012105                    '&TIREPDAT >' WS-TODAY-PKD-DEC-X                      
012106                    '&IDSYSTX3= ' W-IDSYSTX3-LDC                          
012107                    '&KDSTARAD =' W-KDSTARAD                              
012108                    '&KDORDTYP= ' VERKSTADSORDER                          
012109                    '!WDA501KY=>' W-WDA5KEY-MIN-X                         
012110                    '&WDA501KY=<' W-WDA5KEY-MAX-X                         
012111                    '&KDORDKL = ' W-KDORDKL-X                             
012112                    '&TIREPDAT >' WS-TODAY-PKD-DEC-X                      
012113                    '&IDSYSTX3= ' W-IDSYSTX3-LDC                          
012114                    '&KDSTARAD =' W-KDSTARAD                              
012115                    '&KDORDTYP= ' BUTIKSORDER                             
012109                    '!WDA501KY=>' W-WDA5KEY-MIN-X                         
012110                    '&WDA501KY=<' W-WDA5KEY-MAX-X                         
012111                    '&KDORDKL = ' W-KDORDKL-X                             
012112                    '&TIREPDAT >' WS-TODAY-PKD-DEC-X                      
012113                    '&IDSYSTX3= ' W-IDSYSTX3-ECO                          
012114                    '&KDSTARAD =' W-KDSTARAD                              
012115                    '&KDORDTYP= ' VERKSTADSORDER                          
012109                    '!WDA501KY=>' W-WDA5KEY-MIN-X                         
012110                    '&WDA501KY=<' W-WDA5KEY-MAX-X                         
012111                    '&KDORDKL = ' W-KDORDKL-X                             
012112                    '&TIREPDAT >' WS-TODAY-PKD-DEC-X                      
012113                    '&IDSYSTX3= ' W-IDSYSTX3-ECO                          
012114                    '&KDSTARAD =' W-KDSTARAD                              
012115                    '&KDORDTYP= ' BUTIKSORDER ')'                         
012116          DELIMITED BY SIZE  INTO SSA1                                    
012120     MOVE '  GEGB'                 TO GOOD-STATUSCODES                    
012200     CALL CBLTDLI USING GHN WDA5-PCB DLI-IO-WDA501 SSA1                   
012300     MOVE WDA5-STATUS-CODE         TO STATUS-WS                           
012400     PERFORM IMS-STATUSCHECK                                              
012410     .                                                                    
012500                                                                          
012710 IMS-GHU-WDA501 SECTION.                                                  
012711                                                                          
012712     MOVE 'IMS-GHU-WDA501      '   TO CURRENT-IMS-SECTION                 
012713     STRING 'WDA501  (WDA501KY= ' W-WDA5KEY-X ')'                         
012714          DELIMITED BY SIZE  INTO SSA1                                    
012715     MOVE '  '                     TO GOOD-STATUSCODES                    
012716     CALL CBLTDLI USING GHU WDA5-PCB DLI-IO-WDA501 SSA1                   
012717     MOVE WDA5-STATUS-CODE         TO STATUS-WS                           
012718     PERFORM IMS-STATUSCHECK                                              
012719     .                                                                    
012720                                                                          
012721 IMS-DLET-WDA501 SECTION.                                                 
012722     MOVE 'IMS-DLET-WDA501     '   TO CURRENT-IMS-SECTION                 
012723                                                                          
012724     MOVE '  '                     TO GOOD-STATUSCODES                    
012725     CALL CBLTDLI USING DLET WDA5-PCB DLI-IO-WDA501                       
012726     MOVE WDA5-STATUS-CODE         TO STATUS-WS                           
012727     PERFORM IMS-STATUSCHECK                                              
012728     .                                                                    
012729                                                                          
012730 IMS-GU-WDR501 SECTION.                                                   
012731     MOVE 'IMS-GU-WDR501       '   TO CURRENT-IMS-SECTION                 
012732                                                                          
012733     STRING 'WDR501  (WDGXKEY  =' W-4563KEY-X ')'                         
012734            DELIMITED BY SIZE INTO SSA1                                   
012735     STRING 'WDGX4564(KY4564   =' W-4564KEY-X ')'                         
012736            DELIMITED BY SIZE INTO SSA2                                   
012737                                                                          
012738     MOVE '  GE'                   TO GOOD-STATUSCODES                    
012739     CALL CBLTDLI USING GU WDR5-PCB DLI-IO-WDR501 SSA1 SSA2               
012740     MOVE WDR5-STATUS-CODE         TO STATUS-WS                           
012741     PERFORM IMS-STATUSCHECK                                              
012742     .                                                                    
012743                                                                          
012744 IMS-GU-WDB201 SECTION.                                                   
012745     MOVE 'IMS-GU-WDB201       '   TO CURRENT-IMS-SECTION                 
012746                                                                          
012747     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
012748            DELIMITED BY SIZE INTO SSA1                                   
012749                                                                          
012750     MOVE '  GE'                   TO GOOD-STATUSCODES                    
012751     CALL CBLTDLI         USING GU WDB2-PCB DLI-IO-WDB201 SSA1            
012752     MOVE WDB2-STATUS-CODE         TO STATUS-WS                           
012753     PERFORM IMS-STATUSCHECK                                              
012754     .                                                                    
012755                                                                          
012756 IMS-GU-WDD311 SECTION.                                                   
012757     MOVE 'IMS-GU-WDD311       '   TO CURRENT-IMS-SECTION                 
012758                                                                          
012759     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
012760          DELIMITED BY SIZE INTO SSA1                                     
012761     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
012762          DELIMITED BY SIZE INTO SSA2                                     
012763     MOVE '  GE'                   TO GOOD-STATUSCODES                    
012764     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
012765     MOVE WDD3-STATUS-CODE         TO STATUS-WS                           
012766     PERFORM IMS-STATUSCHECK                                              
012767     .                                                                    
012768                                                                          
012769 IMS-GU-WDD901 SECTION.                                                   
012770     MOVE 'IMS-GU-WDD901       '   TO CURRENT-IMS-SECTION                 
012771                                                                          
012772     STRING 'WDD901  (WDD901KY= ' W-WDD901KY-X ')'                        
012773              DELIMITED BY SIZE INTO SSA1                                 
012774     MOVE '  GE'                   TO GOOD-STATUSCODES                    
012775     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD901 SSA1                    
012776     MOVE WDD9-STATUS-CODE         TO STATUS-WS                           
012777     PERFORM IMS-STATUSCHECK                                              
012778     .                                                                    
012779                                                                          
012780 IMS-GNP-WDD902 SECTION.                                                  
012781     MOVE 'IMS-GNP-WDD902      '   TO CURRENT-IMS-SECTION                 
012782                                                                          
012783     STRING 'WDD901  (WDD901KY= ' W-WDD901KY-X ')'                        
012784              DELIMITED BY SIZE INTO SSA1                                 
012785     MOVE 'WDD902  '               TO SSA2                                
012786     MOVE '  GE'                   TO GOOD-STATUSCODES                    
012787     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD902 SSA1 SSA2              
012788     MOVE WDD9-STATUS-CODE         TO STATUS-WS                           
012789     PERFORM IMS-STATUSCHECK                                              
012790     .                                                                    
012791                                                                          
012792 IMS-GNP-WDD924 SECTION.                                                  
012793     MOVE 'IMS-GNP-WDD924      '   TO CURRENT-IMS-SECTION                 
012794                                                                          
012795     STRING 'WDD902  (IDLEVNR = ' W-IDLEVNR-X ')'                         
012796              DELIMITED BY SIZE INTO SSA1                                 
012797     MOVE 'WDD924  '               TO SSA2                                
012798     MOVE '  GE'                   TO GOOD-STATUSCODES                    
012799     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD924 SSA1 SSA2              
012800     MOVE WDD9-STATUS-CODE         TO STATUS-WS                           
012801     PERFORM IMS-STATUSCHECK                                              
012802     .                                                                    
012803                                                                          
012806 IMS-GU-WDB601    SECTION.                                                
012807     MOVE 'IMS-GHU-WDB601      '   TO CURRENT-IMS-SECTION                 
012808                                                                          
012809     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
012810          DELIMITED BY SIZE INTO SSA1                                     
012811     MOVE '  GE'                   TO GOOD-STATUSCODES                    
012812     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
012813     MOVE WDB6-STATUS-CODE         TO STATUS-WS                           
012814     PERFORM IMS-STATUSCHECK                                              
012815     IF SEGMENT-MISSING                                                   
012816         MOVE SPACE                TO DCS-KDDC                            
012817     END-IF                                                               
012818     .                                                                    
012819                                                                          
012820 IMS-GHU-WDK611 SECTION.                                                  
012821     MOVE 'IMS-GHU-WDK611      '   TO CURRENT-IMS-SECTION                 
012822                                                                          
012823     STRING  'WDK601  (IDARTNR  =' W-IDARTNR-WDK601-X ')'                 
012824              DELIMITED BY SIZE INTO SSA1                                 
012825     MOVE 'WDK611  '               TO SSA2                                
012826     MOVE '  '                     TO GOOD-STATUSCODES                    
012827     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
012828     MOVE WDK6-STATUS-CODE         TO STATUS-WS                           
012829     PERFORM  IMS-STATUSCHECK                                             
012830     .                                                                    
012831                                                                          
012832 IMS-REPL-WDK611 SECTION.                                                 
012833     MOVE 'IMS-REPL-WDK611     '   TO CURRENT-IMS-SECTION                 
012834                                                                          
012835     MOVE '  '                     TO GOOD-STATUSCODES                    
012836     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
012837     MOVE WDK6-STATUS-CODE         TO STATUS-WS                           
012838     PERFORM IMS-STATUSCHECK                                              
012839     .                                                                    
012840                                                                          
012841 IMS-GHU-WDK711 SECTION.                                                  
012842     MOVE 'IMS-GHU-WDK711      '   TO CURRENT-IMS-SECTION                 
012843                                                                          
012844     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-K7-X ')'                      
012845              DELIMITED BY SIZE INTO SSA1                                 
012846     STRING 'WDK711  (IDDC     =' W-IDDC-K7 ')'                           
012847          DELIMITED BY SIZE INTO SSA2                                     
012848     MOVE '  '                     TO GOOD-STATUSCODES                    
012849     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
012850     MOVE WDK7-STATUS-CODE         TO STATUS-WS                           
012851     PERFORM  IMS-STATUSCHECK                                             
012852     .                                                                    
012853                                                                          
012854 IMS-REPL-WDK711 SECTION.                                                 
012855     MOVE 'IMS-REPL-WDK711     '   TO CURRENT-IMS-SECTION                 
012856                                                                          
012857     MOVE '  '                     TO GOOD-STATUSCODES                    
012858     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
012859     MOVE WDK7-STATUS-CODE         TO STATUS-WS                           
012860     PERFORM  IMS-STATUSCHECK                                             
012861     .                                                                    
012862                                                                          
012895 IMS-GU-WDQ201 SECTION.                                                   
012896     MOVE 'IMS-GU-WDQ201       '   TO CURRENT-IMS-SECTION                 
012897                                                                          
012898     STRING 'WDQ201  (WDQ2CSEQ =' W-IDGMTREF-X ')'                        
012899          DELIMITED BY SIZE INTO SSA1                                     
012900     MOVE '  '                     TO GOOD-STATUSCODES                    
012901     CALL CBLTDLI USING GU WDQ2-PCB DLI-IO-WDQ201 SSA1                    
012902     MOVE WDQ2-STATUS-CODE         TO STATUS-WS                           
012903     PERFORM IMS-STATUSCHECK                                              
012904     .                                                                    
012905                                                                          
012906 IMS-ISRT-WDQ101 SECTION.                                                 
012907     MOVE 'IMS-ISRT-WDQ101     '   TO CURRENT-IMS-SECTION                 
012908                                                                          
012909     MOVE 'WDQ101   '              TO SSA1                                
012910     MOVE '  II'                   TO GOOD-STATUSCODES                    
012911     CALL CBLTDLI USING ISRT WDQ1-PCB DLI-IO-WDQ101 SSA1                  
012912     MOVE WDQ1-STATUS-CODE         TO STATUS-WS                           
012913     PERFORM IMS-STATUSCHECK                                              
012914     .                                                                    
012915                                                                          
012916 IMS-ISRT-WDG6 SECTION.                                                   
012917     MOVE 'IMS-ISRT-WDG6       '   TO CURRENT-IMS-SECTION                 
012918                                                                          
012919     MOVE 'WDG601  '               TO SSA1                                
012920     MOVE '  II'                   TO GOOD-STATUSCODES                    
012921     CALL CBLTDLI USING ISRT WDG6-PCB DLI-IO-WDG601 SSA1                  
012922     MOVE WDG6-STATUS-CODE         TO STATUS-WS                           
012923     PERFORM IMS-STATUSCHECK                                              
012924     .                                                                    
012925                                                                          
012926 IMS-STATUSCHECK  SECTION.                                                
012927     MOVE 'IMS-STATUSCHECK     '   TO CURRENT-IMS-SECTION                 
012928                                                                          
012929     SET STATUS-IX                 TO 1                                   
012930     SEARCH GOOD-STATUS                                                   
012931       AT END                                                             
012932         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
012933           DELIMITED BY SIZE INTO ERROR-TEXT-STR                          
012934         DISPLAY ERROR-TEXT                                               
012940         CALL FELLOG                                                      
013000       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
013100         CONTINUE                                                         
013200     END-SEARCH                                                           
013300     .                                                                    
013400*    -COPY WY2000P1                                                       
