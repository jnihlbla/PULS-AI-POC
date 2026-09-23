000100 01  XLS-WB1103-RAD.                                                      
000200*                                 TABLE LINE IN XLS FILE                  
000300     03 XLS-IDARTNR          PIC Z(7)9.                                   
000400*                                 PART NUMBER                             
000500     03 XLS-TAB01            PIC X.                                       
000600     03 XLS-KDANNULL         PIC X.                                       
000700*                                 CANCELLATION CODE                       
000800     03 XLS-TAB02            PIC X.                                       
000900     03 XLS-KDARTTYP         PIC X.                                       
001000*                                 TYPE OF PART                            
001100     03 XLS-TAB03            PIC X.                                       
001200     03 XLS-TEARTUTFG        PIC X(8).                                    
001300*                                 PART DESIGN VALIDTY KDP                 
001400     03 XLS-TAB04            PIC X.                                       
001500     03 XLS-BEART            PIC X(25).                                   
001600*                                 PART DESCRIPTION                        
001700     03 XLS-TAB05            PIC X.                                       
001800     03 XLS-TENOTE           PIC X(40).                                   
001900*                                 NOTE FIELD                              
002000     03 XLS-TAB06            PIC X.                                       
002100     03 XLS-IDARTNR-OFARG    PIC Z(7)9.                                   
002200     03 XLS-TAB07            PIC X.                                       
002300     03 XLS-KDFARGST         PIC X.                                       
002400*                                 COLOUR STATUS                           
002500     03 XLS-TAB08            PIC X.                                       
002600     03 XLS-IDPSLAG          PIC X(2).                                    
002700*                                 PSLAG INTENTITY KDP                     
002800     03 XLS-TAB09            PIC X.                                       
002900     03 XLS-BETYP            PIC X(8).                                    
003000*                                 KDP TYPE                                
003100     03 XLS-TAB10            PIC X.                                       
003200     03 XLS-IDFKNGRP         PIC Z(3)9.                                   
003300*                                 FUNCTION GROUP                          
003400     03 XLS-TAB11            PIC X.                                       
003500     03 XLS-IDKDPPOS         PIC 9(3).                                    
003600*                                 PART GROUP POS IN KDP                   
003700     03 XLS-TAB12            PIC X.                                       
003800     03 XLS-IDAOT            PIC X(6).                                    
003900*                                 CONSTRUCTION ORDER T                    
004000     03 XLS-TAB13            PIC X.                                       
004100     03 XLS-TIAOINF-AAVV     PIC Z(4).                                    
004200*                                 CO INTRODUCTION WEEK                    
004300     03 XLS-TAB14            PIC X.                                       
004400     03 XLS-IDAOTUTG         PIC 9(2).                                    
004500*                                 CONSTRUCT.ORDER -T ISSUE                
004600     03 XLS-TAB15            PIC X.                                       
004700     03 XLS-IDUPPDKU         PIC X(8).                                    
004800*                                 KU COMMISSION IDENTITY NO               
004900     03 XLS-TAB16            PIC X.                                       
005000     03 XLS-TESTATUPP        PIC X(6).                                    
005100*                                 ASSIGNMENT NO. STATUS                   
005200     03 XLS-TAB17            PIC X.                                       
005300     03 XLS-BEANST-KU        PIC X(25).                                   
005400*                                 NAME OF KU RESPONSIBLE                  
005500     03 XLS-TAB18            PIC X.                                       
005600     03 XLS-BEASSTYP         PIC X(7).                                    
005700*                                 ACCESSORY ASSIGNMENT TYPE               
005800     03 XLS-TAB19            PIC X.                                       
005900     03 XLS-IDUPPDSU         PIC X(8).                                    
006000*                                 SU COMMISSION IDENTITY NO               
006100     03 XLS-TAB20            PIC X.                                       
006200     03 XLS-BEUPPDSU         PIC X(35).                                   
006300*                                 SU COMMISSION DESCRIPTION               
006400     03 XLS-TAB21            PIC X.                                       
006500     03 XLS-BEANST-SU        PIC X(25).                                   
006600*                                 NAME OF SU RESPONSIBLE                  
006700     03 XLS-TAB22            PIC X.                                       
006800     03 XLS-IDPROJK          PIC X(4).                                    
006900*                                 PROJECT IDENTITY KONSTRUCTION           
007000     03 XLS-TAB23            PIC X.                                       
007100     03 XLS-IDPSS            PIC X(5).                                    
007200*                                 PSS INTENTITY KDP                       
007300     03 XLS-TAB24            PIC X.                                       
007400     03 XLS-FLTPDWKPH1       PIC X.                                       
007500*                                 GENERAL FLAG                            
007600     03 XLS-TAB25            PIC X.                                       
007700     03 XLS-FLRPULS          PIC X.                                       
007800*                                 PARTNO REG IN PULS                      
007900     03 XLS-TAB26            PIC X.                                       
008000     03 XLS-KDFRPTYP         PIC X.                                       
008100*                                 TYPE OF PACKAGE                         
008200     03 XLS-TAB27            PIC X.                                       
008300     03 XLS-KDEMBKOD-2       PIC Z(2)9.                                   
008400     03 XLS-TAB28            PIC X.                                       
008500     03 XLS-BETEXT-OTP       PIC X(5).                                    
008600*                                 ORDER TO PURCHASE, ACCESORIES           
008700     03 XLS-TAB29            PIC X.                                       
008800     03 XLS-TIAVTAL          PIC Z(4)9.                                   
008900*                                 YEAR - WEEK  (YYWW)                     
009000     03 XLS-TAB30            PIC X.                                       
009100     03 XLS-KDLEVPST         PIC X.                                       
009200*                                 DELIVERY SCHEDULE STATUS, ACC.          
009300     03 XLS-TAB31            PIC X.                                       
009400     03 XLS-TILEVBSK         PIC 9(7).                                    
009500     03 XLS-TAB32            PIC X.                                       
009600     03 XLS-KDMDS            PIC X.                                       
009700*                                 MATERIAL DATA CODE                      
009800     03 XLS-TAB33            PIC X.                                       
009900     03 XLS-VKART-KDP        PIC 9(8).                                    
010000*                                 PART WEIGHT KDP (G)                     
010100     03 XLS-TAB34            PIC X.                                       
010200     03 XLS-TITPD-AAVV       PIC 9(4).                                    
010300*                                 TEMP PRODUCTION DEVIATION WEEK          
010400     03 XLS-TAB35            PIC X.                                       
010500     03 XLS-KDTPD            PIC X.                                       
010600*                                 TEMP PRODUCION DEVIATION CODE           
010700     03 XLS-TAB36            PIC X.                                       
010800     03 XLS-DAPSWQP-1        PIC 9(6).                                    
010900*                                 PSW-P-QUAL-PHASE1(YYYYWW)               
011000     03 XLS-TAB37            PIC X.                                       
011100     03 XLS-KDPSWQP-1        PIC X.                                       
011200*                                 STATUS CODE PLAN QUAL PH1               
011300     03 XLS-TAB38            PIC X.                                       
011400     03 XLS-DAPSWPP-2        PIC 9(6).                                    
011500*                                 PSW-P-PROD-PHASE2(YYYYWW)               
011600     03 XLS-TAB39            PIC X.                                       
011700     03 XLS-KDPSWPP-2        PIC X.                                       
011800*                                 STATUS CODE PLAN PROD PH2               
011900     03 XLS-TAB40            PIC X.                                       
012000     03 XLS-DAPSWCP-3        PIC 9(6).                                    
012100*                                 PSW-P-CAP-PHASE3 (YYYYWW)               
012200     03 XLS-TAB41            PIC X.                                       
012300     03 XLS-KDPSWCP-3        PIC X.                                       
012400*                                 STATUS CODE PLAN CAP PH3                
012500     03 XLS-TAB42            PIC X.                                       
012600     03 XLS-DAPSWQA-1        PIC 9(6).                                    
012700*                                 PSW-A-QUAL-PHASE1(YYYYWW)               
012800     03 XLS-TAB43            PIC X.                                       
012900     03 XLS-KDPSWQA-1        PIC X.                                       
013000*                                 STATUS CODE ACT QUAL PH1                
013100     03 XLS-TAB44            PIC X.                                       
013200     03 XLS-DAPSWPA-2        PIC 9(6).                                    
013300*                                 PSW-A-PROD-PHASE2(YYYYWW)               
013400     03 XLS-TAB45            PIC X.                                       
013500     03 XLS-KDPSWPA-2        PIC X.                                       
013600*                                 STATUS CODE ACT PROD PH2                
013700     03 XLS-TAB46            PIC X.                                       
013800     03 XLS-DAPSWCA-3        PIC 9(6).                                    
013900*                                 PSW-A-CAP-PHASE3 (YYYYWW)               
014000     03 XLS-TAB47            PIC X.                                       
014100     03 XLS-KDPSWCA-3        PIC X.                                       
014200*                                 STATUS CODE ACT CAP PH3                 
014300     03 XLS-TAB48            PIC X.                                       
014400     03 XLS-FLPULSPR         PIC X.                                       
014500*                                 FLAG TO TELL IF THERE EXIST A P         
014600*                                 ULS PRICE                               
014700     03 XLS-TAB49            PIC X.                                       
014800     03 XLS-TIAVIDAT         PIC 9(6).                                    
014900*                                 ADVICE NOTE DATE                        
015000     03 XLS-TAB50            PIC X.                                       
015100     03 XLS-KDSIGN           PIC X.                                       
015200*                                 DATA SIGN                               
015300     03 XLS-SULEVANT         PIC Z(8)9.                                   
015400*                                 SUMMARY DELIVERED OF AN ITEM            
015500     03 XLS-TAB51            PIC X.                                       
015600     03 XLS-KVLS             PIC -(7)9.                                   
015700*                                 STOCK BALANCE                           
015800     03 XLS-TAB52            PIC X.                                       
015900     03 XLS-IDINK            PIC X(4).                                    
016000*                                 PURCHASE IDENTIFICATION NUMBER          
016100     03 XLS-TAB53            PIC X.                                       
016200     03 XLS-IDANSK           PIC Z(2)9.                                   
016300*                                 PROCURER NO.                            
016400     03 XLS-TAB54            PIC X.                                       
016500     03 XLS-IDSTEKN          PIC X(8).                                    
016600*                                 QA TECHNICIAN                           
016700     03 XLS-TAB55            PIC X.                                       
016800     03 XLS-IDLEVNR          PIC X(5).                                    
016900*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
017000     03 XLS-TAB56            PIC X.                                       
017100     03 XLS-IDPRODGR         PIC X(4).                                    
017200*                                 PRODUCT GROUP MANAGER                   
017300     03 XLS-TAB57            PIC X.                                       
017400     03 XLS-KVYVOL-INT       PIC Z(5)9.                                   
017500*                                 DECIDED YR VOL TO LAUNCH                
017600     03 XLS-TAB58            PIC X.                                       
017700     03 XLS-KVYVOL-B3        PIC Z(5)9.                                   
017800*                                 ADJ.OF YEAR VOLUME BOARD 3              
017900     03 XLS-TAB59            PIC X.                                       
018000     03 XLS-KVYVOL-B2        PIC Z(5)9.                                   
018100*                                 ADJ. OF YEAR VOLUME BOARD 2             
018200     03 XLS-TAB60            PIC X.                                       
018300     03 XLS-KVYVOL-B1        PIC Z(5)9.                                   
018400*                                 ADJUSTED YEAR VOLUME BOARD 1            
018500     03 XLS-TAB61            PIC X.                                       
018600     03 XLS-KVYVOL-ASS       PIC Z(5)9.                                   
018700*                                 ASSIGNED YEAR VOLUME                    
018800     03 XLS-TAB62            PIC X.                                       
018900     03 XLS-BEMAPP           PIC X(5).                                    
019000*                                 BINDER                                  
019100     03 XLS-TAB63            PIC X.                                       
019200     03 XLS-KVFOTO           PIC Z9.                                      
019300*                                 NUMBER OF PHOTOS                        
019400     03 XLS-TAB64            PIC X.                                       
019500     03 XLS-TIFOTO           PIC Z(4).                                    
019600*                                 PHOTO MTRL WEEK                         
019700     03 XLS-TAB65            PIC X.                                       
019800     03 XLS-TEVERKTYG        PIC X(4).                                    
019900     03 XLS-TAB66            PIC X.                                       
020000     03 XLS-TESTATXT         PIC X(25).                                   
020100     03 XLS-TAB67            PIC X.                                       
020200     03 XLS-TEMATXT          PIC X(25).                                   
020300     03 XLS-TAB68            PIC X.                                       
020400     03 XLS-TEINKTXT         PIC X(25).                                   
020500     03 XLS-TAB69            PIC X.                                       
020600     03 XLS-TEANSTXT         PIC X(25).                                   
020700     03 XLS-TAB70            PIC X.                                       
020800     03 XLS-TEAUXTXT         PIC X(25).                                   
020900     03 XLS-TAB71            PIC X.                                       
021000*** END OF VILMAII-COPY LENGTH= 614 BYTES                                 
