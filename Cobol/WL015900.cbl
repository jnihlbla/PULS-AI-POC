000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     WL015900.                                                
000400 AUTHOR.         TAPAS KUMAR GHOSH.                                       
000500 DATE-WRITTEN.   2004/09/28.                                              
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        VISAR RETURTILLSTÅNDSKÖ, ÄVEN FÖR RETURTERMINALER.               
001000*        ANVÄNDS FÖR BORTTAG AV REGISTRERADE RETURTILLSTÅND.              
001100*                                                                         
001200*        PROGRAMMET UPPDATERAR WLRETA (WDA3)                              
001300*        PROGRAMMET LÄSER      WLRETE (WDA3)                              
001400*        PROGRAMMET LÄSER              WDA3H1                             
001500*        PROGRAMMET LÄSER              WDA3I1                             
001600*        PROGRAMMET LÄSER              WDA2                               
001700*                                                                         
001800*        WL015900 PROGRAM IS A REPLICA OF W4074400 PROGRAM                
001900*        AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS                          
002000*                                                                         
002100*                                                                         
002200*    ADDRESS: 'CARPARTS.LDC.EXTQUEUERETPERMITS'                           
002300*                                                                         
002400*    INDATA.                                                              
002500*        TRANSAKTION: WL0159U                                             
002600*        REQUEST:     WZ01REQU                                            
002700*                     WL0159I1                                            
002800*                                                                         
002900*    UTDATA.                                                              
003000*        RESPONSE:    WZ01RESP                                            
003100*                     WL0159O1                                            
003200*                                                                         
003300*    E-TRACKER: 4230251 2007-03-08 SUSANNE OLSSON                         
003400*                                                                         
003500                                                                          
003600     SKIP3                                                                
003700 ENVIRONMENT DIVISION.                                                    
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000 WORKING-STORAGE SECTION.                                                 
004100                                                                          
004200*    -- CHECKED BY WY2000                                                 
004300 77  IDPGM                       PIC X(08)   VALUE 'WL015900'.            
004400                                                                          
004500*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004600 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004700 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004800 77  KDRC-DISPLAY                PIC Z(5)   VALUE ZERO.                   
004900                                                                          
005000 77  JA                          PIC X       VALUE 'J'.                   
005100 77  YES                         PIC X       VALUE 'Y'.                   
005200 77  NEJ                         PIC X       VALUE 'N'.                   
005300                                                                          
005400*    --- INDEX FÖR BLÄDDRINGSRADER                                        
005500 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
005600 77  D-INDX                      PIC S9(4)  VALUE +0    COMP SYNC.        
005610 77  S-INDX                      PIC S9(4)  VALUE +0    COMP SYNC.        
005700 77  WS-INDX-REC                 PIC S9(4)  VALUE +0    COMP SYNC.        
005800 77  WS-COUNT                    PIC S9(4)  VALUE +0    COMP SYNC.        
005810 77  WS-COUNT-LINES              PIC S9(4)  VALUE +0    COMP SYNC.        
005900 77  WS-PRINT-CNT                PIC S9(4)  VALUE +0    COMP SYNC.        
006000 77  MAX-INDX                    PIC S9(4)  VALUE +500  COMP SYNC.        
006010 77  WS-MAX-S-INDX               PIC S9(4)  VALUE +500  COMP SYNC.        
006020 77  WS-COUNT-IDKOLLI            PIC S9(4)  VALUE +0    COMP SYNC.        
006100*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
006200 77  W-VK-DEC-COUNT              PIC  9(1)  VALUE ZERO.                   
006210 77  W-VK-NUM-COUNT              PIC  9(1)  VALUE ZERO.                   
006220 77  WS-IDDISTR                  PIC  X(4)  VALUE SPACE.                  
006300 77  WS-IDKUNDNR                 PIC  X(6)  VALUE SPACE.                  
006400 77  WS-KDRETSTA                 PIC  X(1)  VALUE SPACE.                  
006500 77  WS-CDC-SE                   PIC  X(2)  VALUE '11'.                   
006600                                                                          
006700 77  WS-IDELMT-ERROR             PIC X(16).                               
006800 77  WS-IDMSG-ERROR              PIC X(03).                               
006900 77  WS-IDMSG-INFO               PIC X(03).                               
007000                                                                          
007100 77  WS-VKART                   PIC S9(7) VALUE ZERO COMP-3.              
007200 77  WS-VKART-UNIT              PIC S9(7)V9(3) VALUE ZERO COMP-3.         
007300 77  WS-VKART-TOT               PIC S9(7)V9(2) VALUE ZERO COMP-3.         
007400 77  WS-SUVKART-TOT             PIC S9(7)V9(2) VALUE ZERO COMP-3.         
007500 77  WS-SUFKTUTL-TOT            PIC S9(7)V9(2) VALUE ZERO COMP-3.         
007600 77  WS-PRFKTUTL-ROW            PIC S9(7)V9(2) VALUE ZERO COMP-3.         
007700 77  WS-PRFKTUTL                PIC S9(7)V9(2) VALUE ZERO COMP-3.         
007800 77  W-SND-REG                   PIC  X(1)  VALUE '1'.                    
007900 77  W-SPAR-IDDISTR              PIC S9(5)  VALUE ZERO COMP-3.            
008000 01  W-SPAR-VKORDBTO             PIC X(8)  VALUE SPACE.                   
008010 01 FILLER REDEFINES W-SPAR-VKORDBTO.                                     
008020     03 W-SPAR-VKORDBTO1         PIC X(1).                                
008030     03 W-SPAR-VKORDBTO2         PIC X(1).                                
008040     03 W-SPAR-VKORDBTO3         PIC X(1).                                
008050     03 W-SPAR-VKORDBTO4         PIC X(1).                                
008060     03 W-SPAR-VKORDBTO5         PIC X(1).                                
008070     03 W-SPAR-VKORDBTO6         PIC X(1).                                
008080     03 W-SPAR-VKORDBTO7         PIC X(1).                                
008090     03 W-SPAR-VKORDBTO8         PIC X(1).                                
008100 01  W-SPAR-VLORDBTO             PIC X(8)  VALUE SPACE.                   
008110 01 FILLER REDEFINES W-SPAR-VLORDBTO.                                     
008120     03 W-SPAR-VLORDBTO1         PIC X(1).                                
008130     03 W-SPAR-VLORDBTO2         PIC X(1).                                
008140     03 W-SPAR-VLORDBTO3         PIC X(1).                                
008150     03 W-SPAR-VLORDBTO4         PIC X(1).                                
008160     03 W-SPAR-VLORDBTO5         PIC X(1).                                
008170     03 W-SPAR-VLORDBTO6         PIC X(1).                                
008180     03 W-SPAR-VLORDBTO7         PIC X(1).                                
008190     03 W-SPAR-VLORDBTO8         PIC X(1).                                
008200 77  W-SPAR-IDKUNDNR             PIC S9(7)  VALUE ZERO COMP-3.            
008210 77  W-IDKOLLI                   PIC 9(5)   VALUE ZERO.                   
008220 77  WS-FIRST-IDKOLLI            PIC 9(5)   VALUE ZERO.                   
008300 77  W-SPAR-IDRAPPNR             PIC  9(7)  VALUE ZERO.                   
008400 77  W-SPAR-IDRT                 PIC  X(3)  VALUE SPACE.                  
008500 77  W-SPAR-IDRTLOP              PIC  9(3)  VALUE ZERO.                   
008600 77  W-KDRETSTA-NUM              PIC  X(1)  VALUE ZERO.                   
008700 77  W-KDRETSTA                  PIC X       VALUE 'T'.                   
008800     88  W-TERM                              VALUE 'T'.                   
008900     88  W-SAENT                             VALUE 'S'.                   
009000     88  W-TRANSIT                           VALUE 'I'.                   
009001                                                                          
009002 77  FIRST-WDA3F-SW              PIC X       VALUE 'J'.                   
009003     88  FIRST-WDA3F                         VALUE 'J'.                   
009004     88  NOT-FIRST-WDA3F                     VALUE 'N'.                   
009005                                                                          
009006                                                                          
009300 77  INDATA-SW                   PIC X       VALUE 'J'.                   
009400     88  INDATA-OK                           VALUE 'J'.                   
009500     88  INDATA-FEL                          VALUE 'N'.                   
009600                                                                          
009700 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
009800     88  NYCKLAR-OK                          VALUE 'J'.                   
009900     88  NYCKLAR-FEL                         VALUE 'N'.                   
010000                                                                          
010100 77  WS-REC-LIMIT                PIC X       VALUE 'N'.                   
010200     88  REC-LIMIT                           VALUE 'J'.                   
010300                                                                          
010400 77  WS-FL-DELET                 PIC X       VALUE 'N'.                   
010500     88  FL-DELET                            VALUE 'J'.                   
010600                                                                          
010700 77  WS-FL-PRINT                 PIC X       VALUE 'N'.                   
010800     88  FL-PRINT                            VALUE 'J'.                   
010900                                                                          
011000 77  W-UPDATE-SW                 PIC X       VALUE 'N'.                   
011100     88  W-UPDATE-OK                         VALUE 'J'.                   
011200                                                                          
011300 77  IDDISTR-SW                  PIC X       VALUE 'N'.                   
011400     88  IDDISTR-VAL                         VALUE 'J'.                   
011500                                                                          
011600 77  IDKUNDNR-SW                 PIC X       VALUE 'N'.                   
011700     88  IDKUNDNR-VAL                        VALUE 'J'.                   
011800                                                                          
011900 77  FIRST-SW                   PIC X       VALUE 'Y'.                    
012000     88  FIRST-LINE                         VALUE 'Y'.                    
152034     EJECT                                                                
152035                                                                          
152036 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
152037 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
152038 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
152039     EJECT                                                                
152040                                                                          
152050 01  DAGENS-DATUM.                                                        
183011     03  DAGENS-AA               PIC 9(2)  VALUE ZERO.                    
183030     03  DAGENS-MM               PIC 9(2)  VALUE ZERO.                    
183031     03  DAGENS-DD               PIC 9(2)  VALUE ZERO.                    
183032                                                                          
183033 01  W-DATE-AAMM                 PIC 9(4)  VALUE ZERO.                    
183036                                                                          
183037 01  W-CHECK1.                                                            
183038     05  W-IDDISTR-CH1           PIC S9(5)   VALUE ZERO COMP-3.           
183039     05  W-IDKUNDNR-CH1          PIC S9(7)   VALUE ZERO COMP-3.           
183040     05  W-IDRAPPNR-CH1          PIC  9(7)   VALUE ZERO.                  
183041                                                                          
183042 01  W-CHECK2.                                                            
183043     05  W-IDDISTR-CH2           PIC S9(5)   VALUE ZERO COMP-3.           
183044     05  W-IDKUNDNR-CH2          PIC S9(7)   VALUE ZERO COMP-3.           
183045     05  W-IDRAPPNR-CH2          PIC  9(7)   VALUE ZERO.                  
183046                                                                          
183047                                                                          
183048 01  WS-CURRENT-DATE             PIC 9(8)    VALUE ZERO.                  
183049 01  FILLER REDEFINES WS-CURRENT-DATE.                                    
183050     03  WS-CURRENT-DATE-YY      PIC 9(2).                                
183051     03  WS-CURRENT-DATE-YYMMDD  PIC 9(6).                                
183052 01  WS-CURRENT-TIME             PIC 9(8)    VALUE ZERO.                  
183053                                                                          
183054 01  WS-CURRENT-DATE-TIME.                                                
183055     03  WS-YEAR                 PIC 9(4).                                
183056     03  WS-MONTH                PIC 9(2).                                
183057     03  WS-DAY                  PIC 9(2).                                
183058     03  WS-HOUR                 PIC 9(2).                                
183059     03  WS-MINUTE               PIC 9(2).                                
183060 01  FILLER REDEFINES WS-CURRENT-DATE-TIME.                               
183061     03  FILLER                  PIC X(2).                                
183062     03  WS-TIYYMMDDHHMM         PIC X(10).                               
183063                                                                          
183064*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
183065 01  GENERELLA-SUBPROGRAM.                                                
183066     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
183068     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
183069     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
183070     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
183071     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
183072     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
183074     03  W335CURR          PIC X(8)    VALUE 'W335CURR'.                  
183075     03  W510CURR          PIC X(8)    VALUE 'W510CURR'.                  
183084     03  WZ01CALL                PIC X(8)    VALUE 'WZ01CALL'.            
183085     EJECT                                                                
183086     SKIP3                                                                
183087*    --- LISTAN                                                           
183088 01  UT-RAD                     PIC X(130)  VALUE SPACE.                  
183089 01  RUBRIK-RAD.                                                          
183090     03  FILLER            PIC X(5)  VALUE 'VOLVO'.                       
183091     03  FILLER            PIC X(1)  VALUE SPACE.                         
183092     03  FILLER            PIC X(6)  VALUE 'RETURN'.                      
183093     03  FILLER            PIC X(1)  VALUE SPACE.                         
183094     03  FILLER            PIC X(8)  VALUE 'PROFORMA'.                    
183095     03  FILLER            PIC X(6)  VALUE SPACE.                         
183096     03  FILLER            PIC X(5)  VALUE 'DATE'.                        
183097     03  LIST-AAR          PIC X(2).                                      
183098     03  FILLER            PIC X(1) VALUE '/'.                            
183099     03  LIST-MAN          PIC X(2).                                      
183100     03  FILLER            PIC X(1) VALUE '/'.                            
183101     03  LIST-DAG          PIC X(2).                                      
183102     03  FILLER            PIC X(13) VALUE SPACE.                         
183103     03  FILLER            PIC X(15) VALUE 'WLO159-001'.                  
183104* ANTAL BYTES                                                             
183105                                                                          
183106 01  E-RUBRIK-RAD.                                                        
183107     03  FILLER            PIC X(5)  VALUE 'VOLVO'.                       
183108     03  FILLER            PIC X(1)  VALUE SPACE.                         
183109     03  FILLER            PIC X(6)  VALUE 'RETURN'.                      
183110     03  FILLER            PIC X(1)  VALUE SPACE.                         
183111     03  FILLER            PIC X(08) VALUE 'PROFORMA'.                    
183112     03  FILLER            PIC X(1)  VALUE ';'.                           
183113     03  FILLER            PIC X(1)  VALUE ';'.                           
183114     03  FILLER            PIC X(5)  VALUE 'DATE'.                        
183115     03  FILLER            PIC X(1)  VALUE ';'.                           
183116     03  E-LIST-AAR        PIC X(2).                                      
183117     03  FILLER            PIC X(1) VALUE '/'.                            
183118     03  E-LIST-MAN        PIC X(2).                                      
183119     03  FILLER            PIC X(1) VALUE '/'.                            
183120     03  E-LIST-DAG        PIC X(2).                                      
183121     03  FILLER            PIC X(1)  VALUE ';'.                           
183122     03  FILLER            PIC X(15) VALUE 'WL0159-001'.                  
183123     03  FILLER            PIC X(1)  VALUE ';'.                           
183124     03  FILLER            PIC X(10) VALUE 'RETURN ID:'.                  
183125     03  FILLER            PIC X(1)  VALUE ';'.                           
183126     03  E-LIST-IDRT       PIC X(4)  VALUE SPACE.                         
183130     03  E-LIST-IDRTLOP    PIC X(3)  VALUE SPACE.                         
183140     03  FILLER            PIC X(1)  VALUE ';'.                           
183200     03  FILLER            PIC X(1)  VALUE ';'.                           
183300     03  FILLER            PIC X(1)  VALUE ';'.                           
183400     03  FILLER            PIC X(1)  VALUE ';'.                           
184302* ANTAL BYTES                                                             
184303                                                                          
184304 01  U-RUBRIK-RAD1.                                                       
184305     03  FILLER            PIC X(10) VALUE 'SENDING DC'.                  
184306     03  FILLER            PIC X(34) VALUE SPACE.                         
184307     03  FILLER            PIC X(12) VALUE 'RECEIVING DC'.                
184308     03  FILLER            PIC X(12) VALUE SPACE.                         
184309     03  FILLER            PIC X(5) VALUE 'PAGE'.                         
184310     03  LIST-SIDA         PIC Z(1)9 VALUE ZERO.                          
184311* ANTAL BYTES                                                             
184312                                                                          
184313 01  E-U-RUBRIK-RAD1.                                                     
184314     03  FILLER            PIC X(10) VALUE 'SENDING DC'.                  
184315     03  FILLER            PIC X(1)  VALUE ';'.                           
184316     03  FILLER            PIC X(1)  VALUE ';'.                           
184317     03  FILLER            PIC X(1)  VALUE ';'.                           
184318     03  FILLER            PIC X(1)  VALUE ';'.                           
184319     03  FILLER            PIC X(12) VALUE 'RECEIVING DC'.                
184320     03  FILLER            PIC X(1)  VALUE ';'.                           
184321     03  FILLER            PIC X(1)  VALUE ';'.                           
184322     03  FILLER            PIC X(1)  VALUE ';'.                           
184323     03  FILLER            PIC X(1)  VALUE ';'.                           
184324     03  FILLER            PIC X(1)  VALUE ';'.                           
184325     03  FILLER            PIC X(1)  VALUE ';'.                           
184326* ANTAL BYTES                                                             
184327                                                                          
184328**01  WS-HEADER.                                                          
184329**   03  FLAG-HDF          PIC X(01)  VALUE 'H'.                          
184330**   03  LIST-SENDR-ADR1   PIC X(35)  VALUE SPACE.                        
184331**   03  LIST-RECIV-ADR1   PIC X(35)  VALUE SPACE.                        
184332**   03  LIST-SENDR-ADR2   PIC X(35)  VALUE SPACE.                        
184333**   03  LIST-RECIV-ADR2   PIC X(35)  VALUE SPACE.                        
184334**   03  LIST-SENDR-GATA   PIC X(35)  VALUE SPACE.                        
184335**   03  LIST-RECIV-GATA   PIC X(35)  VALUE SPACE.                        
184336**   03  LIST-SENDR-PADR   PIC X(35)  VALUE SPACE.                        
184337**   03  LIST-RECIV-PADR   PIC X(35)  VALUE SPACE.                        
184338**   03  LIST-SENDR-LAND   PIC X(35)  VALUE SPACE.                        
184339**   03  LIST-RECIV-LAND   PIC X(35)  VALUE SPACE.                        
184340                                                                          
184341                                                                          
184342 01  E-U-RUBRIK-RAD2.                                                     
184343     03  E-LIST-SENDR-ADR1 PIC X(35) VALUE SPACE.                         
184344     03  FILLER            PIC X(1)  VALUE ';'.                           
184345     03  FILLER            PIC X(1)  VALUE ';'.                           
184346     03  FILLER            PIC X(1)  VALUE ';'.                           
184347     03  FILLER            PIC X(1)  VALUE ';'.                           
184900     03  E-LIST-RECIV-ADR1 PIC X(35) VALUE SPACE.                         
184901     03  FILLER            PIC X(1)  VALUE ';'.                           
184902     03  FILLER            PIC X(1)  VALUE ';'.                           
184903     03  FILLER            PIC X(1)  VALUE ';'.                           
184904     03  FILLER            PIC X(1)  VALUE ';'.                           
184905     03  FILLER            PIC X(1)  VALUE ';'.                           
184906     03  FILLER            PIC X(1)  VALUE ';'.                           
184907* ANTAL BYTES                 116                                         
184908                                                                          
184909 01  E-U-RUBRIK-RAD3.                                                     
184910     03  E-LIST-SENDR-ADR2   PIC X(35) VALUE SPACE.                       
184911     03  FILLER              PIC X(1)  VALUE ';'.                         
184912     03  FILLER              PIC X(1)  VALUE ';'.                         
184913     03  FILLER              PIC X(1)  VALUE ';'.                         
184914     03  FILLER              PIC X(1)  VALUE ';'.                         
185000     03  E-LIST-RECIV-ADR2   PIC X(35) VALUE SPACE.                       
185001     03  FILLER              PIC X(1)  VALUE ';'.                         
185002     03  FILLER              PIC X(1)  VALUE ';'.                         
185003     03  FILLER              PIC X(1)  VALUE ';'.                         
185004     03  FILLER              PIC X(1)  VALUE ';'.                         
185005     03  FILLER              PIC X(1)  VALUE ';'.                         
185006     03  FILLER              PIC X(1)  VALUE ';'.                         
185007                                                                          
185008 01  E-U-RUBRIK-RAD4.                                                     
185009     03  E-LIST-SENDR-GATA   PIC X(35) VALUE SPACE.                       
185010     03  FILLER              PIC X(1)  VALUE ';'.                         
185011     03  FILLER              PIC X(1)  VALUE ';'.                         
185012     03  FILLER              PIC X(1)  VALUE ';'.                         
185013     03  FILLER              PIC X(1)  VALUE ';'.                         
185100     03  E-LIST-RECIV-GATA   PIC X(35) VALUE SPACE.                       
185101     03  FILLER              PIC X(1)  VALUE ';'.                         
185102     03  FILLER              PIC X(1)  VALUE ';'.                         
185103     03  FILLER              PIC X(1)  VALUE ';'.                         
185104     03  FILLER              PIC X(1)  VALUE ';'.                         
185105     03  FILLER              PIC X(1)  VALUE ';'.                         
185106     03  FILLER              PIC X(1)  VALUE ';'.                         
185107                                                                          
185108 01  E-U-RUBRIK-RAD5.                                                     
185109     03  E-LIST-SENDR-PADR   PIC X(35) VALUE SPACE.                       
185110     03  FILLER              PIC X(1)  VALUE ';'.                         
185111     03  FILLER              PIC X(1)  VALUE ';'.                         
185112     03  FILLER              PIC X(1)  VALUE ';'.                         
185113     03  FILLER              PIC X(1)  VALUE ';'.                         
185300     03  E-LIST-RECIV-PADR   PIC X(35) VALUE SPACE.                       
185301     03  FILLER              PIC X(1)  VALUE ';'.                         
185302     03  FILLER              PIC X(1)  VALUE ';'.                         
185303     03  FILLER              PIC X(1)  VALUE ';'.                         
185304     03  FILLER              PIC X(1)  VALUE ';'.                         
185305     03  FILLER              PIC X(1)  VALUE ';'.                         
185306     03  FILLER              PIC X(1)  VALUE ';'.                         
185307                                                                          
185308 01  E-U-RUBRIK-RAD6.                                                     
185700     03  E-LIST-SENDR-LAND   PIC X(35) VALUE SPACE.                       
185701     03  FILLER              PIC X(1)  VALUE ';'.                         
185702     03  FILLER              PIC X(1)  VALUE ';'.                         
185703     03  FILLER              PIC X(1)  VALUE ';'.                         
185704     03  FILLER              PIC X(1)  VALUE ';'.                         
185705     03  E-LIST-RECIV-LAND   PIC X(35) VALUE SPACE.                       
185706     03  FILLER              PIC X(1)  VALUE ';'.                         
185707     03  FILLER              PIC X(1)  VALUE ';'.                         
185708     03  FILLER              PIC X(1)  VALUE ';'.                         
185709     03  FILLER              PIC X(1)  VALUE ';'.                         
185710     03  FILLER              PIC X(1)  VALUE ';'.                         
185711     03  FILLER              PIC X(1)  VALUE ';'.                         
185712                                                                          
185713 01  U-RUBRIK-RAD6A.                                                      
185714     03  FILLER            PIC X(11) VALUE 'DC NUMBER:'.                  
185715     03  LIST-IDDC-SND     PIC X(2)  VALUE SPACE.                         
185716     03  FILLER            PIC X(3)  VALUE SPACE.                         
185717     03  FILLER            PIC X(8)  VALUE 'DISTR.NO'.                    
185718     03  FILLER            PIC X(1)  VALUE SPACE.                         
185719     03  LIST-IDDISTR-SND  PIC Z(4)9 VALUE ZERO.                          
185720     03  FILLER            PIC X(14) VALUE SPACE.                         
185721     03  FILLER            PIC X(10) VALUE 'DISTR.NO. '.                  
185722     03  LIST-IDDISTR      PIC Z(4)9 VALUE ZERO.                          
185723                                                                          
185724 01  E-U-RUBRIK-RAD6A.                                                    
185725     03  FILLER              PIC X(11) VALUE 'DC NUMBER:'.                
185726     03  E-LIST-IDDC-SND     PIC X(2)  VALUE SPACE.                       
185727     03  FILLER              PIC X(1)  VALUE ';'.                         
185728     03  FILLER              PIC X(8)  VALUE 'DISTR.NO'.                  
185729     03  FILLER              PIC X(1)  VALUE SPACE.                       
185730     03  E-LIST-IDDISTR-SND  PIC Z(3)9 VALUE ZERO.                        
185731     03  FILLER              PIC X(1)  VALUE ';'.                         
185732     03  FILLER              PIC X(1)  VALUE ';'.                         
185733     03  FILLER              PIC X(1)  VALUE ';'.                         
185734     03  FILLER              PIC X(10) VALUE 'DISTR.NO. '.                
185735     03  E-LIST-IDDISTR      PIC Z(4)9 VALUE ZERO.                        
185736     03  FILLER              PIC X(1)  VALUE ';'.                         
185737     03  FILLER              PIC X(1)  VALUE ';'.                         
185738     03  FILLER              PIC X(1)  VALUE ';'.                         
185739     03  FILLER              PIC X(1)  VALUE ';'.                         
185740     03  FILLER              PIC X(1)  VALUE ';'.                         
185741     03  FILLER              PIC X(1)  VALUE ';'.                         
185742                                                                          
185743 01  U-RUBRIK-LINJE.                                                      
185744     03  FILLER            PIC X(92) VALUE ALL '_'.                       
185745                                                                          
185746 01  E-U-RUBRIK-LINJE.                                                    
185747     03  FILLER            PIC X(10) VALUE ALL ';;;;;;;;;;'.              
185748                                                                          
185749 01  U-RUBRIK-RAD6C.                                                      
185750     03  FILLER            PIC X(3) VALUE SPACE.                          
185751     03  FILLER            PIC X(8)  VALUE '** RETUR'.                    
185752     03  FILLER            PIC X(19) VALUE 'N OF DEFECTIVE MATE'.         
185753     03  FILLER            PIC X(19) VALUE 'RIAL NO COMMERCIAL '.         
185754     03  FILLER            PIC X(8) VALUE 'VALUE **'.                     
185755                                                                          
185756 01  E-U-RUBRIK-RAD6C.                                                    
185757     03  FILLER            PIC X(8)  VALUE '** RETUR'.                    
185758     03  FILLER            PIC X(19) VALUE 'N OF DEFECTIVE MATE'.         
185759     03  FILLER            PIC X(19) VALUE 'RIAL NO COMMERCIAL '.         
185760     03  FILLER            PIC X(8) VALUE 'VALUE * '.                     
185761     03  FILLER            PIC X(10) VALUE ';;;;;;;;;;'.                  
185762                                                                          
185763**01  WS-FOOTER.                                                          
185764**   03  FLAG-HDF            PIC X(01)      VALUE 'F'.                    
185765**   03  LIST-SUVKART-TOT    PIC Z(7)9.9(2) VALUE ZERO.                   
185766**   03  LIST-UOM-TWEIGHT    PIC X(3)       VALUE SPACE.                  
185767**   03  LIST-GROSS-WEIGHT   PIC X(8)       VALUE SPACE.                  
185768**   03  LIST-UOM-WEIGHT     PIC X(3)       VALUE SPACE.                  
185769**   03  LIST-VLORDBTO-TOT   PIC X(8)       VALUE SPACE.                  
185770**   03  LIST-UOM-VOLUME     PIC X(9)       VALUE SPACE.                  
185771**   03  LIST-SUFKTUTL-TOT   PIC Z(7).9(2)  VALUE ZERO.                   
185772**   03  LIST-KDVALISO       PIC X(3).                                    
185773                                                                          
185774 01  E-SISTA-RADEN1.                                                      
185775     03  FILLER              PIC X(14)    VALUE 'NET WEIGHT '.            
186000     03  E-LIST-SUVKART-TOT  PIC Z(7)9.9(2) VALUE ZERO.                   
186100     03  E-LIST-UOM-TWEIGHT  PIC X(4)       VALUE SPACE.                  
187800     03  FILLER              PIC X(1)       VALUE ';'.                    
187801     03  FILLER              PIC X(1)       VALUE ';'.                    
187810     03  FILLER              PIC X(1)       VALUE ';'.                    
187900     03  FILLER              PIC X(1)       VALUE ';'.                    
187910     03  FILLER              PIC X(1)       VALUE ';'.                    
188000     03  FILLER              PIC X(1)       VALUE ';'.                    
188100     03  FILLER              PIC X(1)       VALUE ';'.                    
188200     03  FILLER              PIC X(1)       VALUE ';'.                    
188210     03  FILLER              PIC X(1)       VALUE ';'.                    
188211                                                                          
188212 01  E-SISTA-RADEN2.                                                      
188213     03  FILLER              PIC X(17)     VALUE 'GROSS WEIGHT '.         
188214     03  E-LIST-GROSS-WEIGHT PIC X(8)      VALUE SPACE.                   
188215     03  E-LIST-UOM-WEIGHT   PIC X(4)      VALUE SPACE.                   
188216     03  FILLER              PIC X(1)      VALUE ';'.                     
188217     03  FILLER              PIC X(1)      VALUE ';'.                     
188218     03  FILLER              PIC X(1)      VALUE ';'.                     
188219     03  FILLER              PIC X(1)      VALUE ';'.                     
188220     03  FILLER              PIC X(1)      VALUE ';'.                     
188221     03  FILLER              PIC X(1)      VALUE ';'.                     
188222     03  FILLER              PIC X(1)      VALUE ';'.                     
188223     03  FILLER              PIC X(1)      VALUE ';'.                     
188224     03  FILLER              PIC X(1)      VALUE ';'.                     
188225                                                                          
188226 01  E-SISTA-RADEN3.                                                      
188227     03  FILLER               PIC X(17)     VALUE 'TOTAL VOLUME '.        
188228     03  E-LIST-VLORDBTO-TOT  PIC X(8)      VALUE SPACE.                  
188229     03  E-LIST-UOM-VOLUME    PIC X(10)     VALUE SPACE.                  
188230     03  FILLER               PIC X(1)      VALUE ';'.                    
188231     03  FILLER               PIC X(1)      VALUE ';'.                    
188232     03  FILLER               PIC X(1)      VALUE ';'.                    
188233     03  FILLER               PIC X(1)      VALUE ';'.                    
188234     03  FILLER               PIC X(1)      VALUE ';'.                    
188235     03  FILLER               PIC X(1)      VALUE ';'.                    
188236     03  FILLER               PIC X(1)      VALUE ';'.                    
188237     03  FILLER               PIC X(1)      VALUE ';'.                    
188238     03  FILLER               PIC X(1)      VALUE ';'.                    
188239                                                                          
188240 01  E-SISTA-RADEN4.                                                      
188241     03  FILLER              PIC X(12)     VALUE 'TOTAL VALUE '.          
188242     03  E-LIST-SUFKTUTL-TOT PIC Z(6)9.9(2) VALUE ZERO.                   
188243     03  FILLER              PIC X(1)      VALUE SPACE.                   
188244     03  E-LIST-KDVALISO     PIC X(3).                                    
188245     03  FILLER              PIC X(1)      VALUE ';'.                     
188246     03  FILLER              PIC X(1)      VALUE ';'.                     
188247     03  FILLER              PIC X(1)      VALUE ';'.                     
188248     03  FILLER              PIC X(1)      VALUE ';'.                     
188249     03  FILLER              PIC X(1)      VALUE ';'.                     
188250     03  FILLER              PIC X(1)      VALUE ';'.                     
188251     03  FILLER              PIC X(1)      VALUE ';'.                     
188252     03  FILLER              PIC X(1)      VALUE ';'.                     
188253     03  FILLER              PIC X(1)      VALUE ';'.                     
188254                                                                          
188255 01  E-SISTA-RADEN5.                                                      
188256     03  FILLER              PIC X(12)     VALUE 'TOTAL CASES '.          
188257     03  E-LIST-IDKOLLI-TOT  PIC Z(4)9     VALUE ZERO.                    
188300     03  FILLER              PIC X(1)      VALUE ';'.                     
188400     03  FILLER              PIC X(1)      VALUE ';'.                     
188500     03  FILLER              PIC X(1)      VALUE ';'.                     
188510     03  FILLER              PIC X(1)      VALUE ';'.                     
188600     03  FILLER              PIC X(1)      VALUE ';'.                     
188700     03  FILLER              PIC X(1)      VALUE ';'.                     
188800     03  FILLER              PIC X(1)      VALUE ';'.                     
188900     03  FILLER              PIC X(1)      VALUE ';'.                     
189000     03  FILLER              PIC X(1)      VALUE ';'.                     
193400     03  FILLER              PIC X(1)      VALUE ';'.                     
193401     03  FILLER              PIC X(1)      VALUE ';'.                     
193402                                                                          
193403 01  E-SISTA-RADEN6.                                                      
193404     03  FILLER            PIC X(8)  VALUE 'SHIPPERS'.                    
193405     03  FILLER            PIC X(15) VALUE ' SIGNATURE AND '.             
193406     03  FILLER            PIC X(19) VALUE 'TITLE ____________ '.         
193407     03  FILLER            PIC X(20) VALUE '____________________'.        
199502     03  FILLER            PIC X(1)  VALUE ';'.                           
199602     03  FILLER            PIC X(1)  VALUE ';'.                           
199702     03  FILLER            PIC X(1)  VALUE ';'.                           
199802     03  FILLER            PIC X(1)  VALUE ';'.                           
199803     03  FILLER            PIC X(1)  VALUE ';'.                           
199804     03  FILLER            PIC X(1)  VALUE ';'.                           
199820     03  FILLER            PIC X(1)  VALUE ';'.                           
199830     03  FILLER            PIC X(1)  VALUE ';'.                           
199831     03  FILLER            PIC X(1)  VALUE ';'.                           
199832                                                                          
199833                                                                          
199834 01  U-RUBRIK-RAD7.                                                       
199835     03  FILLER            PIC X(9) VALUE '  PART NO'.                    
199836     03  FILLER            PIC X(3) VALUE SPACE.                          
199837     03  FILLER            PIC X(10) VALUE 'PART NAME ' .                 
199838     03  FILLER            PIC X(10) VALUE SPACE.                         
199839     03  FILLER            PIC X(1) VALUE SPACE.                          
199840     03  FILLER            PIC X(10) VALUE 'REPORT NO ' .                 
199841     03  FILLER            PIC X(2) VALUE SPACE.                          
199842     03  FILLER            PIC X(8) VALUE 'CASE NO ' .                    
199843     03  FILLER            PIC X(2) VALUE SPACE.                          
199844     03  FILLER            PIC X(5) VALUE 'Q.DEL' .                       
199845     03  FILLER            PIC X(4) VALUE SPACE.                          
199846     03  FILLER            PIC X(7) VALUE 'T-PRICE' .                     
199847     03  FILLER            PIC X(3) VALUE SPACE.                          
199848     03  FILLER            PIC X(3) VALUE 'COO'.                          
199849     03  FILLER            PIC X(6) VALUE SPACE.                          
199850     03  FILLER            PIC X(7) VALUE 'STAT.NO'.                      
199851     03  FILLER            PIC X(3) VALUE SPACE.                          
199852     03  FILLER            PIC X(8) VALUE 'T-WEIGHT'.                     
199853                                                                          
199854 01  E-U-RUBRIK-RAD7.                                                     
199855     03  FILLER            PIC X(9)  VALUE '  PART NO'.                   
199856     03  FILLER            PIC X(1)  VALUE ';'.                           
199857     03  FILLER            PIC X(10) VALUE 'PART NAME ' .                 
199858     03  FILLER            PIC X(1)  VALUE ';'.                           
199859     03  FILLER            PIC X(10) VALUE 'REPORT NO ' .                 
199860     03  FILLER            PIC X(1)  VALUE ';'.                           
199861     03  FILLER            PIC X(5)  VALUE 'Q.DEL' .                      
199862     03  FILLER            PIC X(1)  VALUE ';'.                           
199863     03  FILLER            PIC X(7)  VALUE 'T-PRICE' .                    
199864     03  FILLER            PIC X(1)  VALUE ';'.                           
199865     03  FILLER            PIC X(3)  VALUE 'COO'.                         
199866     03  FILLER            PIC X(1)  VALUE ';'.                           
199867     03  FILLER            PIC X(7)  VALUE 'STAT.NO'.                     
199868     03  FILLER            PIC X(1)  VALUE ';'.                           
199869     03  FILLER            PIC X(8)  VALUE 'T-WEIGHT'.                    
199870     03  FILLER            PIC X(1)  VALUE ';'.                           
199871     03  FILLER            PIC X(1)  VALUE ';'.                           
199872     03  FILLER            PIC X(1)  VALUE ';'.                           
199873                                                                          
199874  01  WS-SORT-TAB.                                                        
213502     03  WS-SOR-DATA  OCCURS 500 TIMES.                                   
213503         05  WS-IDKOLLI         PIC 9(5).                                 
219202  01  WS-DETAIL.                                                          
219203**   03  FLAG-HDF          PIC X(01)  VALUE 'D'.                          
219204     03  WS-DET-DATA  OCCURS 500 TIMES.                                   
219205         05  LISTA-IDARTNR      PIC Z(9).                                 
219206         05  LISTA-BEART        PIC X(25).                                
219207         05  LISTA-IDRAPPNR     PIC Z(6)9.                                
219208*        05  LISTA-IDKOLLI      PIC Z(4)9.                                
219209         05  LISTA-KVLEVART     PIC Z(6)9 VALUE ZERO.                     
219210         05  LISTA-PRFKTUTL-RAD PIC Z(6).9(2) VALUE ZERO.                 
219211         05  LISTA-KDARTURS     PIC X(2) VALUE SPACE.                     
219212         05  LISTA-IDSTATNR     PIC Z(8)9 VALUE ZERO.                     
219213         05  LISTA-VKART-TOT    PIC Z(6)9.9(2) VALUE ZERO.                
219214                                                                          
219215 01  E-U-RAD.                                                             
219216     03  E-LIST-IDARTNR      PIC Z(9).                                    
219217     03  FILLER              PIC X(1)  VALUE ';'.                         
219218     03  E-LIST-BEART        PIC X(20).                                   
219219     03  FILLER              PIC X(1)  VALUE ';'.                         
219220     03  E-LIST-IDRAPPNR     PIC Z(6)9 VALUE ZERO.                        
219221     03  FILLER              PIC X(1)  VALUE ';'.                         
219222     03  E-LIST-KVLEVART     PIC Z(6)9 VALUE ZERO.                        
219223     03  FILLER              PIC X(1)  VALUE ';'.                         
219224     03  E-LIST-PRFKTUTL-RAD PIC Z(5)9.9(2) VALUE ZERO.                   
219225     03  FILLER              PIC X(1)  VALUE ';'.                         
219226     03  E-LIST-KDARTURS     PIC X(3) VALUE SPACE.                        
219227     03  FILLER              PIC X(1)  VALUE ';'.                         
219228     03  E-LIST-IDSTATNR     PIC Z(8)9 VALUE ZERO.                        
219229     03  FILLER              PIC X(1)  VALUE ';'.                         
219230     03  E-LIST-VKART-TOT    PIC Z(6)9.9(2) VALUE ZERO.                   
219231     03  FILLER              PIC X(1)  VALUE ';'.                         
219232     03  FILLER              PIC X(1)  VALUE ';'.                         
219233     03  FILLER              PIC X(1)  VALUE ';'.                         
219234     EJECT                                                                
219235** COPY BOOKS FOR DAP HEADER/DETAILS/FOOTER **                            
228302 01  DOC-HEADER-START            PIC X(24)   VALUE                        
228303                                 'DOC-HEAD-START '.                       
228304 01  DOC-HEAD-AREA.                                                       
228305*03  -COPY WL01591                                                        
228306     EJECT                                                                
228502 01  DOC-LINE-START              PIC X(24)   VALUE                        
228503                                 'DOC-LINE-START '.                       
228504 01  DOC-LINE-AREA.                                                       
228505*03  -COPY WL01592                                                        
228506     EJECT                                                                
228507 01  DOC-FOOTER-START            PIC X(24)   VALUE                        
228508                                 'DOC-FOOT-START '.                       
228509 01  DOC-FOOT-AREA.                                                       
228510*03  -COPY WL01593                                                        
228511     EJECT                                                                
228512*01 -COPY WWDIST79                                                        
228513     EJECT                                                                
244402*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
244403*                                                                         
244404     EJECT                                                                
244405 01  FILLER                PIC X(16)   VALUE 'CALL-CONTROL'.              
244406     SKIP3                                                                
244407 01  -COPY WZ01CALL                                                       
244408     EJECT                                                                
244409 01  FILLER                      PIC X(16)   VALUE 'WZ01SUB '.            
244410*01  -COPY WZ01SUB                                                        
244411     EJECT                                                                
244412 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
244413*01  -COPY WZ01SEND                                                       
244414     EJECT                                                                
244415 01  HDR-AREA.                                                            
247502*    03 -COPY WZ01REQU -PRE HDR-                                          
247602*    03 -COPY WZ04HDR                                                     
247603     EJECT                                                                
247604 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
247605 01  REQU-AREA.                                                           
247606*    03  -COPY WZ01REQU                                                   
247607*    03  -COPY WL0159I1                                                   
247608     EJECT                                                                
247609 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
247610 01  RESP-AREA.                                                           
247611*    03  -COPY WZ01RESP                                                   
247612*    03  -COPY WL0159O1                                                   
247613     EJECT                                                                
247614 01  CALL-REQU-AREA.                                                      
247615*    03  -COPY WZ01REQU -PRE CALL-                                        
247616*    03  CALL-IDDISTR            PIC 9(4)                                 
247617                                                                          
247618     EJECT                                                                
247619 01  CALL-RESP-AREA.                                                      
247620*    03  -COPY WZ01RESP -PRE CALL-                                        
247621*    03  CALL-PRARTNTO           PIC S9(7)V9(5)  COMP-3 VALUE ZERO        
247622                                                                          
247623     EJECT                                                                
247624*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
247625*                                                                         
247626     EJECT                                                                
247630                                                                          
247633*01  -COPY W335CURR                                                       
247634     EJECT                                                                
247635                                                                          
247636*01  -COPY W510CURR                                                       
247637     EJECT                                                                
247638                                                                          
247639 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
247640     SKIP3                                                                
247641                                                                          
247642                                                                          
247643                                                                          
247644 01  NYCKLAR-TILL-DLI.                                                    
247645     03  W-IDRT                  PIC  X(3)          VALUE SPACE.          
247646     03  W-KDRETSTA1             PIC  9(1)          VALUE ZERO.           
247647     03  W-IDRTLOP               PIC  9(3)          VALUE ZERO.           
247648     03  W-WDA301KY-X.                                                    
247649         05  W-IDDC              PIC  X(2)          VALUE SPACE.          
247650         05  W-DAREGDAT          PIC  9(8)          VALUE ZERO.           
247651         05  W-TIKLOCK           PIC S9(9)   COMP-3 VALUE ZERO.           
247652                                                                          
247653     03  W-WDA3FSEQ-MIN-X.                                                
247654         05  W-IDDC-FSEQ-MIN     PIC  X(2)          VALUE SPACE.          
247655         05  W-IDDISTR-FSEQ-MIN  PIC S9(5)   COMP-3 VALUE ZERO.           
247656         05  W-IDKUNDNR-FSEQ-MIN PIC S9(7)   COMP-3 VALUE ZERO.           
247657         05  W-IDRAPPNR-FSEQ-MIN PIC  9(7)   VALUE ZERO.                  
247658                                                                          
247659     03  W-WDA3FSEQ-MAX-X.                                                
247660         05  W-IDDC-FSEQ-MAX     PIC  X(2)          VALUE SPACE.          
247661         05  W-IDDISTR-FSEQ-MAX  PIC S9(5)   COMP-3 VALUE ZERO.           
247662         05  W-IDKUNDNR-FSEQ-MAX PIC S9(7)   COMP-3 VALUE ZERO.           
247663         05  W-IDRAPPNR-FSEQ-MAX PIC  9(7)   VALUE ZERO.                  
247664                                                                          
247665     03  W-WDA3F1KY-MIN-X.                                                
247666         05  W-IDDC-F1-MIN       PIC  X(2)          VALUE SPACE.          
247667         05  W-IDDISTR-F1-MIN    PIC S9(5)   COMP-3 VALUE ZERO.           
247668         05  W-IDKUNDNR-F1-MIN   PIC S9(7)   COMP-3 VALUE ZERO.           
247669         05  W-IDRAPPNR-F1-MIN   PIC  9(7)   VALUE ZERO.                  
247670         05  W-IDRT-F1-MIN       PIC  X(3)          VALUE SPACE.          
247671         05  W-IDRTLOP-F1-MIN    PIC  9(3)          VALUE ZERO.           
247672         05  W-IDKOLLI-F1-MIN    PIC S9(5)   COMP-3 VALUE ZERO.           
247673         05  W-DAREGDAT-F1-MIN   PIC  9(8)          VALUE ZERO.           
247674         05  W-TIKLOCK-F1-MIN    PIC S9(9)   COMP-3 VALUE ZERO.           
247675                                                                          
247676     03  W-WDA3F1KY-MAX-X.                                                
247677         05  W-IDDC-F1-MAX       PIC  X(2)          VALUE SPACE.          
247678         05  W-IDDISTR-F1-MAX    PIC S9(5)   COMP-3 VALUE ZERO.           
247679         05  W-IDKUNDNR-F1-MAX   PIC S9(7)   COMP-3 VALUE ZERO.           
247680         05  W-IDRAPPNR-F1-MAX   PIC  9(7)   VALUE ZERO.                  
247681         05  W-IDRT-F1-MAX       PIC  X(3)          VALUE SPACE.          
247682         05  W-IDRTLOP-F1-MAX    PIC  9(3)          VALUE ZERO.           
247683         05  W-IDKOLLI-F1-MAX    PIC S9(5)   COMP-3 VALUE ZERO.           
247684         05  W-DAREGDAT-F1-MAX   PIC  9(8)          VALUE ZERO.           
247685         05  W-TIKLOCK-F1-MAX    PIC S9(9)   COMP-3 VALUE ZERO.           
247686                                                                          
247687     03  W-WDA3ISEQ-X.                                                    
247688         05  W-IDRT-TRANSIT-ISEQ PIC  X(3)          VALUE SPACE.          
247689         05  W-KDRETSTA-ISEQ     PIC  X(1)          VALUE SPACE.          
247690                                                                          
247691     03  W-WDA3HSEQ-X.                                                    
247692         05  W-IDRT-HSEQ         PIC  X(3)          VALUE SPACE.          
247693         05  W-KDRETSTA-HSEQ     PIC  X(1)          VALUE SPACE.          
247694                                                                          
247695     03  W-WDA3H1-MIN-X.                                                  
247696         05  W-IDRT-H1-MIN       PIC  X(3)          VALUE SPACE.          
247697         05  W-KDRETSTA-H1-MIN   PIC  X(1)          VALUE SPACE.          
247698         05  FILLER              PIC X(20)   VALUE LOW-VALUE.             
247699                                                                          
247700     03  W-WDA3H1-MAX-X.                                                  
247701         05  W-IDRT-H1-MAX       PIC  X(3)          VALUE SPACE.          
247702         05  W-KDRETSTA-H1-MAX   PIC  X(1)          VALUE SPACE.          
247703         05  FILLER              PIC X(20)   VALUE LOW-VALUE.             
247704                                                                          
247705     03  W-WDA3I1-MIN-X.                                                  
247706         05  W-IDRT-TRANSIT-I1-MIN  PIC  X(3)   VALUE SPACE.              
247707         05  W-KDRETSTA-I1-MIN      PIC  X(1)   VALUE SPACE.              
247708         05  FILLER              PIC X(20)   VALUE LOW-VALUE.             
247709                                                                          
247710     03  W-WDA3I1-MAX-X.                                                  
247711         05  W-IDRT-TRANSIT-I1-MAX  PIC  X(3)   VALUE SPACE.              
247712         05  W-KDRETSTA-I1-MAX      PIC  X(1)   VALUE SPACE.              
247713         05  FILLER              PIC X(20)   VALUE LOW-VALUE.             
247714                                                                          
247715     03  W-IDDISTR-A3-X.                                                  
247716         05  W-IDDISTR-A3        PIC S9(5)   COMP-3 VALUE ZERO.           
247717                                                                          
247718     03  W-IDKUNDNR-A3-X.                                                 
247719         05  W-IDKUNDNR-A3       PIC S9(7)   COMP-3 VALUE ZERO.           
247720                                                                          
247721     03  W-IDLEVANM-X.                                                    
247722         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
247723         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
247724         05  W-IDRAPPNR          PIC  9(7)   VALUE ZERO.                  
247725                                                                          
247726     03  W-WDB601-X.                                                      
247727         05  W-IDDC-WDB6     PIC X(2)    VALUE SPACE.                     
247728                                                                          
247729     03  W-KDSEGKEY-X.                                                    
247730         05  W-KDSEGKEY      PIC X(1)    VALUE '1'.                       
247731                                                                          
247732     03  W-IDARTNR-X.                                                     
247733         05  W-IDARTNR       PIC S9(9)   VALUE ZERO COMP-3.               
247734                                                                          
247735     03  W-IDDISTR-X.                                                     
247736         05  W-IDDISTR-2     PIC S9(5)   VALUE ZERO COMP-3.               
247737                                                                          
247738     03  W-IDSKYLT-X.                                                     
247739         05  W-IDSKYLT       PIC X(3)    VALUE 'GB'.                      
247740                                                                          
247741     03  W-IDHTYP-X.                                                      
247742         05  FILLER              PIC X(4)    VALUE '3101'.                
247743         05  W-IDDISTR-3         PIC 9(4)    VALUE ZERO.                  
247744         05  FILLER              PIC X(22)   VALUE LOW-VALUE.             
247745     SKIP2                                                                
247746*    --- STATUS-KOD FRÅN IMS                                              
247747 01  STATUS-WS                   PIC XX.                                  
247748     88  SEGMENT-FINNS                       VALUE '  '.                  
247749     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
247750     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
247751     88  BASEN-SLUT                          VALUE 'GB'.                  
247752     SKIP2                                                                
247753 01  GODK-STATUSKODER.                                                    
247754     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
247755     SKIP3                                                                
247756 01  SSA1                        PIC X(256).                              
247757 01  SSA2                        PIC X(256).                              
247758     EJECT                                                                
247759*    --- IMS FUNKTIONSKODER                                               
247760*01  -COPY W0003                                                          
247761     EJECT                                                                
247762*    ---  DLI INPUT-OUTPUT AREA                                           
247763 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
247764     SKIP3                                                                
247765 01  DLI-IO-AREA.                                                         
247766     03  IO-AREA                 PIC X(300)  VALUE SPACE.                 
247767     SKIP3                                                                
247768     03  WLRETC01 REDEFINES IO-AREA.                                      
247769*        05  -COPY WDA301                                                 
247770     EJECT                                                                
247771     03  WLRETC01 REDEFINES IO-AREA.                                      
247772*        05  -COPY WDA3F1                                                 
247773 01  FILLER               PIC X(16)   VALUE 'WDA201 AREA'.                
247774 01  DLI-IO-AREA-WDA201.                                                  
247775*    03  -COPY WDA201                                                     
247776     EJECT                                                                
247777 01  FILLER               PIC X(16)   VALUE 'WDA211 AREA'.                
247778 01  DLI-IO-AREA-WDA211.                                                  
247779*    03  -COPY WDA211                                                     
247780     EJECT                                                                
247781 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDA3H1'.         
247782 01  DLI-IO-WDA3H1.                                                       
247783*    03  -COPY WDA3H1                                                     
247784     EJECT                                                                
247785 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDA3I1'.         
247786 01  DLI-IO-WDA3I1.                                                       
247787*    03  -COPY WDA3I1                                                     
247788                                                                          
247789     EJECT                                                                
247790 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDB601'.                      
247791 01  DLI-IO-WDB601.                                                       
247792*    03  -COPY WDB601  -PRE WDB6-                                         
247793     EJECT                                                                
247794 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK611'.                      
247795 01  DLI-IO-WDK611.                                                       
247796*    03  -COPY WDK611  -PRE WDK611-                                       
247797     EJECT                                                                
247798 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD311'.                      
247799 01  DLI-IO-WDD311.                                                       
247800*    03  -COPY WDD311  -PRE WDD311-                                       
247801     EJECT                                                                
247802 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDGX3102'.                    
247803 01  DLI-IO-WDGX3102.                                                     
247804*    03  -COPY WDGX3102                                                   
247805     EJECT                                                                
247806 LINKAGE SECTION.                                                         
247807                                                                          
247808 01  MSG-PCB                     PIC X.                                   
247809*01  -COPY W0009   -PRE DISTRDOC-                                         
247810     05  FILLER                  PIC X.                                   
247811     EJECT                                                                
247812*01  -COPY W0008  -PRE RETA1-                                             
247813     05  FILLER                  PIC X.                                   
247814     EJECT                                                                
247815*01  -COPY W0008  -PRE RETA2-                                             
247816     05  FILLER                  PIC X.                                   
247817     EJECT                                                                
247818*01  -COPY W0008  -PRE RETG-                                              
247819     05  FILLER                  PIC X.                                   
247820     EJECT                                                                
247821*01  -COPY W0008  -PRE WDA2-                                              
247822     05  FILLER                  PIC X.                                   
247823     EJECT                                                                
247824*01  -COPY W0008  -PRE RETA3-                                             
247825     05  FILLER                  PIC X.                                   
247826     EJECT                                                                
247827*01  -COPY W0008  -PRE RETA4-                                             
247828     05  FILLER                  PIC X.                                   
247829     EJECT                                                                
247830*01  -COPY W0008  -PRE WDA3H-                                             
247831     05  FILLER                  PIC X.                                   
247832     EJECT                                                                
247833*01  -COPY W0008  -PRE WDA3I-                                             
247834     05  FILLER                  PIC X.                                   
247835     EJECT                                                                
247836                                                                          
247837*01  -COPY W0008  -PRE WDB6-                                              
247838     05  FILLER                  PIC X.                                   
247839     EJECT                                                                
247840                                                                          
247841*01  -COPY W0008  -PRE WDK6-                                              
247842     05  FILLER                  PIC X.                                   
247843     EJECT                                                                
247844                                                                          
247845*01  -COPY W0008  -PRE WDD3-                                              
247846     05  FILLER                  PIC X.                                   
247847     EJECT                                                                
247848                                                                          
247869*01  -COPY W0008 -PRE WDR4-                                               
247870     05  FILLER        PIC X.                                             
247871*01  -COPY W0008 -PRE 9305-                                               
247872     05  FILLER        PIC X.                                             
247873     EJECT                                                                
247884 PROCEDURE DIVISION  USING MSG-PCB                                        
247885                           DISTRDOC-PCB                                   
247886                           RETA1-PCB RETA2-PCB RETG-PCB                   
247887                           WDA2-PCB RETA3-PCB RETA4-PCB                   
247888                           WDA3H-PCB WDA3I-PCB                            
247889                           WDB6-PCB WDK6-PCB WDD3-PCB                     
247900                           WDR4-PCB                                       
247901                           9305-PCB.                                      
247902 MAIN SECTION.                                                            
247903     ENTRY 'DLITCBL' USING MSG-PCB                                        
247904                           DISTRDOC-PCB                                   
247905                           RETA1-PCB RETA2-PCB RETG-PCB                   
247906                           WDA2-PCB RETA3-PCB RETA4-PCB                   
247907                           WDA3H-PCB WDA3I-PCB                            
247908                           WDB6-PCB WDK6-PCB WDD3-PCB                     
247919                           WDR4-PCB                                       
247920                           9305-PCB.                                      
247921                                                                          
247922     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
247923     IF SUB-KDRC = 0                                                      
247924       PERFORM A-INIT                                                     
247925       PERFORM B-KOLLA-NYCKLAR                                            
247926       IF NYCKLAR-OK                                                      
247927         IF REQU-KDPGMACT = 'E'                                           
247928           PERFORM G-KOLLA-INPUT                                          
247929           IF INDATA-OK                                                   
247930             IF FL-DELET                                                  
247931             PERFORM H-UPPDATERA                                          
247932             END-IF                                                       
247933                                                                          
247934             IF FL-PRINT                                                  
247935               PERFORM I10-WDA3-LINES                                     
247936               IF INDATA-OK                                               
247937                 PERFORM S13-SEND-CLOSE                                   
247938                 PERFORM F20-DAP-REPORTS                                  
247939                 MOVE '015'  TO RESP-IDMSG-INFO                           
247940               END-IF                                                     
247941             END-IF                                                       
247942           END-IF                                                         
247943         END-IF                                                           
247944         IF INDATA-OK                                                     
247945            PERFORM F-LAES-VISA-INFO                                      
247946         END-IF                                                           
247947       END-IF                                                             
247948       MOVE RESP-IDMSG-INFO    TO WS-IDMSG-INFO                           
247949       MOVE RESP-IDMSG-ERROR   TO WS-IDMSG-ERROR                          
247950       MOVE RESP-IDELMT-ERROR  TO WS-IDELMT-ERROR                         
247951       IF WS-IDMSG-INFO  NOT = SPACE                                      
247952          MOVE SPACE           TO RESP-IDMSG-ERROR                        
247953          MOVE SPACE           TO RESP-IDELMT-ERROR                       
247954       ELSE                                                               
247955         IF WS-IDMSG-ERROR NOT = SPACE                                    
247956            MOVE ALL '+' TO RESP-WL0159O1                                 
247957            MOVE WS-IDMSG-ERROR   TO RESP-IDMSG-ERROR                     
247958            MOVE WS-IDELMT-ERROR  TO RESP-IDELMT-ERROR                    
247959            MOVE WS-IDMSG-INFO    TO RESP-IDMSG-INFO                      
247960            MOVE  001             TO RESP-IDMSGVER                        
247961            IF  REQU-KDPGMACT = 'S'                                       
247962                MOVE ZERO             TO RESP-KVRADER                     
247963            ELSE                                                          
247964              IF REQU-KVRADER NUMERIC                                     
247965                 MOVE REQU-KVRADER     TO RESP-KVRADER                    
247966              ELSE                                                        
247967                 MOVE ZERO             TO RESP-KVRADER                    
247968              END-IF                                                      
247969            END-IF                                                        
247970         END-IF                                                           
247971       END-IF                                                             
247972       PERFORM S02-RETURN-RESPONSE                                        
247973     END-IF                                                               
247974                                                                          
247975     MOVE ZERO TO RETURN-CODE                                             
247976     GOBACK                                                               
247977     .                                                                    
247978     EJECT                                                                
247979 A-INIT SECTION.                                                          
247980                                                                          
247981     MOVE ALL '+' TO RESP-AREA                                            
247982     MOVE SPACE   TO RESP-IDMSG-INFO                                      
247983                     RESP-IDMSG-ERROR                                     
247984                     RESP-IDELMT-ERROR                                    
247985     MOVE 001     TO RESP-IDMSGVER                                        
247986     MOVE ZERO    TO RESP-KVRADER                                         
247987     MOVE SPACE   TO RESP-WL0159O1                                        
247988                                                                          
247989                                                                          
247990     MOVE LOW-VALUE         TO W-WDA3F1KY-MIN-X                           
247991                               W-WDA3H1-MIN-X                             
247992                               W-WDA3I1-MIN-X                             
247993                                                                          
247994     MOVE HIGH-VALUE        TO W-WDA3F1KY-MAX-X                           
247995                               W-WDA3H1-MAX-X                             
247996                               W-WDA3I1-MAX-X                             
247997                                                                          
247998     ACCEPT DAGENS-DATUM FROM DATE                                        
247999     MOVE DAGENS-AA         TO LIST-AAR                                   
248000                               E-LIST-AAR                                 
248001                               W-DATE-AAMM(1:2)                           
248002     MOVE DAGENS-MM         TO LIST-MAN                                   
248003                               E-LIST-MAN                                 
248004                               W-DATE-AAMM(3:2)                           
248005     MOVE DAGENS-DD         TO LIST-DAG                                   
248006                               E-LIST-DAG                                 
248007                                                                          
248008     MOVE +1 TO D-INDX                                                    
248009     PERFORM UNTIL D-INDX > 500                                           
248010         MOVE +0   TO LISTA-IDARTNR (D-INDX)                              
248011                      LISTA-IDRAPPNR (D-INDX)                             
248012                      LISTA-KVLEVART (D-INDX)                             
248013                      LISTA-PRFKTUTL-RAD (D-INDX)                         
248014                      LISTA-IDSTATNR (D-INDX)                             
248015                      LISTA-VKART-TOT (D-INDX)                            
248016         MOVE SPACES TO                                                   
248017                      LISTA-BEART (D-INDX)                                
248018                      LISTA-KDARTURS (D-INDX)                             
248019         ADD +1 TO D-INDX                                                 
248020     END-PERFORM                                                          
248021     .                                                                    
248022     EJECT                                                                
248023 B-KOLLA-NYCKLAR SECTION.                                                 
248024                                                                          
248025     MOVE JA TO NYCKLAR-SW                                                
248026     MOVE NEJ TO IDDISTR-SW                                               
248027     MOVE NEJ TO IDKUNDNR-SW                                              
248028                                                                          
248029     PERFORM BA-KOLLA-IDDISTR                                             
248030     PERFORM BB-KOLLA-IDKUNDNR                                            
248031     PERFORM BC-KOLLA-KDRETSTA                                            
248032                                                                          
248033     IF NYCKLAR-OK                                                        
248034        MOVE REQU-IDDC-KEY        TO RESP-IDDC-KEY                        
248035        MOVE REQU-KDRETSTA-KEY    TO RESP-KDRETSTA-KEY                    
248036     END-IF                                                               
248037                                                                          
248038     .                                                                    
248039     EJECT                                                                
248040                                                                          
248041 BA-KOLLA-IDDISTR  SECTION.                                               
248042                                                                          
248043     IF REQU-IDDISTR-KEY NUMERIC AND REQU-IDDISTR-KEY > ZERO              
248044       MOVE REQU-IDDISTR-KEY      TO W-IDDISTR-A3                         
248045                                     RESP-IDDISTR-KEY                     
248046       MOVE JA                    TO IDDISTR-SW                           
248047     END-IF                                                               
248048                                                                          
248049     .                                                                    
248050     EJECT                                                                
248051                                                                          
248052 BB-KOLLA-IDKUNDNR   SECTION.                                             
248053                                                                          
248054     IF REQU-IDKUNDNR-KEY   NUMERIC AND REQU-IDKUNDNR-KEY > ZERO          
248055       IF IDDISTR-SW = NEJ                                                
248056         MOVE NEJ  TO NYCKLAR-SW                                          
248057         MOVE 'IDDISTR'           TO RESP-IDELMT-ERROR                    
248058         MOVE '023'               TO RESP-IDMSG-ERROR                     
248059       ELSE                                                               
248060         MOVE REQU-IDKUNDNR-KEY     TO W-IDKUNDNR-A3                      
248061                                       RESP-IDKUNDNR-KEY                  
248062         MOVE JA                    TO IDKUNDNR-SW                        
248063       END-IF                                                             
248064     END-IF                                                               
248065                                                                          
248066     .                                                                    
248067     EJECT                                                                
248068 BC-KOLLA-KDRETSTA   SECTION.                                             
248069                                                                          
248070     MOVE REQU-KDRETSTA-KEY     TO W-KDRETSTA                             
248071     IF W-TERM OR W-SAENT OR W-TRANSIT                                    
248072                                                                          
248073       EVALUATE TRUE                                                      
248074         WHEN W-TERM                                                      
248075           MOVE '1'             TO W-KDRETSTA-NUM                         
248076         WHEN W-SAENT                                                     
248077           MOVE '2'             TO W-KDRETSTA-NUM                         
248078         WHEN W-TRANSIT                                                   
248079           MOVE '7'             TO W-KDRETSTA-NUM                         
248080       END-EVALUATE                                                       
248081     ELSE                                                                 
248082       MOVE NEJ                 TO NYCKLAR-SW                             
248083       MOVE 'KDRETSTA'          TO RESP-IDELMT-ERROR                      
248084       MOVE '023'               TO RESP-IDMSG-ERROR                       
248085     END-IF                                                               
248086                                                                          
248087     .                                                                    
248088     EJECT                                                                
248089 F-LAES-VISA-INFO SECTION.                                                
248090                                                                          
248091     MOVE ZERO         TO W-SPAR-IDDISTR                                  
248092                          W-SPAR-IDKUNDNR                                 
248093                          W-SPAR-IDRAPPNR                                 
248094     EVALUATE TRUE                                                        
248095         WHEN W-TERM                                                      
248096              PERFORM FA-LAES-VISA-TERM                                   
248097         WHEN W-SAENT                                                     
248098              PERFORM FB-LAES-VISA-SAENT                                  
248099         WHEN W-TRANSIT                                                   
248100              PERFORM FC-LAES-VISA-TRANSIT                                
248101     END-EVALUATE                                                         
248102     .                                                                    
248103     EJECT                                                                
248104 FA-LAES-VISA-TERM     SECTION.                                           
248105                                                                          
248106     MOVE REQU-IDRT-KEY  TO W-IDRT-H1-MIN                                 
248107                            W-IDRT-H1-MAX                                 
248108                            W-IDRT-HSEQ                                   
248109     MOVE W-KDRETSTA-NUM TO W-KDRETSTA-H1-MIN                             
248110                            W-KDRETSTA-H1-MAX                             
248111                            W-KDRETSTA-HSEQ                               
248112                                                                          
248113     IF IDDISTR-VAL                                                       
248114       IF IDKUNDNR-VAL                                                    
248115         PERFORM IMS-GU-WDA3H1-DIST-KUND                                  
248116       ELSE                                                               
248117         PERFORM IMS-GU-WDA3H1-DISTR                                      
248118       END-IF                                                             
248119                                                                          
248120       IF SEGMENT-SAKNAS                                                  
248121          MOVE '041'              TO RESP-IDMSG-ERROR                     
248122          MOVE 'IDRT-IDRTLOP'     TO RESP-IDELMT-ERROR                    
248123       ELSE                                                               
248124         MOVE +1                  TO INDX                                 
248125         MOVE +0                  TO WS-COUNT                             
248126                                                                          
248127         PERFORM UNTIL INDX > MAX-INDX                                    
248128                    OR SEGMENT-SAKNAS                                     
248129                                                                          
248130            MOVE SEQH-IDDC       TO W-IDDC                                
248131            MOVE SEQH-DAREGDAT   TO W-DAREGDAT                            
248132            MOVE SEQH-TIKLOCK    TO W-TIKLOCK                             
248133                                                                          
248134            PERFORM IMS-GU-WDA301                                         
248135                                                                          
248136            IF SEGMENT-FINNS                                              
248137              IF RET-IDDISTR      =  W-SPAR-IDDISTR AND                   
248138                 RET-IDKUNDNR     =  W-SPAR-IDKUNDNR AND                  
248139                 RET-IDRAPPNR     =  W-SPAR-IDRAPPNR AND                  
248140                 RET-IDRT         =  W-SPAR-IDRT     AND                  
248141                 RET-IDRTLOP      =  W-SPAR-IDRTLOP                       
248142                 CONTINUE                                                 
248143              ELSE                                                        
248144                 MOVE RET-IDDISTR  TO W-SPAR-IDDISTR                      
248145                 MOVE RET-IDKUNDNR TO W-SPAR-IDKUNDNR                     
248146                 MOVE RET-IDRAPPNR TO W-SPAR-IDRAPPNR                     
248147                 MOVE RET-IDRT     TO W-SPAR-IDRT                         
248148                 MOVE RET-IDRTLOP  TO W-SPAR-IDRTLOP                      
248149                                                                          
248150                 PERFORM S10-REDIGERA-MOD                                 
248151                                                                          
248152                 ADD 1             TO INDX                                
248153                 ADD 1             TO WS-COUNT                            
248154              END-IF                                                      
248155            END-IF                                                        
248156                                                                          
248157            IF IDKUNDNR-VAL                                               
248158              PERFORM IMS-GN-WDA3H1-DIST-KUND                             
248159            ELSE                                                          
248160              PERFORM IMS-GN-WDA3H1-DISTR                                 
248161            END-IF                                                        
248162         END-PERFORM                                                      
248163         MOVE WS-COUNT          TO RESP-KVRADER                           
248164                                                                          
248165         IF WS-COUNT = 500                                                
248166            MOVE '028'  TO RESP-IDMSG-ERROR                               
248167         END-IF                                                           
248168                                                                          
248169       END-IF                                                             
248170     ELSE                                                                 
248171       PERFORM FAA-VISA-ALLA-TERM                                         
248172     END-IF                                                               
248173     .                                                                    
248174     EJECT                                                                
248175 FAA-VISA-ALLA-TERM    SECTION.                                           
248176                                                                          
248177     PERFORM IMS-GU-WDA301-H1                                             
248178                                                                          
248179       IF SEGMENT-SAKNAS                                                  
248180          MOVE '041'              TO RESP-IDMSG-ERROR                     
248181          MOVE 'IDRT-IDRTLOP'     TO RESP-IDELMT-ERROR                    
248182       ELSE                                                               
248183         MOVE +1                  TO INDX                                 
248184         MOVE +0                  TO WS-COUNT                             
248185                                                                          
248186         PERFORM UNTIL INDX > MAX-INDX                                    
248187                    OR SEGMENT-SAKNAS                                     
248188                                                                          
248189            IF RET-IDDISTR      =  W-SPAR-IDDISTR AND                     
248190               RET-IDKUNDNR     =  W-SPAR-IDKUNDNR AND                    
248191               RET-IDRAPPNR     =  W-SPAR-IDRAPPNR AND                    
248192               RET-IDRT         =  W-SPAR-IDRT     AND                    
248193               RET-IDRTLOP      =  W-SPAR-IDRTLOP                         
248194               CONTINUE                                                   
248195            ELSE                                                          
248196               MOVE RET-IDDISTR  TO W-SPAR-IDDISTR                        
248197               MOVE RET-IDKUNDNR TO W-SPAR-IDKUNDNR                       
248198               MOVE RET-IDRAPPNR TO W-SPAR-IDRAPPNR                       
248199               MOVE RET-IDRT     TO W-SPAR-IDRT                           
248200               MOVE RET-IDRTLOP  TO W-SPAR-IDRTLOP                        
248201                                                                          
248202               PERFORM S10-REDIGERA-MOD                                   
248203                                                                          
248204               ADD 1             TO INDX                                  
248205               ADD 1             TO WS-COUNT                              
248206            END-IF                                                        
248207            PERFORM IMS-GN-WDA301-H1                                      
248208         END-PERFORM                                                      
248209         MOVE WS-COUNT          TO RESP-KVRADER                           
248210                                                                          
248211         IF WS-COUNT = 500                                                
248212            MOVE '028'  TO RESP-IDMSG-ERROR                               
248213         END-IF                                                           
248214                                                                          
248215       END-IF                                                             
248216     .                                                                    
248217     EJECT                                                                
248218 FB-LAES-VISA-SAENT    SECTION.                                           
248219                                                                          
248220     MOVE REQU-IDRT-KEY  TO W-IDRT-H1-MIN                                 
248221                            W-IDRT-H1-MAX                                 
248222                            W-IDRT-HSEQ                                   
248223     MOVE W-KDRETSTA-NUM TO W-KDRETSTA-H1-MIN                             
248224                            W-KDRETSTA-H1-MAX                             
248225                            W-KDRETSTA-HSEQ                               
248226                                                                          
248227     IF IDDISTR-VAL                                                       
248228       IF IDKUNDNR-VAL                                                    
248229         PERFORM IMS-GU-WDA3H1-DIST-KUND                                  
248230       ELSE                                                               
248231         PERFORM IMS-GU-WDA3H1-DISTR                                      
248232       END-IF                                                             
248233                                                                          
248234       MOVE +1                TO INDX                                     
248235       MOVE +0                TO WS-COUNT                                 
248236                                                                          
248237       PERFORM UNTIL INDX > MAX-INDX                                      
248238                  OR SEGMENT-SAKNAS                                       
248239                                                                          
248240          MOVE SEQH-IDDC       TO W-IDDC                                  
248241          MOVE SEQH-DAREGDAT   TO W-DAREGDAT                              
248242          MOVE SEQH-TIKLOCK    TO W-TIKLOCK                               
248243                                                                          
248244          PERFORM IMS-GU-WDA301                                           
248245                                                                          
248246          IF SEGMENT-FINNS                                                
248247            IF RET-IDDISTR      =  W-SPAR-IDDISTR AND                     
248248               RET-IDKUNDNR     =  W-SPAR-IDKUNDNR AND                    
248249               RET-IDRAPPNR     =  W-SPAR-IDRAPPNR AND                    
248250               RET-IDRT         =  W-SPAR-IDRT     AND                    
248251               RET-IDRTLOP      =  W-SPAR-IDRTLOP                         
248252               CONTINUE                                                   
248253            ELSE                                                          
248254               MOVE RET-IDDISTR  TO W-SPAR-IDDISTR                        
248255               MOVE RET-IDKUNDNR TO W-SPAR-IDKUNDNR                       
248256               MOVE RET-IDRAPPNR TO W-SPAR-IDRAPPNR                       
248257               MOVE RET-IDRT     TO W-SPAR-IDRT                           
248258               MOVE RET-IDRTLOP  TO W-SPAR-IDRTLOP                        
248259                                                                          
248260               PERFORM S10-REDIGERA-MOD                                   
248261                                                                          
248262               ADD 1             TO INDX                                  
248263               ADD 1             TO WS-COUNT                              
248264            END-IF                                                        
248265          END-IF                                                          
248266                                                                          
248267          IF IDKUNDNR-VAL                                                 
248268            PERFORM IMS-GN-WDA3H1-DIST-KUND                               
248269          ELSE                                                            
248270            PERFORM IMS-GN-WDA3H1-DISTR                                   
248271          END-IF                                                          
248272       END-PERFORM                                                        
248273                                                                          
248274       MOVE REQU-IDRT-KEY     TO W-IDRT-TRANSIT-ISEQ                      
248275                                 W-IDRT-TRANSIT-I1-MIN                    
248276                                 W-IDRT-TRANSIT-I1-MAX                    
248277       MOVE W-KDRETSTA-NUM    TO W-KDRETSTA-ISEQ                          
248278                                 W-KDRETSTA-I1-MIN                        
248279                                 W-KDRETSTA-I1-MAX                        
248280                                                                          
248281       IF IDKUNDNR-VAL                                                    
248282         PERFORM IMS-GU-WDA3I1-DIST-KUND                                  
248283       ELSE                                                               
248284         PERFORM IMS-GU-WDA3I1-DISTR                                      
248285       END-IF                                                             
248286                                                                          
248287       IF SEGMENT-SAKNAS AND WS-COUNT = +0                                
248288          MOVE '041'              TO RESP-IDMSG-ERROR                     
248289          MOVE 'IDRT-IDRTLOP'     TO RESP-IDELMT-ERROR                    
248290       ELSE                                                               
248291                                                                          
248292         PERFORM UNTIL INDX > MAX-INDX                                    
248293                    OR SEGMENT-SAKNAS                                     
248294                                                                          
248295            MOVE SEQI-IDDC       TO W-IDDC                                
248296            MOVE SEQI-DAREGDAT   TO W-DAREGDAT                            
248297            MOVE SEQI-TIKLOCK    TO W-TIKLOCK                             
248298                                                                          
248299            PERFORM IMS-GU-WDA301                                         
248300                                                                          
248301            IF SEGMENT-FINNS                                              
248302              IF RET-IDDISTR      =  W-SPAR-IDDISTR AND                   
248303                 RET-IDKUNDNR     =  W-SPAR-IDKUNDNR AND                  
248304                 RET-IDRAPPNR     =  W-SPAR-IDRAPPNR AND                  
248305                 RET-IDRT         =  W-SPAR-IDRT     AND                  
248306                 RET-IDRTLOP      =  W-SPAR-IDRTLOP                       
248307                 CONTINUE                                                 
248308              ELSE                                                        
248309                 MOVE RET-IDDISTR  TO W-SPAR-IDDISTR                      
248310                 MOVE RET-IDKUNDNR TO W-SPAR-IDKUNDNR                     
248311                 MOVE RET-IDRAPPNR TO W-SPAR-IDRAPPNR                     
248312                 MOVE RET-IDRT     TO W-SPAR-IDRT                         
248313                 MOVE RET-IDRTLOP  TO W-SPAR-IDRTLOP                      
248314                                                                          
248315                 PERFORM S10-REDIGERA-MOD                                 
248316                                                                          
248317                 ADD 1             TO INDX                                
248318                 ADD 1             TO WS-COUNT                            
248319              END-IF                                                      
248320            END-IF                                                        
248321                                                                          
248322            IF IDKUNDNR-VAL                                               
248323              PERFORM IMS-GN-WDA3I1-DIST-KUND                             
248324            ELSE                                                          
248325              PERFORM IMS-GN-WDA3I1-DISTR                                 
248326            END-IF                                                        
248327         END-PERFORM                                                      
248328         MOVE WS-COUNT          TO RESP-KVRADER                           
248329                                                                          
248330         IF WS-COUNT = 500                                                
248331            MOVE '028'  TO RESP-IDMSG-ERROR                               
248332         END-IF                                                           
248333                                                                          
248334       END-IF                                                             
248335     ELSE                                                                 
248336       PERFORM FBA-VISA-ALLA-SAENT                                        
248337     END-IF                                                               
248338     .                                                                    
248339     EJECT                                                                
248340 FBA-VISA-ALLA-SAENT    SECTION.                                          
248341                                                                          
248342     PERFORM IMS-GU-WDA301-H1                                             
248343                                                                          
248344     MOVE +1                  TO INDX                                     
248345     MOVE +0                  TO WS-COUNT                                 
248346                                                                          
248347     PERFORM UNTIL INDX > MAX-INDX                                        
248348                OR SEGMENT-SAKNAS                                         
248349        IF RET-IDDISTR      =  W-SPAR-IDDISTR AND                         
248350           RET-IDKUNDNR     =  W-SPAR-IDKUNDNR AND                        
248351           RET-IDRAPPNR     =  W-SPAR-IDRAPPNR AND                        
248352           RET-IDRT         =  W-SPAR-IDRT     AND                        
248353           RET-IDRTLOP      =  W-SPAR-IDRTLOP                             
248354           CONTINUE                                                       
248355        ELSE                                                              
248356           MOVE RET-IDDISTR  TO W-SPAR-IDDISTR                            
248357           MOVE RET-IDKUNDNR TO W-SPAR-IDKUNDNR                           
248358           MOVE RET-IDRAPPNR TO W-SPAR-IDRAPPNR                           
248359           MOVE RET-IDRT     TO W-SPAR-IDRT                               
248360           MOVE RET-IDRTLOP  TO W-SPAR-IDRTLOP                            
248361                                                                          
248362           PERFORM S10-REDIGERA-MOD                                       
248363                                                                          
248364           ADD 1             TO INDX                                      
248365           ADD 1             TO WS-COUNT                                  
248366        END-IF                                                            
248367        PERFORM IMS-GN-WDA301-H1                                          
248368     END-PERFORM                                                          
248369                                                                          
248370     MOVE REQU-IDRT-KEY     TO W-IDRT-TRANSIT-ISEQ                        
248371     MOVE W-KDRETSTA-NUM    TO W-KDRETSTA-ISEQ                            
248372     PERFORM IMS-GU-WDA301-I1                                             
248373     IF SEGMENT-SAKNAS AND WS-COUNT = +0                                  
248374        MOVE '041'              TO RESP-IDMSG-ERROR                       
248375        MOVE 'IDRT-IDRTLOP'     TO RESP-IDELMT-ERROR                      
248376     ELSE                                                                 
248377                                                                          
248378       PERFORM UNTIL INDX > MAX-INDX                                      
248379                  OR SEGMENT-SAKNAS                                       
248380          IF RET-IDDISTR      =  W-SPAR-IDDISTR AND                       
248381             RET-IDKUNDNR     =  W-SPAR-IDKUNDNR AND                      
248382             RET-IDRAPPNR     =  W-SPAR-IDRAPPNR AND                      
248383             RET-IDRT         =  W-SPAR-IDRT     AND                      
248384             RET-IDRTLOP      =  W-SPAR-IDRTLOP                           
248385             CONTINUE                                                     
248386          ELSE                                                            
248387             MOVE RET-IDDISTR  TO W-SPAR-IDDISTR                          
248388             MOVE RET-IDKUNDNR TO W-SPAR-IDKUNDNR                         
248389             MOVE RET-IDRAPPNR TO W-SPAR-IDRAPPNR                         
248390             MOVE RET-IDRT     TO W-SPAR-IDRT                             
248391             MOVE RET-IDRTLOP  TO W-SPAR-IDRTLOP                          
248392                                                                          
248393             PERFORM S10-REDIGERA-MOD                                     
248394                                                                          
248395             ADD 1             TO INDX                                    
248396             ADD 1             TO WS-COUNT                                
248397          END-IF                                                          
248398          PERFORM IMS-GN-WDA301-I1                                        
248399       END-PERFORM                                                        
248400       MOVE WS-COUNT          TO RESP-KVRADER                             
248401                                                                          
248402       IF WS-COUNT = 500                                                  
248403          MOVE '028'  TO RESP-IDMSG-ERROR                                 
248404       END-IF                                                             
248405                                                                          
248406     END-IF                                                               
248407     .                                                                    
248408     EJECT                                                                
248409                                                                          
248410 FC-LAES-VISA-TRANSIT  SECTION.                                           
248411                                                                          
248412     MOVE REQU-IDRT-KEY     TO W-IDRT-TRANSIT-I1-MIN                      
248413                               W-IDRT-TRANSIT-I1-MAX                      
248414                               W-IDRT-TRANSIT-ISEQ                        
248415     MOVE W-KDRETSTA-NUM    TO W-KDRETSTA-I1-MIN                          
248416                               W-KDRETSTA-I1-MAX                          
248417                               W-KDRETSTA-ISEQ                            
248418                                                                          
248419     IF IDDISTR-VAL                                                       
248420       IF IDKUNDNR-VAL                                                    
248421         PERFORM IMS-GU-WDA3I1-DIST-KUND                                  
248422       ELSE                                                               
248423         PERFORM IMS-GU-WDA3I1-DISTR                                      
248424       END-IF                                                             
248425                                                                          
248426       IF SEGMENT-SAKNAS                                                  
248427          MOVE '041'              TO RESP-IDMSG-ERROR                     
248428          MOVE 'IDRT-IDRTLOP'     TO RESP-IDELMT-ERROR                    
248429       ELSE                                                               
248430         MOVE +1                  TO INDX                                 
248431         MOVE +0                  TO WS-COUNT                             
248432                                                                          
248433         PERFORM UNTIL INDX > MAX-INDX                                    
248434                    OR SEGMENT-SAKNAS                                     
248435                                                                          
248436            MOVE SEQI-IDDC       TO W-IDDC                                
248437            MOVE SEQI-DAREGDAT   TO W-DAREGDAT                            
248438            MOVE SEQI-TIKLOCK    TO W-TIKLOCK                             
248439                                                                          
248440            PERFORM IMS-GU-WDA301                                         
248441                                                                          
248442            IF SEGMENT-FINNS                                              
248443              IF RET-IDDISTR      =  W-SPAR-IDDISTR AND                   
248444                 RET-IDKUNDNR     =  W-SPAR-IDKUNDNR AND                  
248445                 RET-IDRAPPNR     =  W-SPAR-IDRAPPNR AND                  
248446                 RET-IDRT         =  W-SPAR-IDRT     AND                  
248447                 RET-IDRTLOP      =  W-SPAR-IDRTLOP                       
248448                 CONTINUE                                                 
248449              ELSE                                                        
248450                 MOVE RET-IDDISTR  TO W-SPAR-IDDISTR                      
248451                 MOVE RET-IDKUNDNR TO W-SPAR-IDKUNDNR                     
248452                 MOVE RET-IDRAPPNR TO W-SPAR-IDRAPPNR                     
248453                 MOVE RET-IDRT     TO W-SPAR-IDRT                         
248454                 MOVE RET-IDRTLOP  TO W-SPAR-IDRTLOP                      
248455                                                                          
248456                 PERFORM S10-REDIGERA-MOD                                 
248457                                                                          
248458                 ADD 1             TO INDX                                
248459                 ADD 1             TO WS-COUNT                            
248460              END-IF                                                      
248461            END-IF                                                        
248462                                                                          
248463            IF IDKUNDNR-VAL                                               
248464              PERFORM IMS-GN-WDA3I1-DIST-KUND                             
248465            ELSE                                                          
248466              PERFORM IMS-GN-WDA3I1-DISTR                                 
248467            END-IF                                                        
248468         END-PERFORM                                                      
248469         MOVE WS-COUNT          TO RESP-KVRADER                           
248470                                                                          
248471         IF WS-COUNT = 500                                                
248472            MOVE '028'  TO RESP-IDMSG-ERROR                               
248473         END-IF                                                           
248474                                                                          
248475       END-IF                                                             
248476     ELSE                                                                 
248477       PERFORM FCA-VISA-ALLA-TRANSIT                                      
248478     END-IF                                                               
248479     .                                                                    
248480     EJECT                                                                
248481 FCA-VISA-ALLA-TRANSIT  SECTION.                                          
248482                                                                          
248483     PERFORM IMS-GU-WDA301-I1                                             
248484                                                                          
248485     IF SEGMENT-SAKNAS                                                    
248486        MOVE '041'              TO RESP-IDMSG-ERROR                       
248487        MOVE 'IDRT-IDRTLOP'     TO RESP-IDELMT-ERROR                      
248488     ELSE                                                                 
248489       MOVE +1                  TO INDX                                   
248490       MOVE +0                  TO WS-COUNT                               
248491                                                                          
248492       PERFORM UNTIL INDX > MAX-INDX                                      
248493                  OR SEGMENT-SAKNAS                                       
248494                                                                          
248495          IF RET-IDDISTR      =  W-SPAR-IDDISTR AND                       
248496             RET-IDKUNDNR     =  W-SPAR-IDKUNDNR AND                      
248497             RET-IDRAPPNR     =  W-SPAR-IDRAPPNR AND                      
248498             RET-IDRT         =  W-SPAR-IDRT     AND                      
248499             RET-IDRTLOP      =  W-SPAR-IDRTLOP                           
248500             CONTINUE                                                     
248501          ELSE                                                            
248502             MOVE RET-IDDISTR  TO W-SPAR-IDDISTR                          
248503             MOVE RET-IDKUNDNR TO W-SPAR-IDKUNDNR                         
248504             MOVE RET-IDRAPPNR TO W-SPAR-IDRAPPNR                         
248505             MOVE RET-IDRT     TO W-SPAR-IDRT                             
248506             MOVE RET-IDRTLOP  TO W-SPAR-IDRTLOP                          
248507                                                                          
248508             PERFORM S10-REDIGERA-MOD                                     
248509                                                                          
248510             ADD 1             TO INDX                                    
248511             ADD 1             TO WS-COUNT                                
248512          END-IF                                                          
248513          PERFORM IMS-GN-WDA301-I1                                        
248514       END-PERFORM                                                        
248515       MOVE WS-COUNT          TO RESP-KVRADER                             
248516                                                                          
248517       IF WS-COUNT = 500                                                  
248518          MOVE '028'  TO RESP-IDMSG-ERROR                                 
248519       END-IF                                                             
248520                                                                          
248521     END-IF                                                               
248522     .                                                                    
248523     EJECT                                                                
248524                                                                          
248525 G-KOLLA-INPUT SECTION.                                                   
248526                                                                          
248527     MOVE JA  TO INDATA-SW                                                
248528                                                                          
248529     PERFORM GA-FORMELL-KONTROLL                                          
248530     IF INDATA-OK                                                         
248531        PERFORM GB-LOGISK-KONTROLL                                        
248532     END-IF                                                               
248533     .                                                                    
248534     EJECT                                                                
248535                                                                          
248536 GA-FORMELL-KONTROLL SECTION.                                             
248537                                                                          
248538     IF REQU-INPUT                = ALL '+'                               
248539        MOVE '014'                TO RESP-IDMSG-ERROR                     
248540                                     RESP-IDMSG-ERROR-LINE(1)             
248541        MOVE NEJ                  TO INDATA-SW                            
248542     ELSE                                                                 
248543       PERFORM GAA-KOLLA-FLCMD                                            
248544     END-IF                                                               
248545                                                                          
248546     .                                                                    
248547     EJECT                                                                
248548 GAA-KOLLA-FLCMD      SECTION.                                            
248549                                                                          
248550     IF REQU-KVRADER NUMERIC AND REQU-KVRADER > 0                         
248551       MOVE +1                           TO INDX                          
248552       MOVE REQU-KVRADER                 TO WS-INDX-REC                   
248553       MOVE NEJ                          TO WS-REC-LIMIT                  
248554                                                                          
248555       PERFORM UNTIL INDX          >  MAX-INDX OR REC-LIMIT               
248557          IF REQU-FLCMD(INDX) = 'J' OR 'Y' OR 'N' OR '+' OR               
248558                                'P' OR 'D'                                
248559            CONTINUE                                                      
248560          ELSE                                                            
248561            MOVE NEJ                  TO INDATA-SW                        
248562            MOVE 'CMD'      TO RESP-IDELMT-ERROR                          
248563            MOVE '023'      TO RESP-IDMSG-ERROR                           
248564                               RESP-IDMSG-ERROR-LINE(INDX)                
248565          END-IF                                                          
248566          IF INDX = WS-INDX-REC                                           
248567             MOVE JA TO WS-REC-LIMIT                                      
248568          ELSE                                                            
248569             ADD +1          TO INDX                                      
248570          END-IF                                                          
248571       END-PERFORM                                                        
248572     ELSE                                                                 
248573       MOVE NEJ           TO INDATA-SW                                    
248574       IF REQU-KVRADER = 0                                                
248575          MOVE 'KVRADER' TO RESP-IDELMT-ERROR                             
248576          MOVE '126'     TO RESP-IDMSG-ERROR                              
248577       ELSE                                                               
248578          MOVE 'KVRADER' TO RESP-IDELMT-ERROR                             
248579          MOVE '024'     TO RESP-IDMSG-ERROR                              
248580       END-IF                                                             
248581     END-IF                                                               
248582                                                                          
248583     .                                                                    
248584     EJECT                                                                
248585 GB-LOGISK-KONTROLL SECTION.                                              
248586                                                                          
248587     MOVE +1                           TO INDX                            
248588     MOVE 0                            TO WS-PRINT-CNT                    
248589     MOVE REQU-KVRADER                 TO WS-INDX-REC                     
248590     MOVE NEJ                          TO WS-REC-LIMIT                    
248591                                          WS-FL-DELET                     
248592                                          WS-FL-PRINT                     
248593     PERFORM UNTIL INDX        > MAX-INDX OR REC-LIMIT                    
248594        IF REQU-FLCMD(INDX)            NOT = ALL '+' AND                  
248595           REQU-FLCMD(INDX)            NOT = 'N'                          
248596           IF REQU-FLCMD(INDX)         =  'J' OR 'Y' OR 'P' OR 'D'        
248597              IF REQU-FLCMD(INDX)         =  'J' OR 'Y' OR 'D'            
248598                 MOVE JA  TO WS-FL-DELET                                  
248599              ELSE                                                        
248600                 MOVE JA  TO WS-FL-PRINT                                  
248601                 ADD 1 TO WS-PRINT-CNT                                    
248602              END-IF                                                      
248603*             PERFORM GBA-KOLLA-VALT-RETURTILLSTAND                       
248604           ELSE                                                           
248605              MOVE NEJ                 TO INDATA-SW                       
248606              MOVE 'CMD'               TO RESP-IDELMT-ERROR               
248607              MOVE '023'               TO RESP-IDMSG-ERROR                
248608                                       RESP-IDMSG-ERROR-LINE(INDX)        
248609           END-IF                                                         
248610        END-IF                                                            
248611        IF INDX = WS-INDX-REC                                             
248612           MOVE JA TO WS-REC-LIMIT                                        
248613        ELSE                                                              
248614           ADD +1          TO INDX                                        
248615        END-IF                                                            
248616     END-PERFORM                                                          
248617     IF (FL-PRINT AND FL-DELET) OR WS-PRINT-CNT > 1                       
248618        MOVE NEJ                 TO INDATA-SW                             
248619        MOVE 'MULTIPLE PRINT'    TO RESP-IDELMT-ERROR                     
248620** ADD ERROR MESSAGE THAT BOTH PRINT & DELETE!                            
248621        MOVE '023'               TO RESP-IDMSG-ERROR                      
248622                                 RESP-IDMSG-ERROR-LINE(INDX)              
248623     END-IF                                                               
248624                                                                          
248625     MOVE REQU-VKORDBTO TO W-SPAR-VKORDBTO                                
248626     MOVE REQU-VLORDBTO TO W-SPAR-VLORDBTO                                
248627                                                                          
248628     IF (FL-PRINT)                                                        
248629                                                                          
248630        MOVE 0 TO W-VK-DEC-COUNT                                          
248631        MOVE 0 TO W-VK-NUM-COUNT                                          
248632        IF W-SPAR-VKORDBTO > ' '                                          
248633           IF W-SPAR-VKORDBTO1 IS NUMERIC                                 
248634              OR W-SPAR-VKORDBTO1 = ',' OR ' ' OR '.'                     
248635              IF W-SPAR-VKORDBTO1 IS NUMERIC                              
248636                 ADD 1 TO W-VK-NUM-COUNT                                  
248637              END-IF                                                      
248638              IF W-SPAR-VKORDBTO1 = ',' OR '.'                            
248639                 ADD 1 TO W-VK-DEC-COUNT                                  
248640              END-IF                                                      
248641           ELSE                                                           
248642              MOVE NEJ                 TO INDATA-SW                       
248643           END-IF                                                         
248644           IF W-SPAR-VKORDBTO2 IS NUMERIC                                 
248645              OR W-SPAR-VKORDBTO2 = ',' OR ' ' OR '.'                     
248646              IF W-SPAR-VKORDBTO2 IS NUMERIC                              
248647                 ADD 1 TO W-VK-NUM-COUNT                                  
248648              END-IF                                                      
248649              IF W-SPAR-VKORDBTO2 = ',' OR '.'                            
248650                 ADD 1 TO W-VK-DEC-COUNT                                  
248651              END-IF                                                      
248652                                                                          
248653           ELSE                                                           
248654              MOVE NEJ                 TO INDATA-SW                       
248655           END-IF                                                         
248656           IF W-SPAR-VKORDBTO3 IS NUMERIC                                 
248657              OR W-SPAR-VKORDBTO3 = ',' OR ' ' OR '.'                     
248658              IF W-SPAR-VKORDBTO3 IS NUMERIC                              
248659                 ADD 1 TO W-VK-NUM-COUNT                                  
248660              END-IF                                                      
248661              IF W-SPAR-VKORDBTO3 = ',' OR '.'                            
248662                 ADD 1 TO W-VK-DEC-COUNT                                  
248663              END-IF                                                      
248664           ELSE                                                           
248665              MOVE NEJ                 TO INDATA-SW                       
248666           END-IF                                                         
248667           IF W-SPAR-VKORDBTO4 IS NUMERIC                                 
248668              OR W-SPAR-VKORDBTO4 = ',' OR ' ' OR '.'                     
248669              IF W-SPAR-VKORDBTO4 IS NUMERIC                              
248670                 ADD 1 TO W-VK-NUM-COUNT                                  
248671              END-IF                                                      
248672              IF W-SPAR-VKORDBTO4 = ',' OR '.'                            
248673                 ADD 1 TO W-VK-DEC-COUNT                                  
248674              END-IF                                                      
248675           ELSE                                                           
248676              MOVE NEJ                 TO INDATA-SW                       
248677           END-IF                                                         
248678           IF W-SPAR-VKORDBTO5 IS NUMERIC                                 
248679              OR W-SPAR-VKORDBTO5 = ',' OR ' ' OR '.'                     
248680              IF W-SPAR-VKORDBTO5 IS NUMERIC                              
248681                 ADD 1 TO W-VK-NUM-COUNT                                  
248682              END-IF                                                      
248683              IF W-SPAR-VKORDBTO5 = ',' OR '.'                            
248684                 ADD 1 TO W-VK-DEC-COUNT                                  
248685              END-IF                                                      
248686           ELSE                                                           
248687              MOVE NEJ                 TO INDATA-SW                       
248688           END-IF                                                         
248689           IF W-SPAR-VKORDBTO6 IS NUMERIC                                 
248690              OR W-SPAR-VKORDBTO6 = ',' OR ' ' OR '.'                     
248691              IF W-SPAR-VKORDBTO6 IS NUMERIC                              
248692                 ADD 1 TO W-VK-NUM-COUNT                                  
248693              END-IF                                                      
248694              IF W-SPAR-VKORDBTO6 = ',' OR '.'                            
248695                 ADD 1 TO W-VK-DEC-COUNT                                  
248696              END-IF                                                      
248697           ELSE                                                           
248698              MOVE NEJ                 TO INDATA-SW                       
248699           END-IF                                                         
248700           IF W-SPAR-VKORDBTO7 IS NUMERIC                                 
248701              OR W-SPAR-VKORDBTO7 = ',' OR ' ' OR '.'                     
248702              IF W-SPAR-VKORDBTO7 IS NUMERIC                              
248703                 ADD 1 TO W-VK-NUM-COUNT                                  
248704              END-IF                                                      
248705              IF W-SPAR-VKORDBTO7 = ',' OR '.'                            
248706                 ADD 1 TO W-VK-DEC-COUNT                                  
248707              END-IF                                                      
248708           ELSE                                                           
248709              MOVE NEJ                 TO INDATA-SW                       
248710           END-IF                                                         
248711           IF W-SPAR-VKORDBTO8 IS NUMERIC                                 
248712              OR W-SPAR-VKORDBTO8 = ',' OR ' ' OR '.'                     
248713              IF W-SPAR-VKORDBTO8 IS NUMERIC                              
248714                 ADD 1 TO W-VK-NUM-COUNT                                  
248715              END-IF                                                      
248716              IF W-SPAR-VKORDBTO8 = ',' OR '.'                            
248717                 ADD 1 TO W-VK-DEC-COUNT                                  
248718              END-IF                                                      
248719           ELSE                                                           
248720              MOVE NEJ                 TO INDATA-SW                       
248721           END-IF                                                         
248722                                                                          
248723           IF (W-VK-DEC-COUNT > 1 OR W-VK-NUM-COUNT = 0)                  
248724              OR (W-VK-DEC-COUNT = 1 AND W-VK-NUM-COUNT = 1)              
248725              MOVE NEJ                 TO INDATA-SW                       
248726           END-IF                                                         
248727                                                                          
248728           IF INDATA-FEL                                                  
248729******** ADD ERROR MESSAGE THAT WIGHT  IS > 0                             
248730              MOVE 'WEIGHT'            TO RESP-IDELMT-ERROR               
248731              MOVE '023'               TO RESP-IDMSG-ERROR                
248732                                   RESP-IDMSG-ERROR-LINE(INDX)            
248733           END-IF                                                         
248734        END-IF                                                            
248735        IF INDATA-OK                                                      
248736         IF W-SPAR-VLORDBTO > ' '                                         
248737           IF W-SPAR-VLORDBTO1 IS NUMERIC                                 
248738              OR W-SPAR-VLORDBTO1 = ',' OR ' ' OR '.'                     
248739              CONTINUE                                                    
248740           ELSE                                                           
248741              MOVE NEJ                 TO INDATA-SW                       
248742           END-IF                                                         
248743           IF W-SPAR-VLORDBTO2 IS NUMERIC                                 
248744              OR W-SPAR-VLORDBTO2 = ',' OR ' ' OR '.'                     
248745              CONTINUE                                                    
248746           ELSE                                                           
248747              MOVE NEJ                 TO INDATA-SW                       
248748           END-IF                                                         
248749           IF W-SPAR-VLORDBTO3 IS NUMERIC                                 
248750              OR W-SPAR-VLORDBTO3 = ',' OR ' ' OR '.'                     
248751              CONTINUE                                                    
248752           ELSE                                                           
248753              MOVE NEJ                 TO INDATA-SW                       
248754           END-IF                                                         
248755           IF W-SPAR-VLORDBTO4 IS NUMERIC                                 
248756              OR W-SPAR-VLORDBTO4 = ',' OR ' ' OR '.'                     
248757              CONTINUE                                                    
248758           ELSE                                                           
248759              MOVE NEJ                 TO INDATA-SW                       
248760           END-IF                                                         
248761           IF W-SPAR-VLORDBTO5 IS NUMERIC                                 
248762              OR W-SPAR-VLORDBTO5 = ',' OR ' ' OR '.'                     
248763              CONTINUE                                                    
248764           ELSE                                                           
248765              MOVE NEJ                 TO INDATA-SW                       
248766           END-IF                                                         
248767           IF W-SPAR-VLORDBTO6 IS NUMERIC                                 
248768              OR W-SPAR-VLORDBTO6 = ',' OR ' ' OR '.'                     
248769              CONTINUE                                                    
248770           ELSE                                                           
248771              MOVE NEJ                 TO INDATA-SW                       
248772           END-IF                                                         
248773           IF W-SPAR-VLORDBTO7 IS NUMERIC                                 
248774              OR W-SPAR-VLORDBTO7 = ',' OR ' ' OR '.'                     
248775              CONTINUE                                                    
248776           ELSE                                                           
248777              MOVE NEJ                 TO INDATA-SW                       
248778           END-IF                                                         
248779           IF W-SPAR-VLORDBTO8 IS NUMERIC                                 
248780              OR W-SPAR-VLORDBTO8 = ',' OR ' ' OR '.'                     
248781              CONTINUE                                                    
248782           ELSE                                                           
248783              MOVE NEJ                 TO INDATA-SW                       
248784           END-IF                                                         
248785                                                                          
248786           IF INDATA-FEL                                                  
248787******** ADD ERROR MESSAGE THAT WIGHT  IS > 0                             
248788              MOVE 'VOLUME'            TO RESP-IDELMT-ERROR               
248789              MOVE '023'               TO RESP-IDMSG-ERROR                
248790                                   RESP-IDMSG-ERROR-LINE(INDX)            
248791           END-IF                                                         
248792         END-IF                                                           
248793        END-IF                                                            
248794     END-IF                                                               
248795                                                                          
248800**         IF W-SPAR-VLORDBTO IS NUMERIC                                  
249102**                                    AND W-SPAR-VLORDBTO > 0             
250002**            CONTINUE                                                    
250003**         ELSE                                                           
250004**            MOVE NEJ                 TO INDATA-SW                       
250102**            MOVE 'VOL'               TO RESP-IDELMT-ERROR               
250103******** ADD ERROR MESSAGE THAT VOLUME IS > 0                             
250302**            MOVE '023'               TO RESP-IDMSG-ERROR                
250303**                                 RESP-IDMSG-ERROR-LINE(INDX)            
250802**         END-IF                                                         
251002**      ELSE                                                              
251003**            MOVE NEJ                 TO INDATA-SW                       
251004**            MOVE 'WEI'               TO RESP-IDELMT-ERROR               
251005******** ADD ERROR MESSAGE THAT WEIGHT IS > 0                             
251006**            MOVE '023'               TO RESP-IDMSG-ERROR                
251007**                                 RESP-IDMSG-ERROR-LINE(INDX)            
251102**      END-IF                                                            
251103**   END-IF                                                               
251104     .                                                                    
251105     EJECT                                                                
251106                                                                          
251107 GBA-KOLLA-VALT-RETURTILLSTAND    SECTION.                                
251108                                                                          
251109     MOVE REQU-IDDISTR(INDX)       TO W-IDDISTR-FSEQ-MIN                  
251110                                      W-IDDISTR-FSEQ-MAX                  
251111                                      W-IDDISTR                           
251112     INSPECT REQU-IDKUNDNR(INDX) REPLACING LEADING SPACE BY ZERO          
251113     MOVE REQU-IDKUNDNR(INDX)      TO W-IDKUNDNR-FSEQ-MIN                 
251114                                      W-IDKUNDNR-FSEQ-MAX                 
251115                                      W-IDKUNDNR                          
251116     INSPECT REQU-IDRAPPNR(INDX) REPLACING LEADING SPACE BY ZERO          
251117     MOVE REQU-IDRAPPNR(INDX)      TO W-IDRAPPNR-FSEQ-MIN                 
251118                                      W-IDRAPPNR-FSEQ-MAX                 
251119                                      W-IDRAPPNR                          
251120     PERFORM IMS-GU-WDA201                                                
251121     MOVE ANM-IDDC-RET          TO W-IDDC-FSEQ-MIN                        
251122                                   W-IDDC-FSEQ-MAX                        
251123     PERFORM IMS-GU-SEQF-WLRETA01                                         
251124     IF SEGMENT-FINNS                                                     
251125        PERFORM UNTIL SEGMENT-SAKNAS                                      
251126           IF RET-KDRETSTA         NOT = W-SND-REG OR                     
251127              RET-IDKOLLI          >  ZERO                                
251128              MOVE NEJ             TO INDATA-SW                           
251129              MOVE 'CMD'           TO RESP-IDELMT-ERROR                   
251130              MOVE '023'           TO RESP-IDMSG-ERROR                    
251131                                      RESP-IDMSG-ERROR-LINE(INDX)         
251132           END-IF                                                         
251133           PERFORM IMS-GN-SEQF-WLRETA01                                   
251134        END-PERFORM                                                       
251135     ELSE                                                                 
251136        MOVE NEJ                    TO INDATA-SW                          
251137        MOVE 'IDRT-IDRTLOP'         TO RESP-IDELMT-ERROR                  
251138        MOVE '041'                  TO RESP-IDMSG-ERROR                   
251139                                       RESP-IDMSG-ERROR-LINE(INDX)        
251140     END-IF                                                               
251141     .                                                                    
251142     EJECT                                                                
251143                                                                          
251144 H-UPPDATERA SECTION.                                                     
251145                                                                          
251146     PERFORM HA-TAG-BORT-VALDA-TILLSTAND                                  
251147                                                                          
251148     IF W-UPDATE-OK                                                       
251149        MOVE '003'               TO RESP-IDMSG-INFO                       
251150     ELSE                                                                 
251151        MOVE '006'               TO RESP-IDMSG-INFO                       
251152     END-IF                                                               
251153     .                                                                    
251154     EJECT                                                                
251155                                                                          
251156 HA-TAG-BORT-VALDA-TILLSTAND SECTION.                                     
251157                                                                          
251158     MOVE +1                       TO INDX                                
251159     MOVE REQU-KVRADER                 TO WS-INDX-REC                     
251160     MOVE NEJ                          TO WS-REC-LIMIT                    
251161     MOVE NEJ                          TO W-UPDATE-SW                     
251162     PERFORM UNTIL INDX         >  MAX-INDX OR REC-LIMIT                  
251164                                                                          
251165        IF REQU-FLCMD(INDX)        = 'J' OR 'Y' OR 'D'                    
251166           MOVE REQU-IDDISTR(INDX)  TO W-IDDISTR                          
251167           MOVE REQU-IDKUNDNR(INDX) TO W-IDKUNDNR                         
251168           MOVE REQU-IDRAPPNR(INDX) TO W-IDRAPPNR                         
251169           PERFORM IMS-GU-WDA201                                          
251170                                                                          
251171           MOVE LOW-VALUE           TO W-WDA3F1KY-MIN-X                   
251172           MOVE HIGH-VALUE          TO W-WDA3F1KY-MAX-X                   
251173           MOVE ANM-IDDC-RET        TO W-IDDC-F1-MIN                      
251174                                       W-IDDC-F1-MAX                      
251175           MOVE REQU-IDDISTR(INDX)  TO W-IDDISTR-F1-MIN                   
251176                                       W-IDDISTR-F1-MAX                   
251177           MOVE REQU-IDKUNDNR(INDX) TO W-IDKUNDNR-F1-MIN                  
251178                                       W-IDKUNDNR-F1-MAX                  
251179           MOVE REQU-IDRAPPNR(INDX) TO W-IDRAPPNR-F1-MIN                  
251180                                       W-IDRAPPNR-F1-MAX                  
251181           PERFORM IMS-GU-WLRETG01                                        
251182           IF SEGMENT-FINNS                                               
251183              MOVE ANM-IDDC-RET    TO W-IDDC                              
251184              MOVE SEQF-DAREGDAT   TO W-DAREGDAT                          
251185              MOVE SEQF-TIKLOCK    TO W-TIKLOCK                           
251186              PERFORM IMS-GHU-WLRETA01                                    
251187              PERFORM IMS-DLET-WLRETA01                                   
251188              MOVE JA              TO W-UPDATE-SW                         
251191           ELSE                                                           
251192              CALL FELLOG                                                 
251193           END-IF                                                         
251194        END-IF                                                            
251195        IF INDX = WS-INDX-REC                                             
251196           MOVE JA TO WS-REC-LIMIT                                        
251197        ELSE                                                              
251198           ADD +1          TO INDX                                        
251199        END-IF                                                            
251200     END-PERFORM                                                          
251201     .                                                                    
251202     EJECT                                                                
251203                                                                          
251204 C-PROFORMA-HEADER SECTION.                                               
251205                                                                          
251206                                                                          
251207**   IF WRITE-HEADER                                                      
251208       MOVE 'PUT'                   TO SEND-KDFUNC                        
251209       MOVE LENGTH OF E-RUBRIK-RAD  TO SEND-KVDLEN                        
251210       CALL WZ01SEND             USING SEND-CONTROL-AREA                  
251211                                       SEND-KVDLEN                        
251212                                       E-RUBRIK-RAD                       
251213                                                                          
251214       PERFORM S40-SEND-CHECK                                             
251215                                                                          
251216       MOVE 'PUT'                     TO SEND-KDFUNC                      
251217       MOVE LENGTH OF E-U-RUBRIK-RAD1 TO SEND-KVDLEN                      
251218       CALL WZ01SEND             USING   SEND-CONTROL-AREA                
251219                                         SEND-KVDLEN                      
251220                                         E-U-RUBRIK-RAD1                  
251221                                                                          
251222       PERFORM S40-SEND-CHECK                                             
251223                                                                          
251224       MOVE 'PUT'                     TO SEND-KDFUNC                      
251225       MOVE LENGTH OF E-U-RUBRIK-RAD2 TO SEND-KVDLEN                      
251226       CALL WZ01SEND             USING   SEND-CONTROL-AREA                
251227                                         SEND-KVDLEN                      
251228                                         E-U-RUBRIK-RAD2                  
251229                                                                          
251230       PERFORM S40-SEND-CHECK                                             
251231                                                                          
251232       MOVE 'PUT'                     TO SEND-KDFUNC                      
251233       MOVE LENGTH OF E-U-RUBRIK-RAD3 TO SEND-KVDLEN                      
251234       CALL WZ01SEND             USING   SEND-CONTROL-AREA                
251235                                         SEND-KVDLEN                      
251236                                         E-U-RUBRIK-RAD3                  
251237                                                                          
251238       PERFORM S40-SEND-CHECK                                             
251239                                                                          
251240       MOVE 'PUT'                     TO SEND-KDFUNC                      
251241       MOVE LENGTH OF E-U-RUBRIK-RAD4 TO SEND-KVDLEN                      
251242       CALL WZ01SEND             USING   SEND-CONTROL-AREA                
251243                                         SEND-KVDLEN                      
251244                                         E-U-RUBRIK-RAD4                  
251245                                                                          
251246       PERFORM S40-SEND-CHECK                                             
251247                                                                          
251248       MOVE 'PUT'                     TO SEND-KDFUNC                      
251249       MOVE LENGTH OF E-U-RUBRIK-RAD5 TO SEND-KVDLEN                      
251250       CALL WZ01SEND             USING   SEND-CONTROL-AREA                
251251                                         SEND-KVDLEN                      
251252                                         E-U-RUBRIK-RAD5                  
251253                                                                          
251254       PERFORM S40-SEND-CHECK                                             
251255                                                                          
251256       MOVE 'PUT'                     TO SEND-KDFUNC                      
251257       MOVE LENGTH OF E-U-RUBRIK-RAD6 TO SEND-KVDLEN                      
251258       CALL WZ01SEND             USING   SEND-CONTROL-AREA                
251259                                         SEND-KVDLEN                      
251260                                         E-U-RUBRIK-RAD6                  
251261                                                                          
251262       PERFORM S40-SEND-CHECK                                             
251270                                                                          
251502**     MOVE 'PUT'                     TO SEND-KDFUNC                      
251602**     MOVE LENGTH OF E-U-RUBRIK-RAD6A TO SEND-KVDLEN                     
251603**     CALL WZ01SEND             USING   SEND-CONTROL-AREA                
251604**                                       SEND-KVDLEN                      
251605**                                       E-U-RUBRIK-RAD6A                 
251606**                                                                        
251802**     PERFORM S40-SEND-CHECK                                             
251803                                                                          
251804       MOVE 'PUT'                     TO SEND-KDFUNC                      
251805       MOVE LENGTH OF E-U-RUBRIK-LINJE TO SEND-KVDLEN                     
251806       CALL WZ01SEND             USING   SEND-CONTROL-AREA                
251807                                         SEND-KVDLEN                      
251808                                         E-U-RUBRIK-LINJE                 
251809                                                                          
251810       PERFORM S40-SEND-CHECK                                             
251811                                                                          
251812**     MOVE 'PUT'                     TO SEND-KDFUNC                      
251813**     MOVE LENGTH OF E-U-RUBRIK-RAD6C TO SEND-KVDLEN                     
251814**     CALL WZ01SEND             USING   SEND-CONTROL-AREA                
251815**                                       SEND-KVDLEN                      
251816**                                       E-U-RUBRIK-RAD6C                 
251817**                                                                        
251902**     PERFORM S40-SEND-CHECK                                             
251903     .                                                                    
251904     EJECT                                                                
251905                                                                          
251906 D-HANDLE-ROW SECTION.                                                    
251907     SKIP2                                                                
251909       PERFORM DB-GET-DESCRIPTION                                         
251910       PERFORM IMS-GU-WDK611                                              
251920** NEW PRICE CALCULATIONS!! - START                                       
252102**   IF ANM-KDVALISO = 'SEK'                                              
252103** NO CONVERSATION NEEDED!!                                               
252104        MOVE ZERO  TO WS-PRFKTUTL-ROW                                     
252105        IF LEV-PRARTBTO > 0 OR LEV-PRARTBTO-LOC > 0                       
252106           IF LEV-PRARTBTO > 0                                            
252107              MOVE  LEV-PRARTBTO TO WS-PRFKTUTL                           
252108           ELSE                                                           
252109              IF LEV-PRARTBTO-LOC > 0                                     
252110                 MOVE LEV-PRARTBTO-LOC                                    
252111                                 TO WS-PRFKTUTL                           
252112              END-IF                                                      
252113           END-IF                                                         
252114                                                                          
252115           COMPUTE WS-PRFKTUTL-ROW = WS-PRFKTUTL *                        
252116                               LEV-KVLEVANM                               
252117        ELSE                                                              
252118                                                                          
252121              MOVE WDK611-CLAG-PRARTSJK  TO WS-PRFKTUTL                   
252122                                                                          
252123              IF DIST79-DEALER-PRICE                                      
252124                 COMPUTE WS-PRFKTUTL = WS-PRFKTUTL * 4                    
252125                                                                          
252126              ELSE                                                        
252127                 COMPUTE WS-PRFKTUTL = WS-PRFKTUTL * 2                    
252128                                                                          
252129              END-IF                                                      
252130                                                                          
252131              IF ANM-KDVALISO NOT = 'SEK'                                 
252132                 MOVE W-DATE-AAMM           TO CURR-TIAAMM                
252133                 MOVE 'SEK'                 TO CURR-KDVALISO-HUV          
252134                 MOVE 'M'                   TO CURR-KDVALTYP              
252135                 MOVE ANM-KDVALISO          TO CURR-KDVALISO-ROW          
252136                 PERFORM DD-GET-PRIS-SALES-COST                           
252137              END-IF                                                      
252138                                                                          
252139              IF WS-PRFKTUTL NOT > 0                                      
252140                 MOVE 00.01 TO WS-PRFKTUTL                                
252141              END-IF                                                      
252142                                                                          
252144              COMPUTE WS-PRFKTUTL-ROW = WS-PRFKTUTL * LEV-KVLEVANM        
252150        END-IF                                                            
252151                                                                          
252152     MOVE WS-PRFKTUTL-ROW              TO E-LIST-PRFKTUTL-RAD             
252153                                      LISTA-PRFKTUTL-RAD (D-INDX)         
252154     ADD  WS-PRFKTUTL-ROW              TO WS-SUFKTUTL-TOT                 
252155** NEW PRICE CALCULATIONS!! - END                                         
252156       MOVE WDK611-CLAG-VKART              TO WS-VKART                    
252157       COMPUTE WS-VKART-UNIT  = WS-VKART / 1000                           
252158       COMPUTE WS-VKART-TOT ROUNDED =                                     
252159               WS-VKART-UNIT * LEV-KVLEVANM                               
252160       IF WS-VKART-TOT = 0                                                
252161          MOVE 0.01 TO WS-VKART-TOT                                       
252162       END-IF                                                             
252163                                                                          
252164       COMPUTE WS-SUVKART-TOT =                                           
252165               WS-SUVKART-TOT + WS-VKART-TOT                              
252166       MOVE WS-VKART-TOT  TO LISTA-VKART-TOT (D-INDX)                     
252167                             E-LIST-VKART-TOT                             
252168       MOVE WDK611-CLAG-KDARTURS    TO LISTA-KDARTURS (D-INDX)            
252169                                       E-LIST-KDARTURS                    
252170       MOVE WDK611-CLAG-IDSTATNR(3) TO LISTA-IDSTATNR (D-INDX)            
252171                                       E-LIST-IDSTATNR                    
252172       MOVE LEV-IDARTNR             TO LISTA-IDARTNR (D-INDX)             
252173                                       E-LIST-IDARTNR                     
252174       MOVE LEV-KVLEVANM            TO LISTA-KVLEVART (D-INDX)            
252175                                       E-LIST-KVLEVART                    
252176       MOVE SEQF-IDRAPPNR            TO LISTA-IDRAPPNR (D-INDX)           
252177                                        E-LIST-IDRAPPNR                   
252178*      MOVE W-IDKOLLI               TO LISTA-IDKOLLI  (D-INDX)            
252179*                                      E-LIST-IDKOLLI                     
252180                                                                          
252181                                                                          
252182       ADD +1 TO D-INDX                                                   
252183       ADD  1 TO WS-COUNT-LINES                                           
252184                                                                          
252185       MOVE E-U-RAD TO UT-RAD                                             
252186       PERFORM I30-PRINT-LINES                                            
252187       MOVE SPACE TO UT-RAD                                               
252188     .                                                                    
252189     EJECT                                                                
252231 DD-GET-PRIS-SALES-COST  SECTION.                                         
252232                                                                          
252233       CALL W510CURR USING CURR-W510CURR 9305-PCB                         
252234       IF CURR-KDSVAR = ' '                                               
252235       AND CURR-PRKURS-NEW NOT = 1.0                                      
252236         MOVE ZERO                      TO CURR-SUORDV-IN                 
252237                                           CURR-PRARTVNA-IN               
252238                                           CURR-PRARTSTD-IN               
252239                                           CURR-PRKURS-02                 
252240         MOVE SPACE                     TO CURR-KDVALISO-02               
252241         MOVE CURR-PRKURS-NEW           TO CURR-PRKURS                    
252242         MOVE WS-PRFKTUTL               TO CURR-PRARTSJK-IN               
252243         MOVE CURR-KDVALISO-ROW         TO CURR-KDVALISO-01               
252244         MOVE +2                        TO CURR-KDCALL                    
252245         CALL W335CURR            USING CURR-W335CURR                     
252246         MOVE CURR-PRARTSJK-UT          TO WS-PRFKTUTL                    
252247       END-IF                                                             
252248     .                                                                    
252249     EJECT                                                                
252250                                                                          
252251 DB-GET-DESCRIPTION SECTION.                                              
252252     SKIP2                                                                
252253     MOVE LEV-IDARTNR          TO W-IDARTNR                               
252254     PERFORM IMS-GU-WDD311                                                
252255     IF SEGMENT-FINNS                                                     
252256       MOVE WDD311-TEXT-BEART TO LISTA-BEART (D-INDX)                     
252257                                 E-LIST-BEART                             
252258     ELSE                                                                 
252259       MOVE SPACE             TO LISTA-BEART (D-INDX)                     
252260                                 E-LIST-BEART                             
252261     END-IF                                                               
252262     .                                                                    
252263     EJECT                                                                
252264 I10-WDA3-LINES  SECTION.                                                 
252265**   MOVE REQU-IDRT-KEY     TO W-IDRT-TRANSIT-ISEQ                        
252266**   MOVE W-KDRETSTA-NUM    TO W-KDRETSTA-ISEQ                            
252267     MOVE LOW-VALUE           TO W-WDA3F1KY-MIN-X                         
252268     MOVE HIGH-VALUE          TO W-WDA3F1KY-MAX-X                         
252269     MOVE REQU-IDRT-KEY     TO W-IDRT                                     
252270     MOVE W-KDRETSTA-NUM    TO W-KDRETSTA1                                
252271     MOVE +1 TO D-INDX                                                    
252272     MOVE +1 TO S-INDX                                                    
252273     MOVE +0 TO WS-COUNT-LINES                                            
252274     MOVE +0 TO WS-MAX-S-INDX                                             
252275     MOVE REQU-IDRTLOP      TO W-IDRTLOP                                  
252276     PERFORM IMS-GN-WLRETG01                                              
252277     IF SEGMENT-SAKNAS                                                    
252278        MOVE '041'              TO RESP-IDMSG-ERROR                       
252279        MOVE 'IDRT-IDRTLOP'     TO RESP-IDELMT-ERROR                      
252280        SET INDATA-FEL          TO TRUE                                   
252281     ELSE                                                                 
252282             MOVE SEQF-IDDISTR  TO W-IDDISTR                              
252283                                   DIST79-IDDISTR                         
252284             MOVE SEQF-IDKUNDNR TO W-IDKUNDNR                             
252285             MOVE SEQF-IDRAPPNR TO W-IDRAPPNR                             
252286             MOVE 'J'           TO FIRST-WDA3F-SW                         
252287**           PERFORM IMS-GU-WDA201                                        
252288                                                                          
252289       PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                         
252290             MOVE SEQF-IDKOLLI  TO W-IDKOLLI                              
252291                                  WS-IDKOLLI (S-INDX)                     
252292             ADD +1 TO S-INDX                                             
252293             ADD +1 TO WS-MAX-S-INDX                                      
252294             IF SEQF-IDDISTR = W-IDDISTR    AND                           
252295                SEQF-IDKUNDNR =  W-IDKUNDNR AND                           
252296                SEQF-IDRAPPNR =  W-IDRAPPNR AND                           
252297                NOT-FIRST-WDA3F                                           
252298                CONTINUE                                                  
252299             ELSE                                                         
252300                                                                          
252301                MOVE SEQF-IDDISTR  TO W-IDDISTR                           
252302                                     W-IDDISTR-3                          
252303                                     DIST79-IDDISTR                       
252304                MOVE SEQF-IDKUNDNR TO W-IDKUNDNR                          
252305                MOVE SEQF-IDRAPPNR TO W-IDRAPPNR                          
252306                                                                          
252307                PERFORM IMS-GU-WDA201                                     
252308                MOVE ANM-KDVALISO  TO LIST-KDVALISO                       
252309                                      E-LIST-KDVALISO                     
252310                PERFORM IMS-GNP-WDA211                                    
252311                IF SEGMENT-SAKNAS                                         
252312                   CONTINUE                                               
252313                ELSE                                                      
252314                   PERFORM UNTIL SEGMENT-SAKNAS                           
252315                      PERFORM D-HANDLE-ROW                                
252316                      PERFORM IMS-GNP-WDA211                              
252317                   END-PERFORM                                            
252318                END-IF                                                    
252319             END-IF                                                       
252320          MOVE 'N'    TO FIRST-WDA3F-SW                                   
252321          PERFORM IMS-GN-WLRETG01                                         
252322       END-PERFORM                                                        
252323          PERFORM F01-COUNT-IDKOLLI                                       
252324          PERFORM F10-LAST-LINES                                          
252325     END-IF                                                               
252326     .                                                                    
252327     EJECT                                                                
252328                                                                          
252329 I30-PRINT-LINES  SECTION.                                                
252330                                                                          
252331     IF FIRST-LINE                                                        
252332        PERFORM S10-SEND-OPEN                                             
252333        PERFORM S11-PUT-HEADER                                            
252334        PERFORM S101-PUT-HEADER-TITLE                                     
252335        PERFORM C-PROFORMA-HEADER                                         
252336        PERFORM S12-PUT-LINE                                              
252340        MOVE NEJ TO FIRST-SW                                              
252400     END-IF                                                               
252502**   MOVE MOD-BEART (IX)          TO CSV-BEART                            
252503**   MOVE MOD-KVLS-91 (IX)        TO CSV-KVLS-91                          
252504**   MOVE MOD-KVLS-REM (IX)       TO CSV-KVLS-REM                         
252505**   MOVE MOD-KVBYTPKO (IX)       TO CSV-KVBYTPKO                         
252506**   MOVE MOD-KVBEART-UPD (IX)    TO CSV-KVBEART-UPD                      
252802**   MOVE MOD-KVANTAL-PREADV (IX) TO CSV-KVANTAL-PREADV                   
252902**   MOVE MOD-KVAVROP (IX)        TO CSV-KVAVROP                          
253002**   MOVE MOD-KVAVROP-GAM (IX)    TO CSV-KVAVROP-GAM                      
253003**   MOVE MOD-IDORDER (IX)        TO CSV-IDORDER                          
253004**   MOVE MOD-DAORDDAT (IX)       TO CSV-DAORDDAT                         
253102**   MOVE MOD-KVBEART (IX)        TO CSV-KVBEART                          
253103                                                                          
253104     PERFORM S12-PUT-LINE                                                 
253105     .                                                                    
253106     EJECT                                                                
253107 F01-COUNT-IDKOLLI SECTION.                                               
253108     SKIP2                                                                
253109                                                                          
253110     SORT WS-SOR-DATA DESCENDING WS-IDKOLLI                               
253111     MOVE +1 TO S-INDX                                                    
253112     ADD  +1 TO WS-COUNT-IDKOLLI                                          
253113     MOVE WS-IDKOLLI (S-INDX) TO WS-FIRST-IDKOLLI                         
253114     PERFORM UNTIL S-INDX > WS-MAX-S-INDX                                 
253115     IF WS-FIRST-IDKOLLI = WS-IDKOLLI (S-INDX)                            
253116** NO NEED TO ADD JUST KEEP THE SAME VALUE **                             
253117        COMPUTE WS-COUNT-IDKOLLI = WS-COUNT-IDKOLLI + 0                   
253118     ELSE                                                                 
253119        MOVE WS-IDKOLLI (S-INDX) TO WS-FIRST-IDKOLLI                      
253120        ADD +1 TO WS-COUNT-IDKOLLI                                        
253121     END-IF                                                               
253122     ADD +1 TO S-INDX                                                     
253123     END-PERFORM                                                          
253124     MOVE WS-COUNT-IDKOLLI TO E-LIST-IDKOLLI-TOT                          
253125                              LIST-IDKOLLI                                
253126     .                                                                    
253127     EJECT                                                                
253128 F10-LAST-LINES SECTION.                                                  
253129     SKIP2                                                                
253130     MOVE WS-SUFKTUTL-TOT  TO LIST-SUFKTUTL-TOT                           
253131                              E-LIST-SUFKTUTL-TOT                         
253132     MOVE WS-SUVKART-TOT   TO LIST-SUVKART-TOT                            
253133                              E-LIST-SUVKART-TOT                          
253134     MOVE 'KG '            TO LIST-UOM-TWEIGHT                            
253135     MOVE ' KG '           TO E-LIST-UOM-TWEIGHT                          
253136                                                                          
253137     MOVE 'PUT'                      TO SEND-KDFUNC                       
253138     MOVE LENGTH OF E-U-RUBRIK-LINJE TO SEND-KVDLEN                       
253139     CALL WZ01SEND             USING    SEND-CONTROL-AREA                 
253140                                        SEND-KVDLEN                       
253141                                        E-U-RUBRIK-LINJE                  
253142                                                                          
253143     PERFORM S40-SEND-CHECK                                               
253144                                                                          
253145     MOVE 'PUT'                      TO SEND-KDFUNC                       
253146     MOVE LENGTH OF E-SISTA-RADEN1   TO SEND-KVDLEN                       
253147     CALL WZ01SEND             USING    SEND-CONTROL-AREA                 
253148                                        SEND-KVDLEN                       
253149                                        E-SISTA-RADEN1                    
253150                                                                          
253151     PERFORM S40-SEND-CHECK                                               
253152                                                                          
253153     MOVE 'PUT'                      TO SEND-KDFUNC                       
253154     MOVE LENGTH OF E-SISTA-RADEN2   TO SEND-KVDLEN                       
253155     CALL WZ01SEND             USING    SEND-CONTROL-AREA                 
253156                                        SEND-KVDLEN                       
253157                                        E-SISTA-RADEN2                    
253158                                                                          
253159     PERFORM S40-SEND-CHECK                                               
253160                                                                          
253161     MOVE 'PUT'                      TO SEND-KDFUNC                       
253162     MOVE LENGTH OF E-SISTA-RADEN3   TO SEND-KVDLEN                       
253163     CALL WZ01SEND             USING    SEND-CONTROL-AREA                 
253164                                        SEND-KVDLEN                       
253165                                        E-SISTA-RADEN3                    
253166                                                                          
253167     PERFORM S40-SEND-CHECK                                               
253168                                                                          
253169     MOVE 'PUT'                      TO SEND-KDFUNC                       
253170     MOVE LENGTH OF E-SISTA-RADEN4   TO SEND-KVDLEN                       
253171     CALL WZ01SEND             USING    SEND-CONTROL-AREA                 
253172                                        SEND-KVDLEN                       
253173                                        E-SISTA-RADEN4                    
253174                                                                          
253175     PERFORM S40-SEND-CHECK                                               
253176                                                                          
253177     MOVE 'PUT'                      TO SEND-KDFUNC                       
253178     MOVE LENGTH OF E-SISTA-RADEN5   TO SEND-KVDLEN                       
253179     CALL WZ01SEND             USING    SEND-CONTROL-AREA                 
253180                                        SEND-KVDLEN                       
253181                                        E-SISTA-RADEN5                    
253182                                                                          
253183     PERFORM S40-SEND-CHECK                                               
253184                                                                          
253185     MOVE 'PUT'                      TO SEND-KDFUNC                       
253186     MOVE LENGTH OF E-SISTA-RADEN6   TO SEND-KVDLEN                       
253187     CALL WZ01SEND             USING    SEND-CONTROL-AREA                 
253188                                        SEND-KVDLEN                       
253189                                        E-SISTA-RADEN6                    
253190                                                                          
253191     PERFORM S40-SEND-CHECK                                               
253192                                                                          
253193     .                                                                    
253194     EJECT                                                                
256402** DAP REPORTS FOR HEADER - DETAILS - FOORTER **                          
256403 F20-DAP-REPORTS SECTION.                                                 
256404     SKIP2                                                                
256405     PERFORM S10-SEND-OPEN                                                
256406     PERFORM F30-PUT-HEADER                                               
256407     PERFORM F40-PUT-DOC-HEAD                                             
256408     IF WS-COUNT-LINES > 0                                                
256409        MOVE +1 TO D-INDX                                                 
256410        PERFORM UNTIL D-INDX > WS-COUNT-LINES OR 500                      
256411         MOVE LISTA-IDARTNR (D-INDX)                                      
256412                     TO LIST-IDARTNR                                      
256413         MOVE LISTA-IDRAPPNR (D-INDX)                                     
256414                     TO LIST-IDRAPPNR                                     
256415         MOVE LISTA-KVLEVART (D-INDX)                                     
256416                     TO LIST-KVLEVART                                     
256417         MOVE LISTA-PRFKTUTL-RAD (D-INDX)                                 
256418                     TO LIST-PRFKTUTL-RAD                                 
256419         MOVE LISTA-IDSTATNR (D-INDX)                                     
256420                     TO LIST-IDSTATNR                                     
256421         MOVE LISTA-VKART-TOT (D-INDX)                                    
256422                     TO LIST-VKART-TOT                                    
256423         MOVE LISTA-BEART (D-INDX)                                        
256424                     TO LIST-BEART                                        
256425         MOVE LISTA-KDARTURS (D-INDX)                                     
256426                     TO LIST-KDARTURS                                     
256427          PERFORM F50-PUT-DOC-LINE                                        
256428          ADD +1 TO D-INDX                                                
256429        END-PERFORM                                                       
256430     END-IF                                                               
256431     PERFORM F60-PUT-DOC-FOOT                                             
256432     PERFORM S13-SEND-CLOSE                                               
256433                                                                          
256434     .                                                                    
256435     EJECT                                                                
256436 F30-PUT-HEADER SECTION.                                                  
256437     MOVE FUNCTION CURRENT-DATE(1:12)                                     
256438                TO WS-CURRENT-DATE-TIME                                   
256439     MOVE 1                       TO HDR-REQU-IDMSGVER                    
256440     MOVE SPACE                   TO HDR-REQU-KDPGMACT                    
256441     MOVE IDPGM                   TO HDR-REQU-IDUSER                      
256442     MOVE 'RETURN-DOCUMENT'       TO HDR-IDOUTTYPE                        
256443     MOVE REQU-IDDC-KEY              TO HDR-IDOUTREC(1:2)                 
256444     MOVE REQU-IDUSER IN REQU-AREA   TO HDR-IDOUTREC(3:8)                 
256445*    MOVE 'WL0159'                TO HDR-IDOUTREC                         
256446     MOVE WS-TIYYMMDDHHMM         TO HDR-IDLIST                           
256447     MOVE 'PUT'                   TO SEND-KDFUNC                          
256448     MOVE LENGTH OF HDR-AREA      TO SEND-KVDLEN                          
256449     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
256450                                     SEND-KVDLEN                          
256451                                     HDR-AREA                             
256452     IF SEND-KDRC > ZERO                                                  
256453       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
256454       STRING 'WZ01SEND GET  ERROR RC= ' KDRC-DISPLAY                     
256455       DELIMITED BY SIZE       INTO ERROR-TEXT                            
256456       DISPLAY ERROR-TEXT                                                 
256457       CALL FELLOG                                                        
256458     END-IF                                                               
256459     .                                                                    
256460     EJECT                                                                
256461*    --- DISPATCHER SECTIONS                                              
256462 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
256463                                                                          
256464     MOVE 'GETARG'               TO SUB-KDFUNC                            
256465     MOVE 'CARPARTS.LDC.EXTQUEUERETPERMITS'   TO SUB-ADDISPABS            
256466     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
256467                                                                          
256468     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
256469                                                                          
256470     IF SUB-KDRC > 0                                                      
256471       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
256472       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
256473       DELIMITED BY SIZE INTO ERROR-TEXT                                  
256474       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
256475     END-IF                                                               
256476     .                                                                    
256477     SKIP3                                                                
256478 S02-RETURN-RESPONSE SECTION.                                             
256479                                                                          
256480     MOVE 'RETURN'                   TO SUB-KDFUNC                        
256481     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
256482                                                                          
256483     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
256484                                                                          
256485     IF SUB-KDRC > 0                                                      
256486       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
256487       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
256488       DELIMITED BY SIZE INTO ERROR-TEXT                                  
256489       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
256490     END-IF                                                               
256491     .                                                                    
256492     EJECT                                                                
256493                                                                          
256494 S10-SEND-OPEN SECTION.                                                   
256495     MOVE 'CARPARTS.DAP.DISTRDOC' TO SEND-ADDISPABS                       
256496     MOVE 'OPEN'                  TO SEND-KDFUNC                          
256497     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
256498                                     SEND-OPEN-AREA                       
256499     IF SEND-KDRC > ZERO                                                  
256500       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
256501       STRING 'WZ01SEND OPEN ERROR RC= ' KDRC-DISPLAY                     
256502       DELIMITED BY SIZE INTO ERROR-TEXT                                  
256503       DISPLAY ERROR-TEXT                                                 
256504       CALL FELLOG                                                        
256505     END-IF                                                               
256506     .                                                                    
256507     EJECT                                                                
256508 S11-PUT-HEADER SECTION.                                                  
256509     MOVE FUNCTION CURRENT-DATE(1:12)                                     
256510                TO WS-CURRENT-DATE-TIME                                   
256511     MOVE 1                       TO HDR-REQU-IDMSGVER                    
256512     MOVE SPACE                   TO HDR-REQU-KDPGMACT                    
256513     MOVE IDPGM                   TO HDR-REQU-IDUSER                      
256514     MOVE 'WL0159-001'            TO HDR-IDOUTTYPE                        
256515     MOVE REQU-IDDC-KEY           TO HDR-IDOUTREC                         
256516*    MOVE 'WL0159'                TO HDR-IDOUTREC                         
256517     MOVE WS-TIYYMMDDHHMM         TO HDR-IDLIST                           
256518     MOVE 'PUT'                   TO SEND-KDFUNC                          
256519     MOVE LENGTH OF HDR-AREA      TO SEND-KVDLEN                          
256520     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
256521                                     SEND-KVDLEN                          
256522                                     HDR-AREA                             
256523     IF SEND-KDRC > ZERO                                                  
256524       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
256525       STRING 'WZ01SEND GET  ERROR RC= ' KDRC-DISPLAY                     
256526       DELIMITED BY SIZE       INTO ERROR-TEXT                            
256527       DISPLAY ERROR-TEXT                                                 
256528       CALL FELLOG                                                        
256529     END-IF                                                               
256530     .                                                                    
256531     EJECT                                                                
256532 S12-PUT-LINE SECTION.                                                    
256533     IF FIRST-LINE                                                        
256534        MOVE 'PUT'                   TO SEND-KDFUNC                       
256535        MOVE LENGTH OF E-U-RUBRIK-RAD7   TO SEND-KVDLEN                   
256536        CALL WZ01SEND             USING SEND-CONTROL-AREA                 
256537                                        SEND-KVDLEN                       
256538                                        E-U-RUBRIK-RAD7                   
256539     ELSE                                                                 
256540        MOVE 'PUT'                   TO SEND-KDFUNC                       
256541        MOVE LENGTH OF E-U-RAD       TO SEND-KVDLEN                       
256542        CALL WZ01SEND             USING SEND-CONTROL-AREA                 
256543                                        SEND-KVDLEN                       
256544                                        E-U-RAD                           
256545     END-IF                                                               
256546     IF SEND-KDRC > ZERO                                                  
256547       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
256548       STRING 'WZ01SEND GET  ERROR RC= ' KDRC-DISPLAY                     
256549       DELIMITED BY SIZE       INTO ERROR-TEXT                            
256550       DISPLAY ERROR-TEXT                                                 
256551       CALL FELLOG                                                        
256552     END-IF                                                               
256553     .                                                                    
256554     SKIP2                                                                
256555 S13-SEND-CLOSE SECTION.                                                  
256556     MOVE 'CLOSE'                TO SEND-KDFUNC                           
256557     CALL WZ01SEND            USING SEND-CONTROL-AREA                     
256558     IF SEND-KDRC > 0                                                     
256559       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
256560       STRING 'WZ01RECV CLOSE ERROR RC= ' KDRC-DISPLAY                    
256561       DELIMITED BY SIZE       INTO ERROR-TEXT                            
256562       DISPLAY ERROR-TEXT                                                 
256563       CALL FELLOG                                                        
256564     END-IF                                                               
256565     .                                                                    
256566     EJECT                                                                
256567 F40-PUT-DOC-HEAD SECTION.                                                
256568                                                                          
256569     MOVE  '1'                            TO LIST-IDAFPRCD-HEAD           
256570     MOVE 'PUT'                           TO SEND-KDFUNC                  
256571     MOVE LENGTH OF DOC-HEAD-AREA         TO SEND-KVDLEN                  
256572     CALL WZ01SEND USING SEND-CONTROL-AREA                                
256573                         SEND-KVDLEN                                      
256574                         DOC-HEAD-AREA                                    
256575     IF SEND-KDRC > ZERO                                                  
256576       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
256577       STRING 'WZ01SEND PUT  ERROR RC= ' KDRC-DISPLAY                     
256578       DELIMITED BY SIZE       INTO ERROR-TEXT                            
256579       DISPLAY ERROR-TEXT                                                 
256580       CALL FELLOG                                                        
256581     END-IF                                                               
256582     .                                                                    
256583     SKIP3                                                                
256584 F50-PUT-DOC-LINE SECTION.                                                
256585                                                                          
256586     MOVE  '2'                            TO LIST-IDAFPRCD-LINE           
256587     MOVE 'PUT'                           TO SEND-KDFUNC                  
256588     MOVE LENGTH OF DOC-LINE-AREA         TO SEND-KVDLEN                  
256589     CALL WZ01SEND USING SEND-CONTROL-AREA                                
256590                         SEND-KVDLEN                                      
256591                         DOC-LINE-AREA                                    
256592     IF SEND-KDRC > ZERO                                                  
256593       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
256594       STRING 'WZ01SEND PUT  ERROR RC= ' KDRC-DISPLAY                     
256595       DELIMITED BY SIZE       INTO ERROR-TEXT                            
256596       DISPLAY ERROR-TEXT                                                 
256597       CALL FELLOG                                                        
256598     END-IF                                                               
256599     .                                                                    
256600     EJECT                                                                
256601 F60-PUT-DOC-FOOT SECTION.                                                
256602                                                                          
256603     MOVE  '3'                            TO LIST-IDAFPRCD-FOOT           
256604     MOVE 'PUT'                           TO SEND-KDFUNC                  
256605     MOVE LENGTH OF DOC-FOOT-AREA         TO SEND-KVDLEN                  
256606     CALL WZ01SEND USING SEND-CONTROL-AREA                                
256607                         SEND-KVDLEN                                      
256608                         DOC-FOOT-AREA                                    
256609     IF SEND-KDRC > ZERO                                                  
256610       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
256611       STRING 'WZ01SEND PUT  ERROR RC= ' KDRC-DISPLAY                     
256612       DELIMITED BY SIZE       INTO ERROR-TEXT                            
256613       DISPLAY ERROR-TEXT                                                 
256614       CALL FELLOG                                                        
256615     END-IF                                                               
256616     .                                                                    
256617     EJECT                                                                
256618 S10-REDIGERA-MOD         SECTION.                                        
256619                                                                          
256620     MOVE RET-IDRT           TO RESP-IDRT     (INDX)                      
256621     MOVE RET-DAREGDAT (3:6) TO RESP-TIREGDAT (INDX)                      
256622     MOVE RET-DASNDDAT (3:6) TO RESP-TISNDDAT (INDX)                      
256623     MOVE RET-IDRTLOP        TO RESP-IDRTLOP  (INDX)                      
256624     MOVE RET-IDRT-TRANSIT   TO RESP-IDRT-TRANSIT (INDX)                  
256625     MOVE RET-TIREGDAT-TRRT  TO RESP-TIREGDAT-TRRT(INDX)                  
256626     MOVE RET-TISNDDAT-TRRT  TO RESP-TISNDDAT-TRRT(INDX)                  
256627     MOVE RET-IDDISTR        TO RESP-IDDISTR  (INDX)                      
256628     MOVE RET-IDKUNDNR       TO RESP-IDKUNDNR (INDX)                      
256629     MOVE RET-IDRAPPNR       TO RESP-IDRAPPNR (INDX)                      
256630     MOVE RET-KVRADER        TO RESP-KVRADER-LINE (INDX)                  
256631     MOVE RET-KVKOLLI-AAF    TO RESP-KVKOLLI-AAF  (INDX)                  
256632     .                                                                    
256633     EJECT                                                                
256634 S101-PUT-HEADER-TITLE SECTION.                                           
256635                                                                          
256636** MOVE FROM NEW REQU- FILEDS!                                            
256637     MOVE REQU-VLORDBTO          TO LIST-VLORDBTO-TOT                     
256638                                    E-LIST-VLORDBTO-TOT                   
256639     MOVE 'M³       '            TO LIST-UOM-VOLUME                       
256640     MOVE ' M³       '           TO E-LIST-UOM-VOLUME                     
256641                                                                          
256642     MOVE REQU-VKORDBTO          TO LIST-GROSS-WEIGHT                     
256643                                    E-LIST-GROSS-WEIGHT                   
256644     MOVE 'KG '                  TO LIST-UOM-WEIGHT                       
256645     MOVE ' KG '                 TO E-LIST-UOM-WEIGHT                     
256646                                                                          
256647**   MOVE LEV-IDDC               TO LIST-IDDC-SND                         
256648**                                  W-IDDC-WDB6                           
256649**                                  E-LIST-IDDC-SND                       
256650**                                                                        
256651**   MOVE RET-IDDISTR            TO                                       
256652**                                  LIST-IDDISTR                          
256653**                                  E-LIST-IDDISTR                        
256654**                                                                        
256655**   MOVE RET-IDDISTR            TO LIST-IDDISTR-SND                      
256656**                                  E-LIST-IDDISTR-SND                    
256657**                                  W-IDDISTR-2                           
256658                                                                          
256659     PERFORM BA-GET-CUSTOMER-INFO                                         
256660     .                                                                    
256661     EJECT                                                                
256662                                                                          
256663 BA-GET-CUSTOMER-INFO SECTION.                                            
256664                                                                          
256665     MOVE SPACE        TO  LIST-RECIV-ADR1                                
256666                           LIST-RECIV-ADR2                                
256667                           LIST-RECIV-GATA                                
256668                           LIST-RECIV-PADR                                
256669                           LIST-RECIV-LAND                                
256670                           LIST-SENDR-ADR1                                
256671                           LIST-SENDR-ADR2                                
256672                           LIST-SENDR-GATA                                
256673                           LIST-SENDR-PADR                                
256674                           LIST-SENDR-LAND                                
256675                                                                          
256676     MOVE SPACE        TO  E-LIST-RECIV-ADR1                              
256677                           E-LIST-RECIV-ADR2                              
256678                           E-LIST-RECIV-GATA                              
256679                           E-LIST-RECIV-PADR                              
256680                           E-LIST-RECIV-LAND                              
256681                           E-LIST-SENDR-ADR1                              
256682                           E-LIST-SENDR-ADR2                              
256683                           E-LIST-SENDR-GATA                              
256684                           E-LIST-SENDR-PADR                              
256685                           E-LIST-SENDR-LAND                              
256686                           E-LIST-IDRT                                    
256687                           E-LIST-IDRTLOP                                 
256688                                                                          
256689     MOVE 11           TO W-IDDC-WDB6                                     
256690*    MOVE REQU-IDDC-KEY  TO W-IDDC-WDB6                                   
256691     MOVE SEQF-IDRT     TO LIST-IDRT                                      
256692                           E-LIST-IDRT                                    
256693     MOVE SEQF-IDRTLOP  TO LIST-IDRTLOP                                   
256694                           E-LIST-IDRTLOP                                 
256695     PERFORM IMS-GET-WDB601                                               
256696     IF SEGMENT-FINNS                                                     
256697       MOVE WDB6-DCS-BEGMT-RAD1 TO   LIST-RECIV-ADR1                      
256698       MOVE WDB6-DCS-BEGMT-RAD2 TO   LIST-RECIV-ADR2                      
256699       MOVE WDB6-DCS-ADGMT-GATA TO   LIST-RECIV-GATA                      
256700       MOVE WDB6-DCS-ADGMT-PADR TO   LIST-RECIV-PADR                      
256701       MOVE WDB6-DCS-ADGMT-LAND TO   LIST-RECIV-LAND                      
256702                                                                          
256703       MOVE WDB6-DCS-BEGMT-RAD1 TO   E-LIST-RECIV-ADR1                    
256704       MOVE WDB6-DCS-BEGMT-RAD2 TO   E-LIST-RECIV-ADR2                    
256705       MOVE WDB6-DCS-ADGMT-GATA TO   E-LIST-RECIV-GATA                    
256706       MOVE WDB6-DCS-ADGMT-PADR TO   E-LIST-RECIV-PADR                    
256707       MOVE WDB6-DCS-ADGMT-LAND TO   E-LIST-RECIV-LAND                    
256708     ELSE                                                                 
256709       MOVE 'MISSING '          TO   LIST-RECIV-ADR1                      
256710       MOVE 'MISSING '          TO   LIST-RECIV-ADR2                      
256711       MOVE 'MISSING '          TO   LIST-RECIV-GATA                      
256712       MOVE 'MISSING '          TO   LIST-RECIV-PADR                      
256713       MOVE 'MISSING '          TO   LIST-RECIV-LAND                      
256714                                                                          
256715       MOVE 'MISSING '          TO   E-LIST-RECIV-ADR1                    
256716       MOVE 'MISSING '          TO   E-LIST-RECIV-ADR2                    
256717       MOVE 'MISSING '          TO   E-LIST-RECIV-GATA                    
256718       MOVE 'MISSING '          TO   E-LIST-RECIV-PADR                    
256719       MOVE 'MISSING '          TO   E-LIST-RECIV-LAND                    
256720                                                                          
256721     END-IF                                                               
256722                                                                          
256723**   MOVE  +0                    TO   WS-PAGE                             
256724     MOVE REQU-IDDC-KEY          TO   W-IDDC-WDB6                         
256725*    MOVE LEV-IDDC               TO   W-IDDC-WDB6                         
256726                                                                          
256727     PERFORM IMS-GET-WDB601                                               
256728     IF SEGMENT-FINNS                                                     
256729        MOVE WDB6-DCS-BEGMT-RAD1 TO   LIST-SENDR-ADR1                     
256730        MOVE WDB6-DCS-BEGMT-RAD2 TO   LIST-SENDR-ADR2                     
256731        MOVE WDB6-DCS-ADGMT-GATA TO   LIST-SENDR-GATA                     
256732        MOVE WDB6-DCS-ADGMT-PADR TO   LIST-SENDR-PADR                     
256733        MOVE WDB6-DCS-ADGMT-LAND TO   LIST-SENDR-LAND                     
256734                                                                          
256735        MOVE WDB6-DCS-BEGMT-RAD1 TO   E-LIST-SENDR-ADR1                   
256736        MOVE WDB6-DCS-BEGMT-RAD2 TO   E-LIST-SENDR-ADR2                   
256737        MOVE WDB6-DCS-ADGMT-GATA TO   E-LIST-SENDR-GATA                   
256738        MOVE WDB6-DCS-ADGMT-PADR TO   E-LIST-SENDR-PADR                   
256739        MOVE WDB6-DCS-ADGMT-LAND TO   E-LIST-SENDR-LAND                   
256740                                                                          
256741     ELSE                                                                 
256742        MOVE 'MISSING  '         TO   LIST-SENDR-ADR1                     
256743        MOVE 'MISSING  '         TO   LIST-SENDR-ADR2                     
256744        MOVE 'MISSING  '         TO   LIST-SENDR-GATA                     
256745        MOVE 'MISSING  '         TO   LIST-SENDR-PADR                     
256746        MOVE 'MISSING  '         TO   LIST-SENDR-LAND                     
256747                                                                          
256748        MOVE 'MISSING  '         TO   E-LIST-SENDR-ADR1                   
256749        MOVE 'MISSING  '         TO   E-LIST-SENDR-ADR2                   
256750        MOVE 'MISSING  '         TO   E-LIST-SENDR-GATA                   
256751        MOVE 'MISSING  '         TO   E-LIST-SENDR-PADR                   
256752        MOVE 'MISSING  '         TO   E-LIST-SENDR-LAND                   
256753                                                                          
256754     END-IF                                                               
256755     .                                                                    
256756     EJECT                                                                
256757 S40-SEND-CHECK SECTION.                                                  
256758     IF SEND-KDRC > ZERO                                                  
256759       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
256760       STRING 'WZ01SEND GET  ERROR RC= ' KDRC-DISPLAY                     
256761       DELIMITED BY SIZE       INTO ERROR-TEXT                            
256762       DISPLAY ERROR-TEXT                                                 
256763       CALL FELLOG                                                        
256764     END-IF                                                               
256765     .                                                                    
256766     SKIP2                                                                
256767                                                                          
256768* --- IMS SEKTIONER ---                                                   
256769 IMS-GU-WDA301         SECTION.                                           
256770                                                                          
256771     STRING 'WLRETA01(WDA301KY =' W-WDA301KY-X ')'                        
256772          DELIMITED BY SIZE INTO SSA1                                     
256773     MOVE '  GE'           TO GODK-STATUSKODER                            
256774     CALL CBLTDLI USING GU RETA1-PCB DLI-IO-AREA SSA1                     
256775     MOVE RETA1-STATUS-CODE TO STATUS-WS                                  
256776     PERFORM IMS-STATUSKONTROLL                                           
256777     .                                                                    
256778                                                                          
256779 IMS-GHU-WLRETA01       SECTION.                                          
256780                                                                          
256781     STRING 'WLRETA01(WDA301KY =' W-WDA301KY-X ')'                        
256782          DELIMITED BY SIZE INTO SSA1                                     
256783     MOVE '  GE'           TO GODK-STATUSKODER                            
256784     CALL CBLTDLI USING GHU RETA1-PCB DLI-IO-AREA SSA1                    
256785     MOVE RETA1-STATUS-CODE TO STATUS-WS                                  
256786     PERFORM IMS-STATUSKONTROLL                                           
256787     .                                                                    
256788                                                                          
256789 IMS-DLET-WLRETA01      SECTION.                                          
256790                                                                          
256791     MOVE '    '           TO GODK-STATUSKODER                            
256792     CALL CBLTDLI USING DLET RETA1-PCB DLI-IO-AREA                        
256793     MOVE RETA1-STATUS-CODE TO STATUS-WS                                  
256794     PERFORM IMS-STATUSKONTROLL                                           
256795     .                                                                    
256796     EJECT                                                                
256797                                                                          
256798 IMS-GU-SEQF-WLRETA01       SECTION.                                      
256799                                                                          
256800     STRING 'WLRETA01(WDA3FSEQ>=' W-WDA3FSEQ-MIN-X                        
256801                    '&WDA3FSEQ<=' W-WDA3FSEQ-MAX-X ')'                    
256802          DELIMITED BY SIZE INTO SSA1                                     
256803     MOVE '  GE'           TO GODK-STATUSKODER                            
256804     CALL CBLTDLI USING GU RETA2-PCB DLI-IO-AREA SSA1                     
256805     MOVE RETA2-STATUS-CODE TO STATUS-WS                                  
256806     PERFORM IMS-STATUSKONTROLL                                           
256807     .                                                                    
256808                                                                          
256809 IMS-GN-SEQF-WLRETA01       SECTION.                                      
256810                                                                          
256811     STRING 'WLRETA01(WDA3FSEQ>=' W-WDA3FSEQ-MIN-X                        
256812                    '&WDA3FSEQ<=' W-WDA3FSEQ-MAX-X ')'                    
256813          DELIMITED BY SIZE INTO SSA1                                     
256814     MOVE '  GEGB'         TO GODK-STATUSKODER                            
256815     CALL CBLTDLI USING GN RETA2-PCB DLI-IO-AREA SSA1                     
256816     MOVE RETA2-STATUS-CODE TO STATUS-WS                                  
256817     PERFORM IMS-STATUSKONTROLL                                           
256818     .                                                                    
256819                                                                          
256820                                                                          
256821 IMS-GU-WLRETG01       SECTION.                                           
256822                                                                          
256823     STRING 'WLRETG01(WDA3F1KY>=' W-WDA3F1KY-MIN-X                        
256824                    '&WDA3F1KY<=' W-WDA3F1KY-MAX-X ')'                    
256825          DELIMITED BY SIZE INTO SSA1                                     
256826     MOVE '  GE'           TO GODK-STATUSKODER                            
256827     CALL CBLTDLI USING GU RETG-PCB DLI-IO-AREA SSA1                      
256828     MOVE RETG-STATUS-CODE TO STATUS-WS                                   
256829     PERFORM IMS-STATUSKONTROLL                                           
256830     .                                                                    
256831                                                                          
256832 IMS-GN-WLRETG01       SECTION.                                           
256833                                                                          
256834     STRING 'WLRETG01(WDA3F1KY>=' W-WDA3F1KY-MIN-X                        
256835                    '&WDA3F1KY<=' W-WDA3F1KY-MAX-X                        
256836                    '&IDRT     =' W-IDRT                                  
256837                    '&IDRTLOP  =' W-IDRTLOP    ')'                        
256838          DELIMITED BY SIZE INTO SSA1                                     
256839     MOVE '  GEGB'         TO GODK-STATUSKODER                            
256840     CALL CBLTDLI USING GN RETG-PCB DLI-IO-AREA SSA1                      
256841     MOVE RETG-STATUS-CODE TO STATUS-WS                                   
256842     PERFORM IMS-STATUSKONTROLL                                           
256843     .                                                                    
256844                                                                          
256845 IMS-GU-WDA201               SECTION.                                     
256846                                                                          
256847     STRING 'WDA201  (IDLEVANM =' W-IDLEVANM-X ')'                        
256848          DELIMITED BY SIZE INTO SSA1                                     
256849     MOVE '    ' TO GODK-STATUSKODER                                      
256850     CALL CBLTDLI USING GU WDA2-PCB DLI-IO-AREA-WDA201 SSA1               
256851     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
256852     PERFORM IMS-STATUSKONTROLL                                           
256853     .                                                                    
256854                                                                          
256855 IMS-GNP-WDA211               SECTION.                                    
256856                                                                          
256857     STRING 'WDA201  *F(IDLEVANM =' W-IDLEVANM-X ')'                      
256858          DELIMITED BY SIZE INTO SSA1                                     
256859     MOVE 'WDA211  ' TO SSA2                                              
256860     MOVE '  GE' TO GODK-STATUSKODER                                      
256861     CALL CBLTDLI USING GN WDA2-PCB DLI-IO-AREA-WDA211 SSA1 SSA2          
256862     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
256863     PERFORM IMS-STATUSKONTROLL                                           
256864     .                                                                    
256865                                                                          
256866 IMS-GU-WDA301-H1            SECTION.                                     
256867                                                                          
256868     STRING 'WDA301  (WDA3HSEQ =' W-WDA3HSEQ-X ')'                        
256869          DELIMITED BY SIZE INTO SSA1                                     
256870     MOVE '  GE' TO GODK-STATUSKODER                                      
256871     CALL CBLTDLI USING GU RETA3-PCB DLI-IO-AREA SSA1                     
256872     MOVE RETA3-STATUS-CODE TO STATUS-WS                                  
256873     PERFORM IMS-STATUSKONTROLL                                           
256874     .                                                                    
256875                                                                          
256876 IMS-GN-WDA301-H1            SECTION.                                     
256877                                                                          
256878     STRING 'WDA301  (WDA3HSEQ =' W-WDA3HSEQ-X ')'                        
256879          DELIMITED BY SIZE INTO SSA1                                     
256880     MOVE '  GE' TO GODK-STATUSKODER                                      
256881     CALL CBLTDLI USING GN RETA3-PCB DLI-IO-AREA SSA1                     
256882     MOVE RETA3-STATUS-CODE TO STATUS-WS                                  
256883     PERFORM IMS-STATUSKONTROLL                                           
256884     .                                                                    
256885                                                                          
256886 IMS-GN-WDA301-J1            SECTION.                                     
256887                                                                          
256888     STRING 'WDA301  (IDRT     =' W-IDRT                                  
256889                    '&KDRETSTA =' W-KDRETSTA1                             
256890                    '&IDRTLOP  =' W-IDRTLOP    ')'                        
256891          DELIMITED BY SIZE INTO SSA1                                     
256892     MOVE '  GEGB' TO GODK-STATUSKODER                                    
256893     CALL CBLTDLI USING GN RETA3-PCB DLI-IO-AREA SSA1                     
256894     MOVE RETA3-STATUS-CODE TO STATUS-WS                                  
256895     PERFORM IMS-STATUSKONTROLL                                           
256896     .                                                                    
256897                                                                          
256898                                                                          
256899 IMS-GU-WDA3H1-DIST-KUND     SECTION.                                     
256900                                                                          
256901     STRING 'WDA3H1  (WDA3H1KY>=' W-WDA3H1-MIN-X                          
256902                    '&WDA3H1KY<=' W-WDA3H1-MAX-X                          
256903                    '&IDDISTR  =' W-IDDISTR-A3-X                          
256904                    '&IDKUNDNR =' W-IDKUNDNR-A3-X ')'                     
256905          DELIMITED BY SIZE INTO SSA1                                     
256906     MOVE '  GE' TO GODK-STATUSKODER                                      
256907     CALL CBLTDLI USING GU WDA3H-PCB DLI-IO-WDA3H1 SSA1                   
256908     MOVE WDA3H-STATUS-CODE TO STATUS-WS                                  
256909     PERFORM IMS-STATUSKONTROLL                                           
256910     .                                                                    
256911                                                                          
256912 IMS-GN-WDA3H1-DIST-KUND     SECTION.                                     
256913                                                                          
256914     STRING 'WDA3H1  (WDA3H1KY>=' W-WDA3H1-MIN-X                          
256915                    '&WDA3H1KY<=' W-WDA3H1-MAX-X                          
256916                    '&IDDISTR  =' W-IDDISTR-A3-X                          
256917                    '&IDKUNDNR =' W-IDKUNDNR-A3-X ')'                     
256918          DELIMITED BY SIZE INTO SSA1                                     
256919     MOVE '  GE' TO GODK-STATUSKODER                                      
256920     CALL CBLTDLI USING GN WDA3H-PCB DLI-IO-WDA3H1 SSA1                   
256921     MOVE WDA3H-STATUS-CODE TO STATUS-WS                                  
256922     PERFORM IMS-STATUSKONTROLL                                           
256923     .                                                                    
256924                                                                          
256925 IMS-GU-WDA3H1-DISTR         SECTION.                                     
256926                                                                          
256927     STRING 'WDA3H1  (WDA3H1KY>=' W-WDA3H1-MIN-X                          
256928                    '&WDA3H1KY<=' W-WDA3H1-MAX-X                          
256929                    '&IDDISTR  =' W-IDDISTR-A3-X  ')'                     
256930          DELIMITED BY SIZE INTO SSA1                                     
256931     MOVE '  GE' TO GODK-STATUSKODER                                      
256932     CALL CBLTDLI USING GU WDA3H-PCB DLI-IO-WDA3H1 SSA1                   
256933     MOVE WDA3H-STATUS-CODE TO STATUS-WS                                  
256934     PERFORM IMS-STATUSKONTROLL                                           
256935     .                                                                    
256936                                                                          
256937 IMS-GN-WDA3H1-DISTR         SECTION.                                     
256938                                                                          
256939     STRING 'WDA3H1  (WDA3H1KY>=' W-WDA3H1-MIN-X                          
256940                    '&WDA3H1KY<=' W-WDA3H1-MAX-X                          
256941                    '&IDDISTR  =' W-IDDISTR-A3-X  ')'                     
256942          DELIMITED BY SIZE INTO SSA1                                     
256943     MOVE '  GE' TO GODK-STATUSKODER                                      
256944     CALL CBLTDLI USING GN WDA3H-PCB DLI-IO-WDA3H1 SSA1                   
256945     MOVE WDA3H-STATUS-CODE TO STATUS-WS                                  
256946     PERFORM IMS-STATUSKONTROLL                                           
256947     .                                                                    
256948                                                                          
256949 IMS-GU-WDA301-I1            SECTION.                                     
256950                                                                          
256951     STRING 'WDA301  (WDA3ISEQ =' W-WDA3ISEQ-X ')'                        
256952          DELIMITED BY SIZE INTO SSA1                                     
256953     MOVE '  GE' TO GODK-STATUSKODER                                      
256954     CALL CBLTDLI USING GU RETA4-PCB DLI-IO-AREA SSA1                     
256955     MOVE RETA4-STATUS-CODE TO STATUS-WS                                  
256956     PERFORM IMS-STATUSKONTROLL                                           
256957     .                                                                    
256958                                                                          
256959 IMS-GN-WDA301-I1            SECTION.                                     
256960                                                                          
256961     STRING 'WDA301  (WDA3ISEQ =' W-WDA3ISEQ-X ')'                        
256962          DELIMITED BY SIZE INTO SSA1                                     
256963     MOVE '  GE' TO GODK-STATUSKODER                                      
256964     CALL CBLTDLI USING GN RETA4-PCB DLI-IO-AREA SSA1                     
256965     MOVE RETA4-STATUS-CODE TO STATUS-WS                                  
256966     PERFORM IMS-STATUSKONTROLL                                           
256967     .                                                                    
256968     EJECT                                                                
256969 IMS-GU-WDA3I1-DIST-KUND     SECTION.                                     
256970                                                                          
256971     STRING 'WDA3I1  (WDA3I1KY>=' W-WDA3I1-MIN-X                          
256972                    '&WDA3I1KY<=' W-WDA3I1-MAX-X                          
256973                    '&IDDISTR  =' W-IDDISTR-A3-X                          
256974                    '&IDKUNDNR =' W-IDKUNDNR-A3-X ')'                     
256975          DELIMITED BY SIZE INTO SSA1                                     
256976     MOVE '  GE' TO GODK-STATUSKODER                                      
256977     CALL CBLTDLI USING GU WDA3I-PCB DLI-IO-WDA3I1 SSA1                   
256978     MOVE WDA3I-STATUS-CODE TO STATUS-WS                                  
256979     PERFORM IMS-STATUSKONTROLL                                           
256980     .                                                                    
256981                                                                          
256982 IMS-GN-WDA3I1-DIST-KUND     SECTION.                                     
256983                                                                          
256984     STRING 'WDA3I1  (WDA3I1KY>=' W-WDA3I1-MIN-X                          
256985                    '&WDA3I1KY<=' W-WDA3I1-MAX-X                          
256986                    '&IDDISTR  =' W-IDDISTR-A3-X                          
256987                    '&IDKUNDNR =' W-IDKUNDNR-A3-X ')'                     
256988          DELIMITED BY SIZE INTO SSA1                                     
256989     MOVE '  GE' TO GODK-STATUSKODER                                      
256990     CALL CBLTDLI USING GN WDA3I-PCB DLI-IO-WDA3I1 SSA1                   
256991     MOVE WDA3I-STATUS-CODE TO STATUS-WS                                  
256992     PERFORM IMS-STATUSKONTROLL                                           
256993     .                                                                    
256994                                                                          
256995 IMS-GU-WDA3I1-DISTR         SECTION.                                     
256996                                                                          
256997     STRING 'WDA3I1  (WDA3I1KY>=' W-WDA3I1-MIN-X                          
256998                    '&WDA3I1KY<=' W-WDA3I1-MAX-X                          
256999                    '&IDDISTR  =' W-IDDISTR-A3-X  ')'                     
257000          DELIMITED BY SIZE INTO SSA1                                     
257001     MOVE '  GE' TO GODK-STATUSKODER                                      
257002     CALL CBLTDLI USING GU WDA3I-PCB DLI-IO-WDA3I1 SSA1                   
257003     MOVE WDA3I-STATUS-CODE TO STATUS-WS                                  
257004     PERFORM IMS-STATUSKONTROLL                                           
257005     .                                                                    
257006                                                                          
257007 IMS-GN-WDA3I1-DISTR         SECTION.                                     
257008                                                                          
257009     STRING 'WDA3I1  (WDA3I1KY>=' W-WDA3I1-MIN-X                          
257010                    '&WDA3I1KY<=' W-WDA3I1-MAX-X                          
257011                    '&IDDISTR  =' W-IDDISTR-A3-X  ')'                     
257012          DELIMITED BY SIZE INTO SSA1                                     
257013     MOVE '  GE' TO GODK-STATUSKODER                                      
257014     CALL CBLTDLI USING GN WDA3I-PCB DLI-IO-WDA3I1 SSA1                   
257015     MOVE WDA3I-STATUS-CODE TO STATUS-WS                                  
257016     PERFORM IMS-STATUSKONTROLL                                           
257017     .                                                                    
257018                                                                          
257019 IMS-GET-WDB601 SECTION.                                                  
257020                                                                          
257021     STRING 'WDB601  (IDDC     =' W-WDB601-X ')'                          
257022          DELIMITED BY SIZE INTO SSA1                                     
257023     MOVE '  GE' TO GODK-STATUSKODER                                      
257024     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
257025     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
257026     PERFORM IMS-STATUSKONTROLL                                           
257027     .                                                                    
257028     EJECT                                                                
257029 IMS-GU-WDK611 SECTION.                                                   
257030                                                                          
257040     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
257102           DELIMITED BY SIZE INTO SSA1                                    
257202     MOVE 'WDK611   ' TO SSA2                                             
257302     MOVE '  ' TO GODK-STATUSKODER                                        
257402     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
257502     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
257602     PERFORM IMS-STATUSKONTROLL                                           
257702     .                                                                    
257802     EJECT                                                                
257902 IMS-GU-WDD311 SECTION.                                                   
258002                                                                          
258102     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
258202          DELIMITED BY SIZE INTO SSA1                                     
258302     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
258402          DELIMITED BY SIZE INTO SSA2                                     
258502     MOVE '  GE' TO GODK-STATUSKODER                                      
258602     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
258702     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
258802     PERFORM IMS-STATUSKONTROLL                                           
258902     .                                                                    
259002     EJECT                                                                
259003 IMS-GN-WDGX3102  SECTION.                                                
259004                                                                          
259005     STRING 'WDR401  (WDGXKEY  =' W-IDHTYP-X ')'                          
259006          DELIMITED BY SIZE INTO SSA1                                     
259007     MOVE 'WDGX3102 ' TO         SSA2                                     
259008     MOVE '  GE' TO GODK-STATUSKODER                                      
259009     CALL CBLTDLI USING GN  WDR4-PCB DLI-IO-WDGX3102                      
259010                                          SSA1 SSA2                       
259020     MOVE WDR4-STATUS-CODE TO STATUS-WS                                   
259030     PERFORM IMS-STATUSKONTROLL                                           
259040     .                                                                    
259050     SKIP3                                                                
259102 IMS-STATUSKONTROLL SECTION.                                              
259202                                                                          
259302     SET STATUS-IX TO 1                                                   
259402     SEARCH GODK-STATUS                                                   
259502       AT END                                                             
259602         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
259702         DELIMITED BY SIZE INTO FELTEXT                                   
259802         CALL FELLOG                                                      
259902       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
260002         CONTINUE                                                         
261000     END-SEARCH                                                           
270000     .                                                                    
