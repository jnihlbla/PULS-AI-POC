000010*COMPOPT VPOSIX=YES                                                       
000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6122700.                                                
000300 AUTHOR.         BOHLIN HÅKAN.                                            
000400 DATE-WRITTEN.   21/09/21.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNCTION:                                                            
000900*        CALL PROJECT44 TO GET INFO ABOUT ETA TIMES.                      
001000*                                                                         
001100*        THE PROGRAM UPDATES   WDR5 (6301/WDGX6302)                       
001200*                                                                         
001300                                                                          
001400     SKIP3                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP2                                                                
001700 INPUT-OUTPUT SECTION.                                                    
001800                                                                          
001900 FILE-CONTROL.                                                            
002000     SKIP2                                                                
002100*          --- CALL P44 WITH BOOKING NO AND TRANSPORTER                   
002200     SELECT W612211                    ASSIGN TO W61227D1.                
002300     SKIP2                                                                
002400*          --- CALL P44 WITH CONTAINER NO AND TRANSPORTER                 
002500     SELECT W612212                    ASSIGN TO W61227D2.                
002501     SKIP2                                                                
002510*          --- CALL P44 WITH SHIPMENT ID (CER TRANSACTION)                
002520     SELECT W612213                    ASSIGN TO W61227D3.                
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP3                                                                
002900 FILE SECTION.                                                            
003000     SKIP3                                                                
003100 FD  W612211                                                              
003200     RECORDING       F                                                    
003300     BLOCK CONTAINS  0.                                                   
003400                                                                          
003500*01  -COPY W61221      -L.                                                
003600     SKIP3                                                                
003700 FD  W612212                                                              
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000                                                                          
004100*01  -COPY W61221      -L.                                                
004110     SKIP3                                                                
004120 FD  W612213                                                              
004130     RECORDING       F                                                    
004140     BLOCK CONTAINS  0.                                                   
004150                                                                          
004160*01  -COPY W61222      -L.                                                
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400                                                                          
004500 77  IDPGM                       PIC X(8)    VALUE 'W6122700'.            
004600 01  CHKP-VAR.                                                            
004700     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004800     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004900     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
005000     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
005100     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
005200     03 CHKP-MAX                 PIC S9(3)   VALUE +50 COMP-3.            
005300 77  YES                         PIC X       VALUE 'Y'.                   
005400 77  NOO                         PIC X       VALUE 'N'.                   
005410 77  W-SAVE-IDLBBET              PIC X(12)   VALUE SPACE.                 
005420 77  W-SAVE-IDBOKN               PIC X(15)   VALUE SPACE.                 
005430 77  W-SAVE-BETRPFIR             PIC X(15)   VALUE SPACE.                 
005440 77  W-IDSUBSCR                  PIC S9(11)  VALUE ZERO COMP-3.           
005500     SKIP2                                                                
005600 01  ERROR-TEXT.                                                          
005700     03  FILLER                  PIC X(11)   VALUE 'ERROR-TEXT:'.         
005800     03  ERROR-TEXT-STR          PIC X(69)   VALUE SPACE.                 
005900                                                                          
006000 77  W612211-EOF-SW              PIC X       VALUE 'N'.                   
006100     88  END-OF-W612211                      VALUE 'Y'.                   
006200                                                                          
006300 77  W612212-EOF-SW              PIC X       VALUE 'N'.                   
006400     88  END-OF-W612212                      VALUE 'Y'.                   
006401                                                                          
006402 77  W612213-EOF-SW              PIC X       VALUE 'N'.                   
006403     88  END-OF-W612213                      VALUE 'Y'.                   
006410                                                                          
006420 77  P44-SUBSCR-SW               PIC X       VALUE 'N'.                   
006430     88  P44-SUBSCR-OK                       VALUE 'Y'.                   
006431                                                                          
006432 77  P44-CER-SW                  PIC X       VALUE 'N'.                   
006433     88  P44-CER-OK                          VALUE 'Y'.                   
006434                                                                          
006435 77  P44-CANCEL-SW               PIC X       VALUE 'N'.                   
006436     88  P44-CANCEL-OK                       VALUE 'Y'.                   
006440                                                                          
006450 77  BOOKNO-SW                   PIC X       VALUE 'N'.                   
006460     88  BOOKNO-TYPE                         VALUE 'Y'.                   
006461                                                                          
006462 77  CONTNO-SW                   PIC X       VALUE 'N'.                   
006463     88  CONTNO-TYPE                         VALUE 'Y'.                   
006464                                                                          
006465 77  CER-SW                      PIC X       VALUE 'N'.                   
006466     88  CER-TYPE                            VALUE 'Y'.                   
006467                                                                          
006468 77  CANCEL-SW                   PIC X       VALUE 'N'.                   
006469     88  CANCEL-TYPE                         VALUE 'Y'.                   
006470                                                                          
006500     EJECT                                                                
006600 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
006700 01  FILLER REDEFINES TODAYS-DATE.                                        
006800     03  TODAYS-DATE-YEAR        PIC 9(2).                                
006900     03  TODAYS-DATE-MONTH       PIC 9(2).                                
007000     03  TODAYS-DATE-DAY         PIC 9(2).                                
007010     EJECT                                                                
007020 01  P44-CER-DATE.                                                        
007040     03  FILLER                  PIC X(2)    VALUE '20'.                  
007050     03  P44-CER-YEAR            PIC 9(2).                                
007051     03  FILLER                  PIC X(1)    VALUE '-'.                   
007052     03  P44-CER-MONTH           PIC 9(2).                                
007053     03  FILLER                  PIC X(1)    VALUE '-'.                   
007060     03  P44-CER-DAY             PIC 9(2).                                
007100     EJECT                                                                
007200 01  GENERAL-SUBPROGRAMS.                                                 
007300*                                                                         
007400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007610     03  VIMSID                  PIC X(8)    VALUE 'VIMSID'.              
007620     03  BAQCSTUB                PIC X(8)    VALUE 'BAQCSTUB'.            
007630     03  BAQCTERM                PIC X(8)    VALUE 'BAQCTERM'.            
007700     EJECT                                                                
007710*    --- PARAMETERS TO VIMSID                                             
007711 01 WS-VIMSID                PIC X(8)  VALUE SPACE.                       
007712 01 FILLER REDEFINES WS-VIMSID.                                           
007713    03 IMS-REGION            PIC X(3).                                    
007714       88 TEST-REGION                  VALUE 'IMD' 'IMP' 'IMY'            
007715                                             'IMB'.                       
007716       88 ENV-DEVE                     VALUE 'IMP'.                       
007717       88 ENV-IGRT                     VALUE 'IMY'.                       
007718       88 ENV-XDEV                     VALUE 'IMD'.                       
007719       88 ENV-ACPT                     VALUE 'IMB'.                       
007720       88 ENV-PROD                     VALUE 'IMG' 'IMR'.                 
007721    03 FILLER                PIC X(5).                                    
007730     EJECT                                                                
007800*    --- PARAMETRAR TILL POSTSUM                                          
007900*                                                                         
008000*01  -COPY W0005   -PRE  POSTSUM-                                         
008100     EJECT                                                                
008200 01  IN1-AREA-START              PIC X(24)   VALUE                        
008300                                             'IN1-AREA-START'.            
008400     SKIP2                                                                
008500                                                                          
008600*01  AREA -COPY W61221     -PRE IN1-                                      
008700     EJECT                                                                
008800 01  IN2-AREA-START              PIC X(24)   VALUE                        
008900                                             'IN2-AREA-START'.            
009000     SKIP2                                                                
009100                                                                          
009200*01  AREA -COPY W61221     -PRE IN2-                                      
009300*                                                                         
009310     EJECT                                                                
009320 01  IN3-AREA-START              PIC X(24)   VALUE                        
009330                                             'IN3-AREA-START'.            
009340     SKIP2                                                                
009350                                                                          
009360*01  AREA -COPY W61222     -PRE IN3-                                      
009370*                                                                         
009400     EJECT                                                                
009401**************************************************************            
009402 01  P44-API-START               PIC X(24)   VALUE                        
009403                                             'P44-API-START'.             
009404                                                                          
009405 01 BAQ-REQUEST-PTR                          USAGE POINTER.               
009406 01 BAQ-REQUEST-LEN              PIC S9(9)   COMP-5 SYNC.                 
009407 01 BAQ-RESPONSE-PTR                         USAGE POINTER.               
009408 01 BAQ-RESPONSE-LEN             PIC S9(9)   COMP-5 SYNC.                 
009413                                                                          
009414*                                                                         
009415*   -COPY BAQRINFO                                                        
009416*                                                                         
009418*** API INFO                                                              
009419 01 P44-APINAME                  PIC X(36) VALUE                          
009420                           'PULS-Project44-ContainerTracking_1.0'.        
009421 01 P44-APINAME-LEN              PIC 9(4)  VALUE 36.                      
009426 01 P44-APIMETHOD                PIC X(4)  VALUE 'POST'.                  
009427 01 P44-APIMETHOD-LEN            PIC 9(4)  VALUE 4.                       
009428*** API INFO for CREATE SUBSCRIPTION                                      
009429 01 P44-APIPATH-CRSUB            PIC X(69)  VALUE                         
009430              '%2Fb2b%2Fpuls%2Fproject44%2Fcontainertracking%2Fv2%        
009431-             '2Fsubscriptions%2F'.                                       
009432 01 P44-APIPATH-LEN-CRSUB        PIC 9(4)   VALUE 69.                     
009433*** API INFO TO STOP PUSHEVENTS (CER)                                     
009434 01 P44-APIMETHOD-CER            PIC X(3)  VALUE 'PUT'.                   
009435 01 P44-APIMETHOD-LEN-CER        PIC 9(4)  VALUE 3.                       
009436 01 P44-APIPATH-CER              PIC X(100) VALUE                         
009437              '%2Fb2b%2Fpuls%2Fproject44%2Fcontainertracking%2Fv2%        
009438-             '2Fshipments%2F%7Bshipment_id%7D%2Fempty_return%2F'.        
009439 01 P44-APIPATH-LEN-CER          PIC 9(4)   VALUE 100.                    
009440*** API INFO TO CANCEL A SUBSCRIPTION                                     
009441 01 P44-APIPATH-CAN              PIC X(102) VALUE                         
009442              '%2Fb2b%2Fpuls%2Fproject44%2Fcontainertracking%2Fv2%        
009443-           '2Fsubscriptions%2F%7Bsubscription_id%7D%2Fcancel%2F'.        
009444 01 P44-APIPATH-LEN-CAN          PIC 9(4)   VALUE 102.                    
009445                                                                          
009446*** PROXY KEYS FOR DIFFERENT ENVIROMENTS                                  
009447 01 P44-PROXYKEY-PROD            PIC X(32) VALUE                          
009448          '********************************'.                             
009449 01 P44-PROXYKEY-PROD-LEN       PIC 9(04) VALUE 32.                       
009450 01 P44-PROXYKEY-QA              PIC X(32) VALUE                          
009451          '********************************'.                             
009452 01 P44-PROXYKEY-QA-LEN         PIC 9(04) VALUE 32.                       
009453 01 P44-PROXYKEY-TEST            PIC X(32) VALUE                          
009454          '********************************'.                             
009455 01 P44-PROXYKEY-TEST-LEN       PIC 9(04) VALUE 32.                       
009456                                                                          
009457 01 API-INFO.                                                             
009458    03 BAQ-APINAME               PIC X(255).                              
009459    03 BAQ-APINAME-LEN           PIC S9(9) COMP-5 SYNC.                   
009460    03 BAQ-APIPATH               PIC X(255).                              
009461    03 BAQ-APIPATH-LEN           PIC S9(9) COMP-5 SYNC.                   
009462    03 BAQ-APIMETHOD             PIC X(255).                              
009463    03 BAQ-APIMETHOD-LEN         PIC S9(9) COMP-5 SYNC.                   
009464                                                                          
009465 01  API-REQUEST                 PIC X(10000).                            
009466                                                                          
009467 01  API-RESPONSE                PIC X(100000000).                        
009468                                                                          
009469 01  P44-CRSUB-REQU.                                                      
009470*    03 -COPY W6122701 -PRE CRSUB-                                        
009471                                                                          
009472 01  P44-CER-REQU.                                                        
009473*    03 -COPY W6122702 -PRE CER-                                          
009474                                                                          
009475 01  P44-CAN-REQU.                                                        
009476*    03 -COPY W6122704 -PRE CAN-                                          
009477                                                                          
009478 01  RESPONSE-AREA.                                                       
009479     03  idsubscr                PIC 9(11).                               
009480     03  request_type            PIC X(4).                                
009481     03  request_key             PIC X(15).                               
009482     03  request_carrier_code    PIC X(15).                               
009483     03  detail-err              PIC X(255).                              
009484     03  non_field_errors        PIC X(255).                              
009485                                                                          
009486 01  RESPONSE-AREA-CER.                                                   
009487     03  empty_return_customer   PIC X(10).                               
009488     03  detail-err-cer          PIC X(255).                              
009489                                                                          
009490 01  RESPONSE-AREA-CAN.                                                   
009492     03  detail-err-can          PIC X(255).                              
009493     03  non-field-errors-can    PIC X(255).                              
009494**************************************************************            
009495     EJECT                                                                
009500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009600     SKIP3                                                                
009700 01  KEYS-TILL-DLI.                                                       
009800     03  W-6301KEY-X.                                                     
009900         05  W-6301-IDHTYP      PIC X(4)     VALUE '6301'.                
010000         05  W-6301-IDDC        PIC X(2).                                 
010100         05  FILLER             PIC X(24)    VALUE LOW-VALUE.             
010200     03  W-6302KEY-X.                                                     
010300         05  W-6302-DABERANK    PIC 9(8).                                 
010400         05  W-6302-IDFAKT      PIC S9(7)    COMP-3.                      
010500     SKIP2                                                                
010600*    --- STATUS-KOD FRÅN IMS                                              
010700 01  STATUS-WS                   PIC XX.                                  
010800     88  SEGMENT-FOUND                       VALUE '  '.                  
010900     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
011000     88  SEGMENT-MISSING                     VALUE 'GE'.                  
011100     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
011200     88  IMS-NOT-OK                          VALUE 'XD'.                  
011300     SKIP2                                                                
011400 01  GOOD-STATUSCODES.                                                    
011500     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011600     SKIP3                                                                
011700 01  SSA1                        PIC X(128).                              
011800 01  SSA2                        PIC X(128).                              
011900     EJECT                                                                
012000*    --- IMS FUNCTION CODES                                               
012100*01  -COPY W0003                                                          
012200     EJECT                                                                
012300*    ---  DLI INPUT-OUTPUT AREA                                           
012400                                                                          
012800     EJECT                                                                
012900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6302'.                    
013000 01  DLI-IO-WDGX6302.                                                     
013100*    03  -COPY WDGX6302                                                   
013200                                                                          
013300     EJECT                                                                
013400 LINKAGE SECTION.                                                         
013500                                                                          
013600*01  -COPY W0009   -PRE MSG-                                              
013700                                                                          
013800*01  -COPY W0008  -PRE 6301-                                              
013900     05  FILLER                  PIC X.                                   
014000     EJECT                                                                
014100 PROCEDURE DIVISION  USING MSG-PCB 6301-PCB.                              
014200 MAIN SECTION.                                                            
014300     ENTRY 'DLITCBL' USING MSG-PCB 6301-PCB.                              
014400                                                                          
014500     SKIP2                                                                
014600     PERFORM A-INIT                                                       
014610                                                                          
014620     MOVE YES TO BOOKNO-SW                                                
014630     MOVE NOO TO CONTNO-SW                                                
014640     MOVE NOO TO CER-SW                                                   
014650     MOVE NOO TO CANCEL-SW                                                
014700     PERFORM S01-READ-W612211                                             
014900     PERFORM UNTIL END-OF-W612211                                         
015000       IF CHKP-ANT > CHKP-MAX                                             
015100         PERFORM X-TAKE-CHECKPOINT                                        
015200       END-IF                                                             
015202       PERFORM B-HANDLE-IDBOKN                                            
015210       MOVE IN1-IDBOKN   TO W-SAVE-IDBOKN                                 
015220       MOVE IN1-BETRPFIR TO W-SAVE-BETRPFIR                               
015300       PERFORM S01-READ-W612211                                           
015500     END-PERFORM                                                          
015510                                                                          
015512     MOVE NOO TO BOOKNO-SW                                                
015513     MOVE NOO TO CER-SW                                                   
015520     MOVE SPACE TO W-SAVE-BETRPFIR                                        
015610     PERFORM S02-READ-W612212                                             
015620     PERFORM UNTIL END-OF-W612212                                         
015630       IF CHKP-ANT > CHKP-MAX                                             
015640         PERFORM X-TAKE-CHECKPOINT                                        
015650       END-IF                                                             
015652       PERFORM C-HANDLE-IDLBBET                                           
015660       MOVE IN2-IDLBBET  TO W-SAVE-IDLBBET                                
015661       MOVE IN2-BETRPFIR TO W-SAVE-BETRPFIR                               
015670       PERFORM S02-READ-W612212                                           
015680     END-PERFORM                                                          
015690                                                                          
015691     MOVE YES TO CER-SW                                                   
015692     MOVE NOO TO BOOKNO-SW                                                
015693     MOVE NOO TO CONTNO-SW                                                
015694     MOVE NOO TO CANCEL-SW                                                
015695     PERFORM S03-READ-W612213                                             
015696     PERFORM UNTIL END-OF-W612213                                         
015698       IF CHKP-ANT > CHKP-MAX                                             
015699         PERFORM X-TAKE-CHECKPOINT                                        
015700       END-IF                                                             
015701       PERFORM E-HANDLE-IDCONTNR                                          
015702       PERFORM S03-READ-W612213                                           
015703     END-PERFORM                                                          
015710                                                                          
015800                                                                          
015900     PERFORM Z-FINIT                                                      
016000                                                                          
016100     MOVE ZERO TO RETURN-CODE                                             
016200     GOBACK                                                               
016300     .                                                                    
016400     EJECT                                                                
016500 A-INIT SECTION.                                                          
016600     SKIP2                                                                
016700                                                                          
016800     PERFORM IMS-RESTART                                                  
016900                                                                          
017000     OPEN INPUT W612211                                                   
017200                W612212                                                   
017210                W612213                                                   
017300                                                                          
017400     ACCEPT TODAYS-DATE  FROM DATE                                        
017410     MOVE TODAYS-DATE-YEAR  TO P44-CER-YEAR                               
017420     MOVE TODAYS-DATE-MONTH TO P44-CER-MONTH                              
017430     MOVE TODAYS-DATE-DAY   TO P44-CER-DAY                                
017440                                                                          
017500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
017501                                                                          
017502     CALL VIMSID USING WS-VIMSID                                          
017600     .                                                                    
017610     EJECT                                                                
017620 B-HANDLE-IDBOKN SECTION.                                                 
017630     SKIP2                                                                
017631     IF IN1-IDBOKN   = W-SAVE-IDBOKN AND                                  
017632        IN1-BETRPFIR = W-SAVE-BETRPFIR                                    
017634       MOVE IN1-IDDC-REC TO W-6301-IDDC                                   
017635       MOVE IN1-DABERANK TO W-6302-DABERANK                               
017636       MOVE IN1-IDFAKT   TO W-6302-IDFAKT                                 
017637       PERFORM IMS-GHU-WDGX6302                                           
017639       IF SEGMENT-FOUND                                                   
017640         MOVE IN1-IDBOKN   TO 6302-IDBOKN                                 
017641         MOVE IN1-BETRPFIR TO 6302-BETRPFIR                               
017642         MOVE TODAYS-DATE  TO 6302-TILST-CALLP44                          
017643         IF P44-SUBSCR-OK                                                 
017644           MOVE W-IDSUBSCR TO 6302-IDSUBSCR                               
017645         END-IF                                                           
017646         PERFORM IMS-REPL-WDGX6302                                        
017647         ADD 1 TO CHKP-ANT                                                
017648       END-IF                                                             
017649     ELSE                                                                 
017650       MOVE NOO TO P44-SUBSCR-SW                                          
017651       MOVE ZERO TO W-IDSUBSCR                                            
017652       PERFORM D-PREP-P44-CALL                                            
017653       MOVE IN1-IDDC-REC TO W-6301-IDDC                                   
017654       MOVE IN1-DABERANK TO W-6302-DABERANK                               
017655       MOVE IN1-IDFAKT   TO W-6302-IDFAKT                                 
017656       PERFORM IMS-GHU-WDGX6302                                           
017658       IF SEGMENT-FOUND                                                   
017659         MOVE IN1-IDBOKN   TO 6302-IDBOKN                                 
017660         MOVE IN1-BETRPFIR TO 6302-BETRPFIR                               
017661         MOVE TODAYS-DATE  TO 6302-TILST-CALLP44                          
017662         IF P44-SUBSCR-OK                                                 
017663           MOVE W-IDSUBSCR TO 6302-IDSUBSCR                               
017666         END-IF                                                           
017667         PERFORM IMS-REPL-WDGX6302                                        
017668         ADD 1 TO CHKP-ANT                                                
017669       END-IF                                                             
017670     END-IF                                                               
017680                                                                          
017693     .                                                                    
017701     EJECT                                                                
017702 C-HANDLE-IDLBBET SECTION.                                                
017703     SKIP2                                                                
017704     MOVE YES TO CONTNO-SW                                                
017705     MOVE NOO TO CANCEL-SW                                                
017706     IF IN2-IDLBBET  = W-SAVE-IDLBBET AND                                 
017707        IN2-BETRPFIR = W-SAVE-BETRPFIR                                    
017708       MOVE IN2-IDDC-REC TO W-6301-IDDC                                   
017709       MOVE IN2-DABERANK TO W-6302-DABERANK                               
017710       MOVE IN2-IDFAKT   TO W-6302-IDFAKT                                 
017711       PERFORM IMS-GHU-WDGX6302                                           
017713       IF SEGMENT-FOUND                                                   
017714         MOVE IN2-BETRPFIR TO 6302-BETRPFIR                               
017715         MOVE TODAYS-DATE  TO 6302-TILST-CALLP44                          
017716         IF P44-CANCEL-OK                                                 
017717           MOVE SPACE      TO 6302-IDBOKN                                 
017718         END-IF                                                           
017719         IF P44-SUBSCR-OK                                                 
017720           MOVE W-IDSUBSCR TO 6302-IDSUBSCR                               
017721         END-IF                                                           
017722         PERFORM IMS-REPL-WDGX6302                                        
017723         ADD 1 TO CHKP-ANT                                                
017724       END-IF                                                             
017725     ELSE                                                                 
017726       MOVE NOO TO P44-SUBSCR-SW                                          
017727       MOVE NOO TO P44-CANCEL-SW                                          
017728       MOVE ZERO TO W-IDSUBSCR                                            
017729       IF IN2-IDBOKN > SPACE AND IN2-IDSUBSCR > ZERO                      
017730         MOVE YES TO P44-CANCEL-SW                                        
017731***      MOVE NOO TO CONTNO-SW                                            
017732***      MOVE YES TO CANCEL-SW                                            
017733***      PERFORM D-PREP-P44-CALL                                          
017734***      MOVE YES TO CONTNO-SW                                            
017735***      MOVE NOO TO CANCEL-SW                                            
017736       END-IF                                                             
017737       PERFORM D-PREP-P44-CALL                                            
017738       MOVE IN2-IDDC-REC TO W-6301-IDDC                                   
017739       MOVE IN2-DABERANK TO W-6302-DABERANK                               
017740       MOVE IN2-IDFAKT   TO W-6302-IDFAKT                                 
017741       PERFORM IMS-GHU-WDGX6302                                           
017743       IF SEGMENT-FOUND                                                   
017744         MOVE IN2-BETRPFIR TO 6302-BETRPFIR                               
017745         MOVE TODAYS-DATE  TO 6302-TILST-CALLP44                          
017746         IF P44-CANCEL-OK                                                 
017747           MOVE SPACE      TO 6302-IDBOKN                                 
017748         END-IF                                                           
017749         IF P44-SUBSCR-OK                                                 
017750           MOVE W-IDSUBSCR TO 6302-IDSUBSCR                               
017751         END-IF                                                           
017752         PERFORM IMS-REPL-WDGX6302                                        
017753         ADD 1 TO CHKP-ANT                                                
017754       END-IF                                                             
017755     END-IF                                                               
017756                                                                          
017757     .                                                                    
017758     EJECT                                                                
017759 D-PREP-P44-CALL SECTION.                                                 
017760     MOVE SPACE TO API-REQUEST                                            
017761                   API-RESPONSE                                           
017762                                                                          
017763                                                                          
017764     PERFORM DA-SET-PROXYKEY                                              
017765                                                                          
017766     MOVE P44-APINAME       TO BAQ-APINAME                                
017767     MOVE P44-APINAME-LEN   TO BAQ-APINAME-LEN                            
017768                                                                          
017769     IF BOOKNO-TYPE                                                       
017770        MOVE P44-APIMETHOD           TO BAQ-APIMETHOD                     
017771        MOVE P44-APIMETHOD-LEN       TO BAQ-APIMETHOD-LEN                 
017772        MOVE P44-APIPATH-CRSUB       TO BAQ-APIPATH                       
017773        MOVE P44-APIPATH-LEN-CRSUB   TO BAQ-APIPATH-LEN                   
017774                                                                          
017775        MOVE 1                   TO CRSUB-REQUEST-CARRIER-CODE-NUM        
017776                                    CRSUB-REQUEST-TYPE-NUM                
017777                                    CRSUB-REQUEST-KEY-NUM                 
017778                                                                          
017779        MOVE 'b_id'                  TO CRSUB-REQUEST-TYPE2               
017780        MOVE 4                       TO CRSUB-REQUEST-TYPE2-LENGTH        
017781                                                                          
017782        MOVE IN1-IDBOKN   TO CRSUB-REQUEST-KEY2                           
017783        COMPUTE CRSUB-REQUEST-KEY2-LENGTH = FUNCTION BYTE-LENGTH (        
017784                FUNCTION TRIM (CRSUB-REQUEST-KEY2))                       
017785                                                                          
017786        MOVE IN1-BETRPFIR TO CRSUB-REQUEST-CARRIER-CODE2                  
017787        COMPUTE CRSUB-REQUEST-CARRIER-CODE-LEN =                          
017788                FUNCTION BYTE-LENGTH (                                    
017789                FUNCTION TRIM (CRSUB-REQUEST-CARRIER-CODE2))              
017790                                                                          
017791        MOVE P44-CRSUB-REQU TO API-REQUEST                                
017792     END-IF                                                               
017793                                                                          
017794     IF CONTNO-TYPE                                                       
017795***     CALL WITH CONTAINER ID                                            
017796        MOVE P44-APIMETHOD           TO BAQ-APIMETHOD                     
017797        MOVE P44-APIMETHOD-LEN       TO BAQ-APIMETHOD-LEN                 
017798        MOVE P44-APIPATH-CRSUB       TO BAQ-APIPATH                       
017799        MOVE P44-APIPATH-LEN-CRSUB   TO BAQ-APIPATH-LEN                   
017800                                                                          
017801        MOVE 1                   TO CRSUB-REQUEST-CARRIER-CODE-NUM        
017802                                    CRSUB-REQUEST-TYPE-NUM                
017803                                    CRSUB-REQUEST-KEY-NUM                 
017804                                                                          
017805        MOVE 'c_id'                  TO CRSUB-REQUEST-TYPE2               
017806        MOVE 4                       TO CRSUB-REQUEST-TYPE2-LENGTH        
017807                                                                          
017808        MOVE IN2-IDLBBET  TO CRSUB-REQUEST-KEY2                           
017809        COMPUTE CRSUB-REQUEST-KEY2-LENGTH = FUNCTION BYTE-LENGTH (        
017810                FUNCTION TRIM (CRSUB-REQUEST-KEY2))                       
017811                                                                          
017812        MOVE IN2-BETRPFIR TO CRSUB-REQUEST-CARRIER-CODE2                  
017813        COMPUTE CRSUB-REQUEST-CARRIER-CODE-LEN =                          
017814                FUNCTION BYTE-LENGTH (                                    
017815                FUNCTION TRIM (CRSUB-REQUEST-CARRIER-CODE2))              
017816                                                                          
017817        MOVE P44-CRSUB-REQU TO API-REQUEST                                
017818     END-IF                                                               
017819                                                                          
017820     IF CER-TYPE                                                          
017821***     CALL WITH SHIPMENT ID (CER TRANSACTION)                           
017822        MOVE P44-APIMETHOD-CER       TO BAQ-APIMETHOD                     
017823        MOVE P44-APIMETHOD-LEN-CER   TO BAQ-APIMETHOD-LEN                 
017824        MOVE P44-APIPATH-CER         TO BAQ-APIPATH                       
017825        MOVE P44-APIPATH-LEN-CER     TO BAQ-APIPATH-LEN                   
017826                                                                          
017827        MOVE IN3-IDCONTNR  TO CER-SHIPMENT-ID                             
017828                                                                          
017829        MOVE 1             TO CER-EMPTY-RETURN-CUSTOMER-NUM               
017830        MOVE 10            TO CER-EMPTY-RETURN-CUSTOMER-LEN               
017831        MOVE P44-CER-DATE  TO CER-EMPTY-RETURN-CUSTOMER2                  
017832                                                                          
017833        MOVE P44-CER-REQU  TO API-REQUEST                                 
017834     END-IF                                                               
017835                                                                          
017836     IF CANCEL-TYPE                                                       
017837***     CALL WITH SUBSCRIPTION ID (CANCEL TRANSACTION)                    
017838        MOVE P44-APIMETHOD           TO BAQ-APIMETHOD                     
017839        MOVE P44-APIMETHOD-LEN       TO BAQ-APIMETHOD-LEN                 
017840        MOVE P44-APIPATH-CAN         TO BAQ-APIPATH                       
017841        MOVE P44-APIPATH-LEN-CAN     TO BAQ-APIPATH-LEN                   
017842                                                                          
017843        MOVE IN2-IDSUBSCR  TO CAN-SUBSCRIPTION-ID                         
017844                                                                          
017845        MOVE P44-CAN-REQU  TO API-REQUEST                                 
017846     END-IF                                                               
017847                                                                          
017848     SET BAQ-REQUEST-PTR TO ADDRESS OF API-REQUEST                        
017849     MOVE LENGTH OF API-REQUEST TO BAQ-REQUEST-LEN                        
017850     SET BAQ-RESPONSE-PTR TO ADDRESS OF API-RESPONSE                      
017851     MOVE LENGTH OF API-RESPONSE TO BAQ-RESPONSE-LEN                      
017852                                                                          
017853     PERFORM S04-CALL-BAQCSTUB                                            
017854     IF BAQ-SUCCESS                                                       
017855       IF BOOKNO-TYPE OR CONTNO-TYPE                                      
017856         IF BAQ-RESPONSE-LEN > ZERO                                       
017857           PERFORM DB-PARSE-JSON                                          
017858           MOVE YES TO P44-SUBSCR-SW                                      
017859           MOVE idsubscr TO W-IDSUBSCR                                    
017860         END-IF                                                           
017865       END-IF                                                             
017866       IF CER-TYPE                                                        
017867         IF BAQ-RESPONSE-LEN > ZERO                                       
017868           PERFORM DC-PARSE-JSON-CER                                      
017870         END-IF                                                           
017871         MOVE YES TO P44-CER-SW                                           
017872       END-IF                                                             
017873       IF CANCEL-TYPE                                                     
017874         IF BAQ-RESPONSE-LEN > ZERO                                       
017875           PERFORM DD-PARSE-JSON-CAN                                      
017876         END-IF                                                           
017877         MOVE YES TO P44-CANCEL-SW                                        
017878       END-IF                                                             
017879     ELSE                                                                 
017880       IF BOOKNO-TYPE                                                     
017881         DISPLAY 'IN1-AREA:' IN1-AREA                                     
017886       END-IF                                                             
017887       IF CONTNO-TYPE                                                     
017888         DISPLAY 'IN2-AREA:' IN2-AREA                                     
017893       END-IF                                                             
017894       IF CER-TYPE                                                        
017895         DISPLAY 'IN3-AREA:' IN3-AREA                                     
017899       END-IF                                                             
017900     END-IF                                                               
017901     .                                                                    
017902     EJECT                                                                
017903 DA-SET-PROXYKEY SECTION.                                                 
017904                                                                          
017905     IF BOOKNO-TYPE OR CONTNO-TYPE                                        
017906       EVALUATE TRUE                                                      
017907         WHEN ENV-DEVE OR ENV-IGRT OR ENV-XDEV                            
017908          MOVE P44-PROXYKEY-TEST     TO CRSUB-PROXY-KEY                   
017909          MOVE P44-PROXYKEY-TEST-LEN TO CRSUB-PROXY-KEY-LENGTH            
017910         WHEN ENV-ACPT                                                    
017911          MOVE P44-PROXYKEY-QA       TO CRSUB-PROXY-KEY                   
017912          MOVE P44-PROXYKEY-QA-LEN   TO CRSUB-PROXY-KEY-LENGTH            
017913         WHEN ENV-PROD                                                    
017914          MOVE P44-PROXYKEY-PROD     TO CRSUB-PROXY-KEY                   
017915          MOVE P44-PROXYKEY-PROD-LEN TO CRSUB-PROXY-KEY-LENGTH            
017916       END-EVALUATE                                                       
017917     END-IF                                                               
017918                                                                          
017919     IF CER-TYPE                                                          
017920       EVALUATE TRUE                                                      
017921         WHEN ENV-DEVE OR ENV-IGRT OR ENV-XDEV                            
017922          MOVE P44-PROXYKEY-TEST     TO CER-PROXY-KEY                     
017923          MOVE P44-PROXYKEY-TEST-LEN TO CER-PROXY-KEY-LENGTH              
017924         WHEN ENV-ACPT                                                    
017925          MOVE P44-PROXYKEY-QA       TO CER-PROXY-KEY                     
017926          MOVE P44-PROXYKEY-QA-LEN   TO CER-PROXY-KEY-LENGTH              
017927         WHEN ENV-PROD                                                    
017928          MOVE P44-PROXYKEY-PROD     TO CER-PROXY-KEY                     
017929          MOVE P44-PROXYKEY-PROD-LEN TO CER-PROXY-KEY-LENGTH              
017930       END-EVALUATE                                                       
017931     END-IF                                                               
017932                                                                          
017933     IF CANCEL-TYPE                                                       
017934       EVALUATE TRUE                                                      
017935         WHEN ENV-DEVE OR ENV-IGRT OR ENV-XDEV                            
017936          MOVE P44-PROXYKEY-TEST     TO CAN-PROXY-KEY                     
017937          MOVE P44-PROXYKEY-TEST-LEN TO CAN-PROXY-KEY-LENGTH              
017938         WHEN ENV-ACPT                                                    
017939          MOVE P44-PROXYKEY-QA       TO CAN-PROXY-KEY                     
017940          MOVE P44-PROXYKEY-QA-LEN   TO CAN-PROXY-KEY-LENGTH              
017941         WHEN ENV-PROD                                                    
017942          MOVE P44-PROXYKEY-PROD     TO CAN-PROXY-KEY                     
017943          MOVE P44-PROXYKEY-PROD-LEN TO CAN-PROXY-KEY-LENGTH              
017944       END-EVALUATE                                                       
017945     END-IF                                                               
017946     .                                                                    
017947     EJECT                                                                
017948 DB-PARSE-JSON SECTION.                                                   
017949     MOVE SPACE TO RESPONSE-AREA                                          
017950                                                                          
017951     JSON PARSE API-RESPONSE(1:BAQ-RESPONSE-LEN)                          
017952       INTO RESPONSE-AREA                                                 
017953       NAME OF idsubscr is "id"                                           
017954               detail-err  "detail"                                       
017955       RESPONSE-AREA IS OMITTED                                           
017956     PERFORM S99-JSON-CODE-VALIDATION                                     
017957     .                                                                    
017958     EJECT                                                                
017959 DC-PARSE-JSON-CER SECTION.                                               
017960     MOVE SPACE TO RESPONSE-AREA-CER                                      
017961                                                                          
017962     JSON PARSE API-RESPONSE(1:BAQ-RESPONSE-LEN)                          
017963       INTO RESPONSE-AREA-CER                                             
017964       NAME OF detail-err-cer "detail"                                    
017965       RESPONSE-AREA-CER OMITTED                                          
017966     PERFORM S99-JSON-CODE-VALIDATION                                     
017967     .                                                                    
017968     EJECT                                                                
017969 DD-PARSE-JSON-CAN SECTION.                                               
017970                                                                          
017971     MOVE SPACE TO RESPONSE-AREA-CAN                                      
017972                                                                          
017973     JSON PARSE API-RESPONSE(1:BAQ-RESPONSE-LEN)                          
017974       INTO RESPONSE-AREA-CAN                                             
017975       NAME OF detail-err-can        "detail"                             
017976               non-field-errors-can  "non_field_errors"                   
017977       RESPONSE-AREA-CAN OMITTED                                          
017978     PERFORM S99-JSON-CODE-VALIDATION                                     
017979     .                                                                    
017980     EJECT                                                                
017981 E-HANDLE-IDCONTNR SECTION.                                               
017982     SKIP2                                                                
017983     MOVE NOO TO P44-CER-SW                                               
017984     PERFORM D-PREP-P44-CALL                                              
017985     MOVE IN3-IDDC-REC TO W-6301-IDDC                                     
017986     MOVE IN3-DABERANK TO W-6302-DABERANK                                 
017987     MOVE IN3-IDFAKT   TO W-6302-IDFAKT                                   
017988     PERFORM IMS-GHU-WDGX6302                                             
017990     IF SEGMENT-FOUND                                                     
017991       MOVE TODAYS-DATE  TO 6302-TILST-CALLP44                            
017992       IF P44-CER-OK                                                      
017993         MOVE ZERO       TO 6302-IDCONTNR                                 
017994       END-IF                                                             
017995       PERFORM IMS-REPL-WDGX6302                                          
017996       ADD 1 TO CHKP-ANT                                                  
017997     END-IF                                                               
017998     .                                                                    
017999     EJECT                                                                
018000 Z-FINIT SECTION.                                                         
018001                                                                          
018010                                                                          
018100     CLOSE W612211                                                        
018300           W612212                                                        
018310           W612213                                                        
018400     SKIP2                                                                
018500     MOVE 'S' TO POSTSUM-OPKOD                                            
018600     CALL POSTSUM USING POSTSUM-PARM                                      
018610                                                                          
018620     PERFORM S05-CALL-BAQCTERM                                            
018700     .                                                                    
018800     EJECT                                                                
018900 S01-READ-W612211 SECTION.                                                
019000     SKIP2                                                                
019100     READ W612211 INTO IN1-AREA                                           
019200     AT END                                                               
019400        SET END-OF-W612211 TO TRUE                                        
019500                                                                          
019600     NOT AT END                                                           
019700        MOVE 'W612211'  TO POSTSUM-FDNAMN                                 
019800        MOVE 'W61227D1' TO POSTSUM-DDNAMN2                                
019900        MOVE SPACE      TO POSTSUM-TRANSTYP                               
020000        CALL POSTSUM USING POSTSUM-PARM                                   
020100                                                                          
020300     END-READ                                                             
020400     .                                                                    
020500     EJECT                                                                
020600 S02-READ-W612212 SECTION.                                                
020700     SKIP2                                                                
020800     READ W612212 INTO IN2-AREA                                           
020900     AT END                                                               
021100        SET END-OF-W612212 TO TRUE                                        
021200                                                                          
021300     NOT AT END                                                           
021400        MOVE 'W612212'  TO POSTSUM-FDNAMN                                 
021500        MOVE 'W61227D2' TO POSTSUM-DDNAMN2                                
021600        MOVE SPACE      TO POSTSUM-TRANSTYP                               
021700        CALL POSTSUM USING POSTSUM-PARM                                   
021800                                                                          
022000     END-READ                                                             
022100     .                                                                    
022101     EJECT                                                                
022102 S03-READ-W612213 SECTION.                                                
022103     SKIP2                                                                
022104     READ W612213 INTO IN3-AREA                                           
022105     AT END                                                               
022106        SET END-OF-W612213 TO TRUE                                        
022107                                                                          
022108     NOT AT END                                                           
022109        MOVE 'W612213'  TO POSTSUM-FDNAMN                                 
022110        MOVE 'W61227D3' TO POSTSUM-DDNAMN2                                
022111        MOVE SPACE      TO POSTSUM-TRANSTYP                               
022112        CALL POSTSUM USING POSTSUM-PARM                                   
022113                                                                          
022114     END-READ                                                             
022115     .                                                                    
022116     EJECT                                                                
022120 S04-CALL-BAQCSTUB SECTION.                                               
022130     SKIP2                                                                
022140*CALL API VIA z/OS CONNECT                                                
022150     CALL BAQCSTUB USING API-INFO                                         
022160                         BAQ-REQUEST-INFO                                 
022170                         BAQ-REQUEST-PTR                                  
022180                         BAQ-REQUEST-LEN                                  
022190                         BAQ-RESPONSE-INFO                                
022191                         BAQ-RESPONSE-PTR                                 
022192                         BAQ-RESPONSE-LEN                                 
022193                                                                          
022194     IF BAQ-SUCCESS                                                       
022195       CONTINUE                                                           
022206     ELSE                                                                 
022208       EVALUATE TRUE                                                      
022209         WHEN BAQ-ERROR-IN-API                                            
022210           DISPLAY 'ERROR IN API '                                        
022211         WHEN BAQ-ERROR-IN-ZCEE                                           
022212           DISPLAY 'ERROR IN ZCEE'                                        
022213         WHEN BAQ-ERROR-IN-STUB                                           
022214           DISPLAY 'ERROR IN STUB'                                        
022215       END-EVALUATE                                                       
022216     END-IF                                                               
022220     .                                                                    
022221     EJECT                                                                
022222 S05-CALL-BAQCTERM SECTION.                                               
022223*CLOSE AND CLEAR CACHED CONNECTION                                        
022224     INITIALIZE BAQ-RESPONSE-INFO                                         
022225     CALL BAQCTERM USING BAQ-RESPONSE-INFO                                
022226                                                                          
022227     IF BAQ-SUCCESS                                                       
022228       CONTINUE                                                           
022229     ELSE                                                                 
022230       DISPLAY 'ERROR WHEN CALL BAQCTERM'                                 
022231     END-IF                                                               
022232     .                                                                    
022240     EJECT                                                                
022241 S99-JSON-CODE-VALIDATION SECTION.                                        
022242                                                                          
022245     EVALUATE JSON-CODE                                                   
022246       WHEN 100                                                           
022247         DISPLAY 'JSON text was invalid'                                  
022248         IF BOOKNO-TYPE                                                   
022249           DISPLAY 'IN1-AREA:' IN1-AREA                                   
022250         END-IF                                                           
022251         IF CONTNO-TYPE                                                   
022252           DISPLAY 'IN2-AREA:' IN2-AREA                                   
022253         END-IF                                                           
022254         IF CER-TYPE                                                      
022255           DISPLAY 'IN3-AREA:' IN3-AREA                                   
022256         END-IF                                                           
022257       WHEN 101                                                           
022258         DISPLAY 'JSON text was zero length or all whitespace'            
022259         IF BOOKNO-TYPE                                                   
022260           DISPLAY 'IN1-AREA:' IN1-AREA                                   
022261         END-IF                                                           
022262         IF CONTNO-TYPE                                                   
022263           DISPLAY 'IN2-AREA:' IN2-AREA                                   
022264         END-IF                                                           
022265         IF CER-TYPE                                                      
022266           DISPLAY 'IN3-AREA:' IN3-AREA                                   
022267         END-IF                                                           
022268*      WHEN 102                                                           
022269*        DISPLAY 'Superflous nonwhitespace characters were found a        
022270*                'fter closing brace'                                     
022271*      WHEN 103                                                           
022272*        DISPLAY 'One or more data items had multiple matching JSO        
022273*                'N name/value pairs with differet values, and wer        
022274*                'e set to the leftmost value encountered in the J        
022275*                'SON text'                                               
022276*      WHEN 104                                                           
022277*        DISPLAY 'One or more JSON name/value pairs had a value th        
022278*                'at was incompatible with the matching data item'        
022279*      WHEN 105                                                           
022280*        DISPLAY 'One or more JSON name/value pairs had the value         
022281*                'true or false'                                          
022282       WHEN 106                                                           
022283         DISPLAY 'No JSON name/value pair matched any data item'          
022284         IF BOOKNO-TYPE                                                   
022285           DISPLAY 'IN1-AREA:' IN1-AREA                                   
022286         END-IF                                                           
022287         IF CONTNO-TYPE                                                   
022288           DISPLAY 'IN2-AREA:' IN2-AREA                                   
022289         END-IF                                                           
022290         IF CER-TYPE                                                      
022291           DISPLAY 'IN3-AREA:' IN3-AREA                                   
022292         END-IF                                                           
022293     END-EVALUATE                                                         
022294                                                                          
022295*    IF FUNCTION MOD(JSON-STATUS 2 * 1) / 1 = 1                           
022296*      DISPLAY 'STATUS 1 : One or more data items had no matching         
022297*              'JSON name/value pair and thus were not changed'           
022298*    END-IF                                                               
022299*                                                                         
022300*    IF FUNCTION MOD(JSON-STATUS 2 * 2) / 2 = 1                           
022301*      DISPLAY 'STATUS 2 : One or more data JSON name/value pairs         
022302*              'did not match any data item'                              
022303*    END-IF                                                               
022304*                                                                         
022305*    IF FUNCTION MOD(JSON-STATUS 2 * 4) / 4 = 1                           
022306*      DISPLAY 'STATUS 4 : One or more data items had multiple mat        
022307*              'ching JSON name/value pairs with duplicate values'        
022308*    END-IF                                                               
022309*                                                                         
022310*    IF FUNCTION MOD(JSON-STATUS 2 * 8) / 8 = 1                           
022311*      DISPLAY 'STATUS 8 : One or more table data items had more e        
022312*              'lements than the matching JSON array'                     
022313*    END-IF                                                               
022314*                                                                         
022315*    IF FUNCTION MOD(JSON-STATUS 2 * 16) / 16 = 1                         
022316*      DISPLAY 'STATUS 16 : One or more JSON arrays had more value        
022317*              's than the matching data item'                            
022318*    END-IF                                                               
022319*                                                                         
022320*    IF FUNCTION MOD(JSON-STATUS 2 * 32) / 32 = 1                         
022321*      DISPLAY 'STATUS 32 : One or more data items were not change        
022322*              'd because the corresponding JSON name/value pair h        
022323*              'ad the value null'                                        
022324*    END-IF                                                               
022325*                                                                         
022326*    IF FUNCTION MOD(JSON-STATUS 2 * 64) / 64 = 1                         
022327*      DISPLAY 'STATUS 64 : One or more table data items had eleme        
022328*              'nts that were not changed because the correspondin        
022329*              'g JSON value was null'                                    
022330*    END-IF                                                               
022331*                                                                         
022332     IF FUNCTION MOD(JSON-STATUS 2 * 128) / 128 = 1                       
022333       DISPLAY 'STATUS 128 : A SIZE ERROR condition was detected i        
022334-              'n one or more numeric assignments. The data items         
022335-              'were modified anyway'                                     
022336       IF BOOKNO-TYPE                                                     
022337         DISPLAY 'IN1-AREA:' IN1-AREA                                     
022338       END-IF                                                             
022339       IF CONTNO-TYPE                                                     
022340         DISPLAY 'IN2-AREA:' IN2-AREA                                     
022341       END-IF                                                             
022342       IF CER-TYPE                                                        
022343         DISPLAY 'IN3-AREA:' IN3-AREA                                     
022344       END-IF                                                             
022345     END-IF                                                               
022346                                                                          
022347     IF FUNCTION MOD(JSON-STATUS 2 * 256) / 256 = 1                       
022348       DISPLAY 'STATUS 256 : A loss of information occurred in on         
022349-              'e or more alphanumeric assignments. The data items        
022350-              ' were modified anyway'                                    
022351       IF BOOKNO-TYPE                                                     
022352         DISPLAY 'IN1-AREA:' IN1-AREA                                     
022353       END-IF                                                             
022354       IF CONTNO-TYPE                                                     
022355         DISPLAY 'IN2-AREA:' IN2-AREA                                     
022356       END-IF                                                             
022357       IF CER-TYPE                                                        
022358         DISPLAY 'IN3-AREA:' IN3-AREA                                     
022359       END-IF                                                             
022360     END-IF                                                               
022361*                                                                         
022362*    IF FUNCTION MOD(JSON-STATUS 2 * 512) / 512 = 1                       
022363*      DISPLAY 'STATUS 512 : One or more JSON name/value pairs had        
022364*             ' a value that resulted in one or more substitution'        
022365*      DISPLAY 'characters when translated from Unicode to the CCS        
022366*              'ID specified by the CODEPAGE compiler option'             
022367*    END-IF                                                               
022368     .                                                                    
022369     EJECT                                                                
022370 X-TAKE-CHECKPOINT   SECTION.                                             
022400                                                                          
022700     PERFORM IMS-CHECKPOINT                                               
022800     MOVE ZERO TO CHKP-ANT                                                
023000     .                                                                    
023100     EJECT                                                                
023200* --- IMS SECTIONS  ---                                                   
023300                                                                          
024400     EJECT                                                                
024500 IMS-GHU-WDGX6302 SECTION.                                                
024600                                                                          
024610     STRING 'WDR501  (WDGXKEY = ' W-6301KEY-X ')'                         
024620          DELIMITED BY SIZE INTO SSA1                                     
024630     STRING 'WDGX6302(KEY6302 = ' W-6302KEY-X ')'                         
024640          DELIMITED BY SIZE INTO SSA2                                     
024900     MOVE '  GE' TO GOOD-STATUSCODES                                      
025000     CALL CBLTDLI USING GHU 6301-PCB DLI-IO-WDGX6302 SSA1 SSA2            
025100     MOVE 6301-STATUS-CODE TO STATUS-WS                                   
025200     PERFORM IMS-STATUSCHECK                                              
025300     .                                                                    
025400     SKIP3                                                                
025500 IMS-REPL-WDGX6302 SECTION.                                               
025600                                                                          
025700     MOVE '  ' TO GOOD-STATUSCODES                                        
025800     CALL CBLTDLI USING REPL 6301-PCB DLI-IO-WDGX6302                     
025900     MOVE 6301-STATUS-CODE TO STATUS-WS                                   
026000     PERFORM IMS-STATUSCHECK                                              
026100     .                                                                    
026200     EJECT                                                                
026300 IMS-RESTART SECTION.                                                     
026400     SKIP2                                                                
026500     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
026600     MOVE '  ' TO GOOD-STATUSCODES                                        
026700     CALL CBLTDLI USING XRST MSG-PCB                                      
026800                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
026900                        CHKP-AREA-LENGTH CHKP-AREA                        
027000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
027100     PERFORM IMS-STATUSCHECK                                              
027200     .                                                                    
027300     SKIP3                                                                
027400 IMS-CHECKPOINT SECTION.                                                  
027500     SKIP2                                                                
027600     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
027700     MOVE '  XD' TO GOOD-STATUSCODES                                      
027800     CALL CBLTDLI USING CHKP MSG-PCB                                      
027900                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
028000                        CHKP-AREA-LENGTH CHKP-AREA                        
028100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
028200     PERFORM IMS-STATUSCHECK                                              
028300                                                                          
028400     IF IMS-NOT-OK                                                        
028500       MOVE 'IMS CONTROL REGION NOT ACCESSIBLE' TO ERROR-TEXT-STR         
028600       DISPLAY ERROR-TEXT                                                 
028700       CALL FELLOG                                                        
028800     END-IF                                                               
028900     .                                                                    
029000     EJECT                                                                
029100 IMS-STATUSCHECK SECTION.                                                 
029200     SKIP2                                                                
029300     SET STATUS-IX TO 1                                                   
029400     SEARCH GOOD-STATUS                                                   
029500       AT END                                                             
029600         STRING ' WRONG STATUSCODE FROM IMS: ' STATUS-WS                  
029700           DELIMITED BY SIZE INTO ERROR-TEXT-STR                          
029800         DISPLAY ERROR-TEXT                                               
029900         CALL FELLOG                                                      
030000       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
030100         CONTINUE                                                         
030200     END-SEARCH                                                           
030300     .                                                                    
