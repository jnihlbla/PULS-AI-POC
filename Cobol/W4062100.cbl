001400 ID DIVISION.                                                             
001500 PROGRAM-ID.     W4062100.                                                
001600 AUTHOR.         KARANDE DIGAMBAR.                                        
001700 DATE-WRITTEN.   02/09/19.                                                
001800 DATE-COMPILED.                                                           
001900                                                                          
002000*    FUNCTION:                                                            
002100*        IT SHOWS THE SHIPMENT INFORMATION FROM WDE1. THIS SCREEN         
002200*        STARTS ONLY FROM THE 4622.                                       
002210*                                                                         
002220*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!        
002230*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0183      *        
002240*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!        
002300*                                                                         
002410*        THE PROGRAM READS     WDE1                                       
002500*                                                                         
002600*    INDATA.                                                              
002700*        TRANSACTION: W4T621                                              
002800*        MID:         W4I62101                                            
002900*                                                                         
003000*    OUTDATA.                                                             
003100*        MOD:         W4O62101                                            
003200                                                                          
003300     SKIP3                                                                
003400 ENVIRONMENT DIVISION.                                                    
003500                                                                          
003600 DATA DIVISION.                                                           
003700     EJECT                                                                
003800 WORKING-STORAGE SECTION.                                                 
003900 77  IDPGM                       PIC X(08)   VALUE 'W4062100'.            
004000                                                                          
004100*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004200 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004300                                                                          
004400 77  YES                         PIC X       VALUE 'J'.                   
004500 77  NOO                         PIC X       VALUE 'N'.                   
004600                                                                          
004701*    --- INDEX FOR SCROLL LINES                                           
004702 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004710 77  MAX-INDX                    PIC S9(4)  VALUE +14   COMP SYNC.        
004800*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
005000                                                                          
005100 77  MSG-KVLL-TILL-WHELP         PIC S9(4)  VALUE +85   COMP SYNC.        
005200                                                                          
005300 77  KEYS-SW                     PIC X       VALUE 'J'.                   
005400     88  KEYS-OK                             VALUE 'J'.                   
005500     88  KEYS-WRONG                          VALUE 'N'.                   
005600                                                                          
005700 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005800     88  OWN-MID                             VALUE '4621'.                
005900     88  GOOD-MID                            VALUE '4621' '4622'.         
006400     88  HELP-MID                            VALUE '0551'.                
006500     EJECT                                                                
006600*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
006700 01  GENERAL-SUBPROGRAMS.                                                 
006800     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006900     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007300     EJECT                                                                
007400*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
007500*01 -COPY WMEDAREA                                                        
007600     SKIP3                                                                
007610 01  FILLER.                                                              
007620   03  FELMEDD-AREA.                                                      
007630     05  FELMEDD-ENGLISH.                                                 
007640       10  FILLER                PIC X(40)                                
007650           VALUE '622 4621 WRONG SCREEN SELECTED         '.               
007660                                                                          
007700 01  MESSAGE-CODES.                                                       
007901     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
007910     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
007920     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
008000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008100     03  ERR-IDSHIPM-MISSING     PIC X(3)    VALUE '341'.                 
008200     EJECT                                                                
008300*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
008400*                                                                         
008500 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008600     SKIP3                                                                
008700*01 -COPY WMSGINIT                                                        
008800     EJECT                                                                
008900*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
009000*                                                                         
009100 01  SAVE-AREA.                                                           
009200     03  SAVE-IDTRANS            PIC X(4)    VALUE '4621'.                
009210     03  SAVE-4621-4622.                                                  
009230         05  SAVE-IDSHIPM-ENTER  PIC 9(7).                                
009240         05  SAVE-IDSHIPM-NEXT   PIC 9(7).                                
009300     03  SAVE-IDSHIPM            PIC 9(07)   VALUE ZERO.                  
009301     03  SAVE-IDDISTR-ENTER      PIC S9(5)        COMP-3.                 
009302     03  SAVE-IDDISTR-NEXT       PIC S9(5)        COMP-3.                 
009303     03  SAVE-IDKUNDNR-ENTER     PIC S9(7)        COMP-3.                 
009304     03  SAVE-IDKUNDNR-NEXT      PIC S9(7)        COMP-3.                 
009305     03  SAVE-IDPRODNR-ENTER     PIC S9(7)        COMP-3.                 
009306     03  SAVE-IDPRODNR-NEXT      PIC S9(7)        COMP-3.                 
009307     03  SAVE-IDKOLLI-ENTER      PIC S9(5)        COMP-3.                 
009310     03  SAVE-IDKOLLI-NEXT       PIC S9(5)        COMP-3.                 
009400     EJECT                                                                
009500*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
009600*                                                                         
009700 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009800     SKIP3                                                                
009900*01  MID -COPY W4I62101                                                   
009901     SKIP3                                                                
009910 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-SW'.           
009920 01  P-TO-P-SW.                                                           
009930     03  PTOP-LL                 PIC S9(4)   VALUE 0 COMP SYNC.           
009940     03  PTOP-Z1                 PIC  X(1)   VALUE LOW-VALUE.             
009950     03  PTOP-Z2                 PIC  X(1)   VALUE LOW-VALUE.             
009960     03  PTOP-TRANSKOD           PIC  X(7)   VALUE 'W4T622 '.             
009970     03  FILLER                  PIC  X(1)   VALUE SPACE.                 
009980     03  FILLER                  PIC  X(4)   VALUE '4621'.                
009990     03  PTOP-KDMFSFOR           PIC  X(1).                               
009991*    03  -COPY W4I62201   -PRE PTOP-                                      
010000     EJECT                                                                
010001 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
010002     SKIP3                                                                
010003*01  -COPY WMSGAREA                                                       
010004     EJECT                                                                
010010*   TO RETURN TO MAIN MENU                                                
010020*    03  FILLER  -COPY W0O50401  -PRE MOD0504-  -RED MSG-AREA.            
010030     EJECT                                                                
010040     03  MOD REDEFINES MSG-AREA.                                          
010050*      05  -COPY W4O62101                                                 
010700     EJECT                                                                
010800 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010900     SKIP3                                                                
011000*01  -COPY WMFSAREA                                                       
011100     EJECT                                                                
011200*    --- WORK-AREAS FOR IMS-SECTIONS                                      
011300*                                                                         
011400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011500     SKIP3                                                                
011600 01  KEYS-TO-DLI.                                                         
011713                                                                          
011714     03  W-IDSHIPM-X.                                                     
011715         05  W-IDSHIPM           PIC 9(7)    VALUE ZERO.                  
011716                                                                          
011717     03  W-WDE111KY-X.                                                    
011718         05  W-WDE111-IDDISTR    PIC S9(5)   VALUE ZERO  COMP-3.          
011719         05  W-WDE111-IDKUNDNR   PIC S9(7)   VALUE ZERO  COMP-3.          
011720                                                                          
011721     03  W-WDE121KY-X.                                                    
011722         05  W-WDE121-IDPRODNR   PIC S9(7)   VALUE ZERO  COMP-3.          
011723         05  W-WDE121-IDKOLLI    PIC S9(5)   VALUE ZERO  COMP-3.          
011724                                                                          
011800     SKIP2                                                                
011900*    --- STATUS-KOD FRÅN IMS                                              
012000 01  STATUS-WS                   PIC XX.                                  
012100     88  SEGMENT-FOUND                       VALUE '  '.                  
012200     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
012300     88  SEGMENT-MISSING                     VALUE 'GE'.                  
012400     SKIP2                                                                
012500 01  GOOD-STATUSCODES.                                                    
012600     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012700     SKIP3                                                                
012800 01  SSA1                        PIC X(64).                               
012900 01  SSA2                        PIC X(64).                               
012910 01  SSA3                        PIC X(64).                               
013000     EJECT                                                                
013100*    --- IMS FUNCTION CODES                                               
013200*01  -COPY W0003                                                          
013400     EJECT                                                                
013500*    ---  DLI INPUT-OUTPUT AREA                                           
013600                                                                          
013701 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE101'.                      
013702 01  DLI-IO-WDE101.                                                       
013703*    03  -COPY WDE101                                                     
013704     EJECT                                                                
013705 01  FILLER         PIC X(25) VALUE 'DLI-IO-WDE111-WDE121'.               
013706 01  DLI-IO-WDE111-121.                                                   
013707     03  DLI-IO-WDE111.                                                   
013708*        05  -COPY WDE111                                                 
013709     03  DLI-IO-WDE121.                                                   
013710*        05  -COPY WDE121                                                 
013711     EJECT                                                                
014000     EJECT                                                                
014100 LINKAGE SECTION.                                                         
014200*01  -COPY W0009   -PRE MSG-                                              
014210*01  -COPY W0009   -PRE ALT-                                              
014300*01  -COPY W0008   -PRE WDP7-                                             
014400     05  FILLER                  PIC X.                                   
014501                                                                          
014502*01  -COPY W0008  -PRE WDE1-                                              
014510     05  FILLER                  PIC X.                                   
014600     EJECT                                                                
014701 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB WDP7-PCB WDE1-PCB.             
014702 MAIN SECTION.                                                            
014710     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB WDP7-PCB WDE1-PCB.             
014800                                                                          
015000     PERFORM IMS-GET-MSG                                                  
015100     IF SEGMENT-FOUND                                                     
015200       PERFORM A-INIT                                                     
015300       PERFORM B-CHECK-KEYS                                               
015400       IF KEYS-OK                                                         
015500           IF MFS-RETURN                                                  
015600              PERFORM J-RETURN-TO-4622                                    
015601           ELSE                                                           
015602             IF MFS-FIRST                                                 
015603               PERFORM C-FIRST-PAGE                                       
015604             ELSE                                                         
015605               IF MFS-NEXT                                                
015606                 PERFORM D-NEXT-PAGE                                      
015607               ELSE                                                       
015608                 PERFORM E-SAME-PAGE                                      
015609               END-IF                                                     
015610             END-IF                                                       
015620           END-IF                                                         
015900           PERFORM F-READ-SHOW-INFO                                       
016000       END-IF                                                             
016100*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
016200*    --- IF PROGRAM-TO-PROGRAM-SWITCH:       = MOD-LENGTH + 17            
016210       IF MFS-RETURN                                                      
016211         IF KEYS-WRONG                                                    
016222           PERFORM J-RETURN-TO-4622                                       
016223         END-IF                                                           
016224         PERFORM IMS-ISRT-ALT-MSG                                         
016230       ELSE                                                               
016240         IF GOOD-MID                                                      
016300           COMPUTE MSG-KVLL = LENGTH OF MOD-W4O62101 + 4                  
016400           PERFORM IMS-INSERT-MSG                                         
016410         END-IF                                                           
016420       END-IF                                                             
016500     END-IF                                                               
016700                                                                          
016800     MOVE ZERO TO RETURN-CODE                                             
016900     GOBACK                                                               
017000     .                                                                    
017100     EJECT                                                                
017200 A-INIT SECTION.                                                          
017300                                                                          
017400     IF MSG-DOUBLE-TRANSACTIONS                                           
017500       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W4I62101                 
017600       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017700       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017800     ELSE                                                                 
017900       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W4I62101                  
018000       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
018100       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
018200     END-IF                                                               
018300                                                                          
018400     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
018500     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
018600     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018700                                                                          
018800     MOVE LOW-VALUE TO MSG-AREA                                           
018900     MOVE 'W4O621N1' TO MFS-IDMOD                                         
019000     MOVE '4621' TO MOD-IDTRANS                                           
019100     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
019200                                                                          
019300     IF OWN-MID OR HELP-MID                                               
019400       CONTINUE                                                           
019500     ELSE                                                                 
019600       MOVE SPACE TO MFS-KDTRTYP                                          
019700       MOVE '7' TO MFS-IDPFK                                              
019800     END-IF                                                               
019810                                                                          
019910     IF NOT GOOD-MID                                                      
019931       MOVE NOO  TO KEYS-SW                                               
019934       PERFORM S10-WRONG-PICTURE-MESSAGE                                  
019940     END-IF                                                               
020100     .                                                                    
020200     EJECT                                                                
020300 B-CHECK-KEYS SECTION.                                                    
020400                                                                          
020500     MOVE ALL '+'           TO MSGI-WMSGINIT                              
020600     MOVE '001'             TO MSGI-KDCALL                                
020700     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
020800     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
020900     MOVE '4621'            TO MSGI-IDTRANS                               
021300     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
021400     MOVE MSGI-SPAR-AREA TO SAVE-AREA                                     
021500                                                                          
021600*    - LANGUAGE TO BE USED BY MEDKONV                                     
021700     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
021800                                                                          
021900     MOVE YES TO KEYS-SW                                                  
022101                                                                          
022102*    -- CHECK OF IDSHIPM                                                  
022103     MOVE MFS-ERASE-FIELD TO MOD-IDSHIPM-IN                               
022104                                                                          
022105     IF MID-IDSHIPM-IN NOT = ALL '+'                                      
022106       MOVE '7'         TO MFS-IDPFK                                      
022107       MOVE SPACE       TO MFS-KDTRTYP                                    
022108       MOVE MID-IDSHIPM-IN                                                
022109                        TO SAVE-IDSHIPM                                   
022110     END-IF                                                               
022111                                                                          
022113     IF SAVE-IDSHIPM NUMERIC                                              
022114       MOVE SAVE-IDSHIPM TO W-IDSHIPM                                     
022115     ELSE                                                                 
022116       MOVE NOO TO KEYS-SW                                                
022117       MOVE ZERO         TO W-IDSHIPM                                     
022120     END-IF                                                               
022201                                                                          
022202     IF GOOD-MID OR KEYS-OK                                               
022203       IF SAVE-IDSHIPM NUMERIC                                            
022204         MOVE SAVE-IDSHIPM    TO MOD-IDSHIPM-UT                           
022205       ELSE                                                               
022206         MOVE ZERO            TO MOD-IDSHIPM-UT                           
022207       END-IF                                                             
022208     ELSE                                                                 
022209       MOVE MFS-ERASE-FIELD TO MOD-IDSHIPM-UT                             
022210     END-IF                                                               
022300                                                                          
022400     IF KEYS-WRONG                                                        
022500       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
022600       CALL WMEDKONV USING MED-WMEDAREA                                   
022700       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
022800       PERFORM MFS-ERASE-FIELD-IN                                         
022900       PERFORM MFS-ERASE-FIELD-OUT                                        
023000     END-IF                                                               
023100     .                                                                    
023201     EJECT                                                                
023202 C-FIRST-PAGE SECTION.                                                    
023203                                                                          
023204     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
023205     CALL WMEDKONV USING MED-WMEDAREA                                     
023206     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
023207                                                                          
023208     PERFORM MFS-ERASE-FIELD-IN                                           
023209     .                                                                    
023210     EJECT                                                                
023211 D-NEXT-PAGE SECTION.                                                     
023212                                                                          
023213     IF SAVE-IDTRANS = '4621'                                             
023214       MOVE SAVE-IDDISTR-NEXT  TO W-WDE111-IDDISTR                        
023215       MOVE SAVE-IDKUNDNR-NEXT TO W-WDE111-IDKUNDNR                       
023216       MOVE SAVE-IDPRODNR-NEXT TO W-WDE121-IDPRODNR                       
023217       MOVE SAVE-IDKOLLI-NEXT  TO W-WDE121-IDKOLLI                        
023218     ELSE                                                                 
023219       PERFORM MFS-ERASE-FIELD-IN                                         
023220     END-IF                                                               
023221     .                                                                    
023222     EJECT                                                                
023223 E-SAME-PAGE SECTION.                                                     
023224                                                                          
023225     IF SAVE-IDTRANS = '4621' OR '0551'                                   
023226       MOVE SAVE-IDDISTR-ENTER  TO W-WDE111-IDDISTR                       
023227       MOVE SAVE-IDKUNDNR-ENTER TO W-WDE111-IDKUNDNR                      
023228       MOVE SAVE-IDPRODNR-ENTER TO W-WDE121-IDPRODNR                      
023229       MOVE SAVE-IDKOLLI-ENTER  TO W-WDE121-IDKOLLI                       
023230       IF MID-W4I62101 = ALL '+'                                          
023231         PERFORM MFS-ERASE-FIELD-IN                                       
023232       ELSE                                                               
023236         PERFORM MFS-DONT-TOUCH-FIELD-OUT                                 
023237       END-IF                                                             
023238     ELSE                                                                 
023239       PERFORM MFS-ERASE-FIELD-IN                                         
023240     END-IF                                                               
023241     .                                                                    
023242     EJECT                                                                
023500 F-READ-SHOW-INFO SECTION.                                                
023600                                                                          
023700     PERFORM FA-READ-BASICDATA                                            
023800                                                                          
023900     IF SEGMENT-MISSING                                                   
024000* MOVE RIGHT ERRORMESSAGE                                                 
024010        MOVE ERR-IDSHIPM-MISSING TO MED-IDMFSFEL                          
024100        CALL WMEDKONV USING MED-WMEDAREA                                  
024200        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
024300        PERFORM MFS-ERASE-FIELD-OUT                                       
024310        PERFORM MFS-ERASE-LINE-FIELD-OUT                                  
024400     ELSE                                                                 
024508       MOVE +1 TO INDX                                                    
024510       IF SEGMENT-FOUND                                                   
024511         MOVE SGMT-IDDISTR      TO SAVE-IDDISTR-ENTER                     
024512         MOVE SGMT-IDKUNDNR     TO SAVE-IDKUNDNR-ENTER                    
024513         MOVE SKOLLI-IDPRODNR   TO SAVE-IDPRODNR-ENTER                    
024514         MOVE SKOLLI-IDKOLLI    TO SAVE-IDKOLLI-ENTER                     
024515       ELSE                                                               
024516         MOVE W-WDE111-IDDISTR  TO SAVE-IDDISTR-ENTER                     
024517         MOVE W-WDE111-IDKUNDNR TO SAVE-IDKUNDNR-ENTER                    
024518         MOVE W-WDE121-IDPRODNR TO SAVE-IDPRODNR-ENTER                    
024519         MOVE W-WDE121-IDKOLLI  TO SAVE-IDKOLLI-ENTER                     
024520       END-IF                                                             
024521                                                                          
024522       PERFORM UNTIL INDX > MAX-INDX                                      
024523         IF SEGMENT-FOUND                                                 
024524           MOVE SGMT-IDDISTR    TO MOD-IDDISTR (INDX)                     
024525           MOVE SGMT-IDKUNDNR   TO MOD-IDKUNDNR (INDX)                    
024526           MOVE SKOLLI-IDORDNR7 TO MOD-IDORDNR5 (INDX)                    
024527           MOVE SKOLLI-IDKOLLI  TO MOD-IDKOLLI (INDX)                     
024528           PERFORM IMS-GNP-WDE111-121                                     
024529         ELSE                                                             
024530           MOVE MFS-ERASE-FIELD TO MOD-IDDISTR (INDX)                     
024531                                   MOD-IDKUNDNR (INDX)                    
024532                                   MOD-IDORDNR5 (INDX)                    
024533                                   MOD-IDKOLLI (INDX)                     
024534         END-IF                                                           
024535         ADD 1 TO INDX                                                    
024536       END-PERFORM                                                        
024537                                                                          
024538       IF SEGMENT-FOUND                                                   
024539         MOVE SGMT-IDDISTR      TO SAVE-IDDISTR-NEXT                      
024540         MOVE SGMT-IDKUNDNR     TO SAVE-IDKUNDNR-NEXT                     
024541         MOVE SKOLLI-IDPRODNR   TO SAVE-IDPRODNR-NEXT                     
024542         MOVE SKOLLI-IDKOLLI    TO SAVE-IDKOLLI-NEXT                      
024543         MOVE INF-MORE-INFO-EXISTS                                        
024544                                TO MED-IDMFSINF                           
024545         CALL WMEDKONV       USING MED-WMEDAREA                           
024546         MOVE MED-TEMFSINF      TO MOD-TEMFSINF                           
024547       ELSE                                                               
024548         MOVE SAVE-IDDISTR-ENTER  TO SAVE-IDDISTR-NEXT                    
024549         MOVE SAVE-IDKUNDNR-ENTER TO SAVE-IDKUNDNR-NEXT                   
024550         MOVE SAVE-IDPRODNR-ENTER TO SAVE-IDPRODNR-NEXT                   
024551         MOVE SAVE-IDKOLLI-ENTER  TO SAVE-IDKOLLI-NEXT                    
024552         MOVE INF-LAST-PAGE       TO MED-IDMFSINF                         
024553         CALL WMEDKONV         USING MED-WMEDAREA                         
024554         MOVE MED-TEMFSINF        TO MOD-TEMFSINF                         
024555       END-IF                                                             
024556                                                                          
024557       MOVE '002'       TO MSGI-KDCALL                                    
024558       MOVE '4621'      TO SAVE-IDTRANS                                   
024559       MOVE SAVE-AREA   TO MSGI-SPAR-AREA                                 
024560       CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                         
024600     END-IF                                                               
024700     .                                                                    
024800     EJECT                                                                
024900 FA-READ-BASICDATA SECTION.                                               
025000                                                                          
025100     PERFORM IMS-GU-WDE101                                                
025101                                                                          
025110     IF SEGMENT-FOUND                                                     
025120       IF MFS-NEXT OR MFS-ENTER                                           
025121         PERFORM IMS-GNP-WDE111-121-KEY                                   
025130       ELSE                                                               
025131         PERFORM IMS-GNP-WDE111-121                                       
025140       END-IF                                                             
025200     END-IF                                                               
025500     .                                                                    
025601     EJECT                                                                
025602                                                                          
025603 J-RETURN-TO-4622 SECTION.                                                
025604                                                                          
025605** SCREEN 4622 GETS THE DATA FROM COMMON SAVE-AREA                        
025606                                                                          
025607     MOVE ALL '+'              TO PTOP-MID-W4I62201                       
025608     MOVE MFS-KDMFSFOR         TO PTOP-KDMFSFOR                           
025609     COMPUTE PTOP-LL = LENGTH OF PTOP-MID-W4I62201 + 17                   
025610     .                                                                    
025611     EJECT                                                                
025612 S10-WRONG-PICTURE-MESSAGE SECTION.                                       
025613     SKIP2                                                                
025614* *****************************************************                   
025615*                                                     *                   
025616* GIVE WRONG PICTURE MESSAGE FROM WHELP               *                   
025617*                                                     *                   
025618* *****************************************************                   
025619     SKIP2                                                                
025620     MOVE 'W0O50401'          TO MFS-IDMOD                                
025621     MOVE MFS-ERASE-FIELD     TO MOD0504-IDTRANS                          
025622     MOVE FELMEDD-ENGLISH     TO MOD0504-TEMFSINF                         
025623     MOVE MSG-KVLL-TILL-WHELP TO MSG-KVLL                                 
025624     PERFORM IMS-INSERT-MSG                                               
025625     .                                                                    
025626     EJECT                                                                
025630                                                                          
025900 MFS-ERASE-FIELD-OUT SECTION.                                             
026000                                                                          
026100*    --- ALLA UTDATA-FÄLT                                                 
026210*    --- INCL. SCROLL KEYS                                                
026300     MOVE MFS-ERASE-FIELD TO MOD-IDSHIPM-UT                               
026500     .                                                                    
026601     SKIP3                                                                
026602 MFS-ERASE-LINE-FIELD-OUT SECTION.                                        
026603                                                                          
026604*    --- OUTDATA-FIELD ON SCROLL KEYS                                     
026605     MOVE 1 TO INDX                                                       
026606     PERFORM UNTIL INDX > MAX-INDX                                        
026607       MOVE MFS-ERASE-FIELD TO MOD-IDDISTR (INDX)                         
026608                               MOD-IDKUNDNR (INDX)                        
026609                               MOD-IDORDNR5 (INDX)                        
026610                               MOD-IDKOLLI (INDX)                         
026611       ADD 1 TO INDX                                                      
026612     END-PERFORM                                                          
026620     .                                                                    
026700     SKIP3                                                                
026800 MFS-ERASE-FIELD-IN SECTION.                                              
026900                                                                          
027000*    --- ALLA INDATA-FÄLT                                                 
027100     MOVE MFS-ERASE-FIELD TO MOD-IDSHIPM-IN                               
027300     .                                                                    
027400     EJECT                                                                
027500 MFS-DONT-TOUCH-FIELD-OUT  SECTION.                                       
027600                                                                          
027700*    --- ALLA UTDATA-FÄLT                                                 
027810*    --- INCL SCROLL KEYS AND LINEDATA                                    
027900     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDSHIPM-UT                        
028000                                                                          
028101     MOVE +1 TO INDX                                                      
028102     PERFORM UNTIL INDX > MAX-INDX                                        
028103       PERFORM MFS-DONT-TOUCH-LINE-FIELD-OUT                              
028104       ADD +1 TO INDX                                                     
028105     END-PERFORM                                                          
028106     .                                                                    
028107     SKIP2                                                                
028108 MFS-DONT-TOUCH-LINE-FIELD-OUT  SECTION.                                  
028109                                                                          
028110*    --- OUTDATA FIELD ON SCROLL KEYS                                     
028111     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDDISTR (INDX)                    
028112                                    MOD-IDKUNDNR (INDX)                   
028113                                    MOD-IDORDNR5 (INDX)                   
028114                                    MOD-IDKOLLI (INDX)                    
028200     .                                                                    
028300     SKIP3                                                                
030500* --- IMS SECTIONS ---                                                    
030600     SKIP3                                                                
030700 IMS-GET-MSG SECTION.                                                     
030800                                                                          
030900     MOVE '  QC' TO GOOD-STATUSCODES                                      
031000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
031100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031200     PERFORM IMS-STATUSCHECK                                              
031300     .                                                                    
031400     SKIP3                                                                
031500 IMS-INSERT-MSG SECTION.                                                  
031600                                                                          
032000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
032100     MOVE SPACE TO GOOD-STATUSCODES                                       
032200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
032300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
032400     PERFORM IMS-STATUSCHECK                                              
032500     .                                                                    
032601     EJECT                                                                
032602 IMS-ISRT-ALT-MSG SECTION.                                                
032603                                                                          
032604     MOVE SPACE TO GOOD-STATUSCODES                                       
032605     CALL CBLTDLI USING ISRT ALT-PCB P-TO-P-SW                            
032606     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
032607     PERFORM IMS-STATUSCHECK                                              
032608     .                                                                    
032609     SKIP3                                                                
032610 IMS-GU-WDE101 SECTION.                                                   
032611                                                                          
032612     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
032613          DELIMITED BY SIZE INTO SSA1                                     
032614     MOVE '  GE' TO GOOD-STATUSCODES                                      
032615     CALL CBLTDLI USING GU WDE1-PCB DLI-IO-WDE101 SSA1                    
032616     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
032617     PERFORM IMS-STATUSCHECK                                              
032618     .                                                                    
032619     EJECT                                                                
032620 IMS-GNP-WDE111-121 SECTION.                                              
032621                                                                          
032622     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
032623          DELIMITED BY SIZE INTO SSA1                                     
032624     MOVE   'WDE111  *D'  TO SSA2                                         
032626     MOVE   'WDE121  *D'  TO SSA3                                         
032628     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
032629     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE111-121                    
032630                                             SSA1 SSA2 SSA3               
032631     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
032632     PERFORM IMS-STATUSCHECK                                              
032633     .                                                                    
032634     EJECT                                                                
032635 IMS-GNP-WDE111-121-KEY SECTION.                                          
032636                                                                          
032637     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
032638          DELIMITED BY SIZE INTO SSA1                                     
032639     STRING 'WDE111  *D(WDE111KY =' W-WDE111KY-X ')'                      
032640          DELIMITED BY SIZE INTO SSA2                                     
032641     STRING 'WDE121  *D(WDE121KY =' W-WDE121KY-X ')'                      
032642          DELIMITED BY SIZE INTO SSA3                                     
032643     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
032644     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE111-121                    
032645                                             SSA1 SSA2 SSA3               
032646     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
032647     PERFORM IMS-STATUSCHECK                                              
032648     .                                                                    
032649     EJECT                                                                
032800 IMS-STATUSCHECK SECTION.                                                 
032900                                                                          
033000     SET STATUS-IX TO 1                                                   
033100     SEARCH GOOD-STATUS                                                   
033200       AT END                                                             
033300         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
033400         DELIMITED BY SIZE INTO ERROR-TEXT                                
033500         CALL FELLOG                                                      
033600       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
033700         CONTINUE                                                         
033800     END-SEARCH                                                           
033900     .                                                                    
