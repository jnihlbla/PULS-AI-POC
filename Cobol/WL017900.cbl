001200 ID DIVISION.                                                             
001300 PROGRAM-ID.     WL017900.                                                
001400 AUTHOR.         GÖRAN KJELLSON   GUIDE.                                  
001500 DATE-WRITTEN.   NOVEMBER 2001                                            
001600 DATE-COMPILED.                                                           
001700                                                                          
001800*    NAME:       CARPARTS.LDC.SHOWINVENTORYQUEUE2                         
001810*    WEB-LDC: WL017900 PROGRAM IS A REPLICA OF W5030900 PROGRAM           
001820*             AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS.                    
001900*                                                                         
002000*    FUNCTION:                                                            
002100*        *                                                                
002110*        INVENTERING                                                      
002120*        PROGRAMMET HAR TVÅ FUNKTIONER                                    
002130*        - TITTA PÅ INVENTERINGSKÖN                                       
002140*        - VÄLJA VILKA ARTIKLAR MAN VILL HA UT PÅ INVENTERINGS-           
002150*          UNDERLAG                                                       
002200*                                                                         
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSACTION: WL0179U                                             
002700*        REQUEST:     WL0179I1                                            
002800*                                                                         
002900*    OUTDATA.                                                             
003000*        RESPONSE:    WL0179O1                                            
003100                                                                          
003300 ENVIRONMENT DIVISION.                                                    
003500 INPUT-OUTPUT SECTION.                                                    
003700 FILE-CONTROL.                                                            
004000                                                                          
004100 DATA DIVISION.                                                           
004300 FILE SECTION.                                                            
004500                                                                          
004600 WORKING-STORAGE SECTION.                                                 
004700 77  IDPGM                       PIC X(08)   VALUE 'WL017900'.            
004800                                                                          
004900*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
005000 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
005100 77  KDRC-DISPLAY                PIC Z(5).                                
005110 77  CURR-DISPLAY                PIC X(16) VALUE 'MAIN'.                  
005120 77  CURR-SECTION                PIC X(16) VALUE 'MAIN'.                  
005130 77  CURR-IMS-SECTION            PIC X(16) VALUE SPACE.                   
005200                                                                          
005300 77  YES                         PIC X       VALUE 'J'.                   
005400 77  NOO                         PIC X       VALUE 'N'.                   
005410 77  WS-KVRADER                  PIC S9(9).                               
005411 77  IX                          PIC S9(9)  VALUE +0   COMP SYNC.         
005412 77  IND                         PIC S9(9)  VALUE +0   COMP SYNC.         
005413 77  INDX                        PIC S9(9)  VALUE +0   COMP SYNC.         
005420 77  MAX-IX                      PIC S9(9)  VALUE +500 COMP SYNC.         
005500                                                                          
006200 77  KEYS-SW                     PIC X       VALUE 'J'.                   
006300     88  KEYS-OK                             VALUE 'J'.                   
006400     88  KEYS-WRONG                          VALUE 'N'.                   
006401                                                                          
006402 77  INDATA-SW                 PIC X         VALUE 'J'.                   
006403      88  INDATA-OK                          VALUE 'J'.                   
006404      88  INDATA-FEL                         VALUE 'N'.                   
006405                                                                          
006406 01  WS-INV-DAREGDAT.                                                     
006407     03  WS-FIX-DAREGDAT      PIC 9(9).                                   
006408     03  WS-FILLER REDEFINES WS-FIX-DAREGDAT.                             
006409       05  WS-FILLER-AAR      PIC 9(3).                                   
006410       05  WS-DAREGDAT-AAMMDD PIC 9(6).                                   
006411                                                                          
006412 01  WS-KDINVKAT-NUM                                 PIC 9(3).            
006413 01  WS-ADLAGOMR-NUM             PIC 9(3).                                
006420 01  WS-VARIABEL                 PIC 9.                                   
006430 01  WS-LISTNR.                                                           
006440     03 WS-IDPRTOMG-MOD          PIC 9.                                   
006450     03 WS-IDLOPNR-MOD           PIC 9(5).                                
006500     EJECT                                                                
006600*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
006700 01  GENERAL-SUBPROGRAMS.                                                 
006710     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007000     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
007200     SKIP3                                                                
007300*    --- PARAMETERS TO ABEND                                              
007400                                                                          
007500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007600 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007700 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007900     SKIP3                                                                
008000 01  MESSAGE-CODES.                                                       
008200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
008700     EJECT                                                                
008800*                                                                         
008810*01  -COPY WWDC99                                                         
008820*                                                                         
008900 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
009000     SKIP3                                                                
009100*01  -COPY WZ01SUB                                                        
009200     EJECT                                                                
009300 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
009400     SKIP3                                                                
009500 01  REQU-AREA.                                                           
009600*    03  -COPY WZ01REQU                                                   
009700*    03  -COPY WL0179I1                                                   
009800     EJECT                                                                
009900 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
010000     SKIP3                                                                
010100 01  RESP-AREA.                                                           
010200*    03  -COPY WZ01RESP                                                   
010300*    03  -COPY WL0179O1                                                   
010700                                                                          
010800******************************************************************        
010900*****                                                                     
011000*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011010*****                                                                     
011013 01  IMS-KEYS.                                                            
011014   03    FILLER          PIC X(16)   VALUE 'IMS KEYS        '.            
011015                                                                          
011016 01    NYCKLAR-TILL-DLI.                                                  
011017   03    W-IDARTNR-X.                                                     
011018     05    W-IDARTNR             PIC S9(9)   VALUE ZERO  COMP-3.          
011019                                                                          
011020   03    W-ARTC-IDARTNR-X.                                                
011021     05    W-IDARTNR-ARTC        PIC S9(9)   VALUE ZERO  COMP-3.          
011022                                                                          
011023   03    W-WDH111KY-X.                                                    
011024     05    W-IDDC-UNIK           PIC X(2)    VALUE SPACE.                 
011025     05    W-KDINVKAT-UNIK       PIC S9(3)   VALUE ZERO COMP-3.           
011026     05    W-TISEGKEY-UNIK       PIC S9(9)   VALUE ZERO COMP-3.           
011027     05    W-DAREGDAT-9KOMPL-UNIK PIC 9(8)   VALUE ZERO.                  
011028                                                                          
011029   03    W-WDH11-KEY-MIN-X.                                               
011030     05    W-IDDC-MIN        PIC X(2)  VALUE SPACE.                       
011031     05    W-KDINVKAT-MIN    PIC S9(3) VALUE ZERO COMP-3.                 
011032     05    W-TISEGKEY-MIN    PIC S9(9) VALUE ZERO COMP-3.                 
011033     05    W-DAREGDAT-9KOMPL-MIN  PIC 9(8)   VALUE ZERO.                  
011034                                                                          
011035   03    W-WDH11-KEY-MAX-X.                                               
011036     05    W-IDDC-MAX        PIC X(2)  VALUE SPACE.                       
011037     05    W-KDINVKAT-MAX    PIC S9(3) VALUE +049       COMP-3.           
011038     05    W-TISEGKEY-MAX    PIC S9(9) VALUE +999999999 COMP-3.           
011039     05    W-DAREGDAT-9KOMPL-MAX  PIC 9(8)   VALUE 99999999.              
011040                                                                          
011041   03    W-WDH1BSEQ-MIN-X.                                                
011042     05    W-BSEQ-IDDC-MIN      PIC X(2)  VALUE SPACE.                    
011043     05    W-BSEQ-TISEGKEY      PIC S9(9) VALUE ZERO       COMP-3.        
011044     05    W-BSEQ-ADLAGOMR-MIN  PIC S9(3) VALUE ZERO       COMP-3.        
011045     05    W-BSEQ-ADGANG-MIN    PIC S9(3) VALUE ZERO       COMP-3.        
011046     05    W-BSEQ-ADPLATS-MIN   PIC S9(5) VALUE ZERO       COMP-3.        
011047     05    W-BSEQ-IDPRTOMG-MIN  PIC S9    VALUE ZERO       COMP-3.        
011048     05    W-BSEQ-IDLOPNR-MIN   PIC S9(5) VALUE ZERO       COMP-3.        
011049   03    W-WDH1BSEQ-MAX-X.                                                
011050     05    W-BSEQ-IDDC-MAX      PIC X(2)  VALUE SPACE.                    
011051     05    W-BSEQ-TISEGKEY      PIC S9(9) VALUE +999999999 COMP-3.        
011052     05    W-BSEQ-ADLAGOMR-MAX  PIC S9(3) VALUE +999       COMP-3.        
011053     05    W-BSEQ-ADGANG-MAX    PIC S9(3) VALUE +999       COMP-3.        
011054     05    W-BSEQ-ADPLATS-MAX   PIC S9(5) VALUE +99999     COMP-3.        
011055     05    W-BSEQ-IDPRTOMG-MAX  PIC S9    VALUE +3         COMP-3.        
011056     05    W-BSEQ-IDLOPNR-MAX   PIC S9(5) VALUE +99999     COMP-3.        
011057                                                                          
011058                                                                          
011059 01    DLI-IO-AREA.                                                       
011060   03    IO-AREA                 PIC X(200)  VALUE SPACE.                 
011061     SKIP3                                                                
011062*  03    WDH101 -COPY WDH101                 -RED IO-AREA.                
011063     EJECT                                                                
011064*  03    WDH111 -COPY WDH111                 -RED IO-AREA.                
011067     EJECT                                                                
011068 01   FILLER                      PIC X(16) VALUE 'DLI-IO-WDH1B1'.        
011069 01   DLI-IO-WDH1B1.                                                      
011070*   03 -COPY WDH111 -PRE BSEQ-                                            
011071                                                                          
011072 01  IMS-WS.                                                              
011073   03    FILLER          PIC X(16)   VALUE '     IMS-WS     '.            
011074*****                    **** STATUS-KOD FRÅN IMS                         
011075   03    STATUS-WS       PIC XX.                                          
011076         88  SEGMENT-FOUND       VALUE '  '.                              
011080         88  SEGMENT-MISSING     VALUE 'GE'.                              
011090         88  SEGMENT-EXISTS      VALUE 'II'.                              
011091                                                                          
011092   03    GOOD-STATUSCODES.                                                
011093     05  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011094                                                                          
011095 01      SSA1            PIC X(256) VALUE SPACE.                          
011096 01      SSA2            PIC X(256) VALUE SPACE.                          
011097                                                                          
011098*                            IMS FUNKTIONSKODER                           
011099*01      -COPY W0003                                                      
011100                                                                          
011110                                                                          
011200 LINKAGE SECTION.                                                         
011300*01    -COPY W0009     -PRE MSG-                                          
011420     EJECT                                                                
011430*01    -COPY W0008     -PRE INVB-                                         
011440     05  FILLER                  PIC X.                                   
011450     EJECT                                                                
011460*01    -COPY W0008     -PRE INVA-                                         
011470     05  FILLER                  PIC X.                                   
011480     EJECT                                                                
011490*01    -COPY W0008     -PRE ARTC-                                         
011491     05  FILLER                  PIC X.                                   
011492     EJECT                                                                
011493*01    -COPY W0008     -PRE WDH1B-                                        
011494     05  FILLER                  PIC X.                                   
011495     EJECT                                                                
011496*01    -COPY W0008     -PRE ALT2-                                         
011497     05  FILLER                  PIC X.                                   
011500                                                                          
011620 PROCEDURE DIVISION USING MSG-PCB                                         
011630                          INVA-PCB WDH1B-PCB.                             
011640 MAIN SECTION.                                                            
011650     ENTRY 'DLITCBL' USING MSG-PCB                                        
011660                          INVA-PCB WDH1B-PCB.                             
011700                                                                          
011900     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
012000     IF SUB-KDRC = 0                                                      
012100       PERFORM A-INIT                                                     
012200       PERFORM B-CHECK-KEYS                                               
012300       IF KEYS-OK                                                         
012400          IF REQU-KDPGMACT = 'E'                                          
012410             PERFORM C-KOLLA-INDATA-UPPDATERING                           
012411             IF INDATA-OK                                                 
012412                PERFORM D-SKICKA-ART-TILL-UTSKRIFT                        
012413**********      IF REQU-KVINVSKR-5309 > ZERO AND < +11                    
012414**                 PERFORM E-LAES-VISA-KOE2                               
012415*********       END-IF                                                    
012416             END-IF                                                       
012420          ELSE                                                            
012421             PERFORM E-LAES-VISA-KOE2                                     
012600          END-IF                                                          
012800       END-IF                                                             
012810****   CALL FELLOG                                                        
012900       PERFORM S02-RETURN-RESPONSE                                        
013000     END-IF                                                               
013200                                                                          
013500     MOVE ZERO TO RETURN-CODE                                             
013600     GOBACK                                                               
013700     .                                                                    
013800                                                                          
013900 A-INIT SECTION.                                                          
013910     MOVE 'A-INIT      '   TO CURR-SECTION                                
013920                                                                          
013930     MOVE ALL '+'      TO RESP-AREA                                       
013940     MOVE SPACE        TO RESP-IDMSG-ERROR                                
013950                          RESP-IDMSG-INFO                                 
013960                          RESP-IDELMT-ERROR                               
013970     MOVE 001          TO RESP-IDMSGVER                                   
013980     MOVE 0            TO RESP-KVRADER                                    
013992     .                                                                    
014000                                                                          
014900 B-CHECK-KEYS SECTION.                                                    
014910     MOVE 'B-CHECK-KEYS'   TO CURR-SECTION                                
014920                                                                          
014930     MOVE YES              TO KEYS-SW                                     
014931     MOVE REQU-IDDC-KEY    TO RESP-IDDC-KEY                               
014932                              W-BSEQ-IDDC-MIN                             
014933                              W-BSEQ-IDDC-MAX                             
014934                              W-IDDC-MIN                                  
014935                              W-IDDC-MAX                                  
014936                              WS-IDDC                                     
014937                                                                          
014940                                                                          
014950     IF REQU-KDPGMACT = 'S' OR 'E'                                        
014960        CONTINUE                                                          
014970     ELSE                                                                 
014980        MOVE '023'              TO RESP-IDMSG-ERROR                       
014990*       WRONG ACTION KEY ***                                              
014991        MOVE 'KDPGMACT'         TO RESP-IDELMT-ERROR                      
014992        MOVE NOO                TO KEYS-SW                                
014993     END-IF                                                               
014994                                                                          
015005     IF KEYS-OK                                                           
015006        IF REQU-ADLAGOMR-KEY = ALL '+'                                    
015007           MOVE ZERO            TO REQU-ADLAGOMR-KEY                      
015008                                   W-BSEQ-ADLAGOMR-MIN                    
015009                                   W-BSEQ-ADLAGOMR-MAX                    
015010        ELSE                                                              
015011           IF REQU-ADLAGOMR-KEY NOT NUMERIC                               
015012              MOVE '024'        TO RESP-IDMSG-ERROR                       
015013*             NOT NUMERIC ***                                             
015014              MOVE 'ADLAGOMR'   TO RESP-IDELMT-ERROR                      
015015              MOVE NOO          TO KEYS-SW                                
015016           ELSE                                                           
015017              MOVE REQU-ADLAGOMR-KEY                                      
015018                                TO RESP-ADLAGOMR-KEY                      
015019                                   W-BSEQ-ADLAGOMR-MIN                    
015020                                   W-BSEQ-ADLAGOMR-MAX                    
015021           END-IF                                                         
015030        END-IF                                                            
015100     END-IF                                                               
015200                                                                          
015300     IF KEYS-OK                                                           
015400        IF REQU-ADGANG-KEY = ALL '+'                                      
015700           MOVE ZERO            TO REQU-ADGANG-KEY                        
015801                                   W-BSEQ-ADGANG-MIN                      
015802                                   W-BSEQ-ADGANG-MAX                      
015810        ELSE                                                              
015820           IF REQU-ADGANG-KEY NOT NUMERIC                                 
015830              MOVE '024'        TO RESP-IDMSG-ERROR                       
015840*             NOT NUMERIC ***                                             
015850              MOVE 'ADGANG'     TO RESP-IDELMT-ERROR                      
015860              MOVE NOO          TO KEYS-SW                                
015861           ELSE                                                           
015862              MOVE REQU-ADGANG-KEY                                        
015863                                TO RESP-ADGANG-KEY                        
015864                                   W-BSEQ-ADGANG-MIN                      
015865                                   W-BSEQ-ADGANG-MAX                      
015870           END-IF                                                         
015880        END-IF                                                            
015890     END-IF                                                               
015891                                                                          
015892     IF KEYS-OK                                                           
015893        IF REQU-ADPLATS-KEY = ALL '+'                                     
015899           MOVE ZERO            TO REQU-ADPLATS-KEY                       
015900                                   W-BSEQ-ADPLATS-MIN                     
015901                                   W-BSEQ-ADPLATS-MAX                     
015902        ELSE                                                              
015903           IF REQU-ADPLATS-KEY NOT NUMERIC                                
015904              MOVE '024'        TO RESP-IDMSG-ERROR                       
015905*             NOT NUMERIC ***                                             
015906              MOVE 'ADPLATS'    TO RESP-IDELMT-ERROR                      
015907              MOVE NOO          TO KEYS-SW                                
015908           ELSE                                                           
015909              MOVE REQU-ADPLATS-KEY                                       
015910                                TO RESP-ADPLATS-KEY                       
015911                                   W-BSEQ-ADPLATS-MIN                     
015912                                   W-BSEQ-ADPLATS-MAX                     
015913           END-IF                                                         
015914        END-IF                                                            
015915     END-IF                                                               
015916                                                                          
015917     IF KEYS-OK                                                           
015918        IF REQU-IDPRTOMG-KEY = ALL '+'                                    
015919           MOVE '026'           TO RESP-IDMSG-ERROR                       
015920*          NO INPUT DATA IS ENTERED ***                                   
015921           MOVE 'IDPRTOMG'      TO RESP-IDELMT-ERROR                      
015922           MOVE NOO             TO KEYS-SW                                
015923        ELSE                                                              
015924           IF REQU-IDPRTOMG-KEY NUMERIC                                   
015925              IF REQU-IDPRTOMG-KEY = 1 OR 2 OR 3                          
015926                 COMPUTE W-BSEQ-IDPRTOMG-MIN =                            
015927                         REQU-IDPRTOMG-KEY - 1                            
015928                 MOVE W-BSEQ-IDPRTOMG-MIN TO W-BSEQ-IDPRTOMG-MAX          
015929                                                                          
015930                 MOVE REQU-IDPRTOMG-KEY   TO RESP-IDPRTOMG-KEY            
015931                                             W-BSEQ-IDPRTOMG-MIN          
015932                                             W-BSEQ-IDPRTOMG-MAX          
015933              ELSE                                                        
015934                 MOVE '023'        TO RESP-IDMSG-ERROR                    
015935*                IS INVALID  ***                                          
015936                 MOVE 'IDPRTOMG'   TO RESP-IDELMT-ERROR                   
015937                 MOVE NOO          TO KEYS-SW                             
015938              END-IF                                                      
015939           ELSE                                                           
015940              MOVE '024'        TO RESP-IDMSG-ERROR                       
015941*             NOT NUMERIC ***                                             
015942              MOVE 'IDPRTOMG'   TO RESP-IDELMT-ERROR                      
015943              MOVE NOO          TO KEYS-SW                                
015944           END-IF                                                         
015945        END-IF                                                            
015946     END-IF                                                               
015947                                                                          
015948     IF KEYS-OK                                                           
015952        IF REQU-FLINVSKR-KEY = YES OR NOO                                 
015953           MOVE REQU-FLINVSKR-KEY   TO RESP-FLINVSKR-KEY                  
015954        ELSE                                                              
015955           MOVE '185'           TO RESP-IDMSG-ERROR                       
015956*          IS INVALID   ***                                               
015958           MOVE NOO             TO KEYS-SW                                
015959        END-IF                                                            
015960     END-IF                                                               
015970     .                                                                    
016000                                                                          
016010 C-KOLLA-INDATA-UPPDATERING SECTION.                                      
016020     MOVE YES TO INDATA-SW                                                
016030     MOVE +1 TO IX                                                        
016040     IF REQU-KVINVSKR-5309 NUMERIC                                        
016050        IF REQU-KVINVSKR-5309 > ZERO AND < +11                            
016060           IF REQU-IDARTNR-UTSKR(IX) = ' ' OR                             
016070              REQU-FLINVSKR-KEY = YES                                     
016080                                                                          
016081              MOVE '004'           TO RESP-IDMSG-ERROR                    
016082*             NOT UPDATED  ***                                            
016083              MOVE 'IDARTNR '      TO RESP-IDELMT-ERROR                   
016093              MOVE NOO             TO INDATA-SW                           
016094           END-IF                                                         
016095        ELSE                                                              
016096           MOVE '023'           TO RESP-IDMSG-ERROR                       
016097*          IS INVALID   ***                                               
016098           MOVE 'KVINVSKR'      TO RESP-IDELMT-ERROR                      
016099           MOVE NOO             TO INDATA-SW                              
016104        END-IF                                                            
016105     ELSE                                                                 
016106        MOVE '024'           TO RESP-IDMSG-ERROR                          
016107*       NOT NUMERIC  ***                                                  
016108        MOVE 'KVINVSKR'      TO RESP-IDELMT-ERROR                         
016109        MOVE NOO             TO INDATA-SW                                 
016114     END-IF                                                               
016115                                                                          
016116     IF INDATA-OK                                                         
016117       IF REQU-KVINVSKR-5309 > REQU-KVRADER                               
016118         MOVE '023'          TO RESP-IDMSG-ERROR                          
016119*        IS INVALID   ***                                                 
016120         MOVE 'KVINVSKR'     TO RESP-IDELMT-ERROR                         
016121         MOVE NOO            TO INDATA-SW                                 
016122       END-IF                                                             
016123     END-IF                                                               
016124     .                                                                    
016125 D-SKICKA-ART-TILL-UTSKRIFT SECTION.                                      
016126     MOVE 'D-SKICKA-ART      ' TO CURR-SECTION                            
016127                                                                          
016128     MOVE +1      TO IX                                                   
016129     PERFORM UNTIL IX  > 10                                               
016130        MOVE ALL '+' TO RESP-WL0173I1(IX)                                 
016133        ADD +1 TO IX                                                      
016134     END-PERFORM                                                          
016135                                                                          
016136     MOVE +1      TO IX                                                   
016137     MOVE +0      TO IND                                                  
016139                                                                          
016140*    PERFORM UNTIL IND = REQU-KVINVSKR-5309 OR                            
016141*                  IX  > REQU-KVRADER      OR                             
016142*                  REQU-IDARTNR-UTSKR(IX) = 0                             
016143     PERFORM UNTIL IND = REQU-KVINVSKR-5309                               
016146       PERFORM DA-KOLLA-PRINTKOD-WDH1                                     
016147                                                                          
016148       IF INV-FLINVSKR = 'N'                                              
016149         ADD +1 TO IND                                                    
016150         PERFORM DB-REDIGERA-FLYTTA-ART                                   
016151       END-IF                                                             
016152       ADD +1 TO IX                                                       
016153     END-PERFORM                                                          
016155     .                                                                    
016156                                                                          
016188 DA-KOLLA-PRINTKOD-WDH1 SECTION.                                          
016189     MOVE 'DA-KOLLA-PRINTKOD ' TO CURR-SECTION                            
016190                                                                          
016191     MOVE REQU-IDARTNR-UTSKR(IX)      TO W-IDARTNR                        
016193                                         W-IDARTNR-ARTC                   
016194                                                                          
016200     MOVE REQU-KDINVKAT-UTSKR(IX)     TO W-KDINVKAT-MIN                   
016203                                         W-KDINVKAT-MAX                   
016204     PERFORM IMS-04-GET-INVENTERINGS-ROT                                  
016205     IF SEGMENT-MISSING                                                   
016206       CALL FELLOG                                                        
016207     END-IF                                                               
016208                                                                          
016209     PERFORM IMS-05-GNP-INVENTERINGS                                      
016210     IF SEGMENT-MISSING                                                   
016211       CALL FELLOG                                                        
016212     END-IF                                                               
016213     .                                                                    
016214 DB-REDIGERA-FLYTTA-ART SECTION.                                          
016215     MOVE 'DB-REDIGERA-FLYTTA' TO CURR-SECTION                            
016216                                                                          
016217     MOVE REQU-IDARTNR-UTSKR(IX)      TO W-IDARTNR                        
016218                                         W-IDARTNR-ARTC                   
016219                                         RESP-IDARTNR-L173(IND)           
016220                                                                          
016221     IF REQU-KDINVPRIO-UTSKR(IX) = SPACE                                  
016222        MOVE '2'                      TO RESP-KDINVPRIO-L173(IND)         
016223     ELSE                                                                 
016224        MOVE REQU-KDINVPRIO-UTSKR(IX) TO RESP-KDINVPRIO-L173(IND)         
016225     END-IF                                                               
016226     MOVE REQU-KDINVKAT-UTSKR(IX)     TO RESP-KDINVKAT-L173(IND)          
016227     .                                                                    
016228     EJECT                                                                
016229 E-LAES-VISA-KOE2 SECTION.                                                
016230     MOVE 'E-LAES-VISA-KOE2'   TO CURR-SECTION                            
016240*                                                                         
016300     MOVE +1 TO INDX                                                      
016310     MOVE 0  TO WS-KVRADER                                                
016400                                                                          
016500     PERFORM IMS-01-GU-WDH1B1                                             
016510     IF SEGMENT-MISSING                                                   
016531        MOVE '220'              TO RESP-IDMSG-ERROR                       
016532*       NOT FOUND        ***                                              
016534        MOVE NOO                TO KEYS-SW                                
016540     ELSE                                                                 
016600        IF REQU-FLINVSKR-KEY = NOO                                        
016700          MOVE 1 TO WS-VARIABEL                                           
016800        ELSE                                                              
016900          MOVE 0 TO WS-VARIABEL                                           
017000        END-IF                                                            
017200                                                                          
017300        PERFORM UNTIL INDX > MAX-IX OR SEGMENT-MISSING                    
017411           IF  REQU-IDPRTOMG-KEY = BSEQ-INV-IDPRTOMG + WS-VARIABEL        
017420           AND (REQU-ADLAGOMR-KEY = ZERO OR                               
017430                REQU-ADLAGOMR-KEY = BSEQ-INV-ADLAGOMR)                    
017800           AND (REQU-ADGANG-KEY  <= BSEQ-INV-ADGANG OR                    
017900                REQU-ADGANG-KEY   = ZERO)                                 
018000           AND (REQU-ADPLATS-KEY <= BSEQ-INV-ADPLATS OR                   
018100                REQU-ADPLATS-KEY  = ZERO)                                 
018110           AND  REQU-FLINVSKR-KEY = BSEQ-INV-FLINVSKR                     
018200           AND  BSEQ-INV-FLINVBEH = NOO                                   
018210           AND  BSEQ-INV-KDINVKAT NOT = +8                                
018226                                                                          
018230              PERFORM IMS-02-GNP-WDH1B1                                   
018360              PERFORM EA-SKRIV-RAD                                        
018370              ADD +1 TO INDX                                              
018380              ADD +1 TO WS-KVRADER                                        
018593           END-IF                                                         
018595           PERFORM IMS-03-GN-WDH1B1                                       
018660        END-PERFORM                                                       
018661     END-IF                                                               
018670                                                                          
018684     IF INDX = 1                                                          
018685        MOVE '220'              TO RESP-IDMSG-ERROR                       
018686*       NOT FOUND        ***                                              
018687        MOVE NOO                TO KEYS-SW                                
018688     END-IF                                                               
018689                                                                          
018690     IF KEYS-OK                                                           
018691        MOVE ZERO       TO RESP-KVINVSKR-5309                             
018692        MOVE WS-KVRADER TO RESP-KVRADER                                   
018693                           RESP-KVANT-ART                                 
018694     END-IF                                                               
018695                                                                          
018696     IF SEGMENT-FOUND AND INDX > MAX-IX                                   
018700        MOVE '028'              TO RESP-IDMSG-ERROR                       
018800*       TOO MANY LINES   ***                                              
019099     END-IF                                                               
019102     .                                                                    
019103 EA-SKRIV-RAD SECTION.                                                    
019104     MOVE 'EA-SKRIV-RAD    '   TO CURR-SECTION                            
019105*                                                                         
019106     IF WS-VARIABEL = 0                                                   
019107       MOVE BSEQ-INV-IDPRTOMG      TO WS-IDPRTOMG-MOD                     
019108       MOVE BSEQ-INV-IDLOPNR       TO WS-IDLOPNR-MOD                      
019109       MOVE WS-LISTNR              TO RESP-LISTNR       (INDX)            
019110     ELSE                                                                 
019111       MOVE SPACE                  TO RESP-LISTNR       (INDX)            
019112     END-IF                                                               
019113     MOVE BSEQ-INV-ADLAGOMR        TO WS-ADLAGOMR-NUM                     
019114     MOVE WS-ADLAGOMR-NUM          TO RESP-ADLAGOMR-UTSKR  (INDX)         
019115     MOVE BSEQ-INV-ADGANG          TO RESP-ADGANG-UTSKR    (INDX)         
019116     MOVE BSEQ-INV-ADPLATS         TO RESP-ADPLATS-UTSKR   (INDX)         
019117     IF CDC-SE                                                            
019118        IF BSEQ-INV-KDINVPRIO = +2                                        
019119           MOVE +0 TO RESP-KDINVPRIO-UTSKR (INDX)                         
019120        ELSE                                                              
019121           MOVE BSEQ-INV-KDINVPRIO TO RESP-KDINVPRIO-UTSKR (INDX)         
019122        END-IF                                                            
019123     ELSE                                                                 
019124       MOVE BSEQ-INV-KDINVPRIO     TO RESP-KDINVPRIO-UTSKR (INDX)         
019125     END-IF                                                               
019127     MOVE BSEQ-INV-KDVVKL          TO RESP-KDVVKL-UTSKR    (INDX)         
019129     MOVE BSEQ-INV-KDINVKAT        TO WS-KDINVKAT-NUM                     
019130     MOVE WS-KDINVKAT-NUM          TO RESP-KDINVKAT-UTSKR  (INDX)         
019131     IF BSEQ-INV-FLINVSKR = 'J'                                           
019132       MOVE 'Y'                    TO RESP-FLINVSKR-UTSKR  (INDX)         
019133     ELSE                                                                 
019134       MOVE BSEQ-INV-FLINVSKR      TO RESP-FLINVSKR-UTSKR  (INDX)         
019135     END-IF                                                               
019136     MOVE BSEQ-INV-KDPRODSL        TO RESP-KDPRODSL-UTSKR  (INDX)         
019137     MOVE BSEQ-INV-IDFKNGRP        TO RESP-IDFKNGRP-UTSKR  (INDX)         
019138     MOVE BSEQ-INV-DAREGDAT        TO WS-FIX-DAREGDAT                     
019139     MOVE WS-DAREGDAT-AAMMDD       TO RESP-TIREGDAT-UTSKR (INDX)          
019140     MOVE ART-IDARTNR              TO RESP-IDARTNR-UTSKR (INDX)           
019142     MOVE ZERO                     TO RESP-KVANT-ART                      
019147     .                                                                    
019150                                                                          
019200*    --- DISPATCHER SECTIONS                                              
019300 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
019400                                                                          
019500     MOVE 'GETARG'                           TO SUB-KDFUNC                
019600     MOVE 'CARPARTS.LDC.SHOWINVENTORYQUEUE2' TO SUB-ADDISPABS             
019700     MOVE LENGTH OF REQU-AREA                TO SUB-KVDLEN                
019800                                                                          
019900     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
020000                                                                          
020100     IF SUB-KDRC > 0                                                      
020200       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
020300       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
020400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
020500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
020600     END-IF                                                               
020700     .                                                                    
020800     SKIP3                                                                
020900 S02-RETURN-RESPONSE SECTION.                                             
021000                                                                          
021100     MOVE 'RETURN'                   TO SUB-KDFUNC                        
021200     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
021300                                                                          
021400     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
021500                                                                          
021600     IF SUB-KDRC > 0                                                      
021700       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
021800       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
021900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
022000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
022100     END-IF                                                               
022200     .                                                                    
022300 IMS-01-GU-WDH1B1   SECTION.                                              
022310     MOVE 'IMS-01'  TO CURR-IMS-SECTION                                   
022400                                                                          
022500     STRING 'WDH111  (WDH1BSEQ>=' W-WDH1BSEQ-MIN-X                        
022600                    '&WDH1BSEQ<=' W-WDH1BSEQ-MAX-X ')'                    
022700                                                                          
022800            DELIMITED BY SIZE INTO SSA1                                   
022900     MOVE '  GE'                TO GOOD-STATUSCODES                       
023000     CALL CBLTDLI USING GHU WDH1B-PCB DLI-IO-WDH1B1 SSA1                  
023100     MOVE WDH1B-STATUS-CODE     TO STATUS-WS                              
023200     PERFORM IMS-STATUS-CHECK                                             
023300     .                                                                    
023301 IMS-02-GNP-WDH1B1   SECTION.                                             
023302     MOVE 'IMS-02'  TO CURR-IMS-SECTION                                   
023303                                                                          
023304     STRING 'WDH111  (WDH1BSEQ>=' W-WDH1BSEQ-MIN-X                        
023305                    '&WDH1BSEQ<=' W-WDH1BSEQ-MAX-X ')'                    
023306            DELIMITED BY SIZE INTO SSA1                                   
023307     MOVE  'WDH101 '   TO SSA2                                            
023308                                                                          
023309     MOVE '  '                  TO GOOD-STATUSCODES                       
023310     CALL CBLTDLI USING GNP WDH1B-PCB DLI-IO-AREA SSA1 SSA2               
023311     MOVE WDH1B-STATUS-CODE     TO STATUS-WS                              
023312     PERFORM IMS-STATUS-CHECK                                             
023313     .                                                                    
023314 IMS-03-GN-WDH1B1   SECTION.                                              
023315     MOVE 'IMS-03'  TO CURR-IMS-SECTION                                   
023316                                                                          
023317     STRING 'WDH111  (WDH1BSEQ>=' W-WDH1BSEQ-MIN-X                        
023318                    '&WDH1BSEQ<=' W-WDH1BSEQ-MAX-X ')'                    
023319            DELIMITED BY SIZE INTO SSA1                                   
023320     MOVE '  GE'                TO GOOD-STATUSCODES                       
023321     CALL CBLTDLI USING GHN WDH1B-PCB DLI-IO-WDH1B1 SSA1                  
023322     MOVE WDH1B-STATUS-CODE     TO STATUS-WS                              
023323     PERFORM IMS-STATUS-CHECK                                             
023324     .                                                                    
023325 IMS-04-GET-INVENTERINGS-ROT  SECTION.                                    
023326     MOVE 'IMS-04'  TO CURR-IMS-SECTION                                   
023327                                                                          
023328     STRING 'WDH101  (IDARTNR  =' W-IDARTNR-X ')'                         
023329              DELIMITED BY SIZE INTO SSA1                                 
023330     MOVE '  '                    TO GOOD-STATUSCODES                     
023331     CALL CBLTDLI USING GU INVA-PCB DLI-IO-AREA SSA1                      
023332     MOVE INVA-STATUS-CODE        TO STATUS-WS                            
023333     PERFORM IMS-STATUS-CHECK                                             
023334     .                                                                    
023335 IMS-05-GNP-INVENTERINGS SECTION.                                         
023336     MOVE 'IMS-05'  TO CURR-IMS-SECTION                                   
023337     STRING 'WDH111  (WDH111KY>=' W-WDH11-KEY-MIN-X                       
023338                    '&WDH111KY<=' W-WDH11-KEY-MAX-X                       
023339                    '&FLINVBEH =' NOO ')'                                 
023340              DELIMITED BY SIZE INTO SSA1                                 
023341     MOVE '  GE'                  TO GOOD-STATUSCODES                     
023342     CALL CBLTDLI USING GNP INVA-PCB DLI-IO-AREA SSA1                     
023343     MOVE INVA-STATUS-CODE        TO STATUS-WS                            
023344     PERFORM IMS-STATUS-CHECK                                             
023345     .                                                                    
023346 IMS-STATUS-CHECK   SECTION.                                              
023348                                                                          
023349     SET STATUS-IX TO 1                                                   
023350     SEARCH GOOD-STATUS                                                   
023360       AT END                                                             
023370         CALL FELLOG                                                      
023380     WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                             
023390       CONTINUE                                                           
023391     END-SEARCH                                                           
023392     .                                                                    
023400                                                                          
