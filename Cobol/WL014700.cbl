001300 ID DIVISION.                                                             
001500 PROGRAM-ID.     WL014700.                                                
001600 AUTHOR.         TAPAS KUMAR GHOSH.                                       
001700 DATE-WRITTEN.   2004/08/17.                                              
001800 DATE-COMPILED.                                                           
001900                                                                          
002000*    FUNKTION:                                                            
002100*        VISA SÄNDNINGSINNEHÅLL. BILDEN ANVÄNDS FÖR ATT SE VAD EN         
002200*        SÄNDNING INNEHÅLLER ELLER FÖR ATT REGISTRERA AVVIKELSER          
002300*        OM ANTAL KOLLIN EJ STÄMMER VID LOSSNING /MOTTAGNING AV           
002400*        SÄNDNING                                                         
002500*                                                                         
002610*        PROGRAMMET UPPDATERAR WLRETA (WDA3)                              
002700*                                                                         
002701*        WL014700 PROGRAM IS A REPLICA OF W4073200 PROGRAM                
002702*        AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS                          
002703*                                                                         
002704*                                                                         
002710* ADDRESS: 'CARPARTS.LDC.RETURNCONTENTS'                                  
002720*                                                                         
002800*    INDATA.                                                              
002900*        TRANSAKTION: WL0147U                                             
003000*        REQUEST:     WZ01REQU                                            
003010*                     WL0147I1                                            
003100*                                                                         
003200*    UTDATA.                                                              
003300*        RESPONSE:    WZ01RESP                                            
003310*                     WL0147O1                                            
003400                                                                          
003500     SKIP3                                                                
003600 ENVIRONMENT DIVISION.                                                    
003800 DATA DIVISION.                                                           
003810     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
003901                                                                          
003910*    -- CHECKED BY WY2000                                                 
004000 77  IDPGM                       PIC X(08)   VALUE 'WL014700'.            
004100                                                                          
004200*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004300 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004310 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
004320 77  KDRC-DISPLAY                PIC Z(5).                                
004400                                                                          
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004700                                                                          
004900*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005100                                                                          
005201 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005202     88  INDATA-OK                           VALUE 'J'.                   
005210     88  INDATA-FEL                          VALUE 'N'.                   
005300                                                                          
005400 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005500     88  NYCKLAR-OK                          VALUE 'J'.                   
005600     88  NYCKLAR-FEL                         VALUE 'N'.                   
005700                                                                          
005710 77  UPPDATERA-KOLLI-SW          PIC X       VALUE 'J'.                   
005720     88  KOLLI-OK                            VALUE 'J'.                   
005730     88  KOLLI-FEL                           VALUE 'N'.                   
005740                                                                          
005741 77  IDKOLLI-IFYLLT-SW           PIC X       VALUE 'N'.                   
005742     88  IDKOLLI-IFYLLT                      VALUE 'J'.                   
005743     88  IDKOLLI-FEL                         VALUE 'N'.                   
005744                                                                          
005745 77  INPUT-FINNS-SW              PIC X       VALUE 'J'.                   
005746     88  INPUT-FINNS                         VALUE 'J'.                   
005747     88  INPUT-FINNS-INTE                    VALUE 'N'.                   
005748                                                                          
005793                                                                          
005794                                                                          
005795 77  WS-IDELMT-ERROR             PIC X(16).                               
005796 77  WS-IDMSG-ERROR              PIC X(03).                               
005797 77  WS-IDMSG-INFO               PIC X(03).                               
005798                                                                          
005799*    --- PARAMETERS TO ABEND                                              
005800                                                                          
005801 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005802 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
005803 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
005804                                                                          
005805 77  RENSA-FAELT-IN-SW           PIC X       VALUE 'N'.                   
005806     88  RENSA-FAELT-IN                      VALUE 'J'.                   
005810     88  RENSA-FAELT-FEL                     VALUE 'N'.                   
005830                                                                          
005840 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005900     88  EGEN-MID                            VALUE '4732'.                
006000     88  GODK-MID                            VALUE '4731' '4732'          
006100                                                   '4733' '4734'          
006200                                                   '4735' '4736'          
006300                                                   '4737' '4738'          
006400                                                   '4739'.                
006500     88  HELP-MID                            VALUE '0551'.                
006510                                                                          
006520 77  RAD-IX                      PIC S9(4)   VALUE ZERO COMP SYNC.        
006521 77  WS-INDX-REC                 PIC S9(4)   VALUE ZERO COMP SYNC.        
006522 77  WS-DEL-COUNT                PIC S9(4)   VALUE ZERO COMP SYNC.        
006523 77  4792-INDX                   PIC S9(4)   VALUE ZERO COMP SYNC.        
006524 77  4792-MAX-INDX               PIC S9(4)   VALUE +24  COMP SYNC.        
006525 77  WS-COUNT                    PIC S9(4)   VALUE ZERO COMP SYNC.        
006526 77  INDX                        PIC S9(4)   VALUE ZERO COMP SYNC.        
006527 77  MAX-INDX                    PIC S9(4)   VALUE ZERO COMP SYNC.        
006528 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006529 77  DAGENS-TID                  PIC 9(8)    VALUE ZERO.                  
006530 77  LNG-P-TO-P-PREFIX           PIC S9(4)   VALUE +17 COMP SYNC.         
006531 01  W-KLI-SAK                   PIC 9       VALUE 6.                     
006532 01  W-KLI-AVV                   PIC 9       VALUE 7.                     
006533 77  W-KDKOLSTA                  PIC X       VALUE '1'.                   
006534     88  W-PACK-KEY                          VALUE '1'.                   
006535     88  W-LAST-KEY                          VALUE '2'.                   
006536     88  W-SANT-KEY                          VALUE '3'.                   
006537     88  W-LOSS-KEY                          VALUE '4'.                   
006538     88  W-MOTT-KEY                          VALUE '5'.                   
006539     88  W-SAKN-KEY                          VALUE '6'.                   
006540     88  W-AVVI-KEY                          VALUE '7'.                   
006541     88  W-BEHA-KEY                          VALUE '8' '9'.               
006542 01  W-AATGAERDER.                                                        
006553     05  W-RECEIVED              PIC X(3)    VALUE 'REC'.                 
006555     05  W-DEVIATION             PIC X(3)    VALUE 'DEV'.                 
006557     05  W-DELETE                PIC X(3)    VALUE 'DEL'.                 
006600     EJECT                                                                
006700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006800 01  GENERELLA-SUBPROGRAM.                                                
007010     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
007100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007320     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007330     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
007400     EJECT                                                                
007800 01  MESSAGE-CODES.                                                       
007901     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
007902     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
007903     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
007910     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
008100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008200     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
008201     03  INF-MORE-INFO-FINNS     PIC X(3)    VALUE '105'.                 
008202     03  ERR-KOLLI-SAKNAS        PIC X(3)    VALUE '758'.                 
008203     EJECT                                                                
008701                                                                          
008702 77  WS-REC-LIMIT                PIC X       VALUE 'N'.                   
008703     88  REC-LIMIT                           VALUE 'J'.                   
008740                                                                          
008750 01  BILD-HOPP-AREROR.                                                    
008798     03  FILLER                  PIC X(16)   VALUE 'P-TO-P-AREA'.         
008799     03  P-TO-P-SW.                                                       
008800         05  P-TO-P-KVLL         PIC S9(4)   VALUE +117 COMP SYNC.        
008801         05  P-TO-P-KDZ1         PIC X(1)    VALUE LOW-VALUE.             
008802                                                                          
008803         05  P-TO-P-KDZ2         PIC X(1)    VALUE LOW-VALUE.             
008804                                                                          
008805         05  P-TO-P-KDTRANS      PIC X(8).                                
008806         05  P-TO-P-IDTRANS      PIC X(4).                                
008807         05  P-TO-P-KDMFSFOR     PIC X(1).                                
008808         05  P-TO-P-DATA         PIC X(100)  VALUE ALL '+'.               
008809                                                                          
008810     EJECT                                                                
009000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
009100*                                                                         
009110 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
009120     SKIP3                                                                
009130*01  -COPY WZ01SUB                                                        
009140     EJECT                                                                
009150 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
009160     SKIP3                                                                
009170 01  REQU-AREA.                                                           
009180*    03  -COPY WZ01REQU                                                   
009190*    03  -COPY WL0147I1                                                   
009191     EJECT                                                                
009192 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
009193     SKIP3                                                                
009194 01  RESP-AREA.                                                           
009195*    03  -COPY WZ01RESP                                                   
009196*    03  -COPY WL0147O1                                                   
009197     EJECT                                                                
010600     EJECT                                                                
010610 77  KVKOLLI                     PIC 9(4)  VALUE ZERO.                    
010620     EJECT                                                                
010630 01  FILLER                      PIC X(16) VALUE 'KOM-IO-AREA'.           
010640 01  KOM-MSG-IO-AREA.                                                     
010650*03  -COPY WMSGKOM                                                        
010660     EJECT                                                                
010670 01  FILLER                      PIC X(16) VALUE 'MSG/KOM-AREA'.          
010680 01  -COPY WMSGSNUF     -PRE P-TO-P-                                      
010690     EJECT                                                                
010691 01  FILLER                      PIC X(24) VALUE                          
010692                                 'MOD4792-MID-W4I79201'.                  
010693*    -COPY W4I79201  -PRE MOD4792-                                        
010694     EJECT                                                                
010700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010800*                                                                         
010900     EJECT                                                                
011000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011100     SKIP3                                                                
011200 01  W-MINKEY-X.                                                          
011411     03  W-MINKEY-IDTRANS          PIC X(4)    VALUE '4732'.              
011412     03  W-MINKEYB1-ENTER.                                                
011420         05  W-MINKEYB1-IDDC-ENTER    PIC X(2)        VALUE SPACE.        
011421         05  W-MINKEYB1-IDRT-ENTER    PIC X(3)        VALUE SPACE.        
011430         05  W-MINKEYB1-IDRTLOP-ENTER PIC 9(3).                           
011440         05  W-MINKEYB1-IDKOLLI-ENTER PIC S9(5) COMP-3 VALUE ZERO.        
011441     03  W-MINKEYB1-NEXT.                                                 
011442         05  W-MINKEYB1-IDDC-NEXT     PIC X(2)        VALUE SPACE.        
011443         05  W-MINKEYB1-IDRT-NEXT     PIC X(3)        VALUE SPACE.        
011444         05  W-MINKEYB1-IDRTLOP-NEXT  PIC 9(3).                           
011445         05  W-MINKEYB1-IDKOLLI-NEXT  PIC S9(5) COMP-3 VALUE ZERO.        
011450                                                                          
011500 01  NYCKLAR-TILL-DLI.                                                    
011601     03  W-IDUSER-X.                                                      
011602         05  W-IDUSER            PIC X(8)    VALUE SPACE.                 
011603                                                                          
011604     03  W-IDRETSND-X.                                                    
011605         05  W-IDRETSND          PIC X(8)    VALUE SPACE.                 
011606                                                                          
011607     03  W-WDA301KY-X.                                                    
011608         05  W-IDDC-301          PIC  X(2)   VALUE SPACE.                 
011609         05  W-DAREGDAT          PIC  9(8)   VALUE ZERO.                  
011610         05  W-TIKLOCK           PIC S9(9)   VALUE ZERO COMP-3.           
011611                                                                          
011612     03  FILLER              PIC X(8)        VALUE 'KOLLA'.               
011613     03  W-WDA3BSEQ-MIN-X.                                                
011615         05  W-IDRT-BSEQ-MIN     PIC X(3)            VALUE SPACE.         
011616         05  W-IDDC-BSEQ-MIN     PIC X(2)            VALUE SPACE.         
011620         05  W-IDRTLOP-BSEQ-MIN  PIC 9(3).                                
011630         05  W-IDKOLLI-BSEQ-MIN  PIC S9(5)   COMP-3  VALUE ZERO.          
011631                                                                          
011640     03  W-WDA3BSEQ-MAX-X.                                                
011650         05  W-IDRT-BSEQ-MAX     PIC X(3)            VALUE SPACE.         
011651         05  W-IDDC-BSEQ-MAX     PIC X(2)            VALUE SPACE.         
011660         05  W-IDRTLOP-BSEQ-MAX  PIC 9(3).                                
011670         05  W-IDKOLLI-BSEQ-MAX  PIC S9(5)   COMP-3  VALUE ZERO.          
011700     SKIP2                                                                
011710                                                                          
011800*    --- STATUS-KOD FRÅN IMS                                              
011900 01  STATUS-WS                   PIC XX.                                  
012000     88  STATUS-OK                           VALUE '  '.                  
012010     88  SEGMENT-FINNS                       VALUE '  '.                  
012100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012210     88  TRANSKOD-FEL                        VALUE 'A1'.                  
012220     88  SECURITY-FEL                        VALUE 'A4'.                  
012300     SKIP2                                                                
012400 01  GODK-STATUSKODER.                                                    
012500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012600     SKIP3                                                                
012700 01  SSA1                        PIC X(96).                               
012800 01  SSA2                        PIC X(96).                               
012900     EJECT                                                                
013000*    --- IMS FUNKTIONSKODER                                               
013100*01  -COPY W0003                                                          
013300     EJECT                                                                
013400*    ---  DLI INPUT-OUTPUT AREA                                           
013500 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-RETA01'.           
013600                                                                          
013907 01  DLI-IO-RETA01.                                                       
013910*  03  -COPY WDA301                                                       
014200     EJECT                                                                
014300 LINKAGE SECTION.                                                         
014400                                                                          
014500 01  MSG-PCB                     PIC X.                                   
014520*01  -COPY W0009   -PRE DISP-                                             
014530     EJECT                                                                
014805*01  -COPY W0008   -PRE RETA-                                             
014810     05  FILLER                  PIC X.                                   
014811     EJECT                                                                
014812*01  -COPY W0008   -PRE SEQB-                                             
014814     05  FILLER                  PIC X.                                   
014815     EJECT                                                                
014820*01  -COPY W0008   -PRE KOM-KOMA-                                         
014830     05  FILLER                  PIC X.                                   
014900     EJECT                                                                
015001 PROCEDURE DIVISION  USING MSG-PCB DISP-PCB                               
015002                           RETA-PCB SEQB-PCB KOM-KOMA-PCB.                
015003 MAIN SECTION.                                                            
015010     ENTRY 'DLITCBL' USING MSG-PCB DISP-PCB                               
015020                           RETA-PCB SEQB-PCB KOM-KOMA-PCB.                
015100                                                                          
015310     PERFORM S04-FETCH-REQUEST-ARGUMENT                                   
015320     IF SUB-KDRC = 0                                                      
015500       PERFORM A-INIT                                                     
015600       PERFORM B-KOLLA-NYCKLAR                                            
015700       IF NYCKLAR-OK                                                      
015802         IF REQU-KDPGMACT = 'E'                                           
015803           PERFORM G-KOLLA-INPUT                                          
015804           IF INDATA-OK                                                   
015805             PERFORM H-UPPDATERA                                          
015806           END-IF                                                         
016110         END-IF                                                           
016150         IF INDATA-OK                                                     
016200           PERFORM F-LAES-VISA-INFO                                       
016210         END-IF                                                           
016300       END-IF                                                             
016501                                                                          
016502       MOVE RESP-IDMSG-INFO    TO WS-IDMSG-INFO                           
016503       MOVE RESP-IDMSG-ERROR   TO WS-IDMSG-ERROR                          
016504       MOVE RESP-IDELMT-ERROR  TO WS-IDELMT-ERROR                         
016505       IF WS-IDMSG-INFO NOT = SPACE                                       
016506         MOVE SPACE            TO RESP-IDMSG-ERROR                        
016507         MOVE SPACE            TO RESP-IDELMT-ERROR                       
016508       ELSE                                                               
016509         IF WS-IDMSG-ERROR NOT = SPACE                                    
016510           MOVE ALL '+' TO RESP-WL0147O1 (1:28)                           
016511           MOVE WS-IDMSG-ERROR   TO RESP-IDMSG-ERROR                      
016512           MOVE WS-IDELMT-ERROR  TO RESP-IDELMT-ERROR                     
016513           MOVE WS-IDMSG-INFO    TO RESP-IDMSG-INFO                       
016514           MOVE  001             TO RESP-IDMSGVER                         
016515           IF REQU-KDPGMACT = 'S'                                         
016516             MOVE ZERO             TO RESP-KVRADER                        
016517           ELSE                                                           
016518             IF REQU-KVRADER NUMERIC                                      
016519               MOVE REQU-KVRADER     TO RESP-KVRADER                      
016520             ELSE                                                         
016521               MOVE ZERO             TO RESP-KVRADER                      
016522             END-IF                                                       
016523           END-IF                                                         
016524         END-IF                                                           
016525       END-IF                                                             
016526       PERFORM S05-RETURN-RESPONSE                                        
016600     END-IF                                                               
016800                                                                          
016900     MOVE ZERO TO RETURN-CODE                                             
017000     GOBACK                                                               
017100     .                                                                    
017200     EJECT                                                                
017300 A-INIT SECTION.                                                          
017400                                                                          
017411     MOVE ALL '+' TO RESP-AREA                                            
017412     MOVE SPACE   TO RESP-IDMSG-INFO                                      
017413                     RESP-IDMSG-ERROR                                     
017414                     RESP-IDELMT-ERROR                                    
017415     MOVE 001     TO RESP-IDMSGVER                                        
017416     MOVE ZERO    TO RESP-KVRADER                                         
017420                                                                          
020400                                                                          
020500     MOVE LOW-VALUE         TO W-WDA3BSEQ-MIN-X                           
020510     MOVE HIGH-VALUE        TO W-WDA3BSEQ-MAX-X                           
020600     .                                                                    
020700     EJECT                                                                
020800 B-KOLLA-NYCKLAR SECTION.                                                 
020900                                                                          
021700                                                                          
021800     MOVE JA TO NYCKLAR-SW                                                
021900                                                                          
022001     PERFORM BA-KOLLA-IDRT-IDRTLOP                                        
022002     PERFORM BB-KOLLA-IDKOLLI                                             
022004     MOVE REQU-IDDC-KEY TO W-IDDC-301                                     
022005                           W-IDDC-BSEQ-MIN                                
022006                           W-IDDC-BSEQ-MAX                                
022007                           RESP-IDDC-KEY                                  
022008                                                                          
022300     IF NYCKLAR-FEL                                                       
022810       MOVE '043'    TO RESP-IDMSG-ERROR                                  
022900     END-IF                                                               
023000     .                                                                    
023200     EJECT                                                                
023300 BA-KOLLA-IDRT-IDRTLOP SECTION.                                           
023301     SKIP2                                                                
023302*    -- KONTROLL AV IDRT-IN                                               
023312     MOVE REQU-IDRT-KEY TO W-IDRT-BSEQ-MIN                                
023313                           W-IDRT-BSEQ-MAX                                
023314                           RESP-IDRT-KEY                                  
023316                                                                          
023317*    -- KONTROLL AV IDRTLOP-IN                                            
023334     IF REQU-IDRTLOP-KEY NUMERIC AND                                      
023335        REQU-IDRTLOP-KEY > 0                                              
023336                                                                          
023338         MOVE REQU-IDRTLOP-KEY TO W-IDRTLOP-BSEQ-MIN                      
023339                                  W-IDRTLOP-BSEQ-MAX                      
023340                                  RESP-IDRTLOP-KEY                        
023347     ELSE                                                                 
023349         MOVE NEJ TO NYCKLAR-SW                                           
023350     END-IF                                                               
023352     .                                                                    
023353     EJECT                                                                
023354 BB-KOLLA-IDKOLLI SECTION.                                                
023355     SKIP2                                                                
023356                                                                          
023358**    KONTROLL AV IDKOLLI-IN                                              
023367                                                                          
023368     MOVE JA TO IDKOLLI-IFYLLT-SW                                         
023371     IF REQU-IDKOLLI-KEY NUMERIC AND                                      
023372        REQU-IDKOLLI-KEY > 0                                              
023374         MOVE REQU-IDKOLLI-KEY  TO W-IDKOLLI-BSEQ-MIN                     
023375                                   RESP-IDKOLLI-KEY                       
023377     ELSE                                                                 
023378         MOVE NEJ TO IDKOLLI-IFYLLT-SW                                    
023386     END-IF                                                               
023387     .                                                                    
023388     EJECT                                                                
023485     EJECT                                                                
023490 F-LAES-VISA-INFO SECTION.                                                
023500                                                                          
023700                                                                          
023701     IF IDKOLLI-IFYLLT                                                    
023702         CONTINUE                                                         
023703     ELSE                                                                 
023704         MOVE HIGH-VALUE     TO W-WDA3BSEQ-MAX-X                          
023707     END-IF                                                               
023708     MOVE W-IDDC-BSEQ-MIN    TO W-IDDC-BSEQ-MAX                           
023709     MOVE W-IDRT-BSEQ-MIN    TO W-IDRT-BSEQ-MAX                           
023710     MOVE W-IDRTLOP-BSEQ-MIN TO W-IDRTLOP-BSEQ-MAX                        
023711     PERFORM IMS-GU-SEQB-WLRETA01                                         
023800     IF SEGMENT-SAKNAS                                                    
024300        MOVE 'IDRT-IDRTLOP' TO RESP-IDELMT-ERROR                          
024400        MOVE '025'          TO RESP-IDMSG-ERROR                           
024500     ELSE                                                                 
024510         PERFORM FA-REDIGERA-UTDATA                                       
024520     END-IF                                                               
024600     .                                                                    
024700     EJECT                                                                
024800 FA-REDIGERA-UTDATA SECTION.                                              
024900     SKIP2                                                                
024910                                                                          
025001     MOVE RET-IDFRASED-CDC TO RESP-IDFRASED                               
025002     PERFORM FAA-FIXA-ENTER-KEY                                           
025010     MOVE +1 TO RAD-IX                                                    
025011     MOVE +0 TO WS-COUNT                                                  
025012                                                                          
025031     PERFORM UNTIL RAD-IX > 500                                           
025040                                                                          
025050       IF SEGMENT-FINNS                                                   
025051         IF REQU-KDPGMACT = 'S'                                           
025052            MOVE SPACE              TO RESP-KDCMD    (RAD-IX)             
025053            MOVE SPACE              TO RESP-ADINLOMR (RAD-IX)             
025054         END-IF                                                           
025061         MOVE RET-ADINLOMR          TO RESP-ADINLOMR (RAD-IX)             
025071         MOVE RET-IDKOLLI           TO RESP-IDKOLLI  (RAD-IX)             
025081         MOVE RET-FLFARLIG          TO RESP-FLFARLIG (RAD-IX)             
025090         PERFORM FAC-RED-KDKOLSTA                                         
025091         MOVE RET-IDKOLLI           TO W-IDKOLLI-BSEQ-MIN                 
025092         PERFORM IMS-GN-SEQB-WLRETA01                                     
025093         ADD +1 TO WS-COUNT                                               
025094                                                                          
025100       END-IF                                                             
025101                                                                          
025105        ADD +1 TO RAD-IX                                                  
025106                                                                          
025108     END-PERFORM                                                          
025109     MOVE  WS-COUNT   TO  RESP-KVRADER                                    
025110     IF WS-COUNT = 500                                                    
025111          MOVE '028' TO RESP-IDMSG-ERROR                                  
025112          MOVE SPACE TO RESP-IDELMT-ERROR                                 
025113     END-IF                                                               
025114                                                                          
025400     .                                                                    
025601     EJECT                                                                
025602 FAA-FIXA-ENTER-KEY SECTION.                                              
025603     SKIP2                                                                
025604     IF SEGMENT-FINNS                                                     
025609         MOVE RET-IDDC              TO W-MINKEYB1-IDDC-ENTER              
025610         MOVE RET-IDRT              TO W-MINKEYB1-IDRT-ENTER              
025611         MOVE RET-IDRTLOP           TO W-MINKEYB1-IDRTLOP-ENTER           
025612         MOVE RET-IDKOLLI           TO W-MINKEYB1-IDKOLLI-ENTER           
025613     ELSE                                                                 
025615         MOVE REQU-IDDC-KEY         TO W-MINKEYB1-IDDC-ENTER              
025616         MOVE ZERO                  TO W-MINKEYB1-IDRT-ENTER              
025617                                       W-MINKEYB1-IDRTLOP-ENTER           
025618                                       W-MINKEYB1-IDKOLLI-ENTER           
025619     END-IF                                                               
025622     .                                                                    
025623     EJECT                                                                
025645 FAC-RED-KDKOLSTA SECTION.                                                
025646     SKIP2                                                                
025647     MOVE RET-KDKOLSTA          TO W-KDKOLSTA                             
025648     IF W-PACK-KEY OR W-SANT-KEY OR W-LOSS-KEY OR W-MOTT-KEY OR           
025649        W-SAKN-KEY OR W-AVVI-KEY OR W-BEHA-KEY                            
025651         EVALUATE TRUE                                                    
025652         WHEN W-PACK-KEY                                                  
025654            MOVE 'PACK'           TO RESP-BESTATUS (RAD-IX)               
025655         WHEN W-SANT-KEY                                                  
025657            MOVE 'SENT'           TO RESP-BESTATUS (RAD-IX)               
025658         WHEN W-LOSS-KEY                                                  
025660            MOVE 'UNL '           TO RESP-BESTATUS (RAD-IX)               
025661         WHEN W-MOTT-KEY                                                  
025663            MOVE 'REC '           TO RESP-BESTATUS (RAD-IX)               
025664         WHEN W-SAKN-KEY                                                  
025666            MOVE 'MIS '           TO RESP-BESTATUS (RAD-IX)               
025667         WHEN W-AVVI-KEY                                                  
025669            MOVE 'DEV '           TO RESP-BESTATUS (RAD-IX)               
025670         WHEN W-BEHA-KEY                                                  
025672            MOVE 'TREA'           TO RESP-BESTATUS (RAD-IX)               
025673         END-EVALUATE                                                     
025692     END-IF                                                               
025693     .                                                                    
025694     EJECT                                                                
025695 G-KOLLA-INPUT SECTION.                                                   
025696                                                                          
025697     MOVE JA  TO INDATA-SW                                                
025698                                                                          
025699     PERFORM GA-FORMELL-KONTROLL                                          
025700     IF KOLLI-OK                                                          
025701         PERFORM GB-LOGISK-KONTROLL                                       
025702     END-IF                                                               
025703                                                                          
025704                                                                          
025705     IF KOLLI-FEL OR INDATA-FEL                                           
025706       MOVE NEJ       TO INDATA-SW                                        
025713     END-IF                                                               
025714     .                                                                    
025715     EJECT                                                                
025716 GA-FORMELL-KONTROLL SECTION.                                             
025717     SKIP2                                                                
025718     IF REQU-KVRADER NUMERIC AND REQU-KVRADER > 0                         
025719       MOVE +1                            TO RAD-IX                       
025720       MOVE REQU-KVRADER                  TO WS-INDX-REC                  
025721       MOVE NEJ                           TO WS-REC-LIMIT                 
025722                                                                          
025724       MOVE NEJ TO INPUT-FINNS-SW                                         
025726       PERFORM UNTIL RAD-IX > 500 OR REC-LIMIT                            
025728           IF REQU-RAD (RAD-IX) NOT = ALL '+'                             
025729              MOVE JA TO INPUT-FINNS-SW                                   
025730           END-IF                                                         
025731           IF RAD-IX = WS-INDX-REC                                        
025732              MOVE JA TO WS-REC-LIMIT                                     
025733           ELSE                                                           
025734              ADD +1                          TO RAD-IX                   
025735           END-IF                                                         
025736       END-PERFORM                                                        
025740                                                                          
025741       IF INPUT-FINNS                                                     
025742         PERFORM GAA-KOLLA-KDCMD-ADINLOMR                                 
025743       ELSE                                                               
025749         MOVE 'CMD'    TO RESP-IDELMT-ERROR                               
025750         MOVE '026'    TO RESP-IDMSG-ERROR                                
025751         MOVE NEJ TO INDATA-SW                                            
025752       END-IF                                                             
025753     ELSE                                                                 
025754       MOVE NEJ           TO INDATA-SW                                    
025755       IF REQU-KVRADER = 0                                                
025756          MOVE 'KVRADER' TO RESP-IDELMT-ERROR                             
025757          MOVE '126'     TO RESP-IDMSG-ERROR                              
025758       ELSE                                                               
025759          MOVE 'KVRADER' TO RESP-IDELMT-ERROR                             
025760          MOVE '024'     TO RESP-IDMSG-ERROR                              
025761       END-IF                                                             
025762     END-IF                                                               
025763     .                                                                    
025764     EJECT                                                                
025765 GAA-KOLLA-KDCMD-ADINLOMR SECTION.                                        
025766     SKIP2                                                                
025767     MOVE NEJ TO WS-REC-LIMIT                                             
025768     MOVE NEJ TO UPPDATERA-KOLLI-SW                                       
025769     MOVE +1  TO RAD-IX                                                   
025770                                                                          
025772     PERFORM UNTIL RAD-IX > 500 OR REC-LIMIT                              
025779                                                                          
025781         IF REQU-KDCMD (RAD-IX)  NOT = ALL '+' AND SPACE                  
025782            MOVE REQU-KDCMD(RAD-IX)  TO  RESP-KDCMD (RAD-IX)              
025789                                                                          
025791             IF REQU-KDCMD (RAD-IX) = W-DEVIATION                         
025792                                                                          
025794                 IF REQU-ADINLOMR-UPD (RAD-IX) = ALL '+'                  
025798                            MOVE JA TO UPPDATERA-KOLLI-SW                 
025799                 ELSE                                                     
025803                     MOVE NEJ      TO INDATA-SW                           
025804                     MOVE 'ADINLOMR' TO  RESP-IDELMT-ERROR                
025805                     MOVE '033' TO  RESP-IDMSG-ERROR                      
025806                                  RESP-IDMSG-ERROR-LINE (RAD-IX)          
025807                 END-IF                                                   
025809             ELSE                                                         
025813                IF REQU-KDCMD (RAD-IX) = W-DELETE OR                      
025814                                         W-RECEIVED                       
025818                     MOVE JA TO UPPDATERA-KOLLI-SW                        
025841                 END-IF                                                   
025842             END-IF                                                       
025843         ELSE                                                             
025845             IF REQU-ADINLOMR-UPD (RAD-IX) NOT = ALL '+'                  
025848                 MOVE SPACE                                               
025849                         TO  RESP-ADINLOMR-UPD (RAD-IX)                   
025850                 MOVE JA TO UPPDATERA-KOLLI-SW                            
025851             END-IF                                                       
025852         END-IF                                                           
025853        IF RAD-IX = WS-INDX-REC                                           
025854           MOVE JA TO WS-REC-LIMIT                                        
025855        ELSE                                                              
025856           ADD +1 TO RAD-IX                                               
025857        END-IF                                                            
025858     END-PERFORM                                                          
025859     .                                                                    
025860     EJECT                                                                
025861 GB-LOGISK-KONTROLL SECTION.                                              
025862     SKIP2                                                                
025864     MOVE REQU-IDRT-KEY          TO W-IDRT-BSEQ-MIN                       
025865                                    W-IDRT-BSEQ-MAX                       
025867     MOVE REQU-IDRTLOP-KEY       TO W-IDRTLOP-BSEQ-MIN                    
025868                                    W-IDRTLOP-BSEQ-MAX                    
025869     MOVE NEJ TO WS-REC-LIMIT                                             
025870     MOVE +1 TO RAD-IX                                                    
025872     PERFORM UNTIL RAD-IX > 500 OR REC-LIMIT                              
025875       IF REQU-RAD   (RAD-IX) = ALL '+'                                   
025876       OR REQU-KDCMD (RAD-IX) NUMERIC                                     
025877         CONTINUE                                                         
025878       ELSE                                                               
025885         MOVE REQU-IDKOLLI (RAD-IX)   TO W-IDKOLLI-BSEQ-MIN               
025886                                         W-IDKOLLI-BSEQ-MAX               
025887         PERFORM IMS-GU-SEQB-WLRETA01                                     
025888                                                                          
025889         IF SEGMENT-FINNS                                                 
025890           IF RET-IDKOLLI > ZERO                                          
025893             IF REQU-KDCMD (RAD-IX) = W-DEVIATION                         
025894               IF RET-KDRETSTA = 2 OR 3 OR 4                              
025895                 CONTINUE                                                 
025896               ELSE                                                       
025899                 MOVE NEJ TO UPPDATERA-KOLLI-SW                           
025900                 MOVE SPACE TO RESP-IDELMT-ERROR                          
025901                 MOVE '232' TO RESP-IDMSG-ERROR                           
025902                               RESP-IDMSG-ERROR-LINE (RAD-IX)             
025903               END-IF                                                     
025904             ELSE                                                         
025907               IF REQU-KDCMD (RAD-IX) = W-RECEIVED                        
025908                 IF RET-KDKOLSTA = W-KLI-SAK  OR                          
025909                                   W-KLI-AVV                              
025910                   CONTINUE                                               
025911                 ELSE                                                     
025914                   MOVE NEJ TO UPPDATERA-KOLLI-SW                         
025915                   MOVE SPACE TO RESP-IDELMT-ERROR                        
025916                   MOVE '233' TO RESP-IDMSG-ERROR                         
025917                                 RESP-IDMSG-ERROR-LINE (RAD-IX)           
025918                 END-IF                                                   
025919               END-IF                                                     
025922               IF REQU-KDCMD (RAD-IX) = W-DELETE                          
025923                 IF RET-KDRETSTA = 2   OR                                 
025924                   (RET-KDRETSTA = 3   AND                                
025925                    RET-IDDISTR  = ZERO)                                  
025926                   CONTINUE                                               
025927                 ELSE                                                     
025930                   MOVE NEJ TO UPPDATERA-KOLLI-SW                         
025931                   MOVE SPACE TO RESP-IDELMT-ERROR                        
025932                   MOVE '234' TO RESP-IDMSG-ERROR                         
025933                                 RESP-IDMSG-ERROR-LINE (RAD-IX)           
025934                 END-IF                                                   
025935               END-IF                                                     
025936             END-IF                                                       
025937           ELSE                                                           
025941             MOVE NEJ TO UPPDATERA-KOLLI-SW                               
025942             MOVE 'IDKOLLI' TO RESP-IDELMT-ERROR                          
025943             MOVE '026'     TO RESP-IDMSG-ERROR                           
025944                               RESP-IDMSG-ERROR-LINE (RAD-IX)             
025945           END-IF                                                         
025946         ELSE                                                             
025950           MOVE NEJ TO UPPDATERA-KOLLI-SW                                 
025951           MOVE 'IDRT-IDRTLOP'  TO RESP-IDELMT-ERROR                      
025952           MOVE '025' TO RESP-IDMSG-ERROR                                 
025953         END-IF                                                           
025954       END-IF                                                             
025955       IF RAD-IX = WS-INDX-REC                                            
025956         MOVE JA TO WS-REC-LIMIT                                          
025957       ELSE                                                               
025958         ADD +1 TO RAD-IX                                                 
025959       END-IF                                                             
025960     END-PERFORM                                                          
025961     .                                                                    
025962     EJECT                                                                
025963 H-UPPDATERA SECTION.                                                     
025964                                                                          
025965     MOVE NEJ TO WS-REC-LIMIT                                             
025966     MOVE ZERO  TO WS-DEL-COUNT                                           
025967     MOVE +1 TO RAD-IX                                                    
025968                4792-INDX                                                 
025970     PERFORM UNTIL RAD-IX > 500 OR REC-LIMIT                              
025972                                                                          
025980         IF REQU-KDCMD (RAD-IX) = W-RECEIVED                              
025981         OR REQU-KDCMD (RAD-IX) = W-DEVIATION                             
025982         OR REQU-KDCMD (RAD-IX) = W-DELETE                                
025983         OR REQU-ADINLOMR-UPD (RAD-IX) NOT = ALL '+'                      
025986             MOVE REQU-IDRT-KEY TO  W-IDRT-BSEQ-MIN                       
025987                                    W-IDRT-BSEQ-MAX                       
025990             MOVE REQU-IDRTLOP-KEY    TO  W-IDRTLOP-BSEQ-MIN              
025991                                          W-IDRTLOP-BSEQ-MAX              
025994             MOVE REQU-IDKOLLI (RAD-IX) TO W-IDKOLLI-BSEQ-MIN             
025995                                           W-IDKOLLI-BSEQ-MAX             
025997             PERFORM IMS-GHU-SEQB-WLRETA01                                
025999             PERFORM UNTIL SEGMENT-SAKNAS                                 
026002                IF REQU-KDCMD (RAD-IX) = W-RECEIVED                       
026003                                                                          
026004                     IF RET-KDRETSTA = 2 OR 3                             
026005                         IF RET-KDKOLSTA = W-KLI-SAK                      
026006                             PERFORM S02-FYLL-R31-MID                     
026007                         END-IF                                           
026008                     END-IF                                               
026009                                                                          
026010                     MOVE 5 TO RET-KDKOLSTA                               
026011                     MOVE 4 TO RET-KDRETSTA                               
026012                     IF RET-TILOSSN  = ZERO                               
026013                        ACCEPT RET-TILOSSN  FROM DATE                     
026014                     END-IF                                               
026015                     IF RET-DARETANK = ZERO                               
026016                       MOVE FUNCTION CURRENT-DATE (1:8)                   
026017                                     TO RET-DARETANK                      
026018                     END-IF                                               
026019                     IF RET-TIINLMOT = ZERO                               
026020                        ACCEPT RET-TIINLMOT FROM DATE                     
026021                     END-IF                                               
026022                                                                          
026024                     IF REQU-ADINLOMR-UPD (RAD-IX) NOT = ALL '+'          
026027                         MOVE REQU-ADINLOMR-UPD (RAD-IX)                  
026028                             TO RET-ADINLOMR                              
026029                     END-IF                                               
026030                                                                          
026031                     PERFORM IMS-REPL-SEQB-WLRETA01                       
026032                 ELSE                                                     
026033                                                                          
026036                     IF REQU-KDCMD (RAD-IX) = W-DEVIATION                 
026037                         IF RET-KDRETSTA = 2                              
026038                             MOVE W-KLI-SAK TO RET-KDKOLSTA               
026039                         ELSE                                             
026040                             IF RET-KDRETSTA = 3 OR 4                     
026041                                MOVE W-KLI-AVV   TO RET-KDKOLSTA          
026042                             END-IF                                       
026043                         END-IF                                           
026044                         PERFORM IMS-REPL-SEQB-WLRETA01                   
026045                     ELSE                                                 
026048                       IF REQU-KDCMD (RAD-IX) = W-DELETE                  
026049                          MOVE RET-DAREGDAT TO W-DAREGDAT                 
026050                          MOVE RET-TIKLOCK  TO W-TIKLOCK                  
026051                          PERFORM IMS-GHU-WLRETA01                        
026052                          PERFORM IMS-DLET-WLRETA01                       
026053                          ADD +1     TO WS-DEL-COUNT                      
026054                          IF WS-DEL-COUNT  = REQU-KVRADER                 
026055                             MOVE NEJ TO INDATA-SW                        
026056                          END-IF                                          
026057                       ELSE                                               
026061                           IF (REQU-KDCMD (RAD-IX) = ALL '+' OR           
026062                                                    SPACE )               
026063                           AND REQU-ADINLOMR-UPD (RAD-IX)                 
026064                                   NOT = ALL '+'                          
026067                               MOVE REQU-ADINLOMR-UPD (RAD-IX) TO         
026068                                   RET-ADINLOMR                           
026069                               PERFORM IMS-REPL-SEQB-WLRETA01             
026070                           END-IF                                         
026071                       END-IF                                             
026072                     END-IF                                               
026073                 END-IF                                                   
026074                 PERFORM IMS-GHN-SEQB-WLRETA01                            
026075             END-PERFORM                                                  
026076           END-IF                                                         
026077        IF RAD-IX = WS-INDX-REC                                           
026078           MOVE JA TO WS-REC-LIMIT                                        
026079        ELSE                                                              
026080           ADD +1 TO RAD-IX                                               
026081        END-IF                                                            
026082     END-PERFORM                                                          
026083                                                                          
026084     IF 4792-INDX  > +1                                                   
026085         PERFORM S03-STARTA-R31-RAPPORTERING                              
026086     END-IF                                                               
026087                                                                          
026089     MOVE '001'   TO RESP-IDMSG-INFO                                      
026094     .                                                                    
026095     EJECT                                                                
026721                                                                          
026722 S02-FYLL-R31-MID                SECTION.                                 
026723                                                                          
026724     MOVE RET-IDDC             TO MOD4792-MID-IDDC                        
026725     MOVE RET-DAREGDAT (3:6)   TO MOD4792-MID-TIREGDAT(4792-INDX)         
026726     MOVE RET-TIKLOCK          TO MOD4792-MID-TIKLOCK (4792-INDX)         
026727     ADD +1                    TO 4792-INDX                               
026728                                                                          
026729     IF 4792-INDX              >  4792-MAX-INDX                           
026730        PERFORM S03-STARTA-R31-RAPPORTERING                               
026731        MOVE +1                TO 4792-INDX                               
026732     END-IF                                                               
026733     .                                                                    
026734     EJECT                                                                
026735                                                                          
026736 S03-STARTA-R31-RAPPORTERING     SECTION.                                 
026737                                                                          
026738     ACCEPT DAGENS-DATUM       FROM DATE                                  
026739     ACCEPT DAGENS-TID         FROM TIME                                  
026740                                                                          
026741     MOVE SPACE                TO MSG-KOM-WMSGKOM                         
           COMPUTE MSG-KOM-KVLL = LENGTH OF MSG-KOM-WMSGKOM                     
026743     MOVE LOW-VALUE            TO MSG-KOM-KDZ1                            
026744     MOVE LOW-VALUE            TO MSG-KOM-KDZ2                            
026745     MOVE SPACE                TO MSG-KOM-KDTRANS                         
026746     MOVE 'W4I79201'           TO MSG-KOM-IDCPYTXT                        
026747     MOVE 'INLEVRET'           TO MSG-KOM-IDSNDNOD                        
026749     MOVE 'WL014700'           TO MSG-KOM-IDSNDJOB                        
026750     MOVE DAGENS-DATUM         TO MSG-KOM-TIREGDAT                        
026751     MOVE DAGENS-TID           TO MSG-KOM-TIKLOCK                         
026752     MOVE SPACE                TO MSG-KOM-IDMFSMED                        
026753                                                                          
026754     COMPUTE P-TO-P-MSG-KVLL   =  LNG-P-TO-P-PREFIX +                     
026755                                  LENGTH OF MOD4792-MID-W4I79201          
026756                                                                          
026757     MOVE 'W4T792X '           TO P-TO-P-MSG-KDTRANS                      
026759     MOVE 'L147'               TO P-TO-P-MSG-IDTRANS                      
026761     MOVE '2'                  TO P-TO-P-MSG-KDMFSFOR                     
026762                                                                          
026763     COMPUTE MOD4792-MID-KVPOST  = 4792-INDX - 1                          
026764                                                                          
026765     MOVE MOD4792-MID-W4I79201 TO P-TO-P-MSG-INDATA                       
026766                                                                          
026767     CALL W006KOM USING MSG-PCB                                           
026768                        DISP-PCB                                          
026769                        KOM-KOMA-PCB                                      
026770                        MSG-KOM-WMSGKOM                                   
026771                        P-TO-P-MSG-IO-AREA-SNUF                           
026772                                                                          
026780     .                                                                    
026800     SKIP3                                                                
026806*    --- DISPATCHER SECTIONS                                              
026807 S04-FETCH-REQUEST-ARGUMENT SECTION.                                      
026808                                                                          
026809     MOVE 'GETARG'               TO SUB-KDFUNC                            
026810     MOVE 'CARPARTS.LDC.RETURNCONTENTS' TO SUB-ADDISPABS                  
026811     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
026812                                                                          
026813     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
026814                                                                          
026815     IF SUB-KDRC > 0                                                      
026816       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
026817       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
026818       DELIMITED BY SIZE INTO ERROR-TEXT                                  
026819       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
026820     END-IF                                                               
026821     .                                                                    
026822     SKIP3                                                                
026823 S05-RETURN-RESPONSE SECTION.                                             
026824                                                                          
026825     MOVE 'RETURN'                   TO SUB-KDFUNC                        
026826     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
026827                                                                          
026828     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
026829                                                                          
026830     IF SUB-KDRC > 0                                                      
026831       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
026832       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
026833       DELIMITED BY SIZE INTO ERROR-TEXT                                  
026834       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
026835     END-IF                                                               
026836     .                                                                    
026837     EJECT                                                                
026838     SKIP3                                                                
032701     EJECT                                                                
032747 IMS-GHU-WLRETA01 SECTION.                                                
032748                                                                          
032749     STRING 'WLRETA01(WDA301KY =' W-WDA301KY-X ')'                        
032751          DELIMITED BY SIZE INTO SSA1                                     
032752     MOVE '  ' TO GODK-STATUSKODER                                        
032753     CALL CBLTDLI USING GHU RETA-PCB DLI-IO-RETA01 SSA1                   
032754     MOVE RETA-STATUS-CODE TO STATUS-WS                                   
032755     PERFORM IMS-STATUSKONTROLL                                           
032756     .                                                                    
032757     SKIP3                                                                
032758 IMS-DLET-WLRETA01 SECTION.                                               
032759                                                                          
032762     MOVE '  ' TO GODK-STATUSKODER                                        
032763     CALL CBLTDLI USING DLET RETA-PCB DLI-IO-RETA01                       
032764     MOVE RETA-STATUS-CODE TO STATUS-WS                                   
032765     PERFORM IMS-STATUSKONTROLL                                           
032766     .                                                                    
032767     EJECT                                                                
032768 IMS-GU-SEQB-WLRETA01 SECTION.                                            
032769                                                                          
032770     STRING 'WLRETA01(WDA3BSEQ>=' W-WDA3BSEQ-MIN-X                        
032771                    '&WDA3BSEQ<=' W-WDA3BSEQ-MAX-X ')'                    
032772          DELIMITED BY SIZE INTO SSA1                                     
032773     MOVE '  GE' TO GODK-STATUSKODER                                      
032774     CALL CBLTDLI USING GU SEQB-PCB DLI-IO-RETA01 SSA1                    
032775     MOVE SEQB-STATUS-CODE TO STATUS-WS                                   
032776     PERFORM IMS-STATUSKONTROLL                                           
032777     .                                                                    
032778     SKIP3                                                                
032779 IMS-GN-SEQB-WLRETA01 SECTION.                                            
032780                                                                          
032781     STRING 'WLRETA01(WDA3BSEQ> ' W-WDA3BSEQ-MIN-X                        
032782                    '&WDA3BSEQ<=' W-WDA3BSEQ-MAX-X ')'                    
032783          DELIMITED BY SIZE INTO SSA1                                     
032784     MOVE '  GE' TO GODK-STATUSKODER                                      
032785     CALL CBLTDLI USING GN SEQB-PCB DLI-IO-RETA01 SSA1                    
032786     MOVE SEQB-STATUS-CODE TO STATUS-WS                                   
032787     PERFORM IMS-STATUSKONTROLL                                           
032788     .                                                                    
032789     EJECT                                                                
032790 IMS-GHU-SEQB-WLRETA01 SECTION.                                           
032791                                                                          
032792     STRING 'WLRETA01(WDA3BSEQ>=' W-WDA3BSEQ-MIN-X                        
032793                    '&WDA3BSEQ<=' W-WDA3BSEQ-MAX-X ')'                    
032794          DELIMITED BY SIZE INTO SSA1                                     
032795     MOVE '  GE' TO GODK-STATUSKODER                                      
032796     CALL CBLTDLI USING GHU SEQB-PCB DLI-IO-RETA01 SSA1                   
032797     MOVE SEQB-STATUS-CODE TO STATUS-WS                                   
032798     PERFORM IMS-STATUSKONTROLL                                           
032799     .                                                                    
032800     SKIP3                                                                
032801 IMS-GHN-SEQB-WLRETA01 SECTION.                                           
032802                                                                          
032803     STRING 'WLRETA01(WDA3BSEQ>=' W-WDA3BSEQ-MIN-X                        
032804                    '&WDA3BSEQ<=' W-WDA3BSEQ-MAX-X ')'                    
032805          DELIMITED BY SIZE INTO SSA1                                     
032806     MOVE '  GE' TO GODK-STATUSKODER                                      
032807     CALL CBLTDLI USING GHN SEQB-PCB DLI-IO-RETA01 SSA1                   
032808     MOVE SEQB-STATUS-CODE TO STATUS-WS                                   
032809     PERFORM IMS-STATUSKONTROLL                                           
032810     .                                                                    
032811     EJECT                                                                
032812 IMS-REPL-SEQB-WLRETA01 SECTION.                                          
032813                                                                          
032814     MOVE '  ' TO GODK-STATUSKODER                                        
032815     CALL CBLTDLI USING REPL SEQB-PCB DLI-IO-RETA01                       
032816     MOVE SEQB-STATUS-CODE TO STATUS-WS                                   
032817     PERFORM IMS-STATUSKONTROLL                                           
032818     .                                                                    
032819     EJECT                                                                
032890     EJECT                                                                
032900 IMS-STATUSKONTROLL SECTION.                                              
033000                                                                          
033100     SET STATUS-IX TO 1                                                   
033200     SEARCH GODK-STATUS                                                   
033300       AT END                                                             
033400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
033500         DELIMITED BY SIZE INTO FELTEXT                                   
033600         CALL FELLOG                                                      
033700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
033800         CONTINUE                                                         
033900     END-SEARCH                                                           
034000     .                                                                    
