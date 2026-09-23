000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL018000.                                                
000300 AUTHOR.         TAPAS KUMAR GHOSH.                                       
000400 DATE-WRITTEN.   2004/11/04.                                              
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        SCREEN TO START PRINT OF TRANSPORT                               
000900*        DOCUMENTS FOR RELEASED SHIPPINGS.                                
001000*                                                                         
001100*        PROGRAM UPDATES WDE1                                             
001110*                                                                         
001200*    WL018000 PROGRAM IS A REPLICA OF W4062200 PROGRAM                    
001201*    AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS                              
001202*                                                                         
001210*                                                                         
001220* ADDRESS: 'CARPARTS.LDC.SHIPPINGDOCUMENTS'                               
001230*                                                                         
001300*    INDATA.                                                              
001400*        TRANSAKTION: WL0180U                                             
001500*        REQUEST:     WZ01REQU                                            
001510*                     WL0180I1                                            
001600*                                                                         
001700*    UTDATA.                                                              
001800*        RESPONSE:    WZ01RESP                                            
001810*                     WL0180O1                                            
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200                                                                          
002300 DATA DIVISION.                                                           
002400     EJECT                                                                
002500 WORKING-STORAGE SECTION.                                                 
002600 77  IDPGM                       PIC X(08)   VALUE 'WL018000'.            
002700                                                                          
002800*    --- WORKING AREAS FOR ERROR MESSAGES FROM ABEND                      
002900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
002910 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
002920 77  KDRC-DISPLAY                PIC Z(5)   VALUE ZERO.                   
003000                                                                          
003100 77  JA                          PIC X       VALUE 'J'.                   
003200 77  NEJ                         PIC X       VALUE 'N'.                   
003300                                                                          
003400 77  KDCMD-SW                    PIC X       VALUE 'N'.                   
003500     88  KDCMD-GIVEN                         VALUE 'J'.                   
003600                                                                          
003700 77  DATE-INPUT-SW               PIC X       VALUE 'N'.                   
003800     88  DATE-INPUT                          VALUE 'J'.                   
003900                                                                          
004000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004100     88  NYCKLAR-OK                          VALUE 'J'.                   
004200     88  NYCKLAR-FEL                         VALUE 'N'.                   
004300                                                                          
004400 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004500     88  INDATA-OK                           VALUE 'J'.                   
004600     88  INDATA-FEL                          VALUE 'N'.                   
004601                                                                          
004610 77  WS-REC-LIMIT                PIC X       VALUE 'N'.                   
004620     88  REC-LIMIT                           VALUE 'J'.                   
004630                                                                          
005800 77  WS-COUNT                    PIC S9(4)   VALUE +0   COMP SYNC.        
005801 77  WS-INDX-REC                 PIC S9(4)   VALUE +0   COMP SYNC.        
005810 77  INDX                        PIC S9(4)   VALUE +0   COMP SYNC.        
005900 77  MAX-INDX                    PIC S9(4)   VALUE +500 COMP SYNC.        
006100                                                                          
006701 77  WS-TISKEPPN                 PIC 9(6).                                
006710                                                                          
006720 77  WS-IDELMT-ERROR             PIC X(16).                               
006730 77  WS-IDMSG-ERROR              PIC X(03).                               
006740 77  WS-IDMSG-INFO               PIC X(03).                               
006750                                                                          
006760     EJECT                                                                
006770 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006780 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006790 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006791                                                                          
006795     EJECT                                                                
006800                                                                          
006900*    --- SUBPROGRAMS AND PARAMETERAREAS                                   
007000 01  GENERELLA-SUBPROGRAM.                                                
007100     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007710     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007720     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
007730     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
007800     EJECT                                                                
007900                                                                          
008000*    --- PARAMETERS FOR WMEDKONV                                          
008100*01 -COPY WMEDAREA                                                        
008200 01  MESSAGE-CODES.                                                       
008300     03  KEYS-ARE-MISSING        PIC X(3)    VALUE '005'.                 
008400     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
008500     03  INF-ENTER-CMD           PIC X(3)    VALUE '048'.                 
008600     03  INF-PRESS-PF4-TO-PRINT  PIC X(3)    VALUE '081'.                 
008700     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
008800     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
008900     03  INF-PRINT-REQUESTED     PIC X(3)    VALUE '118'.                 
009000     03  INF-PRESS-PF9-TO-SPLIT  PIC X(3)    VALUE '127'.                 
009100     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
009200     03  ERR-MORE-THAN-ONE-CMD   PIC X(3)    VALUE '097'.                 
009300     03  ERR-LAST-PAGE-SHOWN     PIC X(3)    VALUE '115'.                 
009400     03  ERR-PF4-AND-NO-CMD      PIC X(3)    VALUE '231'.                 
009500     03  ERR-SELECT-LINE         PIC X(3)    VALUE '309'.                 
009600     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
009700     03  ERR-ZERO-NOT-ALLOWED    PIC X(3)    VALUE '724'.                 
009800     03  ERR-WRONG-PRINTER       PIC X(3)    VALUE '772'.                 
009900     EJECT                                                                
010000                                                                          
012000*    --- AREA  FOR WZ01  ------                                           
012220 01  FILLER                      PIC X(16)   VALUE 'WZ01SUB '.            
012230*01  -COPY WZ01SUB                                                        
012240     EJECT                                                                
012250 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
012260*01  -COPY WZ01SEND                                                       
012270     EJECT                                                                
012280 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
012290 01  REQU-AREA.                                                           
012291*    03  -COPY WZ01REQU                                                   
012292*    03  -COPY WL0180I1                                                   
012293     EJECT                                                                
012294 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
012295 01  RESP-AREA.                                                           
012296*    03  -COPY WZ01RESP                                                   
012297*    03  -COPY WL0180O1                                                   
012298     EJECT                                                                
012300                                                                          
016200*    --- WORK-AREAS TO IMS-SECTIONS                                       
016300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016400     SKIP3                                                                
016500                                                                          
016600 01  NYCKLAR-TILL-DLI.                                                    
016700                                                                          
016710     03  W-TISKEPPN-X.                                                    
016720         05  W-TISKEPPN          PIC S9(7) COMP-3 VALUE ZERO.             
016721                                                                          
016800     03  W-WDE1A1KY-MIN.                                                  
016900         05  W-IDDC-MIN          PIC X(2).                                
017000         05  W-IDTRPTNR-MIN      PIC S9(3) COMP-3.                        
017100         05  W-IDLBBET-MIN       PIC X(12) VALUE LOW-VALUE.               
017200         05  W-TISKEPPN-MIN      PIC S9(7) COMP-3 VALUE ZERO.             
017210         05  W-TISKPTID-MIN      PIC S9(7) COMP-3 VALUE ZERO.             
017300                                                                          
017400     03  W-WDE1A1KY-MAX.                                                  
017500         05  W-IDDC-MAX          PIC X(2).                                
017600         05  W-IDTRPTNR-MAX      PIC S9(3) COMP-3.                        
017700         05  W-IDLBBET-MAX       PIC X(12) VALUE HIGH-VALUE.              
017900         05  W-TISKEPPN-MAX      PIC S9(7) COMP-3 VALUE 9999999.          
017910         05  W-TISKPTID-MAX      PIC S9(7) COMP-3 VALUE 9999999.          
018000                                                                          
018001     03  W-WDE1ASEQ-MIN.                                                  
018002         05  W-IDDC-ASEQ-MIN     PIC X(2).                                
018003         05  W-IDTRPTNR-ASEQ-MIN PIC S9(3) COMP-3.                        
018004         05  W-IDLBBET-ASEQ-MIN  PIC X(12) VALUE LOW-VALUE.               
018005         05  W-TISKEPPN-ASEQ-MIN PIC S9(7) COMP-3 VALUE ZERO.             
018006                                                                          
018007     03  W-WDE1ASEQ-MAX.                                                  
018008         05  W-IDDC-ASEQ-MAX     PIC X(2).                                
018009         05  W-IDTRPTNR-ASEQ-MAX PIC S9(3) COMP-3.                        
018010         05  W-IDLBBET-ASEQ-MAX  PIC X(12) VALUE HIGH-VALUE.              
018011         05  W-TISKEPPN-ASEQ-MAX PIC S9(7) COMP-3 VALUE 9999999.          
018012                                                                          
018020     03  W-IDSHIPM-X.                                                     
018030         05  W-IDSHIPM           PIC 9(7)    VALUE ZERO.                  
018300                                                                          
018800*    --- STATUS-CODE FROM IMS                                             
018900 01  STATUS-WS                   PIC XX.                                  
019000     88  SEGMENT-FINNS                       VALUE '  '.                  
019100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
019200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
019300     88  BASEN-SLUT                          VALUE 'GB'.                  
019400     SKIP2                                                                
019500 01  GODK-STATUSKODER.                                                    
019600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
019700     SKIP3                                                                
019800 01  SSA1                        PIC X(160).                              
019900     EJECT                                                                
020000                                                                          
020100*    --- IMS FUNCTIONCODES                                                
020200*01  -COPY W0003                                                          
020300     EJECT                                                                
020400                                                                          
020500*    ---  DLI INPUT-OUTPUT AREA                                           
020600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE101'.                      
020700 01  DLI-IO-WDE101.                                                       
020800*    03  -COPY WDE101                                                     
020900     EJECT                                                                
020910 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE1A1'.                      
020920 01  DLI-IO-WDE1A1.                                                       
020930*    03  -COPY WDE1A1                                                     
020940     EJECT                                                                
021000                                                                          
021100 LINKAGE SECTION.                                                         
021200 01  MSG-PCB                     PIC X.                                   
021700*01  -COPY W0008   -PRE WDE1-                                             
021701     05  FILLER                  PIC X.                                   
021710*01  -COPY W0008   -PRE WDE1A-                                            
021800     05  FILLER                  PIC X.                                   
021820*01  -COPY W0008   -PRE WDE1P-                                            
021830     05  FILLER                  PIC X.                                   
021900     EJECT                                                                
022000                                                                          
022100 PROCEDURE DIVISION  USING MSG-PCB                                        
022200                           WDE1-PCB WDE1A-PCB WDE1P-PCB.                  
022300                                                                          
022400 MAIN SECTION.                                                            
022500     ENTRY 'DLITCBL' USING MSG-PCB                                        
022600                           WDE1-PCB WDE1A-PCB WDE1P-PCB.                  
022700                                                                          
022810     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
022910     IF SUB-KDRC = 0                                                      
023000       PERFORM A-INIT                                                     
023100       PERFORM B-KOLLA-NYCKLAR                                            
023200       IF NYCKLAR-OK                                                      
025310         IF REQU-KDPGMACT = 'E'                                           
025320            PERFORM G-CHECK-PRINT                                         
025330         END-IF                                                           
025400         IF INDATA-OK                                                     
025600             PERFORM F-LAES-VISA-INFO                                     
025800         END-IF                                                           
025900       END-IF                                                             
026510       MOVE RESP-IDMSG-INFO    TO WS-IDMSG-INFO                           
026520       MOVE RESP-IDMSG-ERROR   TO WS-IDMSG-ERROR                          
026530       MOVE RESP-IDELMT-ERROR  TO WS-IDELMT-ERROR                         
026540       IF WS-IDMSG-INFO  NOT = SPACE                                      
026550          MOVE SPACE           TO RESP-IDMSG-ERROR                        
026560          MOVE SPACE           TO RESP-IDELMT-ERROR                       
026570       ELSE                                                               
026580         IF WS-IDMSG-ERROR NOT = SPACE                                    
026591            MOVE ALL '+' TO RESP-WL0180O1 (1:38)                          
026592            MOVE WS-IDMSG-ERROR   TO RESP-IDMSG-ERROR                     
026593            MOVE WS-IDELMT-ERROR  TO RESP-IDELMT-ERROR                    
026594            MOVE WS-IDMSG-INFO    TO RESP-IDMSG-INFO                      
026595            MOVE  001             TO RESP-IDMSGVER                        
026596            IF  REQU-KDPGMACT = 'S'                                       
026597                MOVE ZERO             TO RESP-KVRADER                     
026598            ELSE                                                          
026599              IF REQU-KVRADER NUMERIC                                     
026600                 MOVE REQU-KVRADER     TO RESP-KVRADER                    
026601              ELSE                                                        
026602                 MOVE ZERO             TO RESP-KVRADER                    
026603              END-IF                                                      
026604            END-IF                                                        
026605         END-IF                                                           
026606       END-IF                                                             
026607       PERFORM S02-RETURN-RESPONSE                                        
026700     END-IF                                                               
026800                                                                          
026900     MOVE ZERO TO RETURN-CODE                                             
027000     GOBACK                                                               
027100     .                                                                    
027200     EJECT                                                                
027300 A-INIT SECTION.                                                          
027400                                                                          
027410     MOVE ALL '+' TO RESP-AREA                                            
027420     MOVE SPACE   TO RESP-IDMSG-INFO                                      
027430                     RESP-IDMSG-ERROR                                     
027440                     RESP-IDELMT-ERROR                                    
027450     MOVE 001     TO RESP-IDMSGVER                                        
027460     MOVE ZERO    TO RESP-KVRADER                                         
027470                                                                          
028400                                                                          
028800                                                                          
029300                                                                          
030000     MOVE +1 TO INDX                                                      
030100     .                                                                    
030200     EJECT                                                                
030300 B-KOLLA-NYCKLAR SECTION.                                                 
030400                                                                          
031800                                                                          
032200                                                                          
032300*    --- WMEDKONV LANGUAGE                                                
032500                                                                          
032600     MOVE JA TO NYCKLAR-SW                                                
032700                                                                          
033200                                                                          
033300*    --- TRANSPORTNUMBER                                                  
033400                                                                          
033900                                                                          
034000*    INSPECT MSGI-IDTRPTNR REPLACING LEADING SPACE BY ZERO                
034100     IF REQU-IDTRPTNR-KEY NUMERIC AND REQU-IDTRPTNR-KEY > ZERO            
034200       MOVE REQU-IDTRPTNR-KEY TO W-IDTRPTNR-MIN                           
034300                                 W-IDTRPTNR-MAX                           
034310                                 W-IDTRPTNR-ASEQ-MIN                      
034320                                 W-IDTRPTNR-ASEQ-MAX                      
034400     ELSE                                                                 
034500       MOVE NEJ TO NYCKLAR-SW                                             
034510       MOVE 'IDTRP'   TO RESP-IDELMT-ERROR                                
034520       MOVE '023'        TO RESP-IDMSG-ERROR                              
034600     END-IF                                                               
034700                                                                          
034800*    --- CARRIER                                                          
034900                                                                          
035400                                                                          
035500     IF REQU-IDLBBET-KEY NOT = ALL '+' AND                                
035510        REQU-IDLBBET-KEY NOT = SPACE                                      
035600       MOVE REQU-IDLBBET-KEY TO W-IDLBBET-MIN                             
035700                                W-IDLBBET-MAX                             
035710                                W-IDLBBET-ASEQ-MIN                        
035730                                W-IDLBBET-ASEQ-MAX                        
035800     END-IF                                                               
035900                                                                          
036000*    --- SHIPPINGDATE                                                     
036100                                                                          
036600                                                                          
036610*    IF REQU-TISKEPPN-KEY = ALL '+'                                       
036620*       INSPECT REQU-TISKEPPN-KEY REPLACING ALL '+'  BY ZERO              
036630*    END-IF                                                               
036800     IF REQU-TISKEPPN-KEY NUMERIC                                         
036900       MOVE REQU-TISKEPPN-KEY TO W-TISKEPPN                               
036901                                 W-TISKEPPN-MIN                           
036902                                 W-TISKEPPN-MAX                           
036910       MOVE REQU-TISKEPPN-KEY   TO RESP-TISKEPPN-KEY                      
037000     ELSE                                                                 
037010       IF REQU-TISKEPPN-KEY NOT = ALL '+'                                 
037100         MOVE NEJ TO NYCKLAR-SW                                           
037110         MOVE 'TISKEPPN'   TO RESP-IDELMT-ERROR                           
037120         MOVE '023'        TO RESP-IDMSG-ERROR                            
037130       END-IF                                                             
037200     END-IF                                                               
037300                                                                          
037400     IF W-TISKEPPN > ZERO                                                 
037500*      IF W-IDLBBET-MIN <= SPACE                                          
037600         MOVE JA TO DATE-INPUT-SW                                         
037700*      END-IF                                                             
037800     END-IF                                                               
037900                                                                          
038000*    --- DC                                                               
038100                                                                          
038600                                                                          
038700     MOVE REQU-IDDC-KEY TO W-IDDC-MIN                                     
039300                     W-IDDC-MAX                                           
039400                     W-IDDC-ASEQ-MIN                                      
039410                     W-IDDC-ASEQ-MAX                                      
039420                                                                          
039530                                                                          
040200                                                                          
040400                                                                          
040510     IF NYCKLAR-OK                                                        
040600       MOVE REQU-IDTRPTNR-KEY   TO RESP-IDTRPTNR-KEY                      
040700       MOVE REQU-IDLBBET-KEY    TO RESP-IDLBBET-KEY                       
040900       MOVE REQU-IDDC-KEY       TO RESP-IDDC-KEY                          
041500     END-IF                                                               
041600                                                                          
042400     .                                                                    
042500     EJECT                                                                
042600                                                                          
053300 F-LAES-VISA-INFO SECTION.                                                
053400                                                                          
053691       IF DATE-INPUT                                                      
053693         PERFORM IMS-GU-WDE1A1-DATE                                       
053694         MOVE SEQA-IDSHIPM TO W-IDSHIPM                                   
053695         PERFORM IMS-GU-WDE101                                            
053696       ELSE                                                               
053697         PERFORM IMS-GU-WDE101-ASEQ                                       
053698       END-IF                                                             
054400                                                                          
054500     IF SEGMENT-SAKNAS                                                    
054510       MOVE 'SHIPMENT'       TO RESP-IDELMT-ERROR                         
054520       MOVE '041'            TO RESP-IDMSG-ERROR                          
054900     ELSE                                                                 
055010       MOVE SHIP-IDLBBET TO W-IDLBBET-MIN                                 
055020       MOVE SHIP-IDLBBET TO W-IDLBBET-ASEQ-MIN                            
055030       MOVE SHIP-TISKEPPN TO W-TISKEPPN-ASEQ-MIN                          
055040       MOVE SHIP-TISKEPPN TO W-TISKEPPN-MIN                               
055050       MOVE SHIP-TISKPTID TO W-TISKPTID-MIN                               
055100     END-IF                                                               
055200                                                                          
055300     MOVE +1 TO INDX                                                      
055310     MOVE +0 TO WS-COUNT                                                  
055400     PERFORM UNTIL INDX > MAX-INDX                                        
055500       IF SEGMENT-FINNS                                                   
055510         IF REQU-KDPGMACT = 'S'                                           
055520            MOVE SPACE     TO RESP-KDCMD (INDX)                           
055530         END-IF                                                           
055600         MOVE SHIP-IDLBBET    TO RESP-IDLBBET (INDX)                      
055710         MOVE SHIP-TISKEPPN   TO WS-TISKEPPN                              
055711         MOVE WS-TISKEPPN     TO RESP-TISKEPPN (INDX)                     
055712         IF RESP-TISKEPPN (INDX) = '000000'                               
055720           MOVE SPACE         TO RESP-TISKEPPN (INDX)                     
055730         END-IF                                                           
055800         MOVE SHIP-TISKPTID   TO RESP-TISKPTID (INDX)                     
055900         MOVE SHIP-IDSHIPM    TO RESP-IDSHIPM (INDX)                      
056120         ADD +1     TO WS-COUNT                                           
057100       END-IF                                                             
057200                                                                          
057400       ADD 1 TO INDX                                                      
057500                                                                          
057600       IF DATE-INPUT                                                      
057700         PERFORM IMS-GN-WDE1A1-DATE                                       
057710         MOVE SEQA-IDSHIPM TO W-IDSHIPM                                   
057711         IF SEGMENT-FINNS                                                 
057720           PERFORM IMS-GU-WDE101                                          
057730         END-IF                                                           
057800       ELSE                                                               
057900         PERFORM IMS-GN-WDE101-ASEQ                                       
058000       END-IF                                                             
058100                                                                          
058200     END-PERFORM                                                          
058210     MOVE WS-COUNT  TO RESP-KVRADER                                       
058300                                                                          
058500                                                                          
060100                                                                          
060600     .                                                                    
060700     EJECT                                                                
060800 G-CHECK-PRINT SECTION.                                                   
060900                                                                          
061300                                                                          
061400     MOVE JA  TO INDATA-SW                                                
061510                                                                          
061520     IF REQU-KVRADER NUMERIC AND REQU-KVRADER > 0                         
061540       MOVE REQU-KVRADER        TO WS-INDX-REC                            
061550       MOVE NEJ                 TO WS-REC-LIMIT                           
061560       MOVE +1                  TO INDX                                   
061600       PERFORM UNTIL INDX > MAX-INDX OR REC-LIMIT                         
061700                                                                          
061800                                                                          
061900         IF REQU-KDCMD (INDX) = ALL '+' OR 'PR'                           
062000           IF REQU-KDCMD (INDX) = 'PR'                                    
062100             IF KDCMD-GIVEN                                               
062200               MOVE NEJ TO INDATA-SW                                      
062210               MOVE '264'        TO RESP-IDMSG-ERROR                      
062211                                    RESP-IDMSG-ERROR-LINE (INDX)          
062220               MOVE SPACE        TO RESP-IDELMT-ERROR                     
062700             ELSE                                                         
062800               MOVE JA TO KDCMD-SW                                        
063010               MOVE REQU-IDSHIPM (INDX)  TO RESP-IDSHIPM (INDX)           
063100             END-IF                                                       
063200           END-IF                                                         
063300         ELSE                                                             
063400           MOVE JA TO KDCMD-SW                                            
063410           MOVE '023'        TO RESP-IDMSG-ERROR                          
063420           MOVE 'KDCMDVAL'   TO RESP-IDELMT-ERROR                         
063900           MOVE NEJ TO INDATA-SW                                          
064000         END-IF                                                           
064010         IF INDX = WS-INDX-REC                                            
064020            MOVE JA TO WS-REC-LIMIT                                       
064030         ELSE                                                             
064040            ADD +1          TO INDX                                       
064050         END-IF                                                           
064200       END-PERFORM                                                        
064210     ELSE                                                                 
064220       MOVE NEJ           TO INDATA-SW                                    
064230       IF REQU-KVRADER = 0                                                
064240          MOVE 'KVRADER' TO RESP-IDELMT-ERROR                             
064250          MOVE '126'     TO RESP-IDMSG-ERROR                              
064260       ELSE                                                               
064270          MOVE 'KVRADER' TO RESP-IDELMT-ERROR                             
064280          MOVE '024'     TO RESP-IDMSG-ERROR                              
064290       END-IF                                                             
064291     END-IF                                                               
064300                                                                          
067100                                                                          
067200     IF NOT KDCMD-GIVEN                                                   
067510       MOVE 'KDCMDVAL'          TO RESP-IDELMT-ERROR                      
067520       MOVE '026'               TO RESP-IDMSG-ERROR                       
067600       MOVE NEJ TO INDATA-SW                                              
067700     END-IF                                                               
067800                                                                          
068400     .                                                                    
068500     EJECT                                                                
083310 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
083320                                                                          
083330     MOVE 'GETARG'               TO SUB-KDFUNC                            
083340     MOVE 'CARPARTS.LDC.SHIPPINGDOCUMENTS'   TO SUB-ADDISPABS             
083350     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
083360                                                                          
083370     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
083380                                                                          
083390     IF SUB-KDRC > 0                                                      
083391       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
083392       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
083393       DELIMITED BY SIZE INTO ERROR-TEXT                                  
083394       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
083395     END-IF                                                               
083396     .                                                                    
083397     SKIP3                                                                
083398 S02-RETURN-RESPONSE SECTION.                                             
083399                                                                          
083400     MOVE 'RETURN'                   TO SUB-KDFUNC                        
083401     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
083402                                                                          
083403     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
083404                                                                          
083405     IF SUB-KDRC > 0                                                      
083406       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
083407       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
083408       DELIMITED BY SIZE INTO ERROR-TEXT                                  
083409       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
083410     END-IF                                                               
083411     .                                                                    
083412     EJECT                                                                
094510 IMS-GU-WDE101 SECTION.                                                   
094520                                                                          
094530     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
094540          DELIMITED BY SIZE INTO SSA1                                     
094550     MOVE '  GE' TO GODK-STATUSKODER                                      
094560     CALL CBLTDLI USING GU WDE1-PCB DLI-IO-WDE101 SSA1                    
094570     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
094580     PERFORM IMS-STATUSKONTROLL                                           
094590     .                                                                    
094591     SKIP3                                                                
094610 IMS-GU-WDE1A1-DATE SECTION.                                              
094700                                                                          
094800     STRING 'WDE1A1  (WDE1A1KY>=' W-WDE1A1KY-MIN                          
094900                    '&WDE1A1KY<=' W-WDE1A1KY-MAX                          
095000                    '&TISKEPPN =' W-TISKEPPN-X ')'                        
095100          DELIMITED BY SIZE INTO SSA1                                     
095200     MOVE '  GE' TO GODK-STATUSKODER                                      
095300     CALL CBLTDLI USING GU WDE1A-PCB DLI-IO-WDE1A1 SSA1                   
095400     MOVE WDE1A-STATUS-CODE TO STATUS-WS                                  
095500     PERFORM IMS-STATUSKONTROLL                                           
095600     .                                                                    
095700     SKIP3                                                                
095800 IMS-GU-WDE101-ASEQ SECTION.                                              
095900                                                                          
096000     STRING 'WDE101  (WDE1ASEQ>=' W-WDE1ASEQ-MIN                          
096100                    '&WDE1ASEQ<=' W-WDE1ASEQ-MAX ')'                      
096200          DELIMITED BY SIZE INTO SSA1                                     
096300     MOVE '  GE' TO GODK-STATUSKODER                                      
096400     CALL CBLTDLI USING GU WDE1P-PCB DLI-IO-WDE101 SSA1                   
096500     MOVE WDE1P-STATUS-CODE TO STATUS-WS                                  
096600     PERFORM IMS-STATUSKONTROLL                                           
096700     .                                                                    
096800     SKIP3                                                                
096811 IMS-GN-WDE1A1-DATE SECTION.                                              
096820                                                                          
096830     STRING 'WDE1A1  (WDE1A1KY>=' W-WDE1A1KY-MIN                          
096840                    '&WDE1A1KY<=' W-WDE1A1KY-MAX                          
096850                    '&TISKEPPN =' W-TISKEPPN-X ')'                        
096860          DELIMITED BY SIZE INTO SSA1                                     
096870     MOVE '  GE' TO GODK-STATUSKODER                                      
096880     CALL CBLTDLI USING GN WDE1A-PCB DLI-IO-WDE1A1 SSA1                   
096890     MOVE WDE1A-STATUS-CODE TO STATUS-WS                                  
096891     PERFORM IMS-STATUSKONTROLL                                           
096892     .                                                                    
096893     SKIP3                                                                
096900 IMS-GN-WDE101-ASEQ SECTION.                                              
097000                                                                          
097100     STRING 'WDE101  (WDE1ASEQ>=' W-WDE1ASEQ-MIN                          
097200                    '&WDE1ASEQ<=' W-WDE1ASEQ-MAX ')'                      
097300          DELIMITED BY SIZE INTO SSA1                                     
097400     MOVE '  GE' TO GODK-STATUSKODER                                      
097500     CALL CBLTDLI USING GN WDE1P-PCB DLI-IO-WDE101 SSA1                   
097600     MOVE WDE1P-STATUS-CODE TO STATUS-WS                                  
097700     PERFORM IMS-STATUSKONTROLL                                           
097800     .                                                                    
097900     SKIP3                                                                
099200 IMS-STATUSKONTROLL SECTION.                                              
099300                                                                          
099400     SET STATUS-IX TO 1                                                   
099500     SEARCH GODK-STATUS                                                   
099600       AT END                                                             
099700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
099800         DELIMITED BY SIZE INTO FELTEXT                                   
099900         CALL FELLOG                                                      
100000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
100100         CONTINUE                                                         
100200     END-SEARCH                                                           
100300     .                                                                    
