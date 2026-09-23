000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W005WDK7.                                                
000400 AUTHOR.         LASSI OLGRENER.                                          
000500 DATE-WRITTEN.   12/07/13.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        SUBPROGRAM SOM SKÖTER UPPLÄGG AV NYA WDK7-SEGMENT                
001000*                                                                         
001100*                                                                         
001200 ENVIRONMENT DIVISION.                                                    
001300                                                                          
001400 DATA DIVISION.                                                           
001500                                                                          
001600 WORKING-STORAGE SECTION.                                                 
001700 77  IDPGM                       PIC X(8)   VALUE 'W005WDK7'.             
001800 77  JA                          PIC X      VALUE 'J'.                    
001900 77  NEJ                         PIC X      VALUE 'N'.                    
002000 77  FELTEXT                     PIC X(32)  VALUE SPACE.                  
002100 77  IX                          PIC S9(3)  VALUE ZERO COMP SYNC.         
002200 77  RKOD-ABEND-MED-DUMP         PIC S9(4)  VALUE +1000 COMP SYNC.        
002300 77  DAGENS-DATUM                PIC 9(6)   VALUE ZERO.                   
002400                                                                          
002500 01  DC-SW                       PIC XX.                                  
002600   88  CHINA-NDC                            VALUE '71' THRU '74'.         
002700   88  CHINA-NDC-71                         VALUE '71'.                   
002800   88  USA-NDC                              VALUE '41' THRU '49'.         
002810   88  JAP-NDC                              VALUE '61' '6A'.              
002900                                                                          
003000 01  FILLER                      PIC  X(16) VALUE 'WWLANDX2'.             
003100*01  -COPY WWLANDX2.                                                      
003200                                                                          
003300 01  FILLER                      PIC  X(16) VALUE 'WWDCKONS'.             
003400*01  -COPY WWDCKONS.                                                      
003500                                                                          
003600 01  FILLER                      PIC  X(16) VALUE 'WWLNDKON'.             
003700*01  -COPY WWLNDKON.                                                      
003800                                                                          
003900 01  FILLER                      PIC  X(16) VALUE 'WWPRODSL'.             
004000*01  -COPY WWPRODSL.                                                      
004100                                                                          
004200 01  FILLER                      PIC  X(16) VALUE 'WWDC99'.               
004300*01  -COPY WWDC99.                                                        
004400                                                                          
004500 01  DYNAMISKA-SUBPROGRAM.                                                
004600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
004700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
004800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
004900*                                                                         
005000* ARBETS-AREOR TILL IMS-SEKTIONERNA                                       
005100*                                                                         
005200 01  IMS-WS.                                                              
005300   03  FILLER                    PIC X(16)   VALUE 'IMS-WS     '.         
005400                                                                          
005500   03  STATUS-WS                 PIC XX.                                  
005600     88  SEGMENT-FINNS                       VALUE '  '.                  
005700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
005800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
005900                                                                          
006000   03  GODK-STATUSKODER.                                                  
006100     05  GODK-STATUS OCCURS 3 INDEXED BY STATUS-IX PIC XX.                
006200                                                                          
006300 01  NYCKLAR-TILL-DLI.                                                    
006400   03  W-IDARTNR-X.                                                       
006500     05  W-IDARTNR               PIC S9(9)   VALUE +0 COMP-3.             
006600                                                                          
006700   03  W-IDDC-X.                                                          
006800     05  W-IDDC                  PIC X(2)    VALUE SPACE.                 
006900                                                                          
007000   03  W-IDLEVNR-DC-X.                                                    
007100     05  W-IDLEVNR-DC            PIC X(5)    VALUE SPACE.                 
007200                                                                          
007300   03  W-IDDC-B6-X.                                                       
007400     05  W-IDDC-B6               PIC X(2)    VALUE SPACE.                 
007500                                                                          
007600   03  W-IDLANDX2-X.                                                      
007700     05  W-IDLANDX2              PIC X(2)    VALUE SPACE.                 
007800                                                                          
007810 01 WS.                                                                   
007820                                                                          
007893  02     WS-TIAAAAMMDDTTMMSSTH     PIC 9(16)   VALUE ZERO.                
007894  02     FILLER                  REDEFINES WS-TIAAAAMMDDTTMMSSTH.         
007895   03    WS-TISEKEL               PIC 9(2).                               
007896   03    WS-TIAAMMDDTTMMSSTH-DATE PIC 9(6).                               
007897   03    WS-TIAAMMDDTTMMSSTH-TIME PIC 9(8).                               
007898                                                                          
007900 01  SSA1                        PIC X(64).                               
008000 01  SSA2                        PIC X(64).                               
008100 01  SSA3                        PIC X(64).                               
008200                                                                          
008300 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDB601'.        
008400 01  DLI-IO-B601.                                                         
008500*  03   -COPY WDB601                                                      
008600                                                                          
008700 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDK601'.        
008800 01  DLI-IO-K601.                                                         
008900*  03   -COPY WDK601                                                      
009000                                                                          
009100 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDK611'.        
009200 01  DLI-IO-K611.                                                         
009300*  03   -COPY WDK611                                                      
009400                                                                          
009500 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDK701'.        
009600 01  DLI-IO-K701.                                                         
009700*  03   -COPY WDK701                                                      
009800                                                                          
009900 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDK711'.        
010000 01  DLI-IO-K711.                                                         
010100*  03   -COPY WDK711                                                      
010200                                                                          
010300 01  FILLER                      PIC X(16) VALUE 'DLI-IO-K711-SP'.        
010400 01  DLI-IO-K711-SP.                                                      
010500*  03   -COPY WDK711    -PRE SP-                                          
010600                                                                          
010700 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDK712'.        
010800 01  DLI-IO-K712.                                                         
010900*  03   -COPY WDK712                                                      
011000                                                                          
011100 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDK721'.        
011200 01  DLI-IO-K721.                                                         
011300*  03   -COPY WDK721                                                      
011400                                                                          
011500 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDK722'.        
011600 01  DLI-IO-K722.                                                         
011700*  03   -COPY WDK722                                                      
011800                                                                          
011900 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDK723'.        
012000 01  DLI-IO-K723.                                                         
012100*  03   -COPY WDK723                                                      
012200                                                                          
012300 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDK724'.        
012400 01  DLI-IO-K724.                                                         
012500*  03   -COPY WDK724                                                      
012600                                                                          
012700 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDK725'.        
012800 01  DLI-IO-K725.                                                         
012900*  03   -COPY WDK725                                                      
013000                                                                          
013100 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDK726'.        
013200 01  DLI-IO-K726.                                                         
013300*  03   -COPY WDK726                                                      
013400                                                                          
013500 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDK727'.        
013600 01  DLI-IO-K727.                                                         
013700*  03   -COPY WDK727                                                      
013800                                                                          
013810 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDK728'.        
013820 01  DLI-IO-K728.                                                         
013830*  03   -COPY WDK728                                                      
013840                                                                          
013900 01  FILLER                      PIC X(16)  VALUE 'IMS-FUNCTIONS'.        
014000*01    -COPY W0003                                                        
014100                                                                          
014200 LINKAGE SECTION.                                                         
014300                                                                          
014400*01  -COPY W005WDK7                                                       
014500                                                                          
014600*01  -COPY W0008 -PRE WDB6-                                               
014700     05  FILLER                  PIC X.                                   
014800*01  -COPY W0008 -PRE WDK6-                                               
014900     05  FILLER                  PIC X.                                   
015000*01  -COPY W0008 -PRE WDK7-                                               
015100     05  FILLER                  PIC X.                                   
015200                                                                          
015300 PROCEDURE DIVISION USING WDK7-W005WDK7 WDB6-PCB WDK6-PCB                 
015400                                        WDK7-PCB.                         
015500 MAIN SECTION.                                                            
015600                                                                          
015700     ACCEPT DAGENS-DATUM  FROM DATE                                       
015800                                                                          
015900     IF WDK7-IDARTNR-KFB NUMERIC  AND                                     
016000        WDK7-IDARTNR-KFB > ZERO                                           
016100       MOVE WDK7-IDARTNR-KFB TO W-IDARTNR                                 
016200       PERFORM IMS-GU-WDK601                                              
016300       PERFORM IMS-GNP-WDK611                                             
016400       MOVE ART-KDPRODSL   TO TEST-KDPRODSL                               
016500     ELSE                                                                 
016600       MOVE 'IDARTNR-KFB WRONG ARGUMENT' TO FELTEXT                       
016700       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
016800     END-IF                                                               
016900                                                                          
017000     EVALUATE WDK7-IDSEGM                                                 
017100     WHEN 'WDK711'                                                        
017200       PERFORM CHECK-ARGUMENT-WDK711                                      
017300       PERFORM B-CHECK-K701-K712                                          
017400                                                                          
017500       MOVE WDK7-WDK711  TO DLI-IO-K711                                   
017600       PERFORM C-SET-DEFAULT-WDK711                                       
017700       PERFORM IMS-ISRT-WDK711                                            
017800       MOVE DLI-IO-K711 TO WDK7-WDK711                                    
017900                                                                          
018000       IF (SLAG-IDDC = WC-NDC-US-RU OR                                    
018100                       WC-NDC-US-LA OR WC-NDC-US-SE OR WC-NDC-CA          
018200                       OR WC-NDC-US-CH OR WC-NDC-US-JA                    
018210                       OR WC-NDC-US-DA )                                  
018300* WHEN NDC IN NA CREATED THEN ALL OTHER NDC-NA MUST EXIST                 
018400         PERFORM CX-CREATE-K711-NA                                        
018500       END-IF                                                             
018600                                                                          
018700       IF (SLAG-IDDC = WC-NDC-CN-71 OR                                    
018800                       WC-NDC-CN-72 OR                                    
018900                       WC-NDC-CN-73 OR WC-NDC-CN-74)                      
019000* WHEN NDC IN CN(CHINA) CREATED THEN ALL OTHER NDC-CN MUST EXIST          
019100         PERFORM CZ-CREATE-K711-CN                                        
019200       END-IF                                                             
019201                                                                          
019210       IF (SLAG-IDDC = WC-NDC-JP-6A OR                                    
019220                       WC-NDC-JP-61)                                      
019240* WHEN NDC IN JP(JAPAN) CREATED THEN ALL OTHER NDC-JP MUST EXIST          
019250         PERFORM CY-CREATE-K711-JP                                        
019260       END-IF                                                             
019300                                                                          
019400     WHEN 'WDK712'                                                        
019500       PERFORM CHECK-ARGUMENT-WDK712                                      
019600       MOVE WDK7-WDK712  TO DLI-IO-K712                                   
019700       PERFORM D-SET-DEFAULT-WDK712                                       
019800       PERFORM IMS-ISRT-WDK712                                            
019900       MOVE DLI-IO-K712 TO WDK7-WDK712                                    
020000     WHEN 'WDK721'                                                        
020100       PERFORM CHECK-ARGUMENT-WDK721                                      
020200       MOVE WDK7-WDK721  TO DLI-IO-K721                                   
020300       PERFORM E-SET-DEFAULT-WDK721                                       
020400       PERFORM IMS-ISRT-WDK721                                            
020500       MOVE DLI-IO-K721 TO WDK7-WDK721                                    
020600     WHEN 'WDK722'                                                        
020700       PERFORM CHECK-ARGUMENT-WDK722                                      
020800       MOVE WDK7-WDK722  TO DLI-IO-K722                                   
020900       PERFORM F-SET-DEFAULT-WDK722                                       
021000       PERFORM IMS-ISRT-WDK722                                            
021100       MOVE DLI-IO-K722 TO WDK7-WDK722                                    
021200     WHEN 'WDK723'                                                        
021300       PERFORM CHECK-ARGUMENT-WDK723                                      
021400       MOVE WDK7-WDK723  TO DLI-IO-K723                                   
021500       PERFORM G-SET-DEFAULT-WDK723                                       
021600       PERFORM IMS-ISRT-WDK723                                            
021700       MOVE DLI-IO-K723 TO WDK7-WDK723                                    
021800     WHEN 'WDK724'                                                        
021900       PERFORM CHECK-ARGUMENT-WDK724                                      
022000       MOVE WDK7-WDK724  TO DLI-IO-K724                                   
022100       PERFORM H-SET-DEFAULT-WDK724                                       
022200       PERFORM IMS-ISRT-WDK724                                            
022300       MOVE DLI-IO-K724 TO WDK7-WDK724                                    
022400     WHEN 'WDK725'                                                        
022500       PERFORM CHECK-ARGUMENT-WDK725                                      
022600       MOVE WDK7-WDK725  TO DLI-IO-K725                                   
022700       PERFORM I-SET-DEFAULT-WDK725                                       
022800       PERFORM IMS-ISRT-WDK725                                            
022900       MOVE DLI-IO-K725 TO WDK7-WDK725                                    
023000     WHEN 'WDK726'                                                        
023100       PERFORM CHECK-ARGUMENT-WDK726                                      
023200       MOVE WDK7-WDK726  TO DLI-IO-K726                                   
023300       PERFORM J-SET-DEFAULT-WDK726                                       
023400       PERFORM IMS-ISRT-WDK726                                            
023500       MOVE DLI-IO-K726 TO WDK7-WDK726                                    
023600     WHEN 'WDK727'                                                        
023700       PERFORM CHECK-ARGUMENT-WDK727                                      
023800       MOVE WDK7-WDK727  TO DLI-IO-K727                                   
023900       PERFORM K-SET-DEFAULT-WDK727                                       
024000       PERFORM IMS-ISRT-WDK727                                            
024100       MOVE DLI-IO-K727 TO WDK7-WDK727                                    
024110     WHEN 'WDK728'                                                        
024120       PERFORM CHECK-ARGUMENT-WDK728                                      
024130       MOVE WDK7-WDK728  TO DLI-IO-K728                                   
024140       PERFORM L-SET-DEFAULT-WDK728                                       
024150       PERFORM IMS-ISRT-WDK728                                            
024151       PERFORM UNTIL SEGMENT-FINNS                                        
024152         ADD +1 TO TRCK-DAINLEV                                           
024154         PERFORM IMS-ISRT-WDK728                                          
024155       END-PERFORM                                                        
024160       MOVE DLI-IO-K728 TO WDK7-WDK728                                    
024200     WHEN OTHER                                                           
024300       MOVE 'IDSEGM WRONG ARGUMENT' TO FELTEXT                            
024400       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
024500     END-EVALUATE                                                         
024600                                                                          
024700     MOVE ZERO TO RETURN-CODE                                             
024800     GOBACK                                                               
024900     .                                                                    
025000                                                                          
025100 B-CHECK-K701-K712 SECTION.                                               
025200                                                                          
025300     MOVE WDK7-IDARTNR-KFB TO DLI-IO-K701                                 
025400     PERFORM IMS-ISRT-WDK701                                              
025500* ISRT-K701 GODKÄNNER 'II' DÅ ROOTEN KAN FINNAS SEN TIDIGARE              
025600* FOR SURE VALDE VI ATT ALLTID GÖRA K701-ISRT IHOP MED K711               
025700                                                                          
025800     IF DCS-CHINA                                                         
025900       MOVE DCS-IDLANDX2   TO LART-IDLANDX2                               
026000       PERFORM BA-SET-DEFAULT-K712                                        
026100       PERFORM IMS-ISRT-WDK712-II                                         
026200     END-IF                                                               
026300     IF DCS-USA OR DCS-CANADA                                             
026400* OM USA UPPDATERAS SKALL ÄVEN CANADA FÅ LANDSEGMENT.                     
026500* BÅDE USA OCH CANADA HAR MATERIALPRIS.                                   
026600                                                                          
026700       MOVE WC-LAND-US     TO LART-IDLANDX2                               
026800       PERFORM BA-SET-DEFAULT-K712                                        
026900       PERFORM IMS-ISRT-WDK712-II                                         
027000                                                                          
027100       MOVE WC-LAND-CA     TO LART-IDLANDX2                               
027200       PERFORM IMS-ISRT-WDK712-II                                         
027300     END-IF                                                               
027400                                                                          
027500     .                                                                    
027600 BA-SET-DEFAULT-K712 SECTION.                                             
027700                                                                          
027800     MOVE ZERO         TO LART-BEFT                                       
027900     MOVE ZERO         TO LART-DAPUBL                                     
028000     MOVE ZERO         TO LART-IDARTNR-EMBQ0                              
028100     MOVE ZERO         TO LART-IDARTNR-EMBQ1                              
028200     MOVE ZERO         TO LART-IDARTNR-EMBQ2                              
028300     MOVE SPACE        TO LART-IDUSER-EMB                                 
028400     MOVE SPACE        TO LART-KDARTURS                                   
028500     MOVE ZERO         TO LART-KVDAGAR-INLEV                              
028600     MOVE ZERO         TO LART-PRARTSJK                                   
028700     COMPUTE LART-PRMATRL = 2 * CLAG-PRARTSTD                             
028800     MOVE ZERO         TO LART-VKART                                      
028900     MOVE ZERO         TO LART-VLARTNTO                                   
029000     MOVE ZERO         TO LART-TIERSDAT-VIPS                              
029100     MOVE ZERO         TO LART-TIUPPDAT-EMB                               
029200     MOVE '2'          TO LART-KDMATRPR                                   
029300     MOVE ZERO         TO LART-KVQPACK-3                                  
029400     MOVE ZERO         TO LART-IDPSN-DC                                   
029500     MOVE 'N'          TO LART-FLMSKUPD                                   
029600     MOVE 'N'          TO LART-FLREFERAL                                  
029700                                                                          
029800     .                                                                    
029900                                                                          
030000 C-SET-DEFAULT-WDK711 SECTION.                                            
030100                                                                          
030200     MOVE WDK7-IDDC      TO SLAG-IDDC                                     
030300                                                                          
030400     IF WDK7-ADART       = ALL '+'                                        
030500       MOVE ZERO         TO SLAG-ADLAGOMR                                 
030600                            SLAG-ADGANG                                   
030700                            SLAG-ADPLATS                                  
030800     END-IF                                                               
030900                                                                          
031000     IF WDK7-ADLAGOMR-CD NOT NUMERIC                                      
031100       MOVE ZERO         TO SLAG-ADLAGOMR-CD                              
031200     END-IF                                                               
031300                                                                          
031400     IF WDK7-DASPSEA     = ALL '+'                                        
031500       MOVE ZERO         TO SLAG-DASPSEA                                  
031600     END-IF                                                               
031700                                                                          
031800     IF WDK7-DAREFESC = ALL '+'                                           
031900       MOVE ZERO         TO SLAG-DAREFESC                                 
032000     END-IF                                                               
032100                                                                          
032200     IF WDK7-DAREFESC-REOI = ALL '+'                                      
032300       MOVE ZERO         TO SLAG-DAREFESC-REOI                            
032400     END-IF                                                               
032500                                                                          
032600     IF WDK7-FLCDCBEH = ALL '+'                                           
032700       MOVE JA           TO SLAG-FLCDCBEH                                 
032800     END-IF                                                               
032900                                                                          
033000     IF WDK7-FLCDREL = ALL '+'                                            
033100       MOVE NEJ          TO SLAG-FLCDREL                                  
033200     END-IF                                                               
033300                                                                          
033400     IF WDK7-FLFLYG      = ALL '+'                                        
033500       MOVE NEJ          TO SLAG-FLFLYG                                   
033600     END-IF                                                               
033700                                                                          
033800     IF WDK7-FLORDSP     = ALL '+'                                        
033900       MOVE NEJ          TO SLAG-FLORDSP                                  
034000     END-IF                                                               
034100                                                                          
034200     IF WDK7-FLORDSP-EJRO = ALL '+'                                       
034300**BECAUSE LYNK & CO SHOULD NOT BE ABLE TO BE REFILLED TO NDC              
034400       IF NDC AND KDPRODSL-LYNK                                           
034500         MOVE JA          TO SLAG-FLORDSP-EJRO                            
034600       ELSE                                                               
034700         MOVE NEJ         TO SLAG-FLORDSP-EJRO                            
034800       END-IF                                                             
034900     END-IF                                                               
035000                                                                          
035100     IF WDK7-FLPB-FLYTT = ALL '+'                                         
035200       MOVE NEJ          TO SLAG-FLPB-FLYTT                               
035300     END-IF                                                               
035400                                                                          
035500     IF WDK7-FLREFBEO     = ALL '+'                                       
035600       MOVE NEJ           TO SLAG-FLREFBEO                                
035700     END-IF                                                               
035800                                                                          
035900     IF WDK7-FLREFLARM    = ALL '+'                                       
036000       MOVE NEJ           TO SLAG-FLREFLARM                               
036100     END-IF                                                               
036200                                                                          
036300     IF WDK7-FLSKROT-AUTO = ALL '+'                                       
036400       MOVE NEJ          TO SLAG-FLSKROT-AUTO                             
036500     END-IF                                                               
036600                                                                          
036700     IF WDK7-FLSKROT-BEORD = ALL '+'                                      
036800       MOVE NEJ            TO SLAG-FLSKROT-BEORD                          
036900     END-IF                                                               
037000                                                                          
037100     IF WDK7-FLSPBULK     = ALL '+'                                       
037200       MOVE NEJ           TO SLAG-FLSPBULK                                
037300     END-IF                                                               
037400                                                                          
037500     IF WDK7-IDDC > '40' AND NOT = WC-SDC-NL-ET                           
037600       MOVE NEJ           TO SLAG-FLWILSON                                
037700     ELSE                                                                 
037800       MOVE JA            TO SLAG-FLWILSON                                
037900     END-IF                                                               
038000                                                                          
038100     IF KDPRODSL-LOCAL                                                    
038200       IF DCS-SDC                                                         
038300* HÄR HAMNAR ÄVEN LDC I KINA                                              
038400         MOVE '1441 '     TO SLAG-IDLEVNR                                 
038500         MOVE WC-CDC-SE   TO SLAG-IDDC-REF                                
038600       ELSE                                                               
038700         MOVE '9998'      TO SLAG-IDLEVNR                                 
038800         MOVE SPACE       TO SLAG-IDDC-REF                                
038900       END-IF                                                             
039000     ELSE                                                                 
039100       IF WDK7-IDLEVNR = ALL '+'                                          
039200         MOVE DCS-IDDC-REF  TO SLAG-IDDC-REF                              
039300                               W-IDDC-B6                                  
039400* LÄS DC-REF SEGM FÖR ATT HÄMTA DESS LEVERANTÖR                           
039500         PERFORM IMS-GU-WDB601                                            
039600         MOVE DCS-IDLEVNR-DC TO SLAG-IDLEVNR                              
039700       ELSE                                                               
039800         IF WDK7-IDDC-REF = ALL '+'                                       
039900           MOVE WDK7-IDLEVNR TO W-IDLEVNR-DC                              
040000           PERFORM IMS-GU-WDB601-LEV                                      
040100           IF SEGMENT-FINNS                                               
040200             MOVE DCS-IDDC  TO SLAG-IDDC-REF                              
040300           ELSE                                                           
040400             MOVE SPACE     TO SLAG-IDDC-REF                              
040500           END-IF                                                         
040600         END-IF                                                           
040700       END-IF                                                             
040800     END-IF                                                               
040900                                                                          
041000     IF WDK7-IDPERSON-BUY NOT NUMERIC                                     
041100       MOVE ZERO          TO SLAG-IDPERSON-BUY                            
041200     END-IF                                                               
041300                                                                          
041400     IF WDK7-IDREFTAB     = ALL '+'                                       
041500       IF (CHINA-NDC AND SLAG-IDDC-REF = SPACE)                           
041600       OR (USA-NDC AND SLAG-IDDC-REF = SPACE)                             
041700         MOVE 'A'           TO SLAG-IDREFTAB                              
041800       ELSE                                                               
041900         MOVE ZERO          TO SLAG-IDREFTAB                              
042000       END-IF                                                             
042100     END-IF                                                               
042200                                                                          
042300     PERFORM CA-INIT-LEVSP-USERSP                                         
042400                                                                          
042500     IF WDK7-KDREFSTA     = ALL '+'                                       
042600       MOVE 'P'           TO SLAG-KDREFSTA                                
042700     END-IF                                                               
042800                                                                          
042900     IF WDK7-KVAKS-PAV NOT NUMERIC                                        
043000       MOVE ZERO         TO SLAG-KVAKS-PAV                                
043100     END-IF                                                               
043200                                                                          
043300     IF WDK7-KVAKS-SDC NOT NUMERIC                                        
043400       MOVE ZERO         TO SLAG-KVAKS-SDC                                
043500     END-IF                                                               
043600                                                                          
043700     IF WDK7-KVBEART NOT NUMERIC                                          
043800       MOVE ZERO         TO SLAG-KVBEART                                  
043900     END-IF                                                               
044000                                                                          
044100     IF WDK7-KVDAGAR-CDBEH NOT NUMERIC                                    
044200       MOVE ZERO         TO SLAG-KVDAGAR-CDBEH                            
044300     END-IF                                                               
044400                                                                          
044500     IF WDK7-KVDAGAR-MANLT NOT NUMERIC                                    
044600       MOVE ZERO         TO SLAG-KVDAGAR-MANLT                            
044700     END-IF                                                               
044800                                                                          
044900     IF WDK7-KVEFRS NOT NUMERIC                                           
045000       MOVE ZERO         TO SLAG-KVEFRS                                   
045100     END-IF                                                               
045200                                                                          
045300     IF WDK7-KVINVS NOT NUMERIC                                           
045400       MOVE ZERO         TO SLAG-KVINVS                                   
045500     END-IF                                                               
045600                                                                          
045700     IF WDK7-KVLS NOT NUMERIC                                             
045800       MOVE ZERO         TO SLAG-KVLS                                     
045900     END-IF                                                               
046000                                                                          
046100     IF WDK7-KVOKS-BULK NOT NUMERIC                                       
046200       MOVE ZERO         TO SLAG-KVOKS-BULK                               
046300     END-IF                                                               
046400                                                                          
046500     IF WDK7-KVOKS-DAG NOT NUMERIC                                        
046600       MOVE ZERO         TO SLAG-KVOKS-DAG                                
046700     END-IF                                                               
046800                                                                          
046900     IF WDK7-KVPB-HIST NOT NUMERIC                                        
047000       MOVE ZERO         TO SLAG-KVPB-HIST                                
047100     END-IF                                                               
047200                                                                          
047300     IF WDK7-KVPB-REF NOT NUMERIC                                         
047400       MOVE ZERO         TO SLAG-KVPB-REF                                 
047500     END-IF                                                               
047600                                                                          
047700     IF WDK7-KVPBREOI NOT NUMERIC                                         
047800       MOVE ZERO         TO SLAG-KVPBREOI                                 
047900     END-IF                                                               
048000                                                                          
048100     IF WDK7-KVPBREOI-HIST NOT NUMERIC                                    
048200       MOVE ZERO         TO SLAG-KVPBREOI-HIST                            
048300     END-IF                                                               
048400                                                                          
048500     IF WDK7-KVREFBER NOT NUMERIC                                         
048600       MOVE ZERO         TO SLAG-KVREFBER                                 
048700     END-IF                                                               
048800                                                                          
048900     IF WDK7-KVREFOVL NOT NUMERIC                                         
049000       MOVE ZERO         TO SLAG-KVREFOVL                                 
049100     END-IF                                                               
049200                                                                          
049300     IF WDK7-KVREFPKT NOT NUMERIC                                         
049400       MOVE ZERO         TO SLAG-KVREFPKT                                 
049500     END-IF                                                               
049600                                                                          
049700     IF WDK7-KVRESS NOT NUMERIC                                           
049800       MOVE ZERO         TO SLAG-KVRESS                                   
049900     END-IF                                                               
050000                                                                          
050100     IF WDK7-KVRETUR-BEORD NOT NUMERIC                                    
050200       MOVE ZERO         TO SLAG-KVRETUR-BEORD                            
050300     END-IF                                                               
050400                                                                          
050500     IF WDK7-KVROS-BULK NOT NUMERIC                                       
050600       MOVE ZERO         TO SLAG-KVROS-BULK                               
050700     END-IF                                                               
050800                                                                          
050900     IF WDK7-KVROS-DAG NOT NUMERIC                                        
051000       MOVE ZERO         TO SLAG-KVROS-DAG                                
051100     END-IF                                                               
051200                                                                          
051300     IF WDK7-KVSKROT NOT NUMERIC                                          
051400       MOVE ZERO         TO SLAG-KVSKROT                                  
051500     END-IF                                                               
051600                                                                          
051700     IF WDK7-KVSPARR-KVAL NOT NUMERIC                                     
051800       MOVE ZERO         TO SLAG-KVSPARR-KVAL                             
051900     END-IF                                                               
052000                                                                          
052100     IF WDK7-KVUTRS NOT NUMERIC                                           
052200       MOVE ZERO         TO SLAG-KVUTRS                                   
052300     END-IF                                                               
052400                                                                          
052500     IF WDK7-PRAVCOST NOT NUMERIC                                         
052600       MOVE ZERO         TO SLAG-PRAVCOST                                 
052700     END-IF                                                               
052800                                                                          
052900     MOVE 1 TO IX                                                         
053000     PERFORM UNTIL IX > 12                                                
053100       IF WDK7-RESEASON(IX) NOT NUMERIC                                   
053200         MOVE 1.00       TO SLAG-RESEASON(IX)                             
053300       END-IF                                                             
053400       ADD 1 TO IX                                                        
053500     END-PERFORM                                                          
053600                                                                          
053700     IF WDK7-RETREND NOT NUMERIC                                          
053800       MOVE 1.0          TO  SLAG-RETREND                                 
053900     END-IF                                                               
054000                                                                          
054100     IF WDK7-RETREND-REOI NOT NUMERIC                                     
054200       MOVE 1.0          TO  SLAG-RETREND-REOI                            
054300     END-IF                                                               
054400                                                                          
054500     IF WDK7-TEKVAL = ALL '+'                                             
054600       MOVE SPACE        TO SLAG-TEKVAL                                   
054700     END-IF                                                               
054800                                                                          
054900     IF WDK7-TIAVCOST NOT NUMERIC                                         
055000       MOVE ZERO         TO SLAG-TIAVCOST                                 
055100     END-IF                                                               
055200                                                                          
055300     IF WDK7-TIINVDAT NOT NUMERIC                                         
055400       MOVE ZERO         TO SLAG-TIINVDAT                                 
055500     END-IF                                                               
055600                                                                          
055700     IF WDK7-TIMANSEA NOT NUMERIC                                         
055800       MOVE ZERO         TO SLAG-TIMANSEA                                 
055900     END-IF                                                               
056000                                                                          
056100     IF WDK7-TIORDREG NOT NUMERIC                                         
056200       MOVE ZERO         TO SLAG-TIORDREG                                 
056300     END-IF                                                               
056400                                                                          
056500     IF WDK7-TIREFMPB NOT NUMERIC                                         
056600       MOVE ZERO         TO SLAG-TIREFMPB                                 
056700     END-IF                                                               
056800                                                                          
056900     IF WDK7-TIREFPAF NOT NUMERIC                                         
057000       MOVE ZERO         TO SLAG-TIREFPAF                                 
057100     END-IF                                                               
057200                                                                          
057300     IF WDK7-TIREFPKT NOT NUMERIC                                         
057400       MOVE ZERO         TO SLAG-TIREFPKT                                 
057500     END-IF                                                               
057600                                                                          
057700     IF WDK7-TIREFSTA NOT NUMERIC                                         
057800       MOVE DAGENS-DATUM TO  SLAG-TIREFSTA                                
057900     END-IF                                                               
058000                                                                          
058100     IF WDK7-TIREFSTO NOT NUMERIC                                         
058200       MOVE ZERO         TO SLAG-TIREFSTO                                 
058300     END-IF                                                               
058400                                                                          
058500     IF WDK7-TIRETUR-BEORD NOT NUMERIC                                    
058600       MOVE ZERO         TO SLAG-TIRETUR-BEORD                            
058700     END-IF                                                               
058800                                                                          
058900     IF WDK7-TISKROT NOT NUMERIC                                          
059000       MOVE ZERO         TO SLAG-TISKROT                                  
059100     END-IF                                                               
059200                                                                          
059300     IF WDK7-TISKROT-AUTO NOT NUMERIC                                     
059400       MOVE ZERO         TO SLAG-TISKROT-AUTO                             
059500     END-IF                                                               
059600                                                                          
059700     IF WDK7-TISKROT-BEORD NOT NUMERIC                                    
059800       MOVE ZERO         TO SLAG-TISKROT-BEORD                            
059900     END-IF                                                               
060000                                                                          
060100     IF WDK7-TISPARR-KVAL NOT NUMERIC                                     
060200       MOVE ZERO         TO SLAG-TISPARR-KVAL                             
060300     END-IF                                                               
060400                                                                          
060500     IF WDK7-TIPBREOI NOT NUMERIC                                         
060600       MOVE ZERO         TO SLAG-TIPBREOI                                 
060700     END-IF                                                               
060800                                                                          
060900     IF WDK7-TIDATUM-CROSS NOT NUMERIC                                    
061000       MOVE ZERO         TO SLAG-TIDATUM-CROSS                            
061100     END-IF                                                               
061200                                                                          
061300     IF WDK7-TISTODAT-LARM NOT NUMERIC                                    
061400       MOVE ZERO         TO SLAG-TISTODAT-LARM                            
061500     END-IF                                                               
061600                                                                          
061700     IF WDK7-FLREFILL = '+'                                               
061800**BECAUSE LYNK & CO SHOULD NOT BE ABLE TO BE REFILLED TO NDC              
061900       IF (NDC AND KDPRODSL-LYNK)                                         
061910       OR NDC-US-BAT                                                      
061920       OR SDC-NL-ET                                                       
062000         MOVE NEJ          TO SLAG-FLREFILL                               
062100       ELSE                                                               
062200         MOVE JA           TO SLAG-FLREFILL                               
062300       END-IF                                                             
062400     ELSE                                                                 
062500       MOVE WDK7-FLREFILL  TO SLAG-FLREFILL                               
062600     END-IF                                                               
062700                                                                          
062800     IF WDK7-FLREFNYO = '+'                                               
062900       MOVE NEJ            TO SLAG-FLREFNYO                               
063000     ELSE                                                                 
063100       MOVE WDK7-FLREFNYO  TO SLAG-FLREFNYO                               
063200     END-IF                                                               
063300                                                                          
063400     IF WDK7-FLBUYUPD = '+'                                               
063500       MOVE NEJ            TO SLAG-FLBUYUPD                               
063600     ELSE                                                                 
063700       MOVE WDK7-FLBUYUPD  TO SLAG-FLBUYUPD                               
063800     END-IF                                                               
063900                                                                          
064000     IF WDK7-FLTABUPD = '+'                                               
064100       MOVE NEJ            TO SLAG-FLTABUPD                               
064200     ELSE                                                                 
064300       MOVE WDK7-FLTABUPD  TO SLAG-FLTABUPD                               
064400     END-IF                                                               
064500                                                                          
064510     IF WDK7-REPPFAKT NOT NUMERIC                                         
064520       MOVE ZERO         TO SLAG-REPPFAKT                                 
064530     END-IF                                                               
064540                                                                          
064600     MOVE SPACE          TO SLAG-FILLER                                   
064700     .                                                                    
064800                                                                          
064900 CA-INIT-LEVSP-USERSP SECTION.                                            
065000                                                                          
065100     MOVE ZERO                   TO SLAG-KDLEVSP                          
065200     MOVE SPACE                  TO SLAG-IDUSER-SPKVAL                    
065300                                                                          
065400     IF CLAG-KDLEVSP > ZERO                                               
065500       IF CLAG-KDLEVSP = 20                                               
065600         MOVE CLAG-KDLEVSP       TO SLAG-KDLEVSP                          
065700         MOVE CLAG-IDUSER-SPKVAL TO SLAG-IDUSER-SPKVAL                    
065800       END-IF                                                             
065900       IF CLAG-KDLEVSP = 21                                               
066000         PERFORM IMS-GU-WDK701                                            
066100         PERFORM IMS-GNP-WDK711                                           
066200         PERFORM UNTIL SEGMENT-SAKNAS                                     
066300           IF SP-SLAG-KDLEVSP = 21 OR 22                                  
066400             MOVE CLAG-KDLEVSP       TO SLAG-KDLEVSP                      
066500             MOVE CLAG-IDUSER-SPKVAL TO SLAG-IDUSER-SPKVAL                
066600             SET SEGMENT-SAKNAS TO TRUE                                   
066700           ELSE                                                           
066800             PERFORM IMS-GNP-WDK711                                       
066900           END-IF                                                         
067000         END-PERFORM                                                      
067100       END-IF                                                             
067200     END-IF                                                               
067300     .                                                                    
067400 CX-CREATE-K711-NA SECTION.                                               
067500                                                                          
067600     PERFORM CXA-SET-DEFAULT-WDK711                                       
067700                                                                          
067800     IF KDPRODSL-LOCAL                                                    
067900       MOVE WC-NDC-US-RU TO SLAG-IDDC                                     
068000       PERFORM IMS-ISRT-WDK711                                            
068100                                                                          
068200       MOVE WC-NDC-US-LA TO SLAG-IDDC                                     
068300       PERFORM IMS-ISRT-WDK711                                            
068400                                                                          
068500       MOVE WC-NDC-US-SE TO SLAG-IDDC                                     
068600       PERFORM IMS-ISRT-WDK711                                            
068700                                                                          
068800       MOVE WC-NDC-US-CH TO SLAG-IDDC                                     
068900       PERFORM IMS-ISRT-WDK711                                            
069000                                                                          
069100       MOVE WC-NDC-US-JA TO SLAG-IDDC                                     
069200       PERFORM IMS-ISRT-WDK711                                            
069300                                                                          
069310       MOVE WC-NDC-US-DA TO SLAG-IDDC                                     
069320       PERFORM IMS-ISRT-WDK711                                            
069330                                                                          
069400       MOVE WC-NDC-CA  TO SLAG-IDDC                                       
069500       MOVE ZERO       TO SLAG-IDREFTAB                                   
069600       PERFORM IMS-ISRT-WDK711                                            
069700     ELSE                                                                 
069800*** VID LOKAL ANSK ART SÄTTS DC-ANSK SOM DC-REF PÅ ALLA ANDRA             
069900       IF SLAG-IDDC-REF = SPACE                                           
070000         PERFORM CXC-CHECK-SUPPLIER-LOC                                   
070100       ELSE                                                               
070200         MOVE WC-NDC-US-RU TO SLAG-IDDC                                   
070300                              W-IDDC-B6                                   
070400         PERFORM CXB-CHECK-SUPPLIER                                       
070500         PERFORM IMS-ISRT-WDK711                                          
070600                                                                          
070700         MOVE WC-NDC-US-LA TO SLAG-IDDC                                   
070800                              W-IDDC-B6                                   
070900         PERFORM CXB-CHECK-SUPPLIER                                       
071000         PERFORM IMS-ISRT-WDK711                                          
071100                                                                          
071200         MOVE WC-NDC-US-SE TO SLAG-IDDC                                   
071300                              W-IDDC-B6                                   
071400         PERFORM CXB-CHECK-SUPPLIER                                       
071500         PERFORM IMS-ISRT-WDK711                                          
071600                                                                          
071700         MOVE WC-NDC-US-CH TO SLAG-IDDC                                   
071800                              W-IDDC-B6                                   
071900         PERFORM CXB-CHECK-SUPPLIER                                       
072000         PERFORM IMS-ISRT-WDK711                                          
072100                                                                          
072200         MOVE WC-NDC-US-JA TO SLAG-IDDC                                   
072300                              W-IDDC-B6                                   
072400         PERFORM CXB-CHECK-SUPPLIER                                       
072500         PERFORM IMS-ISRT-WDK711                                          
072600                                                                          
072610         MOVE WC-NDC-US-DA TO SLAG-IDDC                                   
072620                              W-IDDC-B6                                   
072630         PERFORM CXB-CHECK-SUPPLIER                                       
072640         PERFORM IMS-ISRT-WDK711                                          
072650                                                                          
072700         MOVE WC-NDC-CA  TO SLAG-IDDC                                     
072800                              W-IDDC-B6                                   
072900         PERFORM CXB-CHECK-SUPPLIER                                       
073000         PERFORM IMS-ISRT-WDK711                                          
073100       END-IF                                                             
073200     END-IF                                                               
073300     .                                                                    
073400                                                                          
073410 CY-CREATE-K711-JP SECTION.                                               
073420                                                                          
073430     PERFORM CXA-SET-DEFAULT-WDK711                                       
073440                                                                          
073510     MOVE WC-NDC-JP-6A TO SLAG-IDDC                                       
073511                          W-IDDC-B6                                       
073512     PERFORM CXB-CHECK-SUPPLIER                                           
073513     PERFORM IMS-ISRT-WDK711                                              
073514                                                                          
073515     MOVE WC-NDC-JP-61 TO SLAG-IDDC                                       
073516                          W-IDDC-B6                                       
073517     PERFORM CXB-CHECK-SUPPLIER                                           
073518     PERFORM IMS-ISRT-WDK711                                              
073519                                                                          
073541     .                                                                    
073542                                                                          
073550 CZ-CREATE-K711-CN SECTION.                                               
073600                                                                          
073700     PERFORM CXA-SET-DEFAULT-WDK711                                       
073800                                                                          
073900     IF KDPRODSL-LOCAL                                                    
074000       MOVE WC-NDC-CN-71 TO SLAG-IDDC                                     
074100       PERFORM IMS-ISRT-WDK711                                            
074200                                                                          
074300       MOVE WC-NDC-CN-72 TO SLAG-IDDC                                     
074400       PERFORM IMS-ISRT-WDK711                                            
074500                                                                          
074600       MOVE WC-NDC-CN-73 TO SLAG-IDDC                                     
074700       PERFORM IMS-ISRT-WDK711                                            
074800                                                                          
074900       MOVE WC-NDC-CN-74 TO SLAG-IDDC                                     
075000       PERFORM IMS-ISRT-WDK711                                            
075100     ELSE                                                                 
075200*** VID LOKAL ANSK ART SÄTTS DC-ANSK SOM DC-REF PÅ ALLA ANDRA             
075300       IF SLAG-IDDC-REF = SPACE                                           
075400         PERFORM CZC-CHECK-SUPPLIER-LOC                                   
075500       ELSE                                                               
075600         MOVE WC-NDC-CN-71 TO SLAG-IDDC                                   
075700                              W-IDDC-B6                                   
075800         PERFORM CXB-CHECK-SUPPLIER                                       
075900         PERFORM IMS-ISRT-WDK711                                          
076000                                                                          
076100         MOVE WC-NDC-CN-72 TO SLAG-IDDC                                   
076200                              W-IDDC-B6                                   
076300         PERFORM CXB-CHECK-SUPPLIER                                       
076400         PERFORM IMS-ISRT-WDK711                                          
076500                                                                          
076600         MOVE WC-NDC-CN-73 TO SLAG-IDDC                                   
076700                              W-IDDC-B6                                   
076800         PERFORM CXB-CHECK-SUPPLIER                                       
076900         PERFORM IMS-ISRT-WDK711                                          
077000                                                                          
077100         MOVE WC-NDC-CN-74 TO SLAG-IDDC                                   
077200                              W-IDDC-B6                                   
077300         PERFORM CXB-CHECK-SUPPLIER                                       
077400         PERFORM IMS-ISRT-WDK711                                          
077500       END-IF                                                             
077600     END-IF                                                               
077700     .                                                                    
077800                                                                          
077900 CXB-CHECK-SUPPLIER SECTION.                                              
078000                                                                          
078100**FETCH STANDARD REFILLING DC SEEN ON 4403 SCREEN                         
078200     PERFORM IMS-GU-WDB601                                                
078300     MOVE DCS-IDDC-REF     TO SLAG-IDDC-REF                               
078400*                             W-IDDC-B6                                   
078500                                                                          
078600     IF SLAG-IDDC-REF = WC-CDC-SE                                         
078700       MOVE '1441 '        TO SLAG-IDLEVNR                                
078800     END-IF                                                               
078900                                                                          
079000     IF SLAG-IDDC-REF = WC-NDC-US-RU                                      
079100       MOVE '63517'        TO SLAG-IDLEVNR                                
079200     END-IF                                                               
079300                                                                          
079400     IF SLAG-IDDC-REF = WC-NDC-US-LA                                      
079500       MOVE '63519'        TO SLAG-IDLEVNR                                
079600     END-IF                                                               
079700                                                                          
079800     IF SLAG-IDDC-REF = WC-NDC-US-SE                                      
079900       MOVE 'AEHRD'        TO SLAG-IDLEVNR                                
080000     END-IF                                                               
080100                                                                          
080200     IF SLAG-IDDC-REF = WC-NDC-US-CH                                      
080300       MOVE 'AEKS1'        TO SLAG-IDLEVNR                                
080400     END-IF                                                               
080500                                                                          
080600     IF SLAG-IDDC-REF = WC-NDC-US-JA                                      
080700       MOVE 'AELGT'        TO SLAG-IDLEVNR                                
080800     END-IF                                                               
080900                                                                          
080910     IF SLAG-IDDC-REF = WC-NDC-US-DA                                      
080920       MOVE 'AE9X5'        TO SLAG-IDLEVNR                                
080930     END-IF                                                               
080940                                                                          
081000     IF SLAG-IDDC-REF = WC-NDC-CA                                         
081100       MOVE 'AEBAL'        TO SLAG-IDLEVNR                                
081200     END-IF                                                               
081210                                                                          
081220*BR* IF SLAG-IDDC-REF = WC-NDC-BR                                         
081230*BR*   MOVE 'XXXXX'        TO SLAG-IDLEVNR                                
081240*BR* END-IF                                                               
081250                                                                          
081260     IF SLAG-IDDC-REF = WC-NDC-MX                                         
081270       MOVE 'AD7CZ'        TO SLAG-IDLEVNR                                
081280     END-IF                                                               
081300                                                                          
081400     IF SLAG-IDDC-REF = WC-NDC-CN-71                                      
081500       MOVE 'CHN07'        TO SLAG-IDLEVNR                                
081600     END-IF                                                               
081700                                                                          
081710     IF SLAG-IDDC-REF = WC-NDC-JP-61                                      
081720       MOVE '15230'        TO SLAG-IDLEVNR                                
081730     END-IF                                                               
081740                                                                          
081800     IF SLAG-IDDC-REF = WC-NDC-CN-72                                      
081900       MOVE 'CHN04'        TO SLAG-IDLEVNR                                
082000     END-IF                                                               
082100                                                                          
082200     IF SLAG-IDDC-REF = WC-NDC-CN-73                                      
082300       MOVE 'AEFZT'        TO SLAG-IDLEVNR                                
082400     END-IF                                                               
082500                                                                          
082600     IF SLAG-IDDC-REF = WC-NDC-CN-74                                      
082700       MOVE 'AEQD2'        TO SLAG-IDLEVNR                                
082800     END-IF                                                               
082900**FETCH STANDARD REFILLING DC'S SUPPLIER NUMBER SEEN ON 4402              
083000*    PERFORM IMS-GU-WDB601                                                
083100*    MOVE DCS-IDLEVNR-DC   TO SLAG-IDLEVNR                                
083200                                                                          
083300     .                                                                    
083400 CXA-SET-DEFAULT-WDK711 SECTION.                                          
083500                                                                          
083600* FOLLOWING FIELDS:                                                       
083700*     SLAG-KDLEVSP/ -IDUSER-SPKVAL/                                       
083800* ARE NOT INITIATED HERE BECAUSE PREVIOUS VALUES ARE VALID                
083900* FOR ALL OTHER NA-NDC:S                                                  
084000* BUT! SÅ LÄNGE REFILL I NA SKER FRÅN CDC BEHÖVS INGEN                    
084100* OBS! OMLÄSNING AV B6 FÖR ATT HÄMTA IDLEVNR-DC/IDDC-REF                  
084200* OBS! MEN OM DET BLIR REFILL FRÅN ANNAT HÅLL MÅSTE B6 LÄSAS OM!          
084300* OBS! BARA USA HAR LOKAL ANSKAFFNING.CANADA HAR ANSK VIA REFILL          
084400                                                                          
084500     MOVE ZERO         TO SLAG-ADLAGOMR                                   
084600                          SLAG-ADGANG                                     
084700                          SLAG-ADPLATS                                    
084800     MOVE ZERO         TO SLAG-ADLAGOMR-CD                                
084900     MOVE ZERO         TO SLAG-DASPSEA                                    
085000     MOVE ZERO         TO SLAG-DAREFESC                                   
085100     MOVE ZERO         TO SLAG-DAREFESC-REOI                              
085200     MOVE JA           TO SLAG-FLCDCBEH                                   
085300     MOVE NEJ          TO SLAG-FLCDREL                                    
085400     MOVE NEJ          TO SLAG-FLFLYG                                     
085500     MOVE NEJ          TO SLAG-FLORDSP                                    
085600     MOVE NEJ          TO SLAG-FLPB-FLYTT                                 
085700     MOVE NEJ          TO SLAG-FLREFBEO                                   
085800     MOVE NEJ          TO SLAG-FLREFLARM                                  
085900     MOVE NEJ          TO SLAG-FLSKROT-AUTO                               
086000     MOVE NEJ          TO SLAG-FLSKROT-BEORD                              
086100     MOVE NEJ          TO SLAG-FLSPBULK                                   
086200     MOVE NEJ          TO SLAG-FLWILSON                                   
086300     MOVE ZERO         TO SLAG-IDPERSON-BUY                               
086400                                                                          
086500     IF KDPRODSL-LOCAL                                                    
086600       MOVE 'A'        TO SLAG-IDREFTAB                                   
086700     ELSE                                                                 
086800       MOVE ZERO       TO SLAG-IDREFTAB                                   
086900     END-IF                                                               
087000                                                                          
087100     MOVE 'P'          TO SLAG-KDREFSTA                                   
087200     MOVE ZERO         TO SLAG-KVAKS-PAV                                  
087300     MOVE ZERO         TO SLAG-KVAKS-SDC                                  
087400     MOVE ZERO         TO SLAG-KVBEART                                    
087500     MOVE ZERO         TO SLAG-KVDAGAR-CDBEH                              
087600     MOVE ZERO         TO SLAG-KVDAGAR-MANLT                              
087700     MOVE ZERO         TO SLAG-KVEFRS                                     
087800     MOVE ZERO         TO SLAG-KVINVS                                     
087900     MOVE ZERO         TO SLAG-KVLS                                       
088000     MOVE ZERO         TO SLAG-KVOKS-BULK                                 
088100     MOVE ZERO         TO SLAG-KVOKS-DAG                                  
088200     MOVE ZERO         TO SLAG-KVPB-HIST                                  
088300     MOVE ZERO         TO SLAG-KVPB-REF                                   
088400     MOVE ZERO         TO SLAG-KVPBREOI                                   
088500     MOVE ZERO         TO SLAG-KVPBREOI-HIST                              
088600     MOVE ZERO         TO SLAG-KVREFBER                                   
088700     MOVE ZERO         TO SLAG-KVREFOVL                                   
088800     MOVE ZERO         TO SLAG-KVREFPKT                                   
088900     MOVE ZERO         TO SLAG-KVRESS                                     
089000     MOVE ZERO         TO SLAG-KVRETUR-BEORD                              
089100     MOVE ZERO         TO SLAG-KVROS-BULK                                 
089200     MOVE ZERO         TO SLAG-KVROS-DAG                                  
089300     MOVE ZERO         TO SLAG-KVSKROT                                    
089400     MOVE ZERO         TO SLAG-KVSPARR-KVAL                               
089500     MOVE ZERO         TO SLAG-KVUTRS                                     
089600     MOVE ZERO         TO SLAG-PRAVCOST                                   
089700     MOVE 1 TO IX                                                         
089800     PERFORM UNTIL IX > 12                                                
089900       MOVE 1.00       TO SLAG-RESEASON(IX)                               
090000       ADD 1 TO IX                                                        
090100     END-PERFORM                                                          
090200     MOVE 1.0          TO SLAG-RETREND                                    
090300     MOVE 1.0          TO SLAG-RETREND-REOI                               
090400     MOVE SPACE        TO SLAG-TEKVAL                                     
090500     MOVE ZERO         TO SLAG-TIAVCOST                                   
090600     MOVE ZERO         TO SLAG-TIINVDAT                                   
090700     MOVE ZERO         TO SLAG-TIMANSEA                                   
090800     MOVE ZERO         TO SLAG-TIORDREG                                   
090900     MOVE ZERO         TO SLAG-TIREFMPB                                   
091000     MOVE ZERO         TO SLAG-TIREFPAF                                   
091100     MOVE ZERO         TO SLAG-TIREFPKT                                   
091200     MOVE DAGENS-DATUM TO SLAG-TIREFSTA                                   
091300     MOVE ZERO         TO SLAG-TIREFSTO                                   
091400     MOVE ZERO         TO SLAG-TIRETUR-BEORD                              
091500     MOVE ZERO         TO SLAG-TISKROT                                    
091600     MOVE ZERO         TO SLAG-TISKROT-AUTO                               
091700     MOVE ZERO         TO SLAG-TISKROT-BEORD                              
091800     MOVE ZERO         TO SLAG-TISPARR-KVAL                               
091900     MOVE ZERO         TO SLAG-TIPBREOI                                   
092000     MOVE ZERO         TO SLAG-TISTODAT-LARM                              
092010     MOVE ZERO         TO SLAG-REPPFAKT                                   
092100     MOVE NEJ          TO SLAG-FLREFNYO                                   
092200     MOVE NEJ          TO SLAG-FLBUYUPD                                   
092300     MOVE NEJ          TO SLAG-FLTABUPD                                   
092400     MOVE SPACE        TO SLAG-FILLER                                     
092500**BECAUSE LYNK & CO SHOULD NOT BE ABLE TO BE REFILLED TO NDC              
092600     IF KDPRODSL-LYNK                                                     
092700       MOVE NEJ        TO SLAG-FLREFILL                                   
092800       MOVE JA         TO SLAG-FLORDSP-EJRO                               
092900     ELSE                                                                 
093000       MOVE JA         TO SLAG-FLREFILL                                   
093100       MOVE NEJ        TO SLAG-FLORDSP-EJRO                               
093200     END-IF                                                               
093300     .                                                                    
093400 CXC-CHECK-SUPPLIER-LOC SECTION.                                          
093500                                                                          
093600**FETCH REFILLING DC FROM IDDC WITH LOCAL SOURCING.                       
093700     MOVE SLAG-IDDC        TO SLAG-IDDC-REF                               
093800                              W-IDDC-B6                                   
093900     PERFORM IMS-GU-WDB601                                                
094000     MOVE DCS-IDLEVNR-DC   TO SLAG-IDLEVNR                                
094100                                                                          
094200     MOVE WC-NDC-US-RU TO SLAG-IDDC                                       
094300     PERFORM IMS-ISRT-WDK711                                              
094400                                                                          
094500     MOVE WC-NDC-US-LA TO SLAG-IDDC                                       
094600     PERFORM IMS-ISRT-WDK711                                              
094700                                                                          
094800     MOVE WC-NDC-US-SE TO SLAG-IDDC                                       
094900     PERFORM IMS-ISRT-WDK711                                              
095000                                                                          
095100     MOVE WC-NDC-US-CH TO SLAG-IDDC                                       
095200     PERFORM IMS-ISRT-WDK711                                              
095300                                                                          
095400     MOVE WC-NDC-US-JA TO SLAG-IDDC                                       
095500     PERFORM IMS-ISRT-WDK711                                              
095600                                                                          
095610     MOVE WC-NDC-US-DA TO SLAG-IDDC                                       
095620     PERFORM IMS-ISRT-WDK711                                              
095630                                                                          
095700     MOVE WC-NDC-CA  TO SLAG-IDDC                                         
095800     PERFORM IMS-ISRT-WDK711                                              
095900     .                                                                    
096000 CZC-CHECK-SUPPLIER-LOC SECTION.                                          
096100                                                                          
096200**FETCH REFILLING DC FROM IDDC WITH LOCAL SOURCING.                       
096300     MOVE SLAG-IDDC        TO SLAG-IDDC-REF                               
096400                              W-IDDC-B6                                   
096500     PERFORM IMS-GU-WDB601                                                
096600     MOVE DCS-IDLEVNR-DC   TO SLAG-IDLEVNR                                
096700                                                                          
096800     MOVE WC-NDC-CN-71 TO SLAG-IDDC                                       
096900     PERFORM IMS-ISRT-WDK711                                              
097000                                                                          
097100     MOVE WC-NDC-CN-72 TO SLAG-IDDC                                       
097200     PERFORM IMS-ISRT-WDK711                                              
097300                                                                          
097400     MOVE WC-NDC-CN-73 TO SLAG-IDDC                                       
097500     PERFORM IMS-ISRT-WDK711                                              
097600                                                                          
097700     MOVE WC-NDC-CN-74 TO SLAG-IDDC                                       
097800     PERFORM IMS-ISRT-WDK711                                              
097900     .                                                                    
098000 D-SET-DEFAULT-WDK712 SECTION.                                            
098100                                                                          
098200     IF WDK7-BEFT NOT NUMERIC                                             
098300       MOVE ZERO         TO LART-BEFT                                     
098400     END-IF                                                               
098500                                                                          
098600     IF WDK7-DAPUBL = ALL '+'                                             
098700       MOVE ZERO         TO LART-DAPUBL                                   
098800     END-IF                                                               
098900                                                                          
099000     IF WDK7-IDARTNR-EMBQ0 NOT NUMERIC                                    
099100       MOVE ZERO         TO LART-IDARTNR-EMBQ0                            
099200     END-IF                                                               
099300                                                                          
099400     IF WDK7-IDARTNR-EMBQ1 NOT NUMERIC                                    
099500       MOVE ZERO         TO LART-IDARTNR-EMBQ1                            
099600     END-IF                                                               
099700                                                                          
099800     IF WDK7-IDARTNR-EMBQ2 NOT NUMERIC                                    
099900       MOVE ZERO         TO LART-IDARTNR-EMBQ2                            
100000     END-IF                                                               
100100                                                                          
100200     IF WDK7-IDUSER-EMB = ALL '+'                                         
100300       MOVE SPACE        TO LART-IDUSER-EMB                               
100400     END-IF                                                               
100500                                                                          
100600     IF WDK7-KDARTURS = ALL '+'                                           
100700       MOVE SPACE        TO LART-KDARTURS                                 
100800     END-IF                                                               
100900                                                                          
101000     IF WDK7-KVDAGAR-INLEV NOT NUMERIC                                    
101100       MOVE ZERO         TO LART-KVDAGAR-INLEV                            
101200     END-IF                                                               
101300                                                                          
101400     IF WDK7-PRARTSJK NOT NUMERIC                                         
101500       MOVE ZERO         TO LART-PRARTSJK                                 
101600     END-IF                                                               
101700                                                                          
101800     IF WDK7-PRMATRL NOT NUMERIC                                          
101900        MOVE ZERO        TO LART-PRMATRL                                  
102000     END-IF                                                               
102100                                                                          
102200     IF WDK7-VKART NOT NUMERIC                                            
102300       MOVE ZERO         TO LART-VKART                                    
102400     END-IF                                                               
102500                                                                          
102600     IF WDK7-VLARTNTO NOT NUMERIC                                         
102700       MOVE ZERO         TO LART-VLARTNTO                                 
102800     END-IF                                                               
102900                                                                          
103000     IF WDK7-TIERSDAT-VIPS NOT NUMERIC                                    
103100       MOVE ZERO         TO LART-TIERSDAT-VIPS                            
103200     END-IF                                                               
103300                                                                          
103400     IF WDK7-TIUPPDAT-EMB  NOT NUMERIC                                    
103500       MOVE ZERO         TO LART-TIUPPDAT-EMB                             
103600     END-IF                                                               
103700                                                                          
103800     IF WDK7-KDMATRPR = ALL '+'                                           
103900        MOVE 2           TO LART-KDMATRPR                                 
104000     END-IF                                                               
104100                                                                          
104200     IF WDK7-KVQPACK-3 NOT NUMERIC                                        
104300       MOVE ZERO          TO LART-KVQPACK-3                               
104400     END-IF                                                               
104500                                                                          
104600     IF WDK7-IDPSN-DC  NOT NUMERIC                                        
104700       MOVE ZERO          TO LART-IDPSN-DC                                
104800     END-IF                                                               
104900                                                                          
105000     IF WDK7-FLMSKUPD = ALL '+'                                           
105100       MOVE 'N'           TO LART-FLMSKUPD                                
105200     END-IF                                                               
105300                                                                          
105400     IF WDK7-FLREFERAL = ALL '+'                                          
105500       MOVE 'N'           TO LART-FLREFERAL                               
105600     END-IF                                                               
105700     .                                                                    
105800                                                                          
105900 E-SET-DEFAULT-WDK721 SECTION.                                            
106000                                                                          
106100     MOVE '1'            TO SBLK-KDSEGKEY                                 
106200                                                                          
106300     IF WDK7-DAORDSP = ALL '+'                                            
106400        MOVE ZERO        TO SBLK-DAORDSP                                  
106500     END-IF                                                               
106600                                                                          
106700     IF WDK7-DASPBULK = ALL '+'                                           
106800        MOVE ZERO        TO SBLK-DASPBULK                                 
106900     END-IF                                                               
107000                                                                          
107100     IF WDK7-DAORDSP-EJRO = ALL '+'                                       
107200        MOVE ZERO        TO SBLK-DAORDSP-EJRO                             
107300     END-IF                                                               
107400                                                                          
107500     IF WDK7-IDUSER-ORDSP = ALL '+'                                       
107600        MOVE SPACE       TO SBLK-IDUSER-ORDSP                             
107700     END-IF                                                               
107800                                                                          
107900     IF WDK7-IDUSER-SPBULK = ALL '+'                                      
108000        MOVE SPACE       TO SBLK-IDUSER-SPBULK                            
108100     END-IF                                                               
108200                                                                          
108300     IF WDK7-IDUSER-ORDSP-EJRO = ALL '+'                                  
108400        MOVE SPACE       TO SBLK-IDUSER-ORDSP-EJRO                        
108500     END-IF                                                               
108600                                                                          
108700     IF WDK7-TEARTNOT-ORDER = ALL '+'                                     
108800        MOVE SPACE       TO SBLK-TEARTNOT-ORDER                           
108900     END-IF                                                               
109000                                                                          
109100     MOVE SPACE          TO SBLK-FILLER                                   
109200     .                                                                    
109300 F-SET-DEFAULT-WDK722 SECTION.                                            
109400                                                                          
109500     MOVE '1'            TO XLAG-KDSEGKEY                                 
109600                                                                          
109700     IF WDK7-DAPBPLAN = ALL '+'                                           
109800        MOVE ZERO        TO XLAG-DAPBPLAN                                 
109900     END-IF                                                               
110000                                                                          
110100     IF WDK7-DASEASON = ALL '+'                                           
110200        MOVE ZERO        TO XLAG-DASEASON                                 
110300     END-IF                                                               
110400                                                                          
110500     IF WDK7-FLJIT = ALL '+'                                              
110600        MOVE NEJ         TO XLAG-FLJIT                                    
110700     END-IF                                                               
110800                                                                          
110900     IF WDK7-IDANSK NOT NUMERIC                                           
111000        MOVE ZERO        TO XLAG-IDANSK                                   
111100     END-IF                                                               
111200                                                                          
111300     IF WDK7-IDINK = ALL '+'                                              
111400        MOVE SPACE       TO XLAG-IDINK                                    
111500     END-IF                                                               
111600                                                                          
111700     IF WDK7-IDLEVNR-FRAM = ALL '+'                                       
111800        MOVE SPACE       TO XLAG-IDLEVNR-FRAM                             
111900     END-IF                                                               
112000                                                                          
112100     IF WDK7-IDLEVNR-SHIP IN WDK7-WDK722 = ALL '+'                        
112200        MOVE SPACE       TO XLAG-IDLEVNR-SHIP                             
112300     END-IF                                                               
112400                                                                          
112500     IF WDK7-IDPLANGR-AG NOT NUMERIC                                      
112600        MOVE ZERO        TO XLAG-IDPLANGR-AG                              
112700     END-IF                                                               
112800                                                                          
112900     IF WDK7-KDAVT NOT NUMERIC                                            
113000        MOVE ZERO        TO XLAG-KDAVT                                    
113100     END-IF                                                               
113200                                                                          
113300     IF WDK7-KDLEVPLF = ALL '+'                                           
113400        MOVE 'P'         TO XLAG-KDLEVPLF                                 
113500     END-IF                                                               
113600                                                                          
113700     IF WDK7-KDLPSP NOT NUMERIC                                           
113800        MOVE ZERO        TO XLAG-KDLPSP                                   
113900     END-IF                                                               
114000                                                                          
114100     IF WDK7-KDOPPLAN = ALL '+'                                           
114200        MOVE 'N'         TO XLAG-KDOPPLAN                                 
114300     END-IF                                                               
114400                                                                          
114500     IF WDK7-KVDAGAR-FFH NOT NUMERIC                                      
114600        MOVE ZERO        TO XLAG-KVDAGAR-FFH                              
114700     END-IF                                                               
114800                                                                          
114900     IF WDK7-KVEOQ NOT NUMERIC                                            
115000        MOVE ZERO        TO XLAG-KVEOQ                                    
115100     END-IF                                                               
115200                                                                          
115300     IF WDK7-KVPB-JUST1 NOT NUMERIC                                       
115400        MOVE ZERO        TO XLAG-KVPB-JUST1                               
115500     END-IF                                                               
115600                                                                          
115700     IF WDK7-KVPB-JUST2 NOT NUMERIC                                       
115800        MOVE ZERO        TO XLAG-KVPB-JUST2                               
115900     END-IF                                                               
116000                                                                          
116100     IF WDK7-KVPB-PLAN NOT NUMERIC                                        
116200        MOVE ZERO        TO XLAG-KVPB-PLAN                                
116300     END-IF                                                               
116400                                                                          
116500     IF WDK7-KVPB-TREND NOT NUMERIC                                       
116600        MOVE ZERO        TO XLAG-KVPB-TREND                               
116700     END-IF                                                               
116800                                                                          
116900     IF WDK7-KVPALL NOT NUMERIC                                           
117000        MOVE ZERO        TO XLAG-KVPALL                                   
117100     END-IF                                                               
117200                                                                          
117300     IF WDK7-KVSLAGER NOT NUMERIC                                         
117400        MOVE ZERO        TO XLAG-KVSLAGER                                 
117500     END-IF                                                               
117600                                                                          
117700     IF WDK7-KVSLUTKP NOT NUMERIC                                         
117800        MOVE ZERO        TO XLAG-KVSLUTKP                                 
117900     END-IF                                                               
118000                                                                          
118100     IF WDK7-KVSPANT NOT NUMERIC                                          
118200        MOVE ZERO        TO XLAG-KVSPANT                                  
118300     ELSE                                                                 
118400        MOVE WDK7-KVSPANT TO XLAG-KVSPANT                                 
118500     END-IF                                                               
118600                                                                          
118700     IF WDK7-KVULOAD NOT NUMERIC                                          
118800        MOVE ZERO        TO XLAG-KVULOAD                                  
118900     END-IF                                                               
119000                                                                          
119100     IF WDK7-KVVECKOR-LT NOT NUMERIC                                      
119200        MOVE ZERO        TO XLAG-KVVECKOR-LT                              
119300     END-IF                                                               
119400                                                                          
119500     IF WDK7-KVVECKOR-FT NOT NUMERIC                                      
119600        MOVE ZERO        TO XLAG-KVVECKOR-FT                              
119700     END-IF                                                               
119800                                                                          
119900     IF WDK7-KVVECKOR-TREND NOT NUMERIC                                   
120000        MOVE ZERO        TO XLAG-KVVECKOR-TREND                           
120100     END-IF                                                               
120200                                                                          
120300     MOVE 1 TO IX                                                         
120400     PERFORM UNTIL IX > 12                                                
120500       IF WDK7-RESEASON-PLAN(IX) NOT NUMERIC                              
120600         MOVE 1.00       TO XLAG-RESEASON-PLAN(IX)                        
120700       END-IF                                                             
120800       ADD 1 TO IX                                                        
120900     END-PERFORM                                                          
121000                                                                          
121100     IF WDK7-TIDATUM-TREND NOT NUMERIC                                    
121200        MOVE ZERO        TO XLAG-TIDATUM-TREND                            
121300     END-IF                                                               
121400                                                                          
121500     IF WDK7-TILPSP NOT NUMERIC                                           
121600        MOVE ZERO        TO XLAG-TILPSP                                   
121700     END-IF                                                               
121800                                                                          
121900     MOVE 1 TO IX                                                         
122000     PERFORM UNTIL IX > 5                                                 
122100       IF WDK7-TILEVDAG(IX) NOT NUMERIC                                   
122200          MOVE ZERO      TO XLAG-TILEVDAG(IX)                             
122300       END-IF                                                             
122400       ADD 1 TO IX                                                        
122500     END-PERFORM                                                          
122600                                                                          
122700     IF WDK7-TILEVDAT NOT NUMERIC                                         
122800        MOVE ZERO        TO XLAG-TILEVDAT                                 
122900     END-IF                                                               
123000                                                                          
123100     IF WDK7-TIMANLED NOT NUMERIC                                         
123200        MOVE ZERO        TO XLAG-TIMANLED                                 
123300     END-IF                                                               
123400                                                                          
123500     IF WDK7-TIMANSEC NOT NUMERIC                                         
123600        MOVE ZERO        TO XLAG-TIMANSEC                                 
123700     END-IF                                                               
123800                                                                          
123900     IF WDK7-TIOMSPEC NOT NUMERIC                                         
124000        MOVE ZERO        TO XLAG-TIOMSPEC                                 
124100     END-IF                                                               
124200                                                                          
124300     IF WDK7-TIREFSTO-LOC NOT NUMERIC                                     
124400        MOVE ZERO        TO XLAG-TIREFSTO-LOC                             
124500     END-IF                                                               
124600                                                                          
124700     IF WDK7-TISLUTKP NOT NUMERIC                                         
124800        MOVE ZERO        TO XLAG-TISLUTKP                                 
124900     END-IF                                                               
125000                                                                          
125100     IF WDK7-TIPBJUST-1 NOT NUMERIC                                       
125200        MOVE ZERO        TO XLAG-TIPBJUST-1                               
125300     END-IF                                                               
125400                                                                          
125500     IF WDK7-TIPBJUST-2 NOT NUMERIC                                       
125600        MOVE ZERO        TO XLAG-TIPBJUST-2                               
125700     END-IF                                                               
125800                                                                          
125900     IF WDK7-IDLEVNR-SHIP-FRAM = ALL '+'                                  
126000        MOVE SPACE       TO XLAG-IDLEVNR-SHIP-FRAM                        
126100     END-IF                                                               
126110                                                                          
126120     IF WDK7-FLLARM-BUF = ALL '+'                                         
126130        MOVE NEJ         TO XLAG-FLLARM-BUF                               
126140     END-IF                                                               
126200                                                                          
126300     MOVE SPACE          TO XLAG-FILLER                                   
126400     .                                                                    
126500 G-SET-DEFAULT-WDK723 SECTION.                                            
126600                                                                          
126700     IF WDK7-IDLEVNR-SHIP IN WDK7-WDK723  = ALL '+'                       
126800        MOVE SPACE       TO SAVT-IDLEVNR-SHIP                             
126900     END-IF                                                               
127000                                                                          
127100     IF WDK7-TIAVTAL NOT NUMERIC                                          
127200        MOVE ZERO        TO SAVT-TIAVTAL                                  
127300     END-IF                                                               
127400     .                                                                    
127500 H-SET-DEFAULT-WDK724 SECTION.                                            
127600                                                                          
127700     IF WDK7-IDUSER   = ALL '+'                                           
127800        MOVE SPACE       TO SPRL-IDUSER                                   
127900     END-IF                                                               
128000                                                                          
128100     IF WDK7-KDFPKPRI = ALL '+'                                           
128200        MOVE SPACE       TO SPRL-KDFPKPRI                                 
128300     END-IF                                                               
128400                                                                          
128500     IF WDK7-KDPRURSP = ALL '+'                                           
128600        MOVE SPACE       TO SPRL-KDPRURSP                                 
128700     END-IF                                                               
128800                                                                          
128900     IF WDK7-KDVALISO = '+++' OR SPACE                                    
129000       MOVE 'WRONG ISO > WDK7-KDVALISO' TO FELTEXT                        
129100       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
129200     END-IF                                                               
129300                                                                          
129400     IF WDK7-PRARTBES-PR < ZERO OR WDK7-PRARTBEL-PR < ZERO                
129500       MOVE 'WRONG PRICE > WDK7-PRARTBEX-PR' TO FELTEXT                   
129600       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
129700     END-IF                                                               
129800                                                                          
129900     IF WDK7-SUINLEV-PR NOT NUMERIC                                       
130000        MOVE ZERO        TO SPRL-SUINLEV-PR                               
130100     END-IF                                                               
130200                                                                          
130300     IF WDK7-TIREGDAT NOT NUMERIC                                         
130400        MOVE ZERO        TO SPRL-TIREGDAT                                 
130500     END-IF                                                               
130600     .                                                                    
130700                                                                          
130800 I-SET-DEFAULT-WDK725 SECTION.                                            
130900                                                                          
131000     IF WDK7-IDLEVNR-BEST = ALL '+'                                       
131100        MOVE SPACE       TO NBES-IDLEVNR-BEST                             
131200     END-IF                                                               
131300                                                                          
131400     IF WDK7-KDBEH-BEST NOT NUMERIC                                       
131500        MOVE ZERO        TO NBES-KDBEH-BEST                               
131600     END-IF                                                               
131700                                                                          
131800     IF WDK7-TIBEST NOT NUMERIC                                           
131900        MOVE ZERO        TO NBES-TIBEST                                   
132000     END-IF                                                               
132100     .                                                                    
132200                                                                          
132300 J-SET-DEFAULT-WDK726 SECTION.                                            
132400                                                                          
132500     IF WDK7-IDARTNR-EMB NOT NUMERIC                                      
132600        MOVE ZERO        TO LEMB-IDARTNR-EMB                              
132700     END-IF                                                               
132800     .                                                                    
132900                                                                          
133000 K-SET-DEFAULT-WDK727 SECTION.                                            
133100                                                                          
133200     MOVE '1'            TO PROG-KDSEGKEY                                 
133300                                                                          
133400     IF WDK7-KVPB-JUST(1) NOT NUMERIC                                     
133500        MOVE ZERO        TO PROG-KVPB-JUST(1)                             
133600     END-IF                                                               
133700     IF WDK7-KVPB-JUST(2) NOT NUMERIC                                     
133800        MOVE ZERO        TO PROG-KVPB-JUST(2)                             
133900     END-IF                                                               
134000     IF WDK7-TIPBJUST(1)  NOT NUMERIC                                     
134100        MOVE ZERO        TO PROG-TIPBJUST(1)                              
134200     END-IF                                                               
134300     IF WDK7-TIPBJUST(2)  NOT NUMERIC                                     
134400        MOVE ZERO        TO PROG-TIPBJUST(2)                              
134500     END-IF                                                               
134600     .                                                                    
134710                                                                          
134720 L-SET-DEFAULT-WDK728 SECTION.                                            
134740     MOVE FUNCTION CURRENT-DATE (1:2) TO WS-TISEKEL                       
134750     ACCEPT WS-TIAAMMDDTTMMSSTH-DATE FROM DATE                            
134760     ACCEPT WS-TIAAMMDDTTMMSSTH-TIME FROM TIME                            
134770     COMPUTE TRCK-DAINLEV        = WS-TIAAAAMMDDTTMMSSTH                  
134799     .                                                                    
134800                                                                          
134810 CHECK-ARGUMENT-WDK711 SECTION.                                           
134900     MOVE WDK7-IDDC     TO W-IDDC-B6                                      
135000     PERFORM IMS-GU-WDB601                                                
135100     IF DCS-SDC OR DCS-NDC                                                
135200       MOVE WDK7-IDDC   TO DC-SW                                          
135300                           WS-IDDC                                        
135400     ELSE                                                                 
135500       MOVE 'IDDC WRONG ARGUMENT' TO FELTEXT                              
135600       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
135700     END-IF                                                               
135800     .                                                                    
135900                                                                          
136000 CHECK-ARGUMENT-WDK712 SECTION.                                           
136100     MOVE WDK7-IDLANDX2   TO LANDX2-IDLANDX2                              
136200     IF NOT LANDX2-NDC                                                    
136300       MOVE 'IDLANDX2 WRONG ARGUMENT' TO FELTEXT                          
136400       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
136500     END-IF                                                               
136600     .                                                                    
136700                                                                          
136800 CHECK-ARGUMENT-WDK721 SECTION.                                           
136900     IF WDK7-IDDC-KFB = '++' OR SPACE                                     
137000       MOVE 'IDDC-KFB WRONG ARGUMENT' TO FELTEXT                          
137100       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
137200     ELSE                                                                 
137300       MOVE WDK7-IDDC-KFB TO W-IDDC                                       
137400     END-IF                                                               
137500     .                                                                    
137600                                                                          
137700 CHECK-ARGUMENT-WDK722 SECTION.                                           
137800     IF WDK7-IDDC-KFB = '++' OR SPACE                                     
137900       MOVE 'IDDC-KFB WRONG ARGUMENT' TO FELTEXT                          
138000       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
138100     ELSE                                                                 
138200       MOVE WDK7-IDDC-KFB TO W-IDDC                                       
138300     END-IF                                                               
138400     .                                                                    
138500                                                                          
138600 CHECK-ARGUMENT-WDK723 SECTION.                                           
138700     IF WDK7-IDDC-KFB = '++' OR SPACE                                     
138800       MOVE 'IDDC-KFB WRONG ARGUMENT' TO FELTEXT                          
138900       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
139000     ELSE                                                                 
139100       MOVE WDK7-IDDC-KFB TO W-IDDC                                       
139200     END-IF                                                               
139300     IF WDK7-IDAVTAL < ZERO                                               
139400       MOVE 'IDAVTAL WRONG ARGUMENT' TO FELTEXT                           
139500       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
139600     END-IF                                                               
139700     IF WDK7-IDLEVNR-AVT = ALL '+' OR SPACE                               
139800       MOVE 'IDLEVNR-AVT WRONG ARGUMENT' TO FELTEXT                       
139900       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
140000     END-IF                                                               
140100     .                                                                    
140200                                                                          
140300 CHECK-ARGUMENT-WDK724 SECTION.                                           
140400     IF WDK7-IDDC-KFB = '++' OR SPACE                                     
140500       MOVE 'IDDC-KFB WRONG ARGUMENT' TO FELTEXT                          
140600       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
140700     ELSE                                                                 
140800       MOVE WDK7-IDDC-KFB TO W-IDDC                                       
140900     END-IF                                                               
141000     IF WDK7-DAPRLIST-9KOMPL < ZERO                                       
141100       MOVE 'DAPRLIST9 WRONG ARGUMENT' TO FELTEXT                         
141200       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
141300     END-IF                                                               
141400     IF WDK7-IDLEVNR-PR = ALL '+' OR SPACE                                
141500       MOVE 'IDLEVNR-PR WRONG ARGUMENT' TO FELTEXT                        
141600       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
141700     END-IF                                                               
141800     .                                                                    
141900                                                                          
142000 CHECK-ARGUMENT-WDK725 SECTION.                                           
142100     IF WDK7-IDDC-KFB = '++' OR SPACE                                     
142200       MOVE 'IDDC-KFB WRONG ARGUMENT' TO FELTEXT                          
142300       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
142400     ELSE                                                                 
142500       MOVE WDK7-IDDC-KFB TO W-IDDC                                       
142600     END-IF                                                               
142700     IF WDK7-IDBEST < ZERO                                                
142800       MOVE 'IDBEST WRONG ARGUMENT' TO FELTEXT                            
142900       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
143000     END-IF                                                               
143100     .                                                                    
143200                                                                          
143300 CHECK-ARGUMENT-WDK726 SECTION.                                           
143400     IF WDK7-IDLANDX2-KFB = '++' OR SPACE                                 
143500       MOVE 'IDLANDX2-KFB WRONG ARGUMENT' TO FELTEXT                      
143600       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
143700     ELSE                                                                 
143800       MOVE WDK7-IDLANDX2-KFB TO W-IDLANDX2                               
143900     END-IF                                                               
144000     IF WDK7-KDEMBKEY = '+++' OR SPACE                                    
144100       MOVE 'KDEMBKEY WRONG ARGUMENT' TO FELTEXT                          
144200       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
144300     END-IF                                                               
144400     .                                                                    
144500                                                                          
144600 CHECK-ARGUMENT-WDK727 SECTION.                                           
144700     IF WDK7-IDDC-KFB = '++' OR SPACE                                     
144800       MOVE 'IDDC-KFB WRONG ARGUMENT' TO FELTEXT                          
144900       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
145000     ELSE                                                                 
145100       MOVE WDK7-IDDC-KFB TO W-IDDC                                       
145200     END-IF                                                               
145300     .                                                                    
145400                                                                          
145410 CHECK-ARGUMENT-WDK728 SECTION.                                           
145420     IF WDK7-IDDC-KFB = '++' OR SPACE                                     
145430       MOVE 'IDDC-KFB WRONG ARGUMENT' TO FELTEXT                          
145440       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
145450     ELSE                                                                 
145460       MOVE WDK7-IDDC-KFB TO W-IDDC                                       
145470     END-IF                                                               
145471                                                                          
145472     IF WDK7-IDARTNR-KFB NUMERIC  AND                                     
145473        WDK7-IDARTNR-KFB > ZERO                                           
145474       MOVE WDK7-IDARTNR-KFB TO W-IDARTNR                                 
145478     ELSE                                                                 
145479       MOVE 'IDARTNR-KFB WRONG ARGUMENT' TO FELTEXT                       
145480       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
145481     END-IF                                                               
145482                                                                          
145494     IF WDK7-KVANTMOT NOT > ZERO                                          
145495       MOVE 'INVALID KVANTMOT' TO FELTEXT                                 
145496       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
145497     END-IF                                                               
145498                                                                          
145503     IF WDK7-KVTRACK-KVAR NOT > ZERO                                      
145504       MOVE 'INVALID KVTRACK-KVAR' TO FELTEXT                             
145505       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
145506     END-IF                                                               
145507                                                                          
145508     IF WDK7-IDTRACK = ALL '+' OR SPACE                                   
145509       MOVE 'INVALID TRACKING ID' TO FELTEXT                              
145510       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
145511     END-IF                                                               
145512     .                                                                    
145513                                                                          
145520 IMS-GU-WDK601 SECTION.                                                   
145600                                                                          
145700     IF WDK6-DBD-NAME = 'WDK6'                                            
145800       STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                       
145900          DELIMITED BY SIZE INTO SSA1                                     
146000     ELSE                                                                 
146100       STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                       
146200          DELIMITED BY SIZE INTO SSA1                                     
146300     END-IF                                                               
146400     MOVE '  '                TO GODK-STATUSKODER                         
146500     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-K601 SSA1                      
146600     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
146700     PERFORM IMS-STATUSKONTROLL                                           
146800     .                                                                    
146900 IMS-GNP-WDK611 SECTION.                                                  
147000                                                                          
147100     IF WDK6-DBD-NAME = 'WDK6'                                            
147200       MOVE 'WDK611   '       TO SSA1                                     
147300     ELSE                                                                 
147400       MOVE 'WLARTC11 '       TO SSA1                                     
147500     END-IF                                                               
147600     MOVE '  '                TO GODK-STATUSKODER                         
147700     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-K611 SSA1                     
147800     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
147900     PERFORM IMS-STATUSKONTROLL                                           
148000     .                                                                    
148100 IMS-GU-WDB601 SECTION.                                                   
148200                                                                          
148300     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
148400          DELIMITED BY SIZE INTO SSA1                                     
148500     MOVE '  '                TO GODK-STATUSKODER                         
148600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-B601 SSA1                      
148700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
148800     PERFORM IMS-STATUSKONTROLL                                           
148900     .                                                                    
149000 IMS-GU-WDB601-LEV SECTION.                                               
149100                                                                          
149200     STRING 'WDB601  (IDLEVNDC =' W-IDLEVNR-DC-X ')'                      
149300          DELIMITED BY SIZE INTO SSA1                                     
149400     MOVE '  GE'              TO GODK-STATUSKODER                         
149500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-B601 SSA1                      
149600     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
149700     PERFORM IMS-STATUSKONTROLL                                           
149800     .                                                                    
149900 IMS-GU-WDK701 SECTION.                                                   
150000                                                                          
150100     IF WDK7-DBD-NAME = 'WDK7'                                            
150200       STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                       
150300          DELIMITED BY SIZE INTO SSA1                                     
150400     ELSE                                                                 
150500       STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                       
150600          DELIMITED BY SIZE INTO SSA1                                     
150700     END-IF                                                               
150800     MOVE '  '                TO GODK-STATUSKODER                         
150900     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-K701 SSA1                      
151000     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
151100     PERFORM IMS-STATUSKONTROLL                                           
151200     .                                                                    
151300 IMS-GNP-WDK711 SECTION.                                                  
151400                                                                          
151500     IF WDK7-DBD-NAME = 'WDK7'                                            
151600       MOVE 'WDK711   '       TO SSA1                                     
151700     ELSE                                                                 
151800       MOVE 'WLARTS11 '       TO SSA1                                     
151900     END-IF                                                               
152000     MOVE '  GE'              TO GODK-STATUSKODER                         
152100     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-K711-SP SSA1                  
152200     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
152300     PERFORM IMS-STATUSKONTROLL                                           
152400     .                                                                    
152500 IMS-ISRT-WDK701 SECTION.                                                 
152600                                                                          
152700     IF WDK7-DBD-NAME = 'WDK7'                                            
152800       MOVE 'WDK701'          TO SSA1                                     
152900     ELSE                                                                 
153000       MOVE 'WLARTS01 '       TO SSA1                                     
153100     END-IF                                                               
153200     MOVE '  II'              TO GODK-STATUSKODER                         
153300     CALL CBLTDLI USING ISRT WDK7-PCB DLI-IO-K701 SSA1                    
153400     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
153500     PERFORM IMS-STATUSKONTROLL                                           
153600     .                                                                    
153700 IMS-ISRT-WDK711 SECTION.                                                 
153800                                                                          
153900     IF WDK7-DBD-NAME = 'WDK7'                                            
154000       STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                       
154100            DELIMITED BY SIZE INTO SSA1                                   
154200       MOVE   'WDK711'          TO SSA2                                   
154300     ELSE                                                                 
154400       STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                       
154500            DELIMITED BY SIZE INTO SSA1                                   
154600       MOVE   'WLARTS11'        TO SSA2                                   
154700     END-IF                                                               
154800     MOVE '  II'              TO GODK-STATUSKODER                         
154900     CALL CBLTDLI USING ISRT WDK7-PCB DLI-IO-K711 SSA1 SSA2               
155000     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
155100     PERFORM IMS-STATUSKONTROLL                                           
155200     .                                                                    
155300 IMS-ISRT-WDK712 SECTION.                                                 
155400                                                                          
155500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
155600          DELIMITED BY SIZE INTO SSA1                                     
155700     MOVE   'WDK712'          TO SSA2                                     
155800     MOVE '  '                TO GODK-STATUSKODER                         
155900     CALL CBLTDLI USING ISRT WDK7-PCB DLI-IO-K712 SSA1 SSA2               
156000     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
156100     PERFORM IMS-STATUSKONTROLL                                           
156200     .                                                                    
156300 IMS-ISRT-WDK712-II SECTION.                                              
156400                                                                          
156500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
156600          DELIMITED BY SIZE INTO SSA1                                     
156700     MOVE   'WDK712'          TO SSA2                                     
156800     MOVE '  II'              TO GODK-STATUSKODER                         
156900     CALL CBLTDLI USING ISRT WDK7-PCB DLI-IO-K712 SSA1 SSA2               
157000     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
157100     PERFORM IMS-STATUSKONTROLL                                           
157200     .                                                                    
157300 IMS-ISRT-WDK721 SECTION.                                                 
157400                                                                          
157500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
157600          DELIMITED BY SIZE INTO SSA1                                     
157700     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
157800          DELIMITED BY SIZE INTO SSA2                                     
157900     MOVE   'WDK721'          TO SSA3                                     
158000     MOVE '  '                TO GODK-STATUSKODER                         
158100     CALL CBLTDLI USING ISRT WDK7-PCB DLI-IO-K721 SSA1 SSA2 SSA3          
158200     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
158300     PERFORM IMS-STATUSKONTROLL                                           
158400     .                                                                    
158500 IMS-ISRT-WDK722 SECTION.                                                 
158600                                                                          
158700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
158800          DELIMITED BY SIZE INTO SSA1                                     
158900     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
159000          DELIMITED BY SIZE INTO SSA2                                     
159100     MOVE   'WDK722'          TO SSA3                                     
159200     MOVE '  '                TO GODK-STATUSKODER                         
159300     CALL CBLTDLI USING ISRT WDK7-PCB DLI-IO-K722 SSA1 SSA2 SSA3          
159400     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
159500     PERFORM IMS-STATUSKONTROLL                                           
159600     .                                                                    
159700 IMS-ISRT-WDK723 SECTION.                                                 
159800                                                                          
159900     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
160000          DELIMITED BY SIZE INTO SSA1                                     
160100     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
160200          DELIMITED BY SIZE INTO SSA2                                     
160300     MOVE   'WDK723'          TO SSA3                                     
160400     MOVE '  '                TO GODK-STATUSKODER                         
160500     CALL CBLTDLI USING ISRT WDK7-PCB DLI-IO-K723 SSA1 SSA2 SSA3          
160600     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
160700     PERFORM IMS-STATUSKONTROLL                                           
160800     .                                                                    
160900 IMS-ISRT-WDK724 SECTION.                                                 
161000                                                                          
161100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
161200          DELIMITED BY SIZE INTO SSA1                                     
161300     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
161400          DELIMITED BY SIZE INTO SSA2                                     
161500     MOVE   'WDK724'          TO SSA3                                     
161600     MOVE '  '                TO GODK-STATUSKODER                         
161700     CALL CBLTDLI USING ISRT WDK7-PCB DLI-IO-K724 SSA1 SSA2 SSA3          
161800     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
161900     PERFORM IMS-STATUSKONTROLL                                           
162000     .                                                                    
162100 IMS-ISRT-WDK725 SECTION.                                                 
162200                                                                          
162300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
162400          DELIMITED BY SIZE INTO SSA1                                     
162500     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
162600          DELIMITED BY SIZE INTO SSA2                                     
162700     MOVE   'WDK725'          TO SSA3                                     
162800     MOVE '  '                TO GODK-STATUSKODER                         
162900     CALL CBLTDLI USING ISRT WDK7-PCB DLI-IO-K725 SSA1 SSA2 SSA3          
163000     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
163100     PERFORM IMS-STATUSKONTROLL                                           
163200     .                                                                    
163300 IMS-ISRT-WDK726 SECTION.                                                 
163400                                                                          
163500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
163600          DELIMITED BY SIZE INTO SSA1                                     
163700     STRING 'WDK712  (IDLAND   =' W-IDLANDX2-X ')'                        
163800          DELIMITED BY SIZE INTO SSA2                                     
163900     MOVE   'WDK726'          TO SSA3                                     
164000     MOVE '  '                TO GODK-STATUSKODER                         
164100     CALL CBLTDLI USING ISRT WDK7-PCB DLI-IO-K726 SSA1 SSA2 SSA3          
164200     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
164300     PERFORM IMS-STATUSKONTROLL                                           
164400     .                                                                    
164500 IMS-ISRT-WDK727 SECTION.                                                 
164600                                                                          
164700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
164800          DELIMITED BY SIZE INTO SSA1                                     
164900     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
165000          DELIMITED BY SIZE INTO SSA2                                     
165100     MOVE   'WDK727'          TO SSA3                                     
165200     MOVE '  '                TO GODK-STATUSKODER                         
165300     CALL CBLTDLI USING ISRT WDK7-PCB DLI-IO-K727 SSA1 SSA2 SSA3          
165400     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
165500     PERFORM IMS-STATUSKONTROLL                                           
165600     .                                                                    
165610 IMS-ISRT-WDK728 SECTION.                                                 
165620                                                                          
165630     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
165640          DELIMITED BY SIZE INTO SSA1                                     
165650     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
165660          DELIMITED BY SIZE INTO SSA2                                     
165670     MOVE   'WDK728'          TO SSA3                                     
165680     MOVE '  '                TO GODK-STATUSKODER                         
165690     CALL CBLTDLI USING ISRT WDK7-PCB DLI-IO-K728 SSA1 SSA2 SSA3          
165691     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
165692     PERFORM IMS-STATUSKONTROLL                                           
165698     .                                                                    
165700 IMS-STATUSKONTROLL SECTION.                                              
165800                                                                          
165900     SET STATUS-IX TO 1                                                   
166000     SEARCH GODK-STATUS                                                   
166100       AT END CALL FELLOG                                                 
166200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
166300     END-SEARCH                                                           
166400     .                                                                    
