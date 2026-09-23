000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W403WHEV.                                                
000500 AUTHOR.         SRINADH NADIMPALLI.                                      
000600 DATE-WRITTEN.   MAR 2026.                                                
000700 DATE-COMPILED.                                                           
000800                                                                          
000900*    FUNCTION.                                                            
001000*                                                                         
001100*        W403WHEV' SENDS WH EVENT TRANSACTION TO SERVICE LAYER.           
001300*                                                                         
001400*    LÄNKAREA :       W403WHEV                                            
001500*                                                                         
001600*                                                                         
001700 DATA DIVISION.                                                           
001800                                                                          
001900 WORKING-STORAGE SECTION.                                                 
002000     SKIP3                                                                
002100 77    IDPGM                     PIC X(8)   VALUE 'W403WHEV'.             
002200 77    WZ04-SEND-IDCOM           PIC S9(9)   COMP VALUE +0.               
002201 77    FELTEXT                   PIC X(80)   VALUE SPACE.                 
002202 77    ERRORTEXT                 PIC X(64)  VALUE SPACE.                  
002300 77    CURRENT-SECTION           PIC X(30)  VALUE SPACE.                  
002400 77    YES                       PIC X      VALUE 'Y'.                    
002500 77    NEJ                       PIC X      VALUE 'N'.                    
002600 77    JSON-DOC                  PIC X(10000).                            
002601 77    JSON-DOC-COUNT            PIC 9(9).                                
002602 77    KDRC-DISPLAY              PIC Z(5).                                
002700 77    RKOD-ABEND                PIC S9(4)  VALUE +33   COMP SYNC.        
002800 01    W-CURRDATE-UTC            PIC X(24).                               
002900 01    WS-DATE                   PIC X(10).                               
003000 01    WS-TIME                   PIC X(08).                               
003100 77  WS-ADDRESS-WHEVENTS         PIC X(50)                                
003200       VALUE 'CARPARTS.PULS.WHEVENTS'.                                    
003300 77  WS-ADDRESS-MQASYNC          PIC X(50)                                
003400       VALUE 'CARPARTS.PULS.MQASYNC'.                                     
005100*                                                                         
007200 01  DYNAMIC-SUBPROGRAM.                                                  
007300     03 CBLTDLI                  PIC X(8)   VALUE 'CBLTDLI '.             
007400     03 FELLOG                   PIC X(8)   VALUE 'FELLOG  '.             
007500     03 ABEND                    PIC X(8)   VALUE 'ABEND   '.             
007600     03 WZ01SEND                 PIC X(8)   VALUE 'WZ01SEND'.             
007800     SKIP2                                                                
007900*    --- PARAMETERS TO ABEND                                              
008000 77  RKOD-ABEND                  PIC S9(4)  COMP VALUE +0.                
008100 77  RKOD-ABEND-NO-DUMP          PIC S9(4)  COMP VALUE +16.               
008200 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)  COMP VALUE +1000.             
008300     EJECT                                                                
008400 01  FILLER                      PIC X(16)   VALUE 'MSG PROP'.            
008401*01  -COPY WZ04PROP                                                       
008402*                                                                         
008500*    --- PARAMETERS TO WZ01SEND FOR SERVICE LAYER                         
008600 01  FILLER                     PIC X(16) VALUE 'WZ01SEND-WHEV'.          
008700     SKIP3                                                                
008800*01  -COPY WZ01SEND                                                       
010100     EJECT                                                                
010200 01  WS-W403WHEV.                                                         
010300     03 eventId pic X(30).                                                
010400     03 eventType pic X(20).                                              
010500     03 eventLocation pic X(10).                                          
010600     03 eventTime pic X(26).                                              
010620     03 eventOriginSystem pic X(8).                                       
010700     03 userId pic X(10).                                                 
010710     03 newUserId pic X(10).                                              
010800     03 metaData.                                                         
010900        05 prc   pic X(04).                                               
011010        05 pickingUnitSeqInPrc pic 9(3).                                  
011100     03 payload.                                                          
011110        05 orderDetails.                                                  
011111           07 district pic 9(4).                                          
011112           07 customer pic 9(6).                                          
011113           07 orderNumber pic 9(7).                                       
011120           07 productionNumber pic 9(07).                                 
011130           07 pickingListNumber pic 9(03).                                
011131           07 lineNumber pic 9(04).                                       
011140           07 supplier pic X(05).                                         
011150           07 orderSeqInPickingUnit pic 9(03).                            
011200        05 partDetails.                                                   
011201           07 partNumber pic X(30).                                       
011202           07 partDescription pic X(25).                                  
011203           07 partVolume PIC S9(8)V9(1).                                  
011204           07 partWeight PIC S9(4)V9(3).                                  
011207           07 dangerousGoodsCode pic X.                                   
011208           07 countryOfOrigin pic X(02).                                  
011209           07 locationArea pic 9(2).                                      
011210           07 locationAisle pic 9(2).                                     
011211           07 locationPlace pic 9(5).                                     
011212           07 unitOfMeasure pic X(2).                                     
011213           07 properShippingName pic 9(3).                                
011214           07 dataMatrix pic X(512).                                      
011215        05 caseDetails.                                                   
011216           07 caseNumber pic 9(05).                                       
011217           07 newCaseNumber pic 9(05).                                    
011218           07 caseCode pic X(08).                                         
011219           07 caseLength pic 9(04).                                       
011220           07 caseBreadth pic 9(04).                                      
011221           07 caseHeight pic 9(04).                                       
011230           07 packageType pic 9.                                          
011360        05 readyForShipmentDate pic X(10).                                
011370        05 readyForShipmentTime pic X(5).                                 
011380        05 orderedQuantity pic 9(6).                                      
011381        05 deliveredQuantity pic 9(07).                                   
011390        05 sumPickingRoundOrderQty pic 9(3).                              
011391        05 sumPickingRoundLineQty pic 9(5).                               
011392        05 goodsInTransit pic 9(7).                                       
011394        05 qualityBlockCode pic 9(2).                                     
011395        05 adviceBalance pic 9(7).                                        
011396        05 forecast PIC 9(6)V9(1).                                        
011397        05 qualityText pic X(79).                                         
011420     EJECT                                                                
014400                                                                          
014500 LINKAGE SECTION.                                                         
014600*                                                                         
014700*    -COPY W403WHEV                                                       
014800     EJECT                                                                
014900 01  MQASYNC-PCB                 PIC X.                                   
015500     EJECT                                                                
015600                                                                          
015700 PROCEDURE DIVISION USING WHEV-W403WHEV MQASYNC-PCB.                      
015900 MAIN SECTION.                                                            
016000                                                                          
016100     PERFORM A-INIT                                                       
016200     EVALUATE WHEV-KDFUNC                                                 
016300     WHEN 'OPEN'                                                          
016400      PERFORM B-SEND-OPEN                                                 
016500      PERFORM C-SEND-PUT-PROP                                             
016600     WHEN 'PUT'                                                           
016700      PERFORM D-CONV-DATA                                                 
016800      PERFORM E-SEND-PUT                                                  
016900     WHEN 'CLOSE'                                                         
017000      PERFORM F-SEND-CLOSE                                                
017001     END-EVALUATE                                                         
017002     MOVE ZERO TO RETURN-CODE                                             
017003     GOBACK                                                               
017004     .                                                                    
017100     EJECT                                                                
017200 A-INIT SECTION.                                                          
017300     MOVE FUNCTION                                                        
017400              FORMATTED-CURRENT-DATE('YYYY-MM-DDThh:mm:ss.sssZ')          
017500                               TO W-CURRDATE-UTC                          
017600                                                                          
017700     MOVE W-CURRDATE-UTC       TO WHEV-EVENT-TIMESTAMP                    
018800     .                                                                    
018900     SKIP2                                                                
022900 D-CONV-DATA SECTION.                                                     
022901                                                                          
022902     MOVE WHEV-IDEVENT         TO eventId                                 
022903     MOVE WHEV-IDEVENTTYP      TO eventType                               
022904     MOVE WHEV-IDORIGSYS       TO eventOriginSystem                       
022910     MOVE WHEV-EVENTLOCATION   TO eventLocation                           
022911     MOVE WHEV-EVENT-TIMESTAMP TO eventTime                               
022913     IF WHEV-USERID > SPACES                                              
022914     MOVE   WHEV-USERID TO userId                                         
022915     END-IF                                                               
022916     IF WHEV-USERID-NEW > SPACES                                          
022917     MOVE   WHEV-USERID TO newUserId                                      
022918     END-IF                                                               
022919     IF WHEV-IDPRODNR NUMERIC                                             
022920      MOVE   WHEV-IDPRODNR TO productionNumber                            
022921     END-IF                                                               
022922     IF WHEV-IDPLKLST NUMERIC                                             
022923      MOVE   WHEV-IDPLKLST TO pickingListNumber                           
022924     END-IF                                                               
022925     IF WHEV-IDKOLLI NUMERIC                                              
022926     MOVE   WHEV-IDKOLLI TO caseNumber                                    
022927     END-IF                                                               
022928     IF WHEV-IDKOLLI-NEW NUMERIC                                          
022929     MOVE   WHEV-IDKOLLI-NEW TO newCaseNumber                             
022930     END-IF                                                               
022931     MOVE   WHEV-KDKOLLI TO caseCode                                      
022932     IF WHEV-IDPURAD NUMERIC                                              
022933     MOVE   WHEV-IDPURAD TO lineNumber                                    
022934     END-IF                                                               
022935     IF WHEV-KVLEVART NUMERIC                                             
022936     MOVE   WHEV-KVLEVART TO deliveredQuantity                            
022937     END-IF                                                               
022938     IF WHEV-KDEMBTYP NUMERIC                                             
022939     MOVE   WHEV-KDEMBTYP TO packageType                                  
022940     END-IF                                                               
022941     IF WHEV-DIKOLLIL NUMERIC                                             
022942     MOVE   WHEV-DIKOLLIL TO caseLength                                   
022943     END-IF                                                               
022944     IF WHEV-DIKOLLIB NUMERIC                                             
022950     MOVE   WHEV-DIKOLLIB TO caseBreadth                                  
022951     END-IF                                                               
022952     IF WHEV-DIKOLLIH NUMERIC                                             
022960     MOVE   WHEV-DIKOLLIH TO caseHeight                                   
022961     END-IF                                                               
022970     MOVE   WHEV-KDARTURS TO countryOfOrigin                              
022971     IF WHEV-IDARTNR > SPACES                                             
022980     MOVE   WHEV-IDARTNR TO partNumber                                    
022981     END-IF                                                               
022982     IF WHEV-IDLEVNR > SPACES                                             
022990     MOVE   WHEV-IDLEVNR TO supplier                                      
022991     END-IF                                                               
022992     IF WHEV-IDLOPNR-ORD NUMERIC                                          
023000     MOVE   WHEV-IDLOPNR-ORD TO orderSeqInPickingUnit                     
023001     END-IF                                                               
023002     IF WHEV-ADLAGOMR NUMERIC                                             
023003     MOVE   WHEV-ADLAGOMR TO locationArea                                 
023004     END-IF                                                               
023005     IF WHEV-ADGANG NUMERIC                                               
023006     MOVE   WHEV-ADGANG TO locationAisle                                  
023007     END-IF                                                               
023008     IF WHEV-ADPLATS NUMERIC                                              
023009     MOVE   WHEV-ADPLATS TO locationPlace                                 
023010     END-IF                                                               
023011     IF WHEV-IDPSN NUMERIC                                                
023012     MOVE   WHEV-IDPSN TO properShippingName                              
023013     END-IF                                                               
023014     IF WHEV-IDPRC > SPACES                                               
023015     MOVE   WHEV-IDPRC TO prc                                             
023016     END-IF                                                               
023017     IF WHEV-IDLOTNR-PLK NUMERIC                                          
023018     MOVE   WHEV-IDLOTNR-PLK TO pickingUnitSeqInPrc                       
023019     END-IF                                                               
023020     IF WHEV-IDDISTR NUMERIC                                              
023021     MOVE   WHEV-IDDISTR TO district                                      
023022     END-IF                                                               
023023     IF WHEV-IDKUNDNR NUMERIC                                             
023024     MOVE   WHEV-IDKUNDNR TO customer                                     
023025     END-IF                                                               
023026     IF WHEV-IDORDNR7 NUMERIC                                             
023027     MOVE   WHEV-IDORDNR7 TO orderNumber                                  
023028     END-IF                                                               
023029     IF WHEV-BEART > SPACES                                               
023030     MOVE   WHEV-BEART TO partDescription                                 
023040     END-IF                                                               
023041     IF WHEV-TIRFSDAT > SPACES                                            
023042     MOVE   WHEV-TIRFSDAT TO readyForShipmentDate                         
023043     END-IF                                                               
023044     IF WHEV-TIRFSTID > SPACES                                            
023045     MOVE   WHEV-TIRFSTID TO readyForShipmentTime                         
023046     END-IF                                                               
023047     IF WHEV-KDSORT > SPACES                                              
023048     MOVE   WHEV-KDSORT TO unitOfMeasure                                  
023049     END-IF                                                               
023050     IF WHEV-TOT-IDLOPNR-ORD NUMERIC                                      
023051     MOVE   WHEV-TOT-IDLOPNR-ORD TO sumPickingRoundOrderQty               
023052     END-IF                                                               
023053     IF WHEV-KVRADER-TOT NUMERIC                                          
023054     MOVE   WHEV-KVRADER-TOT TO sumPickingRoundLineQty                    
023055     END-IF                                                               
023056     IF WHEV-KVAKS-PAV NUMERIC                                            
023057     MOVE   WHEV-KVAKS-PAV TO goodsInTransit                              
023058     END-IF                                                               
023059     IF WHEV-KVBEART NUMERIC                                              
023060     MOVE   WHEV-KVBEART TO orderedQuantity                               
023061     END-IF                                                               
023062     IF WHEV-KDLEVSP NUMERIC                                              
023063     MOVE   WHEV-KDLEVSP TO qualityBlockCode                              
023064     END-IF                                                               
023065     IF WHEV-KVAKS NUMERIC                                                
023066     MOVE   WHEV-KVAKS TO adviceBalance                                   
023067     END-IF                                                               
023068     IF WHEV-KVPB-REF NUMERIC                                             
023069     MOVE   WHEV-KVPB-REF TO forecast                                     
023070     END-IF                                                               
023071     IF WHEV-TEKVAINF-EXT > SPACES                                        
023072     MOVE   WHEV-TEKVAINF-EXT TO qualityText                              
023073     END-IF                                                               
023074     IF WHEV-VLARTNTO NUMERIC                                             
023075     MOVE   WHEV-VLARTNTO TO partVolume                                   
023076     ELSE                                                                 
023077     MOVE ZEROES     TO      partVolume                                   
023078     END-IF                                                               
023079     IF WHEV-VKARTNTO NUMERIC                                             
023080     MOVE   WHEV-VKARTNTO TO partWeight                                   
023081     ELSE                                                                 
023082     MOVE ZEROES     TO      partWeight                                   
023083     END-IF                                                               
023084     IF WHEV-KDFARLIG > SPACES                                            
023085     MOVE   WHEV-KDFARLIG TO dangerousGoodsCode                           
023086     END-IF                                                               
023087     IF WHEV-TEARTLBL > SPACES                                            
023088     MOVE   WHEV-TEARTLBL TO dataMatrix                                   
023089     END-IF                                                               
023090     JSON GENERATE JSON-DOC                                               
023100       FROM WS-W403WHEV                                                   
023200       COUNT IN JSON-DOC-COUNT                                            
023300       NAME OF                                                            
023400            WS-W403WHEV IS OMITTED                                        
027800       SUPPRESS WHEN LOW-VALUES                                           
027900                                                                          
028000     END-JSON                                                             
028100     .                                                                    
028110 B-SEND-OPEN SECTION.                                                     
028120                                                                          
028130     MOVE 'OPEN'                        TO SEND-KDFUNC                    
028140     MOVE WS-ADDRESS-MQASYNC            TO SEND-ADDISPABS                 
028150     CALL WZ01SEND USING SEND-CONTROL-AREA                                
028160                         SEND-OPEN-AREA                                   
028170     IF SEND-KDRC > 0                                                     
028180       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
028190       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
028191       DELIMITED BY SIZE INTO FELTEXT                                     
028192       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
028193     END-IF                                                               
028194     MOVE SEND-IDCOM     TO WZ04-SEND-IDCOM                               
028195     .                                                                    
028196     EJECT                                                                
028197 C-SEND-PUT-PROP SECTION.                                                 
028198                                                                          
028199     SET PROP-IX                 TO +1                                    
028200*    MANDATORY PROPERTY THAT SPECIFIES THE ACTUAL DESTINATION             
028201     MOVE 'ADDRESS'              TO PROP-IDPROPTYPE  (PROP-IX)            
028202     MOVE 'ADDISPABS'            TO PROP-IDPROPNAME  (PROP-IX)            
028203     MOVE WS-ADDRESS-WHEVENTS    TO PROP-BEPROPVALUE (PROP-IX)            
028204*LK  SET PROP-IX UP BY +1                                                 
028205*LK  MOVE 'MSGOPTION'            TO PROP-IDPROPTYPE  (PROP-IX)            
028206*LK  MOVE 'LINE_IS_MSG'          TO PROP-IDPROPNAME  (PROP-IX)            
028207*LK  MOVE 'J'                    TO PROP-BEPROPVALUE (PROP-IX)            
028209*    SET THE NUMBER OF PROPERTIES (KVANTAL) SO CORRECT LENGTH             
028210*    IS CALCULATED.                                                       
028211     SET PROP-KVANTAL            TO PROP-IX                               
028212                                                                          
028213     MOVE 'PUT'                            TO SEND-KDFUNC                 
028214     MOVE LENGTH OF PROP-WZ04PROP          TO SEND-KVDLEN                 
028215     MOVE WZ04-SEND-IDCOM                  TO SEND-IDCOM                  
028216     CALL WZ01SEND USING SEND-CONTROL-AREA                                
028217                         SEND-KVDLEN                                      
028218                         PROP-WZ04PROP                                    
028219                                                                          
028220     IF SEND-KDRC > 1                                                     
028221       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
028222       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
028223       DELIMITED BY SIZE INTO FELTEXT                                     
028224       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
028225     END-IF                                                               
028226     .                                                                    
028230 E-SEND-PUT SECTION.                                                      
028300                                                                          
028400     MOVE 'PUT'                            TO SEND-KDFUNC                 
028500     MOVE JSON-DOC-COUNT                   TO SEND-KVDLEN                 
028600     MOVE WZ04-SEND-IDCOM                  TO SEND-IDCOM                  
028700     CALL WZ01SEND USING SEND-CONTROL-AREA                                
028800                         SEND-KVDLEN                                      
028900                         JSON-DOC                                         
029000     IF SEND-KDRC > 1                                                     
029100       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
029200       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
029300       DELIMITED BY SIZE INTO FELTEXT                                     
029400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
029500     END-IF                                                               
029600     .                                                                    
029700     EJECT                                                                
029800 F-SEND-CLOSE SECTION.                                                    
029900     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
030000     MOVE WZ04-SEND-IDCOM            TO SEND-IDCOM                        
030100     CALL WZ01SEND USING SEND-CONTROL-AREA                                
030200                                                                          
030300     IF SEND-KDRC > 0                                                     
030400       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
030500       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
030600       DELIMITED BY SIZE INTO FELTEXT                                     
030700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
030800     END-IF                                                               
030900     .                                                                    
031000     EJECT                                                                
