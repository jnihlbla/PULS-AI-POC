000100 01  REQU-W60202I1.                                                       
000200*                                 COPYTEXT FÖR REQU W60202I1              
000300*                                                                         
000400     03 REQU-IDKR-KEY        PIC X(5).                                    
000500*                                 KONTROLLRAPPORT NUMMER                  
000600*                                 INSPECTION REPORT NUMBER                
000700     03 REQU-IDDC-KEY        PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900*                                 WAREHOUSE IDENTIFIER                    
001000     03 REQU-IDSPRAK         PIC X(2).                                    
001100*                                 2-STÄLLIG ISO SPRÅKKOD                  
001200*                                 2-LETTER ISO LANGUAGE CODE              
001300     03 REQU-INPUT.                                                       
001400*                                                                         
001500        05 REQU-IDLOPNRM-UPD PIC 9(8).                                    
001600*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001700*                                 (0VVDLLLLK)                             
001800*                                 SERIAL NO RECEIVING REPORT              
001900*                                 (0WWDLLLLC)                             
002000        05 REQU-TIAVSDAT-UPD PIC 9(6).                                    
002100*                                 AVISERINGSDATUM (YYMMDD)                
002200*                                 ADVICE NOTE DATE                        
002300        05 REQU-IDARTNR-UPD  PIC 9(9).                                    
002400*                                 ARTIKELNUMMER                           
002500*                                 PART NUMBER                             
002600        05 REQU-IDLEVNR-UPD  PIC X(5).                                    
002700*                                 LEVERANTÖRNUMMER                        
002800*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002900        05 REQU-IDLEVG-UPD   PIC 9(5).                                    
003000*                                 LEVERANTÖRS GODSADRESS NUMMER           
003100*                                 SUPPLIER WAREHOUSE NUMBER               
003200        05 REQU-KVANTMOT-UPD PIC 9(6).                                    
003300*                                 ANTAL MOTTAGET                          
003400*                                 QUANTITY RECEIVED                       
003500        05 REQU-KVART-RET-UPD                                             
003600                             PIC 9(7).                                    
003700*                                 ANTAL ARTIKLAR I RETUR                  
003800*                                 QUANTITY INSPECTED PARTS                
003900        05 REQU-KVART-SKROT-UPD                                           
004000                             PIC 9(7).                                    
004100*                                 ANTAL SKROTADE ARTIKLAR                 
004200*                                 QUANTITY INSPECTED PARTS                
004300        05 REQU-KVART-SKROT-LDC-UPD                                       
004400                             PIC 9(7).                                    
004500*                                 ANTAL SKROTADE ARTIKLAR LDC             
004600*                                 QUANTITY INSPECTED PARTS LDC            
004700        05 REQU-KVART-KONTR-UPD                                           
004800                             PIC 9(7).                                    
004900*                                 ANTAL KONTROLLERAD ARTIKLAR             
005000*                                 QUANTITY INSPECTED PARTS                
005100        05 REQU-KVART-KJUST-UPD                                           
005200                             PIC 9(7).                                    
005300*                                 ANTAL ARTIKLAR KVAL.JUSTERAS            
005400*                                 QUANTITY INSPECTED PARTS                
005500        05 REQU-KVART-EJ-GODK-UPD                                         
005600                             PIC 9(7).                                    
005700*                                 ANTAL EJ GODKÄNDA ARTIKLAR              
005800*                                 QUANTITY INSPECTED PARTS                
005900        05 REQU-KVART-BEH-UPD                                             
006000                             PIC 9(7).                                    
006100*                                 ANTAL ARTIKLAR SOM BEHÅLLES             
006200*                                 QUANTITY INSPECTED PARTS                
006300        05 REQU-KVART-SJUST-UPD                                           
006400                             PIC X(7).                                    
006500*                                 ANTAL SALDOJUSTERADE ARTIKLAR           
006600*                                 QUANTITY INSPECTED PARTS                
006700        05 REQU-IDKRFEL-UPD  PIC X(2).                                    
006800*                                 FELKOD FÖR KONTROLLRAPPORT              
006900*                                 ERRORCODE FOR INSP.REPORT               
007000        05 REQU-FLKVALSP-UPD PIC X.                                       
007100*                                 KVALITETSBLOCK JUSTERAS                 
007200*                                 QUANLITY BLOCK ADJUSTMENT               
007300        05 REQU-KDDISP-UPD   PIC 9(2).                                    
007400*                                 DISPOSITION CODE                        
007500*                                 DISPOSITION CODE                        
007600        05 REQU-FLBUFJUS-UPD PIC X.                                       
007700*                                 BUFFERTJUSERING                         
007800*                                 ADJUST BUFFER                           
007900        05 REQU-KDHANDCO-UPD PIC 9.                                       
008000*                                 OMKOSTNADSKOD                           
008100*                                 HANDLING COST CODE                      
008200        05 REQU-KDPERSON-UPD PIC 9(3).                                    
008300*                                 PERSONKOD                               
008400*                                 STAFF CODE                              
008500        05 REQU-BEKRBEH-UPD  PIC X(25).                                   
008600*                                 KONTROLLANT                             
008700*                                 INSPECTOR                               
008800        05 REQU-KVKRBEH-UPD  PIC X(4).                                    
008900*                                 BEHANDLINGSTID FÖR KR                   
009000*                                 USED TIME FOR INSP. REPORT              
009100        05 REQU-TEKRPLT-UPD  PIC X(20).                                   
009200*                                 GODS PLACERAT                           
009300*                                 GOODS PLACED                            
009400     03 REQU-KDKRUTF-UPD     PIC X.                                       
009500*                                 UTFÖRANDEKOD FÖR KONTROLLRAPP.          
009600*                                 INSPECTION REPORT CODE                  
009700*** END OF VILMAII-COPY LENGTH= 164 BYTES                                 
