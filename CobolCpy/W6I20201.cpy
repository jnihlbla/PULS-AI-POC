000100 01  MID-W6I20201.                                                        
000200*                                 COPYTEXT FÖR MID W6I20201               
000300*                                                                         
000400     03 MID-IDKR-IN          PIC X(5).                                    
000500*                                 KONTROLLRAPPORT NUMMER                  
000600*                                 INSPECTION REPORT NUMBER                
000700     03 MID-IDKR-UT          PIC X(5).                                    
000800*                                 KONTROLLRAPPORT NUMMER                  
000900*                                 INSPECTION REPORT NUMBER                
001000     03 MID-INPUT.                                                        
001100*                                                                         
001200        05 MID-IDLOPNRM      PIC 9(8).                                    
001300*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001400*                                 (0VVDLLLLK)                             
001500*                                 SERIAL NO RECEIVING REPORT              
001600*                                 (0WWDLLLLC)                             
001700        05 MID-TIAVSDAT      PIC 9(6).                                    
001800*                                 AVISERINGSDATUM (YYMMDD)                
001900*                                 ADVICE NOTE DATE                        
002000        05 MID-IDARTNR       PIC 9(9).                                    
002100*                                 ARTIKELNUMMER                           
002200*                                 PART NUMBER                             
002300        05 MID-IDLEVNR       PIC X(5).                                    
002400*                                 LEVERANTÖRNUMMER                        
002500*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002600        05 MID-IDLEVG        PIC 9(5).                                    
002700*                                 LEVERANTÖRS GODSADRESS NUMMER           
002800*                                 SUPPLIER WAREHOUSE NUMBER               
002900        05 MID-KVANTMOT      PIC 9(6).                                    
003000*                                 ANTAL MOTTAGET                          
003100*                                 QUANTITY RECEIVED                       
003200        05 MID-KVART-RET     PIC 9(7).                                    
003300*                                 ANTAL ARTIKLAR I RETUR                  
003400*                                 QUANTITY INSPECTED PARTS                
003500        05 MID-KVART-SKROT   PIC 9(7).                                    
003600*                                 ANTAL SKROTADE ARTIKLAR                 
003700*                                 QUANTITY INSPECTED PARTS                
003800        05 MID-KVART-SKROT-LDC                                            
003900                             PIC 9(7).                                    
004000*                                 ANTAL SKROTADE ARTIKLAR LDC             
004100*                                 QUANTITY INSPECTED PARTS LDC            
004200        05 MID-KVART-KONTR   PIC 9(7).                                    
004300*                                 ANTAL KONTROLLERAD ARTIKLAR             
004400*                                 QUANTITY INSPECTED PARTS                
004500        05 MID-KVART-KJUST   PIC 9(7).                                    
004600*                                 ANTAL ARTIKLAR KVAL.JUSTERAS            
004700*                                 QUANTITY INSPECTED PARTS                
004800        05 MID-KVART-EJ-GODK PIC 9(7).                                    
004900*                                 ANTAL EJ GODKÄNDA ARTIKLAR              
005000*                                 QUANTITY INSPECTED PARTS                
005100        05 MID-KVART-BEH     PIC 9(7).                                    
005200*                                 ANTAL ARTIKLAR SOM BEHÅLLES             
005300*                                 QUANTITY INSPECTED PARTS                
005400        05 MID-KVART-SJUST   PIC X(7).                                    
005500*                                 ANTAL SALDOJUSTERADE ARTIKLAR           
005600*                                 QUANTITY INSPECTED PARTS                
005700        05 MID-IDKRFEL       PIC X(2).                                    
005800*                                 FELKOD FÖR KONTROLLRAPPORT              
005900*                                 ERRORCODE FOR INSP.REPORT               
006000        05 MID-FLKVALSP      PIC X.                                       
006100*                                 KVALITETSBLOCK JUSTERAS                 
006200*                                 QUANLITY BLOCK ADJUSTMENT               
006300        05 MID-KDDISP        PIC 9(2).                                    
006400*                                 DISPOSITION CODE                        
006500*                                 DISPOSITION CODE                        
006600        05 MID-FLBUFJUS      PIC X.                                       
006700*                                 BUFFERTJUSERING                         
006800*                                 ADJUST BUFFER                           
006900        05 MID-KDHANDCO      PIC 9.                                       
007000*                                 OMKOSTNADSKOD                           
007100*                                 HANDLING COST CODE                      
007200        05 MID-KDPERSON      PIC 9(3).                                    
007300*                                 PERSONKOD                               
007400*                                 STAFF CODE                              
007500        05 MID-BEKRBEH       PIC X(25).                                   
007600*                                 KONTROLLANT                             
007700*                                 INSPECTOR                               
007800        05 MID-KVKRBEH       PIC X(4).                                    
007900*                                 BEHANDLINGSTID FÖR KR                   
008000*                                 USED TIME FOR INSP. REPORT              
008100        05 MID-TEKRPLT       PIC X(20).                                   
008200*                                 GODS PLACERAT                           
008300*                                 GOODS PLACED                            
008400     03 MID-KDKRUTF          PIC X.                                       
008500*                                 UTFÖRANDEKOD FÖR KONTROLLRAPP.          
008600*                                 INSPECTION REPORT CODE                  
008700*** END OF VILMAII-COPY LENGTH= 165 BYTES                                 
