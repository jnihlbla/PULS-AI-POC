000100 01  REQU-WF0272I1.                                                       
000200*                                 REQUEST-COPYTEXT PGM WF0272             
000300*                                 INTERSTATE RULES MAINTENANCE            
000400     03 REQU-IDLEGSEL-KEY    PIC X(4).                                    
000500*                                 FAKTURERANDE FÖRETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 REQU-IDLANDX3-SEND-KEY                                            
000800                             PIC X(3).                                    
000900*                                 3-STÄLLIG LANDSBETECKNINGSKOD           
001000*                                 3-LETTER CODE FOR COUNTRY.              
001100     03 REQU-IDLANDX3-REC-KEY                                             
001200                             PIC X(3).                                    
001300*                                 3-STÄLLIG LANDSBETECKNINGSKOD           
001400*                                 3-LETTER CODE FOR COUNTRY.              
001500     03 REQU-KDSTATUS-KEY    PIC 9(3).                                    
001600*                                 STATUSKOD          KDSTATUS-002         
001700     03 REQU-FLCOMING        PIC X.                                       
001800*                                 ALLMÄN FLAGGA                           
001900*                                 GENERAL FLAG                            
002000     03 REQU-FLEXPORT        PIC X.                                       
002100*                                 EXPORTFAKTURA                           
002200*                                 EXPORT INVOICE                          
002300     03 REQU-FLVAT           PIC X.                                       
002400*                                 MOMS PÅ FAKTURA                         
002500*                                 TAX ON INVOICE                          
002600     03 REQU-FLVAT-PRIV      PIC X.                                       
002700*                                 MOMS PÅ FAKTURA                         
002800*                                 VAT ON INVOICE                          
002900     03 REQU-KDVAT           PIC X(2).                                    
003000*                                 MOMSKOD                                 
003100*                                 VAT CODE                                
003200     03 REQU-KDVAT-SERV      PIC X(2).                                    
003300*                                 MOMSKOD                                 
003400*                                 VAT CODE                                
003500     03 REQU-FLVATREP        PIC X.                                       
003600*                                 MOMSRAPPORT                             
003700*                                 VAT REPORT                              
003800     03 REQU-FLINTREP        PIC X.                                       
003900*                                 INTRASTATRAPPORTERING                   
004000*                                 INTRASTAT REPORTING                     
004100     03 REQU-FLCUSREP        PIC X.                                       
004200*                                 TULLRAPPORT                             
004300*                                 CUSTOMS REPORT                          
004400     03 REQU-DAUPPDAT        PIC X(8).                                    
004500*                                 UPPDATERINGSDATUM  (ÅÅÅÅMMDD)           
004600*                                 UPDATING DATE     (YYYYMMDD)            
004700     03 REQU-BETEXT-1        PIC X(50).                                   
004800     03 REQU-BETEXT-2        PIC X(50).                                   
004900     03 REQU-BETEXT-3        PIC X(50).                                   
005000     03 REQU-BETEXT-4        PIC X(50).                                   
005100     03 REQU-BETEXT-5        PIC X(100).                                  
005200     03 REQU-BETEXT-6        PIC X(100).                                  
005300     03 REQU-BETEXT-7        PIC X(100).                                  
005400     03 REQU-BETEXT-8        PIC X(100).                                  
005500     03 REQU-BETEXT-9        PIC X(100).                                  
005600     03 REQU-BETEXT-10       PIC X(100).                                  
005700     03 REQU-BETEXT-11       PIC X(100).                                  
005800     03 REQU-BETEXT-12       PIC X(100).                                  
005900     03 REQU-BETEXT-13       PIC X(100).                                  
006000     03 REQU-BETEXT-14       PIC X(100).                                  
006100     03 REQU-BETEXT-15       PIC X(100).                                  
006200     03 REQU-BETEXT-16       PIC X(100).                                  
006300     03 REQU-BETEXT-17       PIC X(100).                                  
006400     03 REQU-BETEXT-18       PIC X(100).                                  
006500     03 REQU-BETEXT-19       PIC X(100).                                  
006600     03 REQU-BETEXT-20       PIC X(100).                                  
006700     03 REQU-BETEXT-21       PIC X(100).                                  
006800     03 REQU-BETEXT-22       PIC X(100).                                  
006900     03 REQU-BETEXT-23       PIC X(100).                                  
007000     03 REQU-BETEXT-24       PIC X(100).                                  
007100     03 REQU-BETEXT-25       PIC X(100).                                  
007200     03 REQU-BETEXT-26       PIC X(100).                                  
007300     03 REQU-BETEXT-27       PIC X(100).                                  
007400     03 REQU-BETEXT-28       PIC X(100).                                  
007500     03 REQU-BETEXT-29       PIC X(100).                                  
007600     03 REQU-BETEXT-30       PIC X(100).                                  
007700     03 REQU-BETEXT-31       PIC X(100).                                  
007800     03 REQU-BETEXT-32       PIC X(100).                                  
007900     03 REQU-BETEXT-33       PIC X(100).                                  
008000     03 REQU-BETEXT-34       PIC X(100).                                  
008100     03 REQU-BETEXT-35       PIC X(100).                                  
008200     03 REQU-BETEXT-36       PIC X(100).                                  
008300     03 REQU-BETEXT-37       PIC X(100).                                  
008400     03 REQU-BETEXT-38       PIC X(100).                                  
008500     03 REQU-BETEXT-39       PIC X(100).                                  
008600     03 REQU-BETEXT-40       PIC X(100).                                  
008700*** END OF VILMAII-COPY LENGTH= 3832 BYTES                                
