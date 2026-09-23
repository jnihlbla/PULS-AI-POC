000100 01  RESP-WF0272O1.                                                       
000200*                                 RESPONS-COPYTEXT FÖR PGM WF0272         
000300*                                 INTERSTATE RULES MAINTENANCE            
000400     03 RESP-IDLEGSEL-KEY    PIC X(4).                                    
000500*                                 FAKTURERANDE FÖRETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 RESP-IDLANDX3-SEND-KEY                                            
000800                             PIC X(3).                                    
000900*                                 3-STÄLLIG LANDSBETECKNINGSKOD           
001000*                                 3-LETTER CODE FOR COUNTRY.              
001100     03 RESP-IDLANDX3-REC-KEY                                             
001200                             PIC X(3).                                    
001300*                                 3-STÄLLIG LANDSBETECKNINGSKOD           
001400*                                 3-LETTER CODE FOR COUNTRY.              
001500     03 RESP-KDSTATUS-KEY    PIC 9(3).                                    
001600*                                 STATUSKOD          KDSTATUS-002         
001700     03 RESP-BELEGRAD-1      PIC X(35).                                   
001800*                                 DEL AV LEGAL SELLER NAMN                
001900*                                 PART OF LEGAL SELLER NAME               
002000     03 RESP-FLCOMING        PIC X.                                       
002100*                                 ALLMÄN FLAGGA                           
002200*                                 GENERAL FLAG                            
002300     03 RESP-FLEXPORT        PIC X.                                       
002400*                                 EXPORTFAKTURA                           
002500*                                 EXPORT INVOICE                          
002600     03 RESP-FLVAT           PIC X.                                       
002700*                                 MOMS PÅ FAKTURA                         
002800*                                 TAX ON INVOICE                          
002900     03 RESP-FLVAT-PRIV      PIC X.                                       
003000*                                 MOMS PÅ FAKTURA                         
003100*                                 VAT ON INVOICE                          
003200     03 RESP-KDVAT           PIC X(2).                                    
003300*                                 MOMSKOD                                 
003400*                                 VAT CODE                                
003500     03 RESP-KDVAT-SERV      PIC X(2).                                    
003600*                                 MOMSKOD                                 
003700*                                 VAT CODE                                
003800     03 RESP-FLVATREP        PIC X.                                       
003900*                                 MOMSRAPPORT                             
004000*                                 VAT REPORT                              
004100     03 RESP-FLCUSREP        PIC X.                                       
004200*                                 TULLRAPPORT                             
004300*                                 CUSTOMS REPORT                          
004400     03 RESP-FLINTREP        PIC X.                                       
004500*                                 INTRASTATRAPPORTERING                   
004600*                                 INTRASTAT REPORTING                     
004700     03 RESP-DAREGDAT        PIC Z(8).                                    
004800*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
004900*                                 REGISTRATION DATE (YYYYMMDD)            
005000     03 RESP-DAUPPDAT        PIC Z(8).                                    
005100*                                 UPPDATERINGSDATUM  (ÅÅÅÅMMDD)           
005200*                                 UPDATING DATE     (YYYYMMDD)            
005300     03 RESP-DADELDAT        PIC Z(8).                                    
005400*                                 BORTTAGSDATUM      (ÅÅÅÅMMDD)           
005500*                                 DELETION DATE     (YYYYMMDD)            
005600     03 RESP-IDUSER          PIC X(8).                                    
005700*                                 ANVÄNDARENS SÄKERHETS ID                
005800*                                 USER SECURITY-IDENTITY                  
005900     03 RESP-BETEXT-1        PIC X(50).                                   
006000     03 RESP-BETEXT-2        PIC X(50).                                   
006100     03 RESP-BETEXT-3        PIC X(50).                                   
006200     03 RESP-BETEXT-4        PIC X(50).                                   
006300     03 RESP-BETEXT-5        PIC X(100).                                  
006400     03 RESP-BETEXT-6        PIC X(100).                                  
006500     03 RESP-BETEXT-7        PIC X(100).                                  
006600     03 RESP-BETEXT-8        PIC X(100).                                  
006700     03 RESP-BETEXT-9        PIC X(100).                                  
006800     03 RESP-BETEXT-10       PIC X(100).                                  
006900     03 RESP-BETEXT-11       PIC X(100).                                  
007000     03 RESP-BETEXT-12       PIC X(100).                                  
007100     03 RESP-BETEXT-13       PIC X(100).                                  
007200     03 RESP-BETEXT-14       PIC X(100).                                  
007300     03 RESP-BETEXT-15       PIC X(100).                                  
007400     03 RESP-BETEXT-16       PIC X(100).                                  
007500     03 RESP-BETEXT-17       PIC X(100).                                  
007600     03 RESP-BETEXT-18       PIC X(100).                                  
007700     03 RESP-BETEXT-19       PIC X(100).                                  
007800     03 RESP-BETEXT-20       PIC X(100).                                  
007900     03 RESP-BETEXT-21       PIC X(100).                                  
008000     03 RESP-BETEXT-22       PIC X(100).                                  
008100     03 RESP-BETEXT-23       PIC X(100).                                  
008200     03 RESP-BETEXT-24       PIC X(100).                                  
008300     03 RESP-BETEXT-25       PIC X(100).                                  
008400     03 RESP-BETEXT-26       PIC X(100).                                  
008500     03 RESP-BETEXT-27       PIC X(100).                                  
008600     03 RESP-BETEXT-28       PIC X(100).                                  
008700     03 RESP-BETEXT-29       PIC X(100).                                  
008800     03 RESP-BETEXT-30       PIC X(100).                                  
008900     03 RESP-BETEXT-31       PIC X(100).                                  
009000     03 RESP-BETEXT-32       PIC X(100).                                  
009100     03 RESP-BETEXT-33       PIC X(100).                                  
009200     03 RESP-BETEXT-34       PIC X(100).                                  
009300     03 RESP-BETEXT-35       PIC X(100).                                  
009400     03 RESP-BETEXT-36       PIC X(100).                                  
009500     03 RESP-BETEXT-37       PIC X(100).                                  
009600     03 RESP-BETEXT-38       PIC X(100).                                  
009700     03 RESP-BETEXT-39       PIC X(100).                                  
009800     03 RESP-BETEXT-40       PIC X(100).                                  
009900*** END OF VILMAII-COPY LENGTH= 3891 BYTES                                
