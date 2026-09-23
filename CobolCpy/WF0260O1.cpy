000100 01  RESP-WF0260O1.                                                       
000200*                                 RESPONS-COPYTEXT F÷R PGM WF0260         
000300*                                 V.A.T LOCATE                            
000400     03 RESP-IDLEGSEL-KEY    PIC X(4).                                    
000500*                                 FAKTURERANDE F÷RETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 RESP-IDLANDX2-KEY    PIC X(2).                                    
000800*                                 2-STƒLLIG LANDSBETECKNINGSKOD           
000900*                                 2-LETTER CODE FOR COUNTRY               
001000     03 RESP-BELEGRAD-1      PIC X(35).                                   
001100*                                 DEL AV LEGAL SELLER NAMN                
001200*                                 PART OF LEGAL SELLER NAME               
001300     03 RESP-KVRADER         PIC Z(4)9.                                   
001400*                                 ANTAL RADER                             
001500*                                 NUMBER OF LINES                         
001600     03 RESP-TABELLRAD       OCCURS 500 TIMES.                            
001700*                                 GRUPP MED TABELLRADER                   
001800        05 RESP-IDLANDX2-LINE                                             
001900                             PIC X(2).                                    
002000*                                 2-STƒLLIG LANDSBETECKNINGSKOD           
002100*                                 2-LETTER CODE FOR COUNTRY               
002200        05 RESP-KDVAT-LINE   PIC X(2).                                    
002300*                                 MOMSKOD                                 
002400*                                 VAT CODE                                
002500        05 RESP-REVAT-LINE   PIC Z(2)9.9(2).                              
002600*                                 MULTIPLIKATIONSFAKTOR F÷R MOMS          
002700*                                 VAT FACTOR                              
002800        05 RESP-DAREGDAT-LINE                                             
002900                             PIC Z(8).                                    
003000*                                 REGISTRERINGSDATUM (≈≈≈≈MMDD)           
003100*                                 REGISTRATION DATE (YYYYMMDD)            
003200        05 RESP-DAUPPDAT-LINE                                             
003300                             PIC Z(8).                                    
003400*                                 UPPDATERINGSDATUM  (≈≈≈≈MMDD)           
003500*                                                                         
003600*                                 UPDATING DATE     (YYYYMMDD)            
003700*                                                                         
003800        05 RESP-DADELDAT-LINE                                             
003900                             PIC Z(8).                                    
004000*                                 BORTTAGSDATUM      (≈≈≈≈MMDD)           
004100*                                 DELETION DATE     (YYYYMMDD)            
004200        05 RESP-BEVAT-LINE   PIC X(50).                                   
004300*                                 MOMSKODSBENƒMNING R3                    
004400*                                 VAT CODE DESCRIPTION R3                 
004500*** END OF VILMAII-COPY LENGTH= 42046 BYTES                               
