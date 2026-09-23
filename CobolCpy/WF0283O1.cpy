000100 01  RESP-WF0283O1.                                                       
000200*                                 RESPONS-COPYTEXT F÷R PGM WF0283         
000300*                                 CURRENCY MAINTENANCE                    
000400     03 RESP-IDLEGSEL-KEY    PIC X(4).                                    
000500*                                 FAKTURERANDE F÷RETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 RESP-KDVALISO-KEY    PIC X(3).                                    
000800*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
000900*                                 CURRENCY CODE BY ISO-STANDARD.          
001000     03 RESP-DASTADAT-KEY    PIC 9(8).                                    
001100*                                 GENERELLT STARTDATUM                    
001200*                                 GENERAL START DATE                      
001300     03 RESP-BELEGRAD-1      PIC X(35).                                   
001400*                                 DEL AV LEGAL SELLER NAMN                
001500*                                 PART OF LEGAL SELLER NAME               
001600     03 RESP-REVALUTA        PIC Z(2)9.                                   
001700*                                 OMRƒKNINGSTAL F÷R VALUTA                
001800*                                 CONVERT VALUE FOR CURRENCY CODE         
001900     03 RESP-PRKURS          PIC Z(5)9.9(5).                              
002000*                                 VALUTAKURS                              
002100*                                 CURRENCY EXCHANGE RATE                  
002200     03 RESP-DAREGDAT        PIC Z(8).                                    
002300*                                 REGISTRERINGSDATUM (≈≈≈≈MMDD)           
002400*                                 REGISTRATION DATE (YYYYMMDD)            
002500     03 RESP-DAUPPDAT        PIC Z(8).                                    
002600*                                 UPPDATERINGSDATUM  (≈≈≈≈MMDD)           
002700*                                                                         
002800*                                 UPDATING DATE     (YYYYMMDD)            
002900*                                                                         
003000     03 RESP-DADELDAT        PIC Z(8).                                    
003100*                                 BORTTAGSDATUM      (≈≈≈≈MMDD)           
003200*                                 DELETION DATE     (YYYYMMDD)            
003300     03 RESP-IDUSER          PIC X(8).                                    
003400*                                 ANVƒNDARENS SƒKERHETS ID                
003500*                                 USER SECURITY-IDENTITY                  
003600*** END OF VILMAII-COPY LENGTH= 97 BYTES                                  
