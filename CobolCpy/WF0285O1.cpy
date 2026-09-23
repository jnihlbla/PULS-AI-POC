000100 01  RESP-WF0285O1.                                                       
000200*                                 RESPONS-COPYTEXT F÷R PGM WF0285         
000300*                                 DOCUMENT NUMBER SERIE MAINTENAN         
000400*                                 CE                                      
000500     03 RESP-IDLEGSEL-KEY    PIC X(4).                                    
000600*                                 FAKTURERANDE F÷RETAG TEX VCCS           
000700*                                 LEGAL SELLER IDENTITY                   
000800     03 RESP-IDLOPNR-KEY     PIC 9(3).                                    
000900*                                 L÷PNUMMER                               
001000*                                 SEQUENCE NUMBER                         
001100     03 RESP-BELEGRAD-1      PIC X(35).                                   
001200*                                 DEL AV LEGAL SELLER NAMN                
001300*                                 PART OF LEGAL SELLER NAME               
001400     03 RESP-BETEXT          PIC X(55).                                   
001500     03 RESP-IDFINDOC-START  PIC Z(9).                                    
001600*                                 FINANSIELLT DOKUMENT ID                 
001700*                                 FINANCIAL DOCUMENT ID                   
001800     03 RESP-IDFINDOC-NEXT   PIC Z(9).                                    
001900*                                 FINANSIELLT DOKUMENT ID                 
002000*                                 FINANCIAL DOCUMENT ID                   
002100     03 RESP-IDFINDOC-STOP   PIC Z(9).                                    
002200*                                 FINANSIELLT DOKUMENT ID                 
002300*                                 FINANCIAL DOCUMENT ID                   
002400     03 RESP-DAREGDAT        PIC Z(8).                                    
002500*                                 REGISTRERINGSDATUM (≈≈≈≈MMDD)           
002600*                                 REGISTRATION DATE (YYYYMMDD)            
002700     03 RESP-DAUPPDAT        PIC Z(8).                                    
002800*                                 UPPDATERINGSDATUM  (≈≈≈≈MMDD)           
002900*                                 UPDATING DATE     (YYYYMMDD)            
003000     03 RESP-DADELDAT        PIC Z(8).                                    
003100*                                 BORTTAGSDATUM      (≈≈≈≈MMDD)           
003200*                                 DELETION DATE     (YYYYMMDD)            
003300     03 RESP-IDUSER          PIC X(8).                                    
003400*                                 ANVƒNDARENS SƒKERHETS ID                
003500*                                 USER SECURITY-IDENTITY                  
003600*** END OF VILMAII-COPY LENGTH= 156 BYTES                                 
