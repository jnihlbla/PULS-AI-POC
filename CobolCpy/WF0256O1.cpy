000100 01  RESP-WF0256O1.                                                       
000200*                                 RESPONS-COPYTEXT F÷R PGM WF0256         
000300*                                 PERIOD CALENDER MAINTENANCE             
000400     03 RESP-IDLEGSEL-KEY    PIC X(4).                                    
000500*                                 FAKTURERANDE F÷RETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 RESP-DASTADAT-KEY    PIC Z(8).                                    
000800*                                 GENERELLT STARTDATUM                    
000900*                                 GENERAL START DATE                      
001000     03 RESP-BELEGRAD-1      PIC X(35).                                   
001100*                                 DEL AV LEGAL SELLER NAMN                
001200*                                 PART OF LEGAL SELLER NAME               
001300     03 RESP-TIRP            PIC Z(2).                                    
001400*                                 REDOVISNINGSPERIOD                      
001500*                                 12 PER ≈R                               
001600*                                 ACCOUNTING PERIOD                       
001700*                                 12 PER YEAR                             
001800     03 RESP-DAFINDOC        PIC Z(8).                                    
001900*                                 DOKUMENT DATUM (≈≈≈≈MMDD)               
002000*                                 INVOICING DATE   (YYYYMMDD)             
002100     03 RESP-FLPERIOD        PIC X.                                       
002200*                                 ANGER OM PERIODK÷RNING K÷RD F÷R         
002300*                                  AKTUELL PERIOD                         
002400*                                 STATES THAT PERIOD-EXECUTION IS         
002500*                                  PERFORMED                              
002600     03 RESP-DAREGDAT        PIC Z(8).                                    
002700*                                 REGISTRERINGSDATUM (≈≈≈≈MMDD)           
002800*                                 REGISTRATION DATE (YYYYMMDD)            
002900     03 RESP-DAUPPDAT        PIC Z(8).                                    
003000*                                 UPPDATERINGSDATUM  (≈≈≈≈MMDD)           
003100*                                                                         
003200*                                 UPDATING DATE     (YYYYMMDD)            
003300*                                                                         
003400     03 RESP-DADELDAT        PIC Z(8).                                    
003500*                                 BORTTAGSDATUM      (≈≈≈≈MMDD)           
003600*                                 DELETION DATE     (YYYYMMDD)            
003700     03 RESP-IDUSER          PIC X(8).                                    
003800*                                 ANVƒNDARENS SƒKERHETS ID                
003900*                                 USER SECURITY-IDENTITY                  
004000*** END OF VILMAII-COPY LENGTH= 90 BYTES                                  
