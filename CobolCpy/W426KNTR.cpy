000100 01  KVAL-W426KNTR.                                                       
000200*                                 LÄNKAREA TILL W426KNTR -                
000300*                                 GER SVAR PÅ FRÅGA ANTAL TILL KO         
000400*                                 NTROLL                                  
000500     03 KVAL-IDDC            PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700*                                 WAREHOUSE IDENTIFIER                    
000800     03 KVAL-IDLEVNR         PIC X(5).                                    
000900*                                 LEVERANTÖRNUMMER                        
001000*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001100     03 KVAL-IDARTNR         PIC S9(9)           COMP-3.                  
001200*                                 ARTIKELNUMMER                           
001300*                                 PART NUMBER                             
001400     03 KVAL-IDLOPNRM        PIC S9(9)           COMP-3.                  
001500*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001600*                                 (0VVDLLLLK)                             
001700*                                 SERIAL NO RECEIVING REPORT              
001800*                                 (0WWDLLLLC)                             
001900     03 KVAL-KVAVIS          PIC S9(7)           COMP-3.                  
002000*                                 AVISERAT ANTAL                          
002100*                                 QUANTITY NOTIFIED                       
002200     03 KVAL-BEFT            PIC S9(3)           COMP-3.                  
002300*                                 FÖRPACKNINGSTYP                         
002400*                                 PACKAGING TYPE                          
002500     03 KVAL-FLKVARED        PIC X.                                       
002600*                                 REDUCERAD KONTROLL FLAGGA               
002700*                                 REDUCED CONTROL FLAG                    
002800     03 KVAL-KVKVAPRIM       PIC S9(7)           COMP-3.                  
002900*                                 ANTAL TILL PRIMÄRKONTROLL               
003000*                                 QTY TO PRIMARY CONTROL                  
003100     03 KVAL-KVKVASEK        PIC S9(7)           COMP-3.                  
003200*                                  ANTAL TILL SEKUNDÄRKONTROLL            
003300*                                  QTY TO SECONDARY CONTROL               
003400     03 KVAL-KDKVAANT        PIC 9.                                       
003500*                                 KOD ANTALSKONTR SKALL UTFÖRAS           
003600*                                 CODE THE QUANT WILL BE COUNTED          
003700     03 KVAL-FLKVAKAR        PIC X.                                       
003800*                                 FLAGGA KARANTÄN                         
003900*                                 FLAG QUARANTINE                         
004000*** END OF VILMAII-COPY LENGTH= 34 BYTES                                  
