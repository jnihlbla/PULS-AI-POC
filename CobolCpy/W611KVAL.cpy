000010 01  KVAL-W611KVAL.                                                       
000020*                                 LÄNKAREA TILL W611KVAL -                
000030*                                 GER SVAR PÅ FRÅGA ANTAL TILL KO         
000040*                                 NTROLL                                  
000050     03 KVAL-IDLEVNR         PIC S9(5)           COMP-3.                  
000060*                                 LEVERANTÖRNUMMER                        
000070     03 KVAL-IDARTNR         PIC S9(9)           COMP-3.                  
000080*                                 ARTIKELNUMMER                           
000090     03 KVAL-IDLOPNRM        PIC S9(9)           COMP-3.                  
000100*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
000110*                                 (0VVDLLLLK)                             
000120     03 KVAL-KVAVIS          PIC S9(7)           COMP-3.                  
000130*                                 AVISERAT ANTAL                          
000140     03 KVAL-FLKVARED        PIC X.                                       
000150*                                 REDUCERAD KONTROLL FLAGGA               
000160     03 KVAL-KVKVAPRIM       PIC S9(7)           COMP-3.                  
000170*                                 ANTAL TILL PRIMÄRKONTROLL               
000180     03 KVAL-KVKVASEK        PIC S9(7)           COMP-3.                  
000190*                                  ANTAL TILL SEKUNDÄRKONTROLL            
000200     03 KVAL-KDKVAANT        PIC 9.                                       
000210*                                 KOD ANTALSKONTR SKALL UTFÖRAS           
000220     03 KVAL-FLKVAKAR        PIC X.                                       
000230*                                 FLAGGA KARANTÄN                         
      *** END COPY W611KVAL    LENGTH=28                                        
