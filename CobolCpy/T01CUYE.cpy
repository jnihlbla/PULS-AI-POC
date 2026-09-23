000100* GENERATION OF COBOL HOST STRUCTURE FROM T01CUYE-TAB                     
000200  01 T01CUYE.                                                             
000300*              T01CUYE                                                    
000400   03 IDLEGSEL        PIC X(4).                                           
000500*              FAKTURERANDE F÷RETAG TEX VCCS                              
000600   03 KDVALISO        PIC X(5).                                           
000700*              VALUTAKOD ENLIGT ISO-STANDARD.                             
000800   03 DASTADAT        PIC X(8).                                           
000900*              GENERELLT STARTDATUM                                       
001000   03 REVALUTA-FROM   PIC S9(5) COMP-3.                                   
001100*              OMRƒKNINGSFAKTOR FR≈N HUVUDVALUTA TILL ANDRA VALUTO        
001200   03 REVALUTA-TO     PIC S9(5) COMP-3.                                   
001300*              OMRƒKNINGSFAKTOR TILL HUVUDVALUTA FROM ANDRA VALUTO        
001400   03 PRKURS-NEW      PIC S9(6)V9(6) COMP-3.                              
001500*              VALUTAKURS                                                 
001600   03 DAREGDAT        PIC X(8).                                           
001700*              REGISTRERINGSDATUM (≈≈≈≈MMDD)                              
001800   03 DAUPPDAT        PIC X(8).                                           
001900*              UPPDATERINGSDATUM  (≈≈≈≈MMDD)                              
002000   03 DADELDAT        PIC X(8).                                           
002100*              BORTTAGSDATUM      (≈≈≈≈MMDD)                              
002200   03 IDUSER          PIC X(8).                                           
002300*              ANVƒNDARENS SƒKERHETS ID                                   
002400*                                                                         
002500*** END OF VILMAII-COPY LENGTH= 62 OLD LENGTH=                            
