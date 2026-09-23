000100* GENERATION OF COBOL HOST STRUCTURE FROM T01CURR-TAB                     
000200  01 T01CURR.                                                             
000300*              T01CURR                                                    
000400   03 IDLEGSEL        PIC X(4).                                           
000500*              FAKTURERANDE FÖRETAG TEX VCCS                              
000600   03 KDVALISO        PIC X(5).                                           
000700*              VALUTAKOD ENLIGT ISO-STANDARD.                             
000800   03 DASTADAT        PIC X(8).                                           
000900*              GENERELLT STARTDATUM                                       
001000   03 REVALUTA        PIC S9(5) COMP-3.                                   
001100*              OMRÄKNINGSTAL FÖR VALUTA                                   
001200   03 PRKURS          PIC S9(6)V9(5) COMP-3.                              
001300*              VALUTAKURS                                                 
001400   03 DAREGDAT        PIC X(8).                                           
001500*              REGISTRERINGSDATUM (ÅÅÅÅMMDD)                              
001600   03 REVALUTA-FROM   PIC S9(5) COMP-3.                                   
001700*              OMRÄKNINGSFAKTOR FRÅN HUVUDVALUTA TILL ANDRA VALUTO        
001800   03 REVALUTA-TO     PIC S9(5) COMP-3.                                   
001900*              OMRÄKNINGSFAKTOR TILL HUVUDVALUTA FROM ANDRA VALUTO        
002000   03 PRKURS-NEW      PIC S9(6)V9(6) COMP-3.                              
002100*              VALUTAKURS                                                 
002200   03 IDUSER          PIC X(8).                                           
002300*              ANVÄNDARENS SÄKERHETS ID                                   
002400*                                                                         
002500*** END OF VILMAII-COPY LENGTH= 55 OLD LENGTH=                            
