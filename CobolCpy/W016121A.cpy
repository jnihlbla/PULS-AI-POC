000100 01  121A-W016121A.                                                       
000200*                                 COPYTEXT FÖR VCOM-MOTTAGNING            
000300*                                 KDCALL  = TYP AV OUTPUT                 
000400*                                   001 FIL SKAPAS                        
000500*                                   002 FIL SKAPAS MED EN FÖRSTA          
000600*                                       POST ENL. CTEXT W016001A          
000700*                                   003 FIL SKAPAS  + EN FIL MED          
000800*                                       EN POST   CTEXT W016001A          
000900*                                   OCH EN POST   CTEXT W016011A          
001000*                                 KVLRECL = RECORDLÄNGD FRÅN VCOM         
001100*                                 KVTRANS = ANTAL TRANSAR                 
001200*                                           PER VCOM-ÖVERFÖRING           
001300     03 121A-IDCPYTXT.                                                    
001400*                                 COPYTEXT IDENTITET                      
001500*                                 IDENTITY OF A COPYTEXT                  
001600        05 121A-CT-IDSYSTEM  PIC X(4).                                    
001700*                                 VOLVO VCAS SYSTEMNUMMER                 
001800*                                 VOLVO VCAS SYSTEM NUMBER                
001900        05 121A-CT-IDPTYP    PIC X(3).                                    
002000*                                 POSTTYP                                 
002100*                                 RECORD TYPE                             
002200        05 121A-CT-IDVTYP    PIC X.                                       
002300*                                 POSTTYPSVERSION                         
002400*                                 RECORD TYPE VERSION                     
002500     03 121A-FILLER          PIC X.                                       
002600     03 121A-KDCALL          PIC X(3).                                    
002700*                                 ANROPSTYP                               
002800*                                 CALL TYPE                               
002900     03 121A-FILLER          PIC X.                                       
003000     03 121A-KVLRECL         PIC 9(4).                                    
003100*                                 LRECL I ETT VARIABELT RECORD            
003200*                                 LRECL I A VARIABLE RECORD               
003300     03 121A-FILLER          PIC X.                                       
003400     03 121A-KVTRANS         PIC 9(3).                                    
003500*                                 ANTAL TRANSAKTIONER                     
003600*                                 NUMBER OF TRANSACTIONS                  
003700     03 121A-FILLER          PIC X.                                       
003800     03 121A-IDVCINIT        PIC X(8).                                    
003900*                                 VCOM INITIATOR PROGRAM NAMN             
004000*                                 VCOM INITIATOR PROGRAM NAME             
004100     03 121A-FILLER          PIC X(10).                                   
004200     03 121A-NOTERING        PIC X(40).                                   
004300*** END OF VILMAII-COPY LENGTH= 80 BYTES                                  
