000100 01  W4266801.                                                            
000200*                                 UPPFÖLJNINGSREG.                        
000300*                                 PÅ HISTORIK FIL                         
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDLOPNRM             PIC S9(9)           COMP-3.                  
000700*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
000800*                                 (0VVDLLLLK)                             
000900     03 ADKVAULG             PIC X(2).                                    
001000*                                 PLATS UNDERLAG KVAL.KONTROLL            
001100     03 BEANST               PIC X(25).                                   
001200*                                 ANSTÄLLDS NAMN                          
001300     03 FLANNULL             PIC X.                                       
001400*                                 ANNULLATION                             
001500     03 FLKVARED             PIC X.                                       
001600*                                 REDUCERAD KONTROLL FLAGGA               
001700     03 FLKVAUTV-ANT         PIC X.                                       
001800*                                 PARTIET UTVALT FÖR ANTALSKONTR          
001900     03 FLKVAUTV-KVAL        PIC X.                                       
002000*                                 PARTIET UTVALT KVALITETSKONTR           
002100     03 FLSKPSAK             PIC X.                                       
002200*                                 FLAGGA SKIPLOT-SÄKRAD                   
002300     03 IDARTNR              PIC S9(9)           COMP-3.                  
002400*                                 ARTIKELNUMMER                           
002500     03 IDLEVNR              PIC X(5).                                    
002600*                                 LEVERANTÖRNUMMER                        
002700     03 IDUSER-PRI           PIC X(8).                                    
002800*                                 ANVÄNDAR-ID PRIMÄRKONTROLL              
002900     03 IDUSER-SEK           PIC X(8).                                    
003000*                                 ANVÄNDAR-ID SEKUNDÄRKONTROLL            
003100     03 KDKVAKTL.                                                         
003200*                                 KVALITETSKONTROLL KOD                   
003300        05 KDKVATYP          PIC X.                                       
003400*                                 NORMAL/VERIFIKATIONS KVAL.KONTR         
003500        05 IDPROVPL-PRI      PIC X.                                       
003600*                                 PROVTAGNINGSPLAN PRIM.KONTROLL          
003700        05 IDPROVPL-SEK      PIC X.                                       
003800*                                 PROVTAGNINGSPLAN SEK.KONTROLL           
003900        05 KDKVAULG          PIC X.                                       
004000*                                 UNDERLAG FÖR KVALITETSKONTROLL          
004100     03 KDKVASTA-ANT         PIC X.                                       
004200*                                 STATUS ANTALSKONTROLL                   
004300     03 KDKVASTA-PRI         PIC X.                                       
004400*                                 STATUS PRIMÄRKONTROLL                   
004500     03 KDKVASTA-SEK         PIC X.                                       
004600*                                 STATUS SEKUNDÄRKONTROLL                 
004700     03 KVKVAPRIM            PIC S9(7)           COMP-3.                  
004800*                                 ANTAL TILL PRIMÄRKONTROLL               
004900     03 KVKVASEK             PIC S9(7)           COMP-3.                  
005000*                                  ANTAL TILL SEKUNDÄRKONTROLL            
005100     03 TIREGDAT             PIC S9(7)           COMP-3.                  
005200*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
005300*** END OF VILMAII-COPY LENGTH= 85 BYTES                                  
