000100 01  MOD-W5O16401.                                                        
000200*                                 MOD-COPYTEXT FÖR W5016400               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-UT       PIC 9(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDDC-UT          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MOD-IDHUVTYP-UT      PIC X(4).                                    
001200*                                 LOGGTYP EKONOMISK HÄNDELSE              
001300     03 MOD-IDSUBTYP-UT      PIC X(3).                                    
001400*                                 LOGGTYP EKONOMISK HÄNDELSE              
001500     03 MOD-TIREGDAT-FOM-UT  PIC 9(6).                                    
001600*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001700     03 MOD-TIREGDAT-TOM-UT  PIC 9(6).                                    
001800*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001900     03 MOD-IDTRANS-UT       PIC X(4).                                    
002000*                                 BILDNUMMER                              
002100     03 MOD-BEART-UT         PIC X(25).                                   
002200*                                 ARTIKELBENÄMNING                        
002300     03 MOD-IDDC             PIC X(2).                                    
002400*                                 IDENTIFIERARE LAGER                     
002500     03 MOD-IDHUVTYP         PIC X(4).                                    
002600*                                 LOGGTYP EKONOMISK HÄNDELSE              
002700     03 MOD-IDSUBTYP         PIC X(3).                                    
002800*                                 LOGGTYP EKONOMISK HÄNDELSE              
002900     03 MOD-IDTRANS-UT2      PIC X(4).                                    
003000*                                 BILDNUMMER                              
003100     03 MOD-KVART-SALDO      PIC Z(5)9-.                                  
003200*                                 ANTAL SALDOFÖRÄNDRADE ARTIKLAR          
003300     03 MOD-IDTECKEN-KVAKS-PAV                                            
003400                             PIC X.                                       
003500     03 MOD-KVAKS-PAV        PIC Z(6)9-.                                  
003600*                                 DEL AV AK PÅ VÄG                        
003700     03 MOD-IDTECKEN-KVAKS   PIC X.                                       
003800     03 MOD-KVAKS            PIC Z(6)9-.                                  
003900*                                 ANKOMSTSALDO                            
004000     03 MOD-IDTECKEN-KVEFRS  PIC X.                                       
004100     03 MOD-KVEFRS           PIC Z(6)9-.                                  
004200*                                 EJ FAKTURERAT ANTAL STYCK               
004300     03 MOD-IDTECKEN-KVLS    PIC X.                                       
004400     03 MOD-KVLS             PIC Z(6)9-.                                  
004500*                                 LAGERSALDO                              
004600     03 MOD-IDPGM            PIC X(8).                                    
004700*                                 PROGRAM IDENTITET                       
004800     03 MOD-IDUSER           PIC X(8).                                    
004900*                                 ANVÄNDARENS SÄKERHETS ID                
005000     03 MOD-TIREGDAT         PIC 9(6).                                    
005100*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
005200     03 MOD-TIKLOCK          PIC 9(9).                                    
005300*                                 KLOCKSLAG (TTMMSSTH)                    
005400     03 MOD-LADD-DAT         PIC 9(6).                                    
005500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
005600     03 MOD-REF1             PIC X(25).                                   
005700     03 MOD-REF2             PIC X(25).                                   
005800     03 MOD-TEMFSINF         PIC X(55).                                   
005900*                                 INFORMATIONSMEDDELANDE                  
006000*** END OF VILMAII-COPY LENGTH= 301 BYTES                                 
