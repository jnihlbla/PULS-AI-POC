000100 01  MOD-W4O35101.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD 4351              
000300*                                 UTSKRIFT AV PLOCKSATS                   
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDPRC-IN         PIC X(2).                                    
000900*                                 MFS BEHANDLING AV INPUTFÄLT             
001000     03 MOD-IDPRC-UT.                                                     
001100*                                 PRODUKTIONSKANAL                        
001200        05 MOD-IDPRCBAS      PIC X(3).                                    
001300*                                 PRC-BAS                                 
001400        05 MOD-IDPRCVAR      PIC X.                                       
001500*                                 PRC-VARIANT                             
001600     03 MOD-IDUSER-IN        PIC X(2).                                    
001700*                                 MFS BEHANDLING AV INPUTFÄLT             
001800     03 MOD-IDUSER-UT        PIC X(8).                                    
001900*                                 ANVÄNDARENS SÄKERHETS ID                
002000     03 MOD-IDBORD-IN        PIC X(2).                                    
002100*                                 MFS BEHANDLING AV INPUTFÄLT             
002200     03 MOD-IDBORD-UT        PIC X(3).                                    
002300*                                 PACK-BORD                               
002400     03 MOD-IDDC-IN          PIC X(2).                                    
002500*                                 IDENTIFIERARE LAGER                     
002600     03 MOD-IDDC-UT          PIC X(2).                                    
002700*                                 IDENTIFIERARE LAGER                     
002800     03 MOD-KDPRT-PU-ATTR    PIC X(2).                                    
002900*                                 MFS ATTRIBUTFÄLT                        
003000     03 MOD-KDPRT-PU         PIC X(3).                                    
003100*                                 PRINTERKOD PACKUNDERLAG                 
003200     03 MOD-KDPRT-PLE-ATTR   PIC X(2).                                    
003300*                                 MFS ATTRIBUTFÄLT                        
003400     03 MOD-KDPRT-PLE        PIC X(3).                                    
003500*                                 PRINTERKOD PLOCKETIKETTER               
003600     03 MOD-FLORDKNY-ATTR    PIC X(2).                                    
003700*                                 MFS ATTRIBUTFÄLT                        
003800     03 MOD-FLORDKNY         PIC X.                                       
003900*                                 KNYTER ORDER ELLER ORDERDEL             
004000*                                 TILL EN PLOCKARE                        
004100*                                 ANVÄNDS NÄR INTE HELA ORDERN            
004200*                                 PLOCKAS SAMTIDIGT                       
004300*                                 J=HELA ORDERDELEN TILL                  
004400*                                 SAMMA PACKARE                           
004500*                                 N=VEM SOM HELST FÅR TA UT               
004600*                                 RESTERANDE RADER                        
004700     03 MOD-MIXAT-ATTR       PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900     03 MOD-MIXAT            PIC X.                                       
005000*                                 ALLMÄN SVARSFLAGGA                      
005100     03 MOD-UTSKR-EJ-KOMPL-PS-ATTR                                        
005200                             PIC X(2).                                    
005300*                                 MFS ATTRIBUTFÄLT                        
005400     03 MOD-UTSKR-EJ-KOMPL-PS                                             
005500                             PIC X.                                       
005600*                                 ALLMÄN SVARSFLAGGA                      
005700     03 MOD-KVORDER          PIC Z(6)9.                                   
005800*                                 ANTAL ORDER                             
005900     03 MOD-KVRADER          PIC Z(4)9.                                   
006000*                                 ANTAL RADER                             
006100     03 MOD-IDPRC.                                                        
006200*                                 PRODUKTIONSKANAL                        
006300        05 MOD-IDPRCBAS      PIC X(3).                                    
006400*                                 PRC-BAS                                 
006500        05 MOD-IDPRCVAR      PIC X.                                       
006600*                                 PRC-VARIANT                             
006700     03 MOD-VKORDNTO         PIC Z(5)9.9.                                 
006800*                                 ORDERVIKT NETTO (KG)                    
006900     03 MOD-VLORDNTO         PIC Z(3)9.9(3).                              
007000*                                 ORDERVOLYM NETTO (M3)                   
007100     03 MOD-FRAKTDATA        PIC X(40).                                   
007200*                                 FÖRRÅDSDATAKRAV                         
007300     03 MOD-BELAGINS-GRP.                                                 
007400*                                 LAGERINSTRUKTIONER                      
007500        05 MOD-BELAGINS-DEL1 PIC X(60).                                   
007600*                                 DEL AV LAGERINSTRUKTION                 
007700        05 MOD-BELAGINS-DEL2 PIC X(60).                                   
007800*                                 DEL AV LAGERINSTRUKTION                 
007900     03 MOD-TEMFSINF         PIC X(55).                                   
008000*                                 INFORMATIONSMEDDELANDE                  
008100*** END OF VILMAII-COPY LENGTH= 335 BYTES                                 
