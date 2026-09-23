000100*** EDIT ALLOWED                                                          
000200*                            *************************************        
000300*                            ***                                          
000400*                            *** ANVÄNDS FöR ATT öVERSÄTTA                
000500*                            *** SUPLIER TILL PARMANUMMER                 
000600*                            *** I TRANSAR FÖR DIRECT-BUSINESS.           
000700*                            ***                                          
000800*                            *************************************        
000900*                                                                         
001000*   TABELL FÖR ATT SÖKA LEVERANTöRSNUMMER.                                
002000*                                                                         
003000*                                                                         
004000 01  SUPL-LEVNR-VALUES.                                                   
005000************************************PROGLANSULEVNR                        
006000     03  FILLER    PIC X(16) VALUE 'VCONAUSCA0357989'.                    
006010     03  FILLER    PIC X(16) VALUE 'VCONAUSWU0356683'.                    
006100     03  FILLER    PIC X(16) VALUE 'VCONBELAN0014465'.                    
006110     03  FILLER    PIC X(16) VALUE 'VCONBELAU0397428'.                    
006200     03  FILLER    PIC X(16) VALUE 'VCONBELBE0399561'.                    
006201     03  FILLER    PIC X(16) VALUE 'VCONBELDI0417846'.                    
006210     03  FILLER    PIC X(16) VALUE 'VCONBELGD0172739'.                    
006300     03  FILLER    PIC X(16) VALUE 'VCONBELHA0311387'.                    
007200     03  FILLER    PIC X(16) VALUE 'VCONBELIN0256473'.                    
007600     03  FILLER    PIC X(16) VALUE 'VCONBELLH0399326'.                    
007610     03  FILLER    PIC X(16) VALUE 'VCONBELOH0427047'.                    
007620     03  FILLER    PIC X(16) VALUE 'VCONBELVE0416887'.                    
007700     03  FILLER    PIC X(16) VALUE 'VCONBELWU0172412'.                    
008100     03  FILLER    PIC X(16) VALUE 'VCONBRISU0342877'.                    
008110     03  FILLER    PIC X(16) VALUE 'VCONBRIWU0210145'.                    
008120     03  FILLER    PIC X(16) VALUE 'VCONCZEWU0417577'.                    
008200     03  FILLER    PIC X(16) VALUE 'VCONFRABE0380283'.                    
008300     03  FILLER    PIC X(16) VALUE 'VCONFRAFA0382099'.                    
008400     03  FILLER    PIC X(16) VALUE 'VCONFRAGE0381017'.                    
008410     03  FILLER    PIC X(16) VALUE 'VCONFRAKO0380282'.                    
008500     03  FILLER    PIC X(16) VALUE 'VCONFRASN0360038'.                    
008510     03  FILLER    PIC X(16) VALUE 'VCONFRASO0380281'.                    
008600     03  FILLER    PIC X(16) VALUE 'VCONFRAWU0229964'.                    
009000     03  FILLER    PIC X(16) VALUE 'VCONGERBE0134004'.                    
010000     03  FILLER    PIC X(16) VALUE 'VCONGERFO0227715'.                    
020000     03  FILLER    PIC X(16) VALUE 'VCONGERWU0358563'.                    
020100     03  FILLER    PIC X(16) VALUE 'VCONIREWU0414689'.                    
030000     03  FILLER    PIC X(16) VALUE 'VCONJPACA0027074'.                    
030010     03  FILLER    PIC X(16) VALUE 'VCONJPASP0433244'.                    
030100     03  FILLER    PIC X(16) VALUE 'VCONJPAWU0027655'.                    
040000     03  FILLER    PIC X(16) VALUE 'VCONNETIN0271285'.                    
040100     03  FILLER    PIC X(16) VALUE 'VCONNORAT0427946'.                    
040200     03  FILLER    PIC X(16) VALUE 'VCONNORHO0415027'.                    
040300     03  FILLER    PIC X(16) VALUE 'VCONNORSK0425755'.                    
050000     03  FILLER    PIC X(16) VALUE 'VCONNORWU0265626'.                    
050100     03  FILLER    PIC X(16) VALUE 'VCONPOLWU0386719'.                    
050200     03  FILLER    PIC X(16) VALUE 'VCONPORBS0435019'.                    
050300     03  FILLER    PIC X(16) VALUE 'VCONPORWU0405399'.                    
060000     03  FILLER    PIC X(16) VALUE 'VCONSWIES0361114'.                    
060100     03  FILLER    PIC X(16) VALUE 'VCONSWIWU0146900'.                    
070000     03  FILLER    PIC X(16) VALUE 'VMERBELSC0019633'.                    
080000     03  FILLER    PIC X(16) VALUE 'VMERBRISC0019633'.                    
090000     03  FILLER    PIC X(16) VALUE 'VMERITASC0019633'.                    
100000     03  FILLER    PIC X(16) VALUE 'VMERNETSC0019633'.                    
110000     03  FILLER    PIC X(16) VALUE 'VSERAUSVO0024888'.                    
120000     03  FILLER    PIC X(16) VALUE 'VSERBELVO0024888'.                    
120010     03  FILLER    PIC X(16) VALUE 'VSERBRIVC0366086'.                    
120100     03  FILLER    PIC X(16) VALUE 'VSERBRIVO1183063'.                    
120200     03  FILLER    PIC X(16) VALUE 'VSERFRAAT0421161'.                    
120300     03  FILLER    PIC X(16) VALUE 'VSERGERVC0113732'.                    
120310     03  FILLER    PIC X(16) VALUE 'VSERGERVO0024888'.                    
120320     03  FILLER    PIC X(16) VALUE 'VSERIREVC0369743'.                    
120400     03  FILLER    PIC X(16) VALUE 'VSERNETVO0024888'.                    
120500     03  FILLER    PIC X(16) VALUE 'VSERNORVO0024888'.                    
120600     03  FILLER    PIC X(16) VALUE 'VSERSPAVO0024888'.                    
120700     03  FILLER    PIC X(16) VALUE 'VSERSWIVO0024888'.                    
120800     03  FILLER    PIC X(16) VALUE 'VTYRAUSAT0481995'.                    
120800     03  FILLER    PIC X(16) VALUE 'VTYRAUSCO0435798'.                    
120801     03  FILLER    PIC X(16) VALUE 'VTYRAUSGO0413590'.                    
120802     03  FILLER    PIC X(16) VALUE 'VTYRAUSMI0286681'.                    
120810     03  FILLER    PIC X(16) VALUE 'VTYRAUSNO0381976'.                    
120900     03  FILLER    PIC X(16) VALUE 'VTYRAUSPI0327677'.                    
121000     03  FILLER    PIC X(16) VALUE 'VTYRAUSRJ0420418'.                    
121100     03  FILLER    PIC X(16) VALUE 'VTYRBELCO0014616'.                    
121200     03  FILLER    PIC X(16) VALUE 'VTYRBELDK0367086'.                    
121210     03  FILLER    PIC X(16) VALUE 'VTYRBELGO0024250'.                    
121300     03  FILLER    PIC X(16) VALUE 'VTYRBELMI0024524'.                    
121400     03  FILLER    PIC X(16) VALUE 'VTYRBELPI0004667'.                    
121410     03  FILLER    PIC X(16) VALUE 'VTYRBELWY0406745'.                    
121500     03  FILLER    PIC X(16) VALUE 'VTYRBRICP1180196'.                    
121600     03  FILLER    PIC X(16) VALUE 'VTYRBRIKW1180828'.                    
121700     03  FILLER    PIC X(16) VALUE 'VTYRBRIMD0360287'.                    
121800     03  FILLER    PIC X(16) VALUE 'VTYRBRIPI1180196'.                    
121900     03  FILLER    PIC X(16) VALUE 'VTYRBRIST1182992'.                    
122000     03  FILLER    PIC X(16) VALUE 'VTYRDENCO0101283'.                    
122100     03  FILLER    PIC X(16) VALUE 'VTYRDENGO0024913'.                    
122200     03  FILLER    PIC X(16) VALUE 'VTYRDENMI0024912'.                    
122201     03  FILLER    PIC X(16) VALUE 'VTYRFINCO0185338'.                    
122202     03  FILLER    PIC X(16) VALUE 'VTYRFINMI0320287'.                    
122203     03  FILLER    PIC X(16) VALUE 'VTYRFINNO0007660'.                    
122204     03  FILLER    PIC X(16) VALUE 'VTYRFINPI0416309'.                    
122205     03  FILLER    PIC X(16) VALUE 'VTYRFINVI0175018'.                    
122210     03  FILLER    PIC X(16) VALUE 'VTYRFRACO0380234'.                    
122220     03  FILLER    PIC X(16) VALUE 'VTYRFRAGO0449544'.                    
122250     03  FILLER    PIC X(16) VALUE 'VTYRFRAMI0380339'.                    
122260     03  FILLER    PIC X(16) VALUE 'VTYRFRANO0381976'.                    
122270     03  FILLER    PIC X(16) VALUE 'VTYRFRAPI0371783'.                    
122700     03  FILLER    PIC X(16) VALUE 'VTYRGERBR0014363'.                    
122800     03  FILLER    PIC X(16) VALUE 'VTYRGERCO0016210'.                    
122900     03  FILLER    PIC X(16) VALUE 'VTYRGERDU0006058'.                    
123000     03  FILLER    PIC X(16) VALUE 'VTYRGERFU0348045'.                    
123100     03  FILLER    PIC X(16) VALUE 'VTYRGERGO0014370'.                    
124000     03  FILLER    PIC X(16) VALUE 'VTYRGERMI0014416'.                    
124100     03  FILLER    PIC X(16) VALUE 'VTYRGERNO0318976'.                    
125000     03  FILLER    PIC X(16) VALUE 'VTYRGERPI0006495'.                    
126000     03  FILLER    PIC X(16) VALUE 'VTYRGERPS0326764'.                    
127000     03  FILLER    PIC X(16) VALUE 'VTYRIRECO0424461'.                    
127010     03  FILLER    PIC X(16) VALUE 'VTYRIREFA0414709'.                    
127100     03  FILLER    PIC X(16) VALUE 'VTYRITAGO0024914'.                    
128000     03  FILLER    PIC X(16) VALUE 'VTYRITAMI0024526'.                    
129000     03  FILLER    PIC X(16) VALUE 'VTYRITAPI0024249'.                    
130000     03  FILLER    PIC X(16) VALUE 'VTYRJPACO0425664'.                    
130200     03  FILLER    PIC X(16) VALUE 'VTYRJPAGO0475354'.                    
130010     03  FILLER    PIC X(16) VALUE 'VTYRJPAMI0020522'.                    
130100     03  FILLER    PIC X(16) VALUE 'VTYRJPAPI0020523'.                    
130200     03  FILLER    PIC X(16) VALUE 'VTYRJPASU0396647'.                    
140000     03  FILLER    PIC X(16) VALUE 'VTYRNETMT0023379'.                    
150000     03  FILLER    PIC X(16) VALUE 'VTYRNETVB0357854'.                    
150100     03  FILLER    PIC X(16) VALUE 'VTYRNETWL0429605'.                    
160000     03  FILLER    PIC X(16) VALUE 'VTYRNORCO0100225'.                    
170000     03  FILLER    PIC X(16) VALUE 'VTYRNORGO0100470'.                    
171000     03  FILLER    PIC X(16) VALUE 'VTYRNORMI0100360'.                    
172000     03  FILLER    PIC X(16) VALUE 'VTYRNORNO0333729'.                    
173000     03  FILLER    PIC X(16) VALUE 'VTYRNORPI0110659'.                    
173010     03  FILLER    PIC X(16) VALUE 'VTYRNORPI0433928'.                    
173100     03  FILLER    PIC X(16) VALUE 'VTYRPOLCO0406039'.                    
173100     03  FILLER    PIC X(16) VALUE 'VTYRPOLGO0453616'.                    
173101     03  FILLER    PIC X(16) VALUE 'VTYRPOLMI0411105'.                    
173102     03  FILLER    PIC X(16) VALUE 'VTYRPOLNO0403255'.                    
173110     03  FILLER    PIC X(16) VALUE 'VTYRPOLPI0348655'.                    
173111     03  FILLER    PIC X(16) VALUE 'VTYRPORCO0025909'.                    
173130     03  FILLER    PIC X(16) VALUE 'VTYRPORMI0025181'.                    
173112     03  FILLER    PIC X(16) VALUE 'VTYRPORNE0436164'.                    
173120     03  FILLER    PIC X(16) VALUE 'VTYRPORPI0355039'.                    
173200     03  FILLER    PIC X(16) VALUE 'VTYRSPACO0024245'.                    
173300     03  FILLER    PIC X(16) VALUE 'VTYRSPAMI0011743'.                    
173400     03  FILLER    PIC X(16) VALUE 'VTYRSPAPI0024246'.                    
173500     03  FILLER    PIC X(16) VALUE 'VTYRSWIBR0223966'.                    
173600     03  FILLER    PIC X(16) VALUE 'VTYRSWICO0144879'.                    
173700     03  FILLER    PIC X(16) VALUE 'VTYRSWIES0361114'.                    
173700     03  FILLER    PIC X(16) VALUE 'VTYRSWIFI0501738'.                    
173800     03  FILLER    PIC X(16) VALUE 'VTYRSWIGO0223964'.                    
173900     03  FILLER    PIC X(16) VALUE 'VTYRSWIMI0147056'.                    
174000     03  FILLER    PIC X(16) VALUE 'VTYRSWINO0316464'.                    
175000     03  FILLER    PIC X(16) VALUE 'VTYRSWIPI0146988'.                    
175100     03  FILLER    PIC X(16) VALUE 'VTYRSWIVR0333665'.                    
175200     03  FILLER    PIC X(16) VALUE 'VTYRTURPI0025934'.                    
175700*                                                                         
175800 01  SUPL-LEVNR-TAB       REDEFINES SUPL-LEVNR-VALUES.                    
175900     03  SUPL-LEVNR-ING   OCCURS 137 TIMES                                
176000                          ASCENDING KEY IS SUPL-SOK                       
177000                          INDEXED BY SUPL-IX.                             
178000       05  SUPL-SOK            PIC X(9).                                  
179000       05  SUPL-LEVNR          PIC 9(7).                                  
180000*                                                                         
190000*                                                                         
200000*** END COPY W463SUPL    LENGTH=2128                                      
