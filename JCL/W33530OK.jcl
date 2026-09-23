//W33530OK JOB (540W3350100W33530OK,W100),'RTN W335D3',                         
//         CLASS=K                                                              
/*JOBPARM TIME=60,LINES=30,CARDS=0,FORMS=1800,LINECT=00                         
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
/*AFTER MEMOAPIX                                                                
//*---  WMEMOSND,EXC                                                            
//*                                                                             
//MEMOSND EXEC WMEMOSND,DSIN=NULLFILE                                           
)SEND                                                                           
*       SÄNDER MEMO TILL SYSTEMAVD.                                             
  TITLE WIN.XBMS.30                                                             
  OPTION FORCE                                                                  
  DEST  ELEONOR.OSTROM(A)VOLVO.COM                                              
  MEMO                                                                          
                                                                                
  Hej, detta är ett MEMO från FILEMONÖVERFÖRING XFERID=W335d3                   
                                                                                
  Fil WIN.w335d3.W33530(+1)  är nu katalogiserad                                
                                                                                
  kolla att den verkar ok eller ta bort det här memot.                          
                                                                                
)END                                                                            
/*                                                                              
