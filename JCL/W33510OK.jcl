//W33510OK JOB (540W3350100W33510OK,W100),'RTN W335V1',                         
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
  TITLE WIN.XBMS.10                                                             
  OPTION FORCE                                                                  
  DEST  ELEONOR.OSTROM(A)VOLVO.COM                                              
  MEMO                                                                          
                                                                                
  Hej, detta är ett MEMO från FILEMONÖVERFÖRING XFERID=W335v1                   
                                                                                
  Fil WIN.w335v1.W33510(+1)  är nu katalogiserad                                
                                                                                
  kolla att den verkar ok.                                                      
                                                                                
)END                                                                            
/*                                                                              
