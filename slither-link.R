params <- list(

  heuristic <- function(node, goal)
  {
    cost = maximalEvaluation(node)
    matrixOfLines = createMatrixOfPoints(node)
    evaluate = evaluate(node)
    ret= cost-evaluate

    return (ret)
  },

  distance <- function(node, parent, parentDistance) parentDistance+1,

  # Neighbourhood function generates the moves feasible from a given board configuration
  # index 1 stands for left line
  # index 2 stands for top line
  # index 3 stands for right line
  # index 4 stands for bottom line

  neighbours <- function(node)
  {
  res = list()

  if(isStartingState(node))
  {
    for(r in 1:nrow(node))
    {
      for(c in 1:ncol(node))
      {
        res[length(res)+1] = list(nodeForInitialState(node, 1, r, c))
        res[length(res)+1] = list(nodeForInitialState(node, 2, r, c))

        if(c == ncol(node))
          res[length(res)+1] = list(nodeForInitialState(node, 3, r, c))

        if(r == nrow(node))
          res[length(res)+1] = list(nodeForInitialState(node, 4, r, c))
        }
      }
    }
    else
    {
      lastLine = findLastLine(node)
      x = lastLine[1]
      y = lastLine[2]
      if(lastLine[4] != 1 && !isTheSamePoint(x, y, lastLine[3], lastLine[4]-1))
      {
        #mozna w lewo
        newX = lastLine[3]-1
        newY = lastLine[4]-1

        if(lastLine[3] != 1 )
          res[length(res)+1] = list(nodeForInitialState(node, 4, newX, newY))  

        newX = lastLine[3]
        newY = lastLine[4]-1  
        
        if(lastLine[3] != nrow(node) +1)
          res[length(res)+1] = list(nodeForInitialState(node, 2, newX, newY))
      }
      if(lastLine[3] != nrow(node)+1 && !isTheSamePoint(x, y, lastLine[3]+1, lastLine[4]))
      {
        #można w dol
        newX = lastLine[3]
        newY = lastLine[4]

         if(lastLine[4] != ncol(node)+1 )
          res[length(res)+1] = list(nodeForInitialState(node, 1, newX, newY))

        newX = lastLine[3]
        newY = lastLine[4]-1  

        if(lastLine[4] != 1)
         res[length(res)+1] = list(nodeForInitialState(node, 3, newX, newY))  
      }
      if(lastLine[4] != ncol(node)+1 && !isTheSamePoint(x, y, lastLine[3], lastLine[4]+1))
      {
        #można w prawo
        newX = lastLine[3]-1
        newY = lastLine[4]

        if(lastLine[3] != 1)
          res[length(res)+1] = list(nodeForInitialState(node, 4, newX, newY))

        newX = lastLine[3]
        newY = lastLine[4]    

        if(lastLine[3] != nrow(node)+1)
         res[length(res)+1] = list(nodeForInitialState(node, 2, newX, newY)) 
      }
      if(lastLine[3] != 1 && !isTheSamePoint(x, y, lastLine[3]-1, lastLine[4]))
      {
        #można w góre
        newX = lastLine[3]-1
        newY = lastLine[4]-1   

        if(lastLine[4] != 1)
          res[length(res)+1] = list(nodeForInitialState(node, 3, newX, newY))

        newX = lastLine[3]-1
        newY = lastLine[4]     

        if(lastLine[4] != ncol(node)+1)
         res[length(res)+1] = list(nodeForInitialState(node, 1, newX, newY)) 
      }
    }

    return (res)
  }
)

isTheSamePoint <- function(r,c , newR, newC)
{
    if(r == newR && c == newC)
      return (TRUE)
    else
      return (FALSE)  
}

isFinished <- function(node){
 
   matrixOfPoints = matrix ()
   if(is.na(node))
      return (FALSE)

 for(r in 1:nrow(node))
 {
    for(c in 1:ncol(node))
    {
      if(node[[r,c]][1] != sum(node[[r,c]][2:5]) && node[[r,c]][1] != -1) 
        return (FALSE)
      else
      {
        if(node[[r,c]][2] == 1) # 2 = left line
          matrixOfPoints = addPointToMatrix(matrixOfPoints, r,c, r+1, c)
        if(node[[r,c]][3] == 1) # 3 = top line
          matrixOfPoints = addPointToMatrix(matrixOfPoints, r,c, r, c+1)
        if(node[[r,c]][4] == 1) # 4 = right line
          matrixOfPoints = addPointToMatrix(matrixOfPoints, r,c+1, r+1, c+1)
        if(node[[r,c]][5] == 1) # 5 = bottom line
          matrixOfPoints = addPointToMatrix(matrixOfPoints, r+1,c, r+1, c+1)
      }
    }
 }  
 
  return (isOneLoop((matrixOfPoints)))   
}

createMatrixOfPoints <- function(node)
{
  matrixOfPoints = matrix ()
   if(is.na(node[1,1]))
      return (matrixOfPoints)

 for(r in 1:nrow(node))
 {
    for(c in 1:ncol(node))
    {
        if(node[[r,c]][2] == 1) # 2 = left line
          matrixOfPoints = addPointToMatrix(matrixOfPoints, r,c, r+1, c)
        if(node[[r,c]][3] == 1) # 3 = top line
          matrixOfPoints = addPointToMatrix(matrixOfPoints, r,c, r, c+1)
        if(node[[r,c]][4] == 1) # 4 = right line
          matrixOfPoints = addPointToMatrix(matrixOfPoints, r,c+1, r+1, c+1)
        if(node[[r,c]][5] == 1) # 5 = bottom line
          matrixOfPoints = addPointToMatrix(matrixOfPoints, r+1,c, r+1, c+1)
    }
 }  

 return (matrixOfPoints)
}

addPointToMatrix <- function(matrix, startX, endX, startY, endY)
{
  if(is.na(matrix [1,1]))
     matrix = matrix(c(startX, endX, startY, endY), ncol=4)
  else
  {
      if(!checkIfAdded(matrix, startX, endX, startY, endY))
        matrix = rbind(matrix, c(startX, endX, startY, endY))
  }

  return (matrix)
}

checkIfAdded <- function(matrix, startX, endX, startY, endY)
{
  if(is.na(matrix[1,1]))
     return (FALSE)

  for(i in 1:nrow(matrix))
  {
    currRow = matrix[i,] 
    if(currRow[1] == startX && currRow[2] == endX && currRow[3] == startY && currRow[4] == endY)

      return (TRUE)
  }

  return (FALSE)
}


isOneLoop <- function(matrix)
{  
startingPoint = c(matrix[1,1],matrix[1,2])
tempRow = matrix[1,]
rowIndex = 1
  
  while(nrow(matrix) > 1)
  {
    if(is.na(matrix[1,1]))
      return (FALSE)

    for(i in 1:nrow(matrix))
    {
      if(tempRow[3] == matrix[i, 1] && tempRow[4] == matrix[i, 2] && i != rowIndex)
       {
        tempRow = matrix[i,]
        matrix = matrix(matrix[-rowIndex,], ncol=4)

        if (rowIndex < i)
          rowIndex = i-1
        else
          rowIndex = i

        break
      }
      else if(tempRow[3] == matrix[i, 3] && tempRow[4] == matrix[i, 4] && i != rowIndex)
      {
        tempRow = c(matrix[i,3],matrix[i,4],matrix[i,1],matrix[i,2]) 
        matrix = matrix(matrix[-rowIndex,], ncol=4)

        if (rowIndex < i)
          rowIndex = i-1
        else
          rowIndex = i

        break
      }

      if (i == nrow(matrix))
      {
      	return (FALSE)
      }
    }
  }

  if (tempRow[3] == startingPoint[1] && tempRow[4] == startingPoint[2])
    return (TRUE)
  else
  {
    return (FALSE)
  }

}

isStartingState <- function(node)
{ 
  if(is.na(node[1,1]))
      return (TRUE)

  for(r in 1:nrow(node))
  {
    for(c in 1:ncol(node))
    {
      if(node[[r,c]][2]==1 || node[[r,c]][3]==1 || node[[r,c]][4]==1 || node[[r,c]][5]==1)
        return (FALSE)
    }
  }

  return (TRUE)
}

nodeForInitialState <- function(node, direction, r,c)
{   
   	res = node
    if(direction == 1)
    {
      if(c==1)
        res[[r,c]][2] = 1
      else
      {
        res[[r,c]][2] = 1
        res[[r,c-1]][4] = 1
      }     
    }
    else if(direction == 2 )
    {
      if(r==1)
        res[[r,c]][3] = 1
      else
      {
        res[[r,c]][3] = 1
        res[[r-1,c]][5] = 1
      }
    }
    else if(direction == 3)
    {
      if(c==ncol(res))
        res[[r,c]][4] = 1
      else
      {
        res[[r,c]][4] = 1  
        res[[r,c+1]][2] = 1
      }
    }
    else 
    {
      if(r==nrow(res))
        res[[r,c]][5] = 1
      else
      {
        res[[r,c]][5] = 1
        res[[r+1,c]][3] = 1
      }  
    }

    return (res)
  }


findLastLine <- function(node)
{
	matrix = createMatrixOfPoints(node)
	startingPoint = c(matrix[1,1],matrix[1,2])
	tempRow = matrix[1,]
	rowIndex = 1
  
  while(nrow(matrix) > 1)
  {
    if(is.na(matrix[1,1]))
      return (tempRow)

    for(i in 1:nrow(matrix))
    {
      if(tempRow[3] == matrix[i, 1] && tempRow[4] == matrix[i, 2] && i != rowIndex) 
      {
        tempRow = matrix[i,]
        matrix = matrix(matrix[-rowIndex,], ncol=4)

        if (rowIndex < i)
          rowIndex = i-1
        else
          rowIndex = i

        break
      }
      else if(tempRow[3] == matrix[i, 3] && tempRow[4] == matrix[i, 4] && i != rowIndex)
      {
        tempRow = c(matrix[i,3],matrix[i,4],matrix[i,1],matrix[i,2]) 
        matrix = matrix(matrix[-rowIndex,], ncol=4)

        if (rowIndex < i)
          rowIndex = i-1
        else
          rowIndex = i

        break
      }

      if (i == nrow(matrix))
      {
        return (tempRow)
      }
    }
  }
	return (tempRow)
}

 maximalEvaluation <- function(node)
 {
  outerEdges = nrow(node) * 2 + ncol(node) * 2
  innerEdges = (nrow(node) - 1) * ncol(node) + (ncol(node) - 1) * nrow(node)

  return (outerEdges * 3 + innerEdges * 6)
 }

evaluate <- function(node)
{
 totalCost = 0
 matrixOfLines = createMatrixOfPoints(node)

if(is.na(matrixOfLines[1,1]))
  return (totalCost)

      for(r in 1:nrow(matrixOfLines))
      {
          lastLine = matrixOfLines[r,]
          x = lastLine[1]
          y = lastLine[2]

        if(lastLine[4] != 1 && !isTheSamePoint(x, y, lastLine[3], lastLine[4]-1))
        {
          #mozna w lewo
          newX = lastLine[3]-1
          newY = lastLine[4]-1

          newX2 = lastLine[3]
          newY2 = lastLine[4]-1  
            
            if(lastLine[3] != 1 && node[[newX,newY]][1] != 0)
            {
              if(node[[newX,newY]][1] != -1)
                totalCost = totalCost + node[[newX,newY]][1]
            }
            else if(lastLine[3] != nrow(node) +1)
            {
                if(node[[newX2,newY2]][1] != -1)
                  totalCost = totalCost - node[[newX2,newY2]][1]
            }

            if(lastLine[3] != nrow(node) +1 && node[[newX2,newY2]][1] != 0)
            {
              if(node[[newX2,newY2]][1] != -1)
              totalCost = totalCost + node[[newX2,newY2]][1]
            }
            else if(lastLine[3] != 1 )
            {
              if(node[[newX,newY]][1] != -1)
              totalCost = totalCost - node[[newX,newY]][1]
            }
           
        }
        if(lastLine[3] != nrow(node)+1 && !isTheSamePoint(x, y, lastLine[3]+1, lastLine[4]))
        {
          #można w dol
          newX = lastLine[3]
          newY = lastLine[4]

          newX2 = lastLine[3]
          newY2 = lastLine[4]-1  

           if(lastLine[4] != ncol(node)+1 && node[[newX,newY]][1] != 0)
           {
              if(node[[newX,newY]][1] != -1)
             	totalCost = totalCost + node[[newX,newY]][1]
           }
           else if(lastLine[4] != 1)
           {
              if(node[[newX2,newY2]][1] != -1)
                 totalCost = totalCost - node[[newX2,newY2]][1]
           }
            if(lastLine[4] != 1 && node[[newX2,newY2]][1] != 0)
            {
                if(node[[newX2,newY2]][1] != -1)
              	totalCost = totalCost + node[[newX2,newY2]][1]
            }
            else if(lastLine[4] != ncol(node)+1)
            {
                if(node[[newX,newY]][1] != -1)
              	totalCost = totalCost - node[[newX,newY]][1]
            }

        }
        if(lastLine[4] != ncol(node)+1 && !isTheSamePoint(x, y, lastLine[3], lastLine[4]+1))
        {
          #można w prawo
          newX = lastLine[3]-1
          newY = lastLine[4]

          newX2 = lastLine[3]
          newY2 = lastLine[4]
   
            if(lastLine[3] != 1 && node[[newX,newY]][1] != 0)
            {
              if(node[[newX,newY]][1] != -1)
               totalCost = totalCost + node[[newX,newY]][1]
            }
            else if(lastLine[3] != nrow(node)+1){
                if(node[[newX2,newY2]][1] != -1)
                 totalCost = totalCost - node[[newX2,newY2]][1]
            }

            if(lastLine[3] != nrow(node)+1 && node[[newX2,newY2]][1] != 0)
            {
                if(node[[newX2,newY2]][1] != -1)
              totalCost = totalCost + node[[newX2,newY2]][1]
            }
            else if(lastLine[3] != 1)
            {
                if(node[[newX,newY]][1] != -1)
              totalCost = totalCost - node[[newX,newY]][1]
            }
        }
        if(lastLine[3] != 1 && !isTheSamePoint(x, y, lastLine[3]-1, lastLine[4]))
        {
          #można w góre
          newX = lastLine[3]-1
          newY = lastLine[4]-1  

          newX2 = lastLine[3]-1
          newY2 = lastLine[4]  

            if(lastLine[4] != 1 && node[[newX,newY]][1] != 0) 
            {
                if(node[[newX,newY]][1] != -1)
               totalCost = totalCost + node[[newX,newY]][1]
             }
            else if(lastLine[4] != ncol(node)+1)
            {
                if(node[[newX2,newY2]][1] != -1)
              totalCost = totalCost - node[[newX2,newY2]][1]
            }

            if(lastLine[4] != ncol(node)+1 && node[[newX2,newY2]][1] != 0)
            {
                if(node[[newX2,newY2]][1] != -1)
              totalCost = totalCost + node[[newX2,newY2]][1]
            }
            else if(lastLine[4] != 1 )
            {
                if(node[[newX,newY]][1] != -1)
              totalCost = totalCost - node[[newX,newY]][1]
            }
        }
      }

  return (totalCost)
}

res = astar::astar(input, isFinished, params)

#examples

input = matrix(c(list(c(3,0,0,0,0)), list(c(-1,0,0,0,0)), list(c(3,0,0,0,0)),
                 list(c(2,0,0,0,0)), list(c(-1,0,0,0,0)), list(c(-1,0,0,0,0)),
                 list(c(2,0,0,0,0)), list(c(-1,0,0,0,0)), list(c(2,0,0,0,0))), nrow = 3)

input = matrix(c(list(c(3,0,0,0,0)), list(c(3,0,0,0,0)), list(c(3,0,0,0,0)),
                 list(c(1,0,0,0,0)), list(c(2,0,0,0,0)), list(c(2,0,0,0,0)),
                 list(c(3,0,0,0,0)), list(c(2,0,0,0,0)), list(c(1,0,0,0,0))), nrow = 3)

input = matrix(c(list(c(3,0,0,0,0)), list(c(-1,0,0,0,0)), list(c(0,0,0,0,0)),
                 list(c(-1,0,0,0,0)), list(c(2,0,0,0,0)), list(c(2,0,0,0,0)),
                 list(c(2,0,0,0,0)), list(c(-1,0,0,0,0)), list(c(3,0,0,0,0))), nrow = 3)

input = matrix(c(list(c(1,0,0,0,0)), list(c(2,0,0,0,0)), list(c(-1,0,0,0,0)),
                 list(c(-1,0,0,0,0)), list(c(-1,0,0,0,0)), list(c(-1,0,0,0,0)),
                 list(c(2,0,0,0,0)), list(c(1,0,0,0,0)), list(c(3,0,0,0,0)),
                 list(c(-1,0,0,0,0)), list(c(-1,0,0,0,0)), list(c(-1,0,0,0,0))), nrow = 3)